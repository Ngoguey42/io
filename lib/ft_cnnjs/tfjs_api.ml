(* Js_of_ocaml bindings for the tensorflow.js library.

   It is not exhaustive, I implemented what I needed.

   It also contains helper functions to avoid using tensorflows' losses, optimizers, training loops
   and layer/model abstraction in order to create workflows as functional as possible.
   - Gradients can be computed using the `variable_grads` function.
   - Several optimizers are hard coded
   - Tensor normalisation is hard coded for several algorithms
   - `Tfjs_api.Ops` should be favored against `Tfjs_api.Layer`.

   It also provides conversions between tensor and genarray/typed_array/float/int

   Designed using tfjs=1.5.2

   Tfjs documentation : https://js.tensorflow.org/api/latest/#class:Tensor
    /!\ The official documentation is not exhaustive.

 *)
module Js = Js_of_ocaml.Js
module Typed_array = Js_of_ocaml.Typed_array
module Firebug = Js_of_ocaml.Firebug

(* Types  *************************************************************************************** *)

type uint8_ta = (int, [ `Uint8 ]) Typed_array.typedArray

type float32_ta = (float, [ `Float32 ]) Typed_array.typedArray

type int32_ta = (int, [ `Int32 ]) Typed_array.typedArray

type float32_ba = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Genarray.t

type int32_ba = (int, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Genarray.t

type backend = [ `Webgl | `Cpu | `Wasm ]

class type symbolicTensor = object end

(**
Tfjs.Tensor.shape == Numpy.Ndarray.shape == Torch.tensor.size() == Torch.tensor.shape
Tfjs.Tensor.rank  == Numpy.Ndarray.ndim  == Torch.tensor.dim()  == Torch.tensor.ndim
Tfjs.Tensor.size  == Numpy.Ndarray.size  == Torch.tensor.numel()
 *)
class type tensor =
  object
    method shape : int Js.js_array Js.t Js.readonly_prop

    method size : int Js.readonly_prop

    method rank : int Js.readonly_prop

    method asType : Js.js_string Js.t -> tensor Js.t Js.meth

    method asScalar : tensor Js.t Js.meth

    method print : unit Js.meth

    method reshape : int Js.js_array Js.t -> tensor Js.t Js.meth

    method toString : Js.js_string Js.t Js.meth

    method data_float : float32_ta Js.t Ft_js.promise Js.t Js.meth

    method data_int : int32_ta Js.t Ft_js.promise Js.t Js.meth

    method dataSync_float32 : float32_ta Js.t Js.meth

    method dataSync_int32 : int32_ta Js.t Js.meth

    method arraySync_int : int Js.js_array Js.t Js.meth

    method arraySync_float : float Js.js_array Js.t Js.meth

    method arraySync_intScalar : int Js.meth

    method arraySync_floatScalar : float Js.meth
  end

class type variable =
  object
    inherit tensor

    method assign : tensor Js.t -> unit Js.meth
  end

class type layer_variable =
  object
    method name : Js.js_string Js.t Js.readonly_prop

    (* Tfjs documentation states that `.val` only contains the initial value of the variable.
     * The optimizers inside tfjs all use tf.engine().registeredVariables to retrieve their
     * variables, but it seems that using `layer.(bias|weight).val` works too.
     *)
    method val_ : variable Js.t Js.readonly_prop
  end

module Named_tensor_map = struct
  class type ['a] js_t =
    object
      constraint 'a = #tensor Js.t
    end

  include Map.Make (String)

  let of_js : 'a js_t Js.t -> 'a t =
   fun map_js ->
    Js.object_keys map_js |> Js.to_array |> Array.to_list
    |> List.fold_left (fun map k -> add (Js.to_string k) (Js.Unsafe.get map_js k) map) empty
end

class type layer =
  object
    method getWeights : tensor Js.t Js.js_array Js.t Js.meth

    method apply : symbolicTensor Js.t -> symbolicTensor Js.t Js.meth

    method apply_array : symbolicTensor Js.t Js.js_array Js.t -> symbolicTensor Js.t Js.meth

    method getClassName : Js.js_string Js.t Js.meth

    method name : Js.js_string Js.t Js.readonly_prop
  end

class type conv2d =
  object
    inherit layer

    method kernel : layer_variable Js.t Js.readonly_prop

    method bias : layer_variable Js.t Js.readonly_prop
  end

class type optimizer =
  object
    method applyGradients_array : tensor Js.t Js.js_array Js.t -> unit Js.meth

    method applyGradients_map : tensor Js.t Named_tensor_map.js_t Js.t -> unit Js.meth
  end

class type adam =
  object
    inherit optimizer
  end

class type model =
  object
    inherit layer

    (* About the model##apply method:
     * (On tfjs:1.5.2) the parameter cannot be a `model` instance because it causes an exception
     * about some fictional dtype mismatches.
     * i.e.: m1.apply(m0) // fails (should return a model?)
     *       m1.apply(m0.outputLayers[0]) // works (returns a symbolicTensor)
     * Use the `chain` function below to concatenate two indenpendant trees
     *)
    method layers : layer Js.t Js.js_array Js.t Js.readonly_prop

    method trainOnBatch : tensor Js.t -> tensor Js.t -> unit Ft_js.promise Js.t Js.meth

    method predict : tensor Js.t -> tensor Js.t Js.meth
  end

class type memory =
  object
    method numTensors : int Js.readonly_prop
  end

(* ********************************************************************************************** *)
module Ops = struct
  open Js.Unsafe

  let ones : int array -> tensor Js.t =
   fun shape -> fun_call global##.tf##.ones [| shape |> Js.array |> inject |]

  let zeros : int array -> tensor Js.t =
   fun shape -> fun_call global##.tf##.zeros [| shape |> Js.array |> inject |]

  let reshape : int array -> #tensor Js.t -> tensor Js.t =
   fun shape x -> fun_call global##.tf##.reshape [| inject x; shape |> Js.array |> inject |]

  let shape : #tensor Js.t -> int array = fun x -> x##.shape |> Js.to_array

  let squeeze : ?axes:int list -> #tensor Js.t -> tensor Js.t =
   fun ?axes x ->
    let axes =
      match axes with
      | Some axes -> axes |> Array.of_list |> Js.array |> Js.Opt.return
      | None -> Js.Opt.empty
    in
    fun_call global##.tf##.squeeze [| inject x; inject axes |]

  let expand_dims : int -> #tensor Js.t -> tensor Js.t =
   fun axis x -> fun_call global##.tf##.expandDims [| inject x; inject axis |]

  let transpose : ?perm:int list -> #tensor Js.t -> tensor Js.t =
   fun ?perm x ->
    match perm with
    | None -> fun_call global##.tf##.transpose [| inject x |]
    | Some perm ->
        fun_call global##.tf##.transpose [| inject x; perm |> Array.of_list |> Js.array |> inject |]

  let flatten : #tensor Js.t -> tensor Js.t = fun x -> reshape [| -1 |] x

  let clip_by_value : float -> float -> #tensor Js.t -> tensor Js.t =
   fun min max x -> fun_call global##.tf##.clipByValue [| inject x; inject min; inject max |]

  let neg : #tensor Js.t -> tensor Js.t = fun x -> fun_call global##.tf##.neg [| inject x |]

  let sqrt : #tensor Js.t -> tensor Js.t = fun x -> fun_call global##.tf##.sqrt [| inject x |]

  let mul : #tensor Js.t -> #tensor Js.t -> tensor Js.t =
   fun x x' -> fun_call global##.tf##.mul [| inject x; inject x' |]

  let div : #tensor Js.t -> #tensor Js.t -> tensor Js.t =
   fun x x' -> fun_call global##.tf##.div [| inject x; inject x' |]

  let add : #tensor Js.t -> #tensor Js.t -> tensor Js.t =
   fun x x' -> fun_call global##.tf##.add [| inject x; inject x' |]

  let sub : #tensor Js.t -> #tensor Js.t -> tensor Js.t =
   fun x x' -> fun_call global##.tf##.sub [| inject x; inject x' |]

  let pow : #tensor Js.t -> #tensor Js.t -> tensor Js.t =
   fun x x' -> fun_call global##.tf##.pow [| inject x; inject x' |]

  let maximum : #tensor Js.t -> #tensor Js.t -> tensor Js.t =
   fun x x' -> fun_call global##.tf##.maximum [| inject x; inject x' |]

  let log : #tensor Js.t -> tensor Js.t = fun x -> fun_call global##.tf##.log [| inject x |]

  let sum : ?axis:int -> bool -> #tensor Js.t -> tensor Js.t =
   fun ?axis keepdims x ->
    let axis = match axis with None -> Js.Opt.empty | Some axis -> Js.Opt.return axis in
    fun_call global##.tf##.sum [| inject x; inject axis; inject keepdims |]

  let mean : ?axis:int -> bool -> #tensor Js.t -> tensor Js.t =
   fun ?axis keepdims x ->
    let axis = match axis with None -> Js.Opt.empty | Some axis -> Js.Opt.return axis in
    fun_call global##.tf##.mean [| inject x; inject axis; inject keepdims |]

  let min : ?axis:int -> bool -> #tensor Js.t -> tensor Js.t =
   fun ?axis keepdims x ->
    let axis = match axis with None -> Js.Opt.empty | Some axis -> Js.Opt.return axis in
    fun_call global##.tf##.min [| inject x; inject axis; inject keepdims |]

  let max : ?axis:int -> bool -> #tensor Js.t -> tensor Js.t =
   fun ?axis keepdims x ->
    let axis = match axis with None -> Js.Opt.empty | Some axis -> Js.Opt.return axis in
    fun_call global##.tf##.max [| inject x; inject axis; inject keepdims |]

  let relu : #tensor Js.t -> tensor Js.t = fun x -> fun_call global##.tf##.relu [| inject x |]

  let conv2d :
      ?s:int * int ->
      ?d:int * int ->
      ?b:[< `Valid | `Same ] ->
      #tensor Js.t ->
      #tensor Js.t ->
      tensor Js.t =
   fun ?s:(stride = (1, 1)) ?d:(dilation = (1, 1)) ?b:boundary_mode x kernel ->
    let stride = Js.array [| fst stride; snd stride |] in
    let dilation = Js.array [| fst dilation; snd dilation |] in
    let boundary_mode =
      match boundary_mode with None -> "same" | Some `Same -> "same" | Some `Valid -> "valid"
    in
    let boundary_mode = Js.string boundary_mode in
    fun_call
      global##.tf##.conv2d
      [|
        inject x;
        inject kernel;
        inject stride;
        inject boundary_mode;
        "NHWC" |> Js.string |> inject;
        inject dilation;
      |]

  let depthwise_conv2d :
      ?s:int * int ->
      ?d:int * int ->
      ?b:[< `Valid | `Same ] ->
      #tensor Js.t ->
      #tensor Js.t ->
      tensor Js.t =
   fun ?s:(stride = (1, 1)) ?d:(dilation = (1, 1)) ?b:boundary_mode x kernel ->
    let stride = Js.array [| fst stride; snd stride |] in
    let dilation = Js.array [| fst dilation; snd dilation |] in
    let boundary_mode =
      match boundary_mode with None -> "same" | Some `Same -> "same" | Some `Valid -> "valid"
    in
    let boundary_mode = Js.string boundary_mode in
    fun_call
      global##.tf##.depthwiseConv2d
      [|
        inject x;
        inject kernel;
        inject stride;
        inject boundary_mode;
        "NHWC" |> Js.string |> inject;
        inject dilation;
      |]

  let concat : int -> #tensor Js.t list -> tensor Js.t =
   fun axis tensors ->
    let tensors = tensors |> Array.of_list |> Js.array in
    fun_call global##.tf##.concat [| inject tensors; inject axis |]

  let softmax : int -> #tensor Js.t -> tensor Js.t =
   fun axis x -> fun_call global##.tf##.softmax [| inject x; inject axis |]

  let pad : ?value:float -> (int * int list) list -> #tensor Js.t -> tensor Js.t =
   fun ?(value = 0.) paddings_per_axis x ->
    let paddings =
      Array.init x##.rank (fun i ->
          match List.find_all (fun (ax, _) -> ax = i) paddings_per_axis with
          | _ :: _ :: _ -> invalid_arg "In padding: Duplicate axis"
          | [] -> Js.array [| 0; 0 |]
          | [ (_, [ p ]) ] -> Js.array [| p; p |]
          | [ (_, [ bef; aft ]) ] -> Js.array [| bef; aft |]
          | [ _ ] -> invalid_arg "In padding: padding list should contain 1 or 2 integers")
      |> Js.array
    in
    fun_call global##.tf##.pad [| inject x; inject paddings; inject value |]

  let slice : (int * int * int) list -> #tensor Js.t -> tensor Js.t =
   fun per_axis x ->
    let shape = x##.shape |> Js.to_array in
    let begins, sizes =
      List.init x##.rank (fun i ->
          match List.find_all (fun (ax, _, _) -> ax = i) per_axis with
          | _ :: _ :: _ -> invalid_arg "In padding: Duplicate axis"
          | [] -> (0, shape.(i))
          | [ (_, start, size) ] -> (start, size))
      |> List.split
    in
    let begins = begins |> Array.of_list |> Js.array in
    let sizes = sizes |> Array.of_list |> Js.array in
    fun_call global##.tf##.slice [| inject x; inject begins; inject sizes |]

  let topk : int -> #tensor Js.t -> tensor Js.t * tensor Js.t =
   fun k x ->
    let res = fun_call global##.tf##.topk [| inject x; inject k |] in
    (get res "values", get res "indices")

  let confusion_matrix : int -> #tensor Js.t -> #tensor Js.t -> tensor Js.t =
   fun class_count truth pred ->
    if truth##.rank <> 1 || pred##.rank <> 1 then
      invalid_arg "In confusion_matrix: Input tensors should have 1 dimension";
    fun_call
      global##.tf##.math##.confusionMatrix
      [| inject truth; inject pred; inject class_count |]

  let one_hot : int -> #tensor Js.t -> tensor Js.t =
   fun width x -> fun_call global##.tf##.oneHot [| inject x; inject width |]

  let astype : [< `Float32 | `Int32 ] -> #tensor Js.t -> tensor Js.t =
   fun dtype x ->
    let dtype = Js.string (match dtype with `Float32 -> "float32" | `Int32 -> "int32") in
    x##asType dtype

  let dot : #tensor Js.t -> #tensor Js.t -> tensor Js.t =
   fun x x' -> fun_call global##.tf##.dot [| inject x; inject x' |]

  let tensordot : (int * int) list -> #tensor Js.t -> #tensor Js.t -> tensor Js.t =
   fun contract_axes x y ->
    let normalize_axis ndim i =
      let i = if i < 0 then -(i + 1) else i in
      if i >= ndim then invalid_arg "In tensordot: Index out of bound";
      i
    in
    let none_if_mem l i = if List.mem i l then None else Some i in
    let prod l = List.fold_left (fun a b -> a * b) 1 l in
    let xcontract_axes, ycontract_axes = List.split contract_axes in

    let xshape = x##.shape |> Js.to_array in
    let xndim = x##.rank in
    let x_axes = List.init xndim (fun i -> i) in
    let xcontract_axes = List.map (normalize_axis xndim) xcontract_axes in
    let xkeep_axes = List.filter_map (none_if_mem xcontract_axes) x_axes in
    let xcontract_sizes = List.map (fun i -> xshape.(i)) xcontract_axes in
    let xkeep_sizes = List.map (fun i -> xshape.(i)) xkeep_axes in

    let yshape = y##.shape |> Js.to_array in
    let yndim = y##.rank in
    let y_axes = List.init yndim (fun i -> i) in
    let ycontract_axes = List.map (normalize_axis yndim) ycontract_axes in
    let ykeep_axes = List.filter_map (none_if_mem ycontract_axes) y_axes in
    let ycontract_sizes = List.map (fun i -> yshape.(i)) ycontract_axes in
    let ykeep_sizes = List.map (fun i -> yshape.(i)) ykeep_axes in

    let x = transpose ~perm:(xkeep_axes @ xcontract_axes) x in
    let x = reshape [| prod xkeep_sizes; prod xcontract_sizes |] x in
    let y = transpose ~perm:(ycontract_axes @ ykeep_axes) y in
    let y = reshape [| prod ycontract_sizes; prod ykeep_sizes |] y in
    let z = dot x y in
    let z = reshape (xkeep_sizes @ ykeep_sizes |> Array.of_list) z in
    z

  let range : int list -> [< `Float32 | `Int32 ] -> tensor Js.t =
   fun ints dtype ->
    let start, stop, step =
      match ints with
      | [ start; stop; step ] -> (start, stop, step)
      | [ start; stop ] -> (start, stop, 1)
      | [ stop ] -> (0, stop, 1)
      | _ -> invalid_arg "In range: int list should have lenght 1, 2 or 3"
    in
    let dtype = Js.string (match dtype with `Float32 -> "float32" | `Int32 -> "int32") in
    fun_call global##.tf##.range [| inject start; inject stop; inject step; inject dtype |]

  let maxpool2d : ?s:int * int -> ?b:[< `Valid | `Same ] -> int * int -> #tensor Js.t -> tensor Js.t
      =
   fun ?s:stride ?b:boundary_mode (ky, kx) x ->
    let filter_size = Js.array [| ky; kx |] in
    let stride = match stride with None -> filter_size | Some (sy, sx) -> Js.array [| sy; sx |] in
    let boundary_mode =
      match boundary_mode with None -> "same" | Some `Same -> "same" | Some `Valid -> "valid"
    in
    let boundary_mode = Js.string boundary_mode in
    fun_call
      global##.tf##.maxPool
      [| inject x; inject filter_size; inject stride; inject boundary_mode |]

  let avgpool :
      ?s:int list -> ?b:[< `Valid | `Same | `AssertFit ] -> int list -> #tensor Js.t -> tensor Js.t
      =
   fun ?s:strides ?b:boundary_mode kernel_sizes x ->
    let boundary_mode =
      match boundary_mode with None -> `Same | Some b -> (b :> [ `Valid | `Same | `AssertFit ])
    in
    let strides = match strides with None -> kernel_sizes | Some s -> s in

    if List.length kernel_sizes <> x##.rank then
      invalid_arg "In avgpool: kernel_sizes length should match tensor's ndim";
    if List.exists (fun ks -> ks <= 0) kernel_sizes then
      invalid_arg "In avgpool: kernel_sizes must be >= 1";
    if List.exists (fun ks -> ks <= 0) strides then
      invalid_arg "In avgpool: kernel_sizes must be >= 1";

    let strides = Array.of_list strides in
    let kernel_sizes = Array.of_list kernel_sizes in
    let b =
      Js.string
        (match boundary_mode with `AssertFit -> "valid" | `Same -> "same" | `Valid -> "valid")
    in
    let axes = List.init x##.rank (fun idx -> idx) in

    let aux x idx =
      let dims = Js.to_array x##.shape in
      let dim = dims.(idx) in
      let ks = kernel_sizes.(idx) in
      let s = strides.(idx) in
      if ks = 1 && s = 1 then x
      else
        let fits = float_of_int (dim - ks) /. float_of_int s |> Float.is_integer in
        if boundary_mode = `AssertFit && not fits then invalid_arg "In avgpool: `AssertFit failed";
        let prod l = List.fold_left ( * ) 1 l in
        let sizes_before =
          List.filter_map (fun i -> if i < idx then Some dims.(i) else None) axes
        in
        let sizes_after = List.filter_map (fun i -> if i > idx then Some dims.(i) else None) axes in

        let ks = Js.array [| ks; 1 |] in
        let s = Js.array [| s; 1 |] in
        let f x = fun_call global##.tf##.avgPool [| inject x; inject ks; inject s; inject b |] in
        x
        |> reshape [| prod sizes_before; dim; 1; prod sizes_after |]
        |> f
        |> reshape (sizes_before @ [ -1 ] @ sizes_after |> Array.of_list)
    in
    List.fold_left aux x axes

  let maxpool :
      ?s:int list -> ?b:[< `Valid | `Same | `AssertFit ] -> int list -> #tensor Js.t -> tensor Js.t
      =
   fun ?s:strides ?b:boundary_mode kernel_sizes x ->
    let boundary_mode =
      match boundary_mode with None -> `Same | Some b -> (b :> [ `Valid | `Same | `AssertFit ])
    in
    let strides = match strides with None -> kernel_sizes | Some s -> s in

    if List.length kernel_sizes <> x##.rank then
      invalid_arg "In maxpool: kernel_sizes length should match tensor's ndim";
    if List.exists (fun ks -> ks <= 0) kernel_sizes then
      invalid_arg "In maxpool: kernel_sizes must be >= 1";
    if List.exists (fun ks -> ks <= 0) strides then
      invalid_arg "In maxpool: kernel_sizes must be >= 1";

    let strides = Array.of_list strides in
    let kernel_sizes = Array.of_list kernel_sizes in
    let b =
      Js.string
        (match boundary_mode with `AssertFit -> "valid" | `Same -> "same" | `Valid -> "valid")
    in
    let axes = List.init x##.rank (fun idx -> idx) in

    let aux x idx =
      let dims = Js.to_array x##.shape in
      let dim = dims.(idx) in
      let ks = kernel_sizes.(idx) in
      let s = strides.(idx) in
      if ks = 1 && s = 1 then x
      else
        let fits = float_of_int (dim - ks) /. float_of_int s |> Float.is_integer in
        if boundary_mode = `AssertFit && not fits then invalid_arg "In maxpool: `AssertFit failed";
        let prod l = List.fold_left ( * ) 1 l in
        let sizes_before =
          List.filter_map (fun i -> if i < idx then Some dims.(i) else None) axes
        in
        let sizes_after = List.filter_map (fun i -> if i > idx then Some dims.(i) else None) axes in

        let ks = Js.array [| ks; 1 |] in
        let s = Js.array [| s; 1 |] in
        let f x = fun_call global##.tf##.maxPool [| inject x; inject ks; inject s; inject b |] in
        x
        |> reshape [| prod sizes_before; dim; 1; prod sizes_after |]
        |> f
        |> reshape (sizes_before @ [ -1 ] @ sizes_after |> Array.of_list)
    in
    List.fold_left aux x axes

  let upsample : int list -> #tensor Js.t -> tensor Js.t =
   fun expansion_factors x ->
    if List.length expansion_factors <> x##.rank then
      invalid_arg "In upsampling: expansion_factors length should match tensor's ndim";
    if List.exists (fun ks -> ks <= 0) expansion_factors then
      invalid_arg "In upsampling: expansion_factors must be >= 1";

    let expansion_factors = Array.of_list expansion_factors in
    let axes = List.init x##.rank (fun idx -> idx) in

    let aux x idx =
      let dims = Js.to_array x##.shape in
      let dim = dims.(idx) in
      let fact = expansion_factors.(idx) in
      if fact = 1 then x
      else
        let prod l = List.fold_left ( * ) 1 l in
        let sizes_before =
          List.filter_map (fun i -> if i < idx then Some dims.(i) else None) axes
        in
        let sizes_after = List.filter_map (fun i -> if i > idx then Some dims.(i) else None) axes in
        let fact = Js.array [| dim * fact; 1 |] in
        let f x =
          fun_call
            global##.tf##.image##.resizeNearestNeighbor
            [| inject x; inject fact; inject false |]
        in
        x
        |> reshape [| prod sizes_before; dim; 1; prod sizes_after |]
        |> f
        |> reshape (sizes_before @ [ -1 ] @ sizes_after |> Array.of_list)
    in
    List.fold_left aux x axes

  let ( * ) = mul

  let ( / ) = div

  let ( + ) = add

  let ( - ) = sub

  let ( ** ) = pow
end

(* Constructors ********************************************************************************* *)
let tensor_of_ta : int array -> ('a, 'b) Typed_array.typedArray Js.t -> tensor Js.t =
 fun shape ta ->
  let open Js.Unsafe in
  fun_call global##.tf##.tensor [| inject ta; shape |> Js.array |> inject |]

let tensor_of_ba (type a b) : (a, b, Bigarray.c_layout) Bigarray.Genarray.t -> tensor Js.t =
 fun arr ->
  match Bigarray.Genarray.kind arr with
  | Bigarray.Float32 -> tensor_of_ta (Bigarray.Genarray.dims arr) (Ft_js.Conv.Float32.ta_of_ba arr)
  | Bigarray.Int8_unsigned ->
      tensor_of_ta (Bigarray.Genarray.dims arr) (Ft_js.Conv.Uint8.ta_of_ba arr)
  | _ -> failwith "In tensor_of_ta: Unsopported dtype"

let ba_of_tensor (type a b) :
    (a, b) Bigarray.kind -> #tensor Js.t -> (a, b, Bigarray.c_layout) Bigarray.Genarray.t =
 fun kind tensor ->
  let tensor_astype dtype = tensor##asType (Js.string dtype) in
  let reshape arr = Bigarray.reshape arr (Js.to_array tensor##.shape) in
  match kind with
  | Bigarray.Float32 ->
      (tensor_astype "float32")##dataSync_float32 |> Ft_js.Conv.Float32.ba_of_ta |> reshape
  | _ -> failwith "In ba_of_tensor: Kind not implemented"

let variable :
    ?trainable:bool -> ?name:string -> ?dtype:[< `Float32 | `Int32 ] -> tensor Js.t -> variable Js.t
    =
 fun ?(trainable = false) ?name ?(dtype = `Float32) arr ->
  let open Js.Unsafe in
  let name =
    match name with None -> Js.Opt.empty | Some name -> name |> Js.string |> Js.Opt.return
  in
  let dtype = Js.string (match dtype with `Float32 -> "float32" | `Int32 -> "int32") in
  fun_call global##.tf##.variable [| inject arr; inject trainable; inject name; inject dtype |]

let float : float -> tensor Js.t =
 fun x ->
  let open Js.Unsafe in
  fun_call global##.tf##.scalar [| inject x; "float32" |> Js.string |> inject |]

let int : int -> tensor Js.t =
 fun x ->
  let open Js.Unsafe in
  fun_call global##.tf##.scalar [| inject x; "int32" |> Js.string |> inject |]

let to_float : #tensor Js.t -> float = fun x -> x##asScalar##arraySync_floatScalar

let to_int : #tensor Js.t -> int = fun x -> x##asScalar##arraySync_intScalar

(* Tensorflow model and optimizers  ************************************************************* *)
let model : symbolicTensor Js.t list -> symbolicTensor Js.t list -> model Js.t =
 fun inputs outputs ->
  let params =
    object%js (self)
      val inputs = inputs |> Array.of_list |> Js.array

      val outputs = outputs |> Array.of_list |> Js.array
    end
  in
  let open Js.Unsafe in
  fun_call global##.tf##.model [| inject params |]

let adam : float -> float -> float -> float -> adam Js.t =
 fun lr beta1 beta2 epsilon ->
  let open Js.Unsafe in
  fun_call global##.tf##.train##.adam [| inject lr; inject beta1; inject beta2; inject epsilon |]

let sgd : float -> optimizer Js.t =
 fun lr ->
  let open Js.Unsafe in
  fun_call global##.tf##.train##.sgd [| inject lr |]

let compile : model Js.t -> optimizer Js.t -> string -> unit =
 fun m optim loss ->
  let params =
    object%js (self)
      val optimizer = optim

      val loss = Js.string loss
    end
  in
  let open Js.Unsafe in
  meth_call m "compile" [| inject params |]

(* Tensorflow layers **************************************************************************** *)
module Layers = struct
  let input : ?dtype:[< `Float32 | `Int32 ] -> ?name:string -> int array -> symbolicTensor Js.t =
   fun ?(dtype = `Float32) ?name shape ->
    let open Js.Unsafe in
    let params =
      object%js (self)
        val shape = Js.array shape

        val dtype = Js.string (match dtype with `Float32 -> "float32" | `Int32 -> "int32")

        val name =
          match name with None -> Js.Opt.empty | Some name -> name |> Js.string |> Js.Opt.return
      end
    in
    fun_call global##.tf##.input [| params |> inject |]

  let conv2d :
      ?weights:float32_ba * float32_ba -> ?name:string -> _ -> bool -> _ -> int -> conv2d Js.t =
   fun ?weights ?name kernel_size padding stride out_filters ->
    let ky, kx = kernel_size in
    let sy, sx = stride in
    let padding = match padding with true -> "same" | false -> "valid" in
    let weights =
      match weights with
      | None -> Js.Opt.empty
      | Some (kernel, bias) ->
          let shape = Bigarray.Genarray.dims kernel in
          let kernel = kernel |> Ft_js.Conv.Float32.ta_of_ba |> tensor_of_ta shape in
          let shape = Bigarray.Genarray.dims bias in
          let bias = bias |> Ft_js.Conv.Float32.ta_of_ba |> tensor_of_ta shape in
          [| kernel; bias |] |> Js.array |> Js.Opt.return
    in
    let params =
      object%js (self)
        val kernelSize = Js.array [| kx; ky |]

        val filters = out_filters

        val strides = Js.array [| sx; sy |]

        val padding = Js.string padding

        val weights = weights

        val name =
          match name with None -> Js.Opt.empty | Some name -> name |> Js.string |> Js.Opt.return
      end
    in
    let open Js.Unsafe in
    fun_call global##.tf##.layers##.conv2d [| inject params |]

  let maxpool2d ?name kernel_size stride : layer Js.t =
    let ky, kx = kernel_size in
    let sy, sx = stride in
    let params =
      object%js (self)
        val poolSize = Js.array [| kx; ky |]

        val strides = Js.array [| sx; sy |]

        val name =
          match name with None -> Js.Opt.empty | Some name -> name |> Js.string |> Js.Opt.return
      end
    in
    let open Js.Unsafe in
    fun_call global##.tf##.layers##.maxPooling2d [| inject params |]

  let relu ?name () : layer Js.t =
    let open Js.Unsafe in
    let params =
      object%js (self)
        val name =
          match name with None -> Js.Opt.empty | Some name -> name |> Js.string |> Js.Opt.return
      end
    in
    fun_call global##.tf##.layers##.reLU [| inject params |]

  let softmax ?(axis = 3) ?name () : layer Js.t =
    let params =
      object%js (self)
        val axis = axis

        val name =
          match name with None -> Js.Opt.empty | Some name -> name |> Js.string |> Js.Opt.return
      end
    in
    let open Js.Unsafe in
    fun_call global##.tf##.layers##.softmax [| inject params |]

  let concatenate ?(axis = 3) ?name () : layer Js.t =
    let params =
      object%js (self)
        val axis = axis

        val name =
          match name with None -> Js.Opt.empty | Some name -> name |> Js.string |> Js.Opt.return
      end
    in
    let open Js.Unsafe in
    fun_call global##.tf##.layers##.concatenate [| inject params |]

  let add ?name () : layer Js.t =
    let params =
      object%js (self)
        val name =
          match name with None -> Js.Opt.empty | Some name -> name |> Js.string |> Js.Opt.return
      end
    in
    let open Js.Unsafe in
    fun_call global##.tf##.layers##.add [| inject params |]

  let classify : layer Js.t -> [> ] =
   fun l ->
    match Js.to_string l##getClassName with
    | "ReLU" -> `Relu l
    | "Softmax" -> `Softmax l
    | "MaxPooling2D" -> `Maxpool2d
    | "Conv2D" -> `Conv2d (Js.Unsafe.coerce l :> conv2d Js.t)
    | "Sequential" | "Model" -> `Model (Js.Unsafe.coerce l :> model Js.t)
    | name -> failwith ("unknown class name:" ^ name)

  let chain :
      symbolicTensor Js.t -> symbolicTensor Js.t list -> symbolicTensor Js.t -> symbolicTensor Js.t
      =
   fun a b_inputs b ->
    let b = model b_inputs [ b ] in
    b##apply a
end

(* Hard coded algorithms  *********************************************************************** *)
let categorical_crossentropy : float -> tensor Js.t -> tensor Js.t -> tensor Js.t =
 fun epsilon softmaxed_pred truth ->
  (* a truth element must be 0. or 1. *)
  softmaxed_pred
  |> Ops.clip_by_value epsilon (1. -. epsilon)
  |> Ops.log |> Ops.mul truth |> Ops.neg |> Ops.sum ~axis:(-1) true |> Ops.mean ~axis:0 true

let hinge : ?margin:float -> tensor Js.t -> tensor Js.t -> tensor Js.t =
 fun ?(margin = 1.) pred truth ->
  (* a truth element must be -1. or 1. *)
  let open Ops in
  float margin - (truth * pred) |> maximum (float 0.) |> sum ~axis:(-1) true |> mean ~axis:0 true

let iou_recall_precision_of_cm : #tensor Js.t -> tensor Js.t =
 fun cm ->
  let w =
    match cm##.shape |> Js.to_array with
    | [| v0; v1 |] ->
        if v0 <> v1 then invalid_arg "In recall_of_cm: invalid confusion matrix";
        v0
    | _ -> invalid_arg "In recall_of_cm: invalid confusion matrix"
  in
  let stats_of_cat catidx =
    let open Ops in
    let true_pos = Ops.slice [ (0, catidx, 1); (1, catidx, 1) ] cm in
    let false_neg = (Ops.slice [ (0, catidx, 1) ] cm |> Ops.sum true) - true_pos in
    let false_pos = (Ops.slice [ (1, catidx, 1) ] cm |> Ops.sum true) - true_pos in
    let true_pos = reshape [| 1 |] true_pos |> astype `Float32 in
    let false_neg = reshape [| 1 |] false_neg |> astype `Float32 in
    let false_pos = reshape [| 1 |] false_pos |> astype `Float32 in
    let iou = true_pos / (true_pos + false_neg + false_pos) in
    let recall = true_pos / (true_pos + false_neg) in
    let precision = true_pos / (true_pos + false_pos) in
    reshape [| 1; 3 |] (concat 0 [ iou; recall; precision ])
  in
  let res = List.init w stats_of_cat |> Ops.concat 0 in
  assert (Js.to_array res##.shape = [| w; 3 |]);
  res

(* Hard coded stateful algorithms *************************************************************** *)
let create_local_normaliser : float -> int list -> < normalise : #tensor Js.t -> tensor Js.t > =
 fun epsilon norm_axes ->
  if epsilon < 0. then invalid_arg "In create_local_normaliser: epsilon should be >=0";
  let epsilon = float epsilon in
  if List.length norm_axes = 0 then
    invalid_arg "In create_local_normaliser: norm_axes shouldn't be an empty list";
  object
    method normalise x =
      let dims = x##.shape |> Js.to_array in
      let ndim = Array.length dims in
      let norm_axes =
        List.map
          (fun ax ->
            let ax = if ax < 0 then -(ax + 1) else ax in
            if ax >= ndim then
              failwith "In create_local_normaliser#normalise: Input tensor is too small";
            ax)
          norm_axes
      in
      let kernel_sizes = List.init ndim (fun i -> if List.mem i norm_axes then 1 else dims.(i)) in

      let open Ops in
      let avg = avgpool ~b:`AssertFit kernel_sizes x in
      let x_centered = x - avg in
      let var = avgpool ~b:`AssertFit kernel_sizes (x_centered * x_centered) in
      x_centered / (var + epsilon |> sqrt)
  end

let create_global_normaliser :
    float ->
    int ->
    float32_ba ->
    float32_ba ->
    bool ->
    int list ->
    < normalise : #tensor Js.t -> tensor Js.t
    ; get_step : int
    ; get_avg : float32_ba
    ; get_var : float32_ba > =
 fun epsilon step moving_avg moving_var update norm_axes ->
  let err_hd = "In create_global_normaliser" in

  if epsilon < 0. then invalid_arg (err_hd ^ ": epsilon should be >=0");
  if List.length norm_axes = 0 then invalid_arg (err_hd ^ ": norm_axes shouldn't be an empty list");
  if Bigarray.Genarray.dims moving_avg <> Bigarray.Genarray.dims moving_var then
    invalid_arg (err_hd ^ ": moving_avg and moving_var should have the same shape");
  if Bigarray.Genarray.num_dims moving_avg <> List.length norm_axes then
    invalid_arg (err_hd ^ ": norm_axes incompatible with moving_avg and moving_var");

  let epsilon = float epsilon in
  let norm_axes_original = norm_axes in
  let norm_axes = List.sort compare norm_axes in
  let transpose_to_sorted tensor =
    let norm_axes = Array.of_list norm_axes in
    let perm =
      List.init (Array.length norm_axes) Fun.id
      |> List.map (fun dst_idx ->
             let ax = norm_axes.(dst_idx) in
             let src_idx =
               List.mapi (fun i j -> (i, j)) norm_axes_original
               |> List.find (fun (_, ax') -> ax = ax')
               |> fst
             in
             src_idx)
    in
    Ops.transpose ~perm tensor
  in
  let transpose_to_original tensor =
    let norm_axes_original = Array.of_list norm_axes_original in
    let perm =
      List.init (Array.length norm_axes_original) Fun.id
      |> List.map (fun src_idx ->
             let ax = norm_axes_original.(src_idx) in
             let dst_idx =
               List.mapi (fun i j -> (i, j)) norm_axes
               |> List.find (fun (_, ax') -> ax = ax')
               |> fst
             in
             dst_idx)
    in
    Ops.transpose ~perm tensor
  in

  let moving_avg = tensor_of_ba moving_avg |> transpose_to_sorted |> variable ~trainable:false in
  let moving_var = tensor_of_ba moving_var |> transpose_to_sorted |> variable ~trainable:false in
  let step = int step |> variable ~trainable:false in
  object
    method normalise x =
      let dims = Ops.shape x in
      let ndim = Array.length dims in
      let axes = List.init ndim Fun.id in
      let squeezed_axes = List.filter (fun i -> not (List.mem i norm_axes)) axes in
      let kernel_sizes = List.init ndim (fun i -> if List.mem i norm_axes then 1 else dims.(i)) in
      let dims' = Array.mapi (fun i s -> if List.mem i norm_axes then s else 1) dims in
      let dims'' =
        List.filter_map (fun i -> if List.mem i norm_axes then Some dims.(i) else None) axes
        |> Array.of_list
      in
      if dims'' <> Ops.shape moving_var then
        failwith (err_hd ^ "#normalise: Unexpected input tensor shape");
      let open Ops in
      if update then (
        step##assign (step + int 1);
        let momentum' = float 1. / step##asType (Js.string "float32") in
        let momentum = float 1. - momentum' in
        Printf.eprintf "%d %10.6f %10.6f\n%!" (to_int step) (to_float momentum') (to_float momentum);

        let avg = avgpool ~b:`AssertFit kernel_sizes x in
        let x_centered = x - avg in
        let var = avgpool ~b:`AssertFit kernel_sizes (x_centered * x_centered) in
        assert (shape avg = dims');

        let avg = squeeze ~axes:squeezed_axes avg in
        let var = squeeze ~axes:squeezed_axes var in
        assert (shape avg = dims'');

        moving_avg##assign ((momentum * moving_avg) + (momentum' * avg));
        moving_var##assign ((momentum * moving_var) + (momentum' * var)) );
      let moving_avg = reshape dims' moving_avg in
      let moving_var = reshape dims' moving_var in
      (x - moving_avg) / (moving_var + epsilon |> sqrt)

    method get_step = to_int step

    method get_avg = moving_avg |> transpose_to_original |> ba_of_tensor Bigarray.Float32

    method get_var = moving_var |> transpose_to_original |> ba_of_tensor Bigarray.Float32
  end

let create_exp_moving32_normaliser :
    float ->
    float ->
    float32_ba ->
    float32_ba ->
    bool ->
    int list ->
    < normalise : #tensor Js.t -> tensor Js.t ; get_avg : float32_ba ; get_var : float32_ba > =
 fun epsilon momentum moving_avg moving_var update norm_axes ->
  let err_hd = "In create_exp_moving32_normaliser" in

  if epsilon < 0. then invalid_arg (err_hd ^ ": epsilon should be >=0");
  if momentum < 0. || momentum > 1. then invalid_arg (err_hd ^ ": momentum should be >=0 and <=1");
  if List.length norm_axes = 0 then invalid_arg (err_hd ^ ": norm_axes shouldn't be an empty list");
  if Bigarray.Genarray.dims moving_avg <> Bigarray.Genarray.dims moving_var then
    invalid_arg (err_hd ^ ": moving_avg and moving_var should have the same shape");
  if Bigarray.Genarray.num_dims moving_avg <> List.length norm_axes then
    invalid_arg (err_hd ^ ": norm_axes incompatible with moving_avg and moving_var");

  let epsilon = float epsilon in
  let momentum = float momentum in
  let momentum' = Ops.sub (float 1.) momentum in
  let norm_axes_original = norm_axes in
  let norm_axes = List.sort compare norm_axes in
  let transpose_to_sorted tensor =
    let norm_axes = Array.of_list norm_axes in
    let perm =
      List.init (Array.length norm_axes) Fun.id
      |> List.map (fun dst_idx ->
             let ax = norm_axes.(dst_idx) in
             let src_idx =
               List.mapi (fun i j -> (i, j)) norm_axes_original
               |> List.find (fun (_, ax') -> ax = ax')
               |> fst
             in
             src_idx)
    in
    Ops.transpose ~perm tensor
  in
  let transpose_to_original tensor =
    let norm_axes_original = Array.of_list norm_axes_original in
    let perm =
      List.init (Array.length norm_axes_original) Fun.id
      |> List.map (fun src_idx ->
             let ax = norm_axes_original.(src_idx) in
             let dst_idx =
               List.mapi (fun i j -> (i, j)) norm_axes
               |> List.find (fun (_, ax') -> ax = ax')
               |> fst
             in
             dst_idx)
    in
    Ops.transpose ~perm tensor
  in

  let moving_avg = tensor_of_ba moving_avg |> transpose_to_sorted |> variable ~trainable:false in
  let moving_var = tensor_of_ba moving_var |> transpose_to_sorted |> variable ~trainable:false in

  object
    method normalise x =
      let dims = Ops.shape x in
      let ndim = Array.length dims in
      let axes = List.init ndim Fun.id in
      let squeezed_axes = List.filter (fun i -> not (List.mem i norm_axes)) axes in
      let kernel_sizes = List.init ndim (fun i -> if List.mem i norm_axes then 1 else dims.(i)) in
      let dims' = Array.mapi (fun i s -> if List.mem i norm_axes then s else 1) dims in
      let dims'' =
        List.filter_map (fun i -> if List.mem i norm_axes then Some dims.(i) else None) axes
        |> Array.of_list
      in
      if dims'' <> Ops.shape moving_var then
        failwith (err_hd ^ "#normalise: Unexpected input tensor shape");
      let open Ops in
      if update then (
        let avg = avgpool ~b:`AssertFit kernel_sizes x in
        let x_centered = x - avg in
        let var = avgpool ~b:`AssertFit kernel_sizes (x_centered * x_centered) in
        assert (shape avg = dims');

        let avg = squeeze ~axes:squeezed_axes avg in
        let var = squeeze ~axes:squeezed_axes var in
        assert (shape avg = dims'');

        moving_avg##assign ((momentum * moving_avg) + (momentum' * avg));
        moving_var##assign ((momentum * moving_var) + (momentum' * var)) );
      let moving_avg = reshape dims' moving_avg in
      let moving_var = reshape dims' moving_var in
      (x - moving_avg) / (moving_var + epsilon |> sqrt)

    method get_avg = moving_avg |> transpose_to_original |> ba_of_tensor Bigarray.Float32

    method get_var = moving_var |> transpose_to_original |> ba_of_tensor Bigarray.Float32
  end

let create_sgd_updater : variable Js.t -> < update : float -> tensor Js.t -> unit > =
 fun weights ->
  object
    method update lr grad =
      let open Ops in
      weights##assign (grad |> mul (float lr) |> neg |> add weights)
  end

let create_adam32_updater :
    float ->
    float ->
    float ->
    int ->
    float32_ba ->
    float32_ba ->
    variable Js.t ->
    < update : float -> tensor Js.t -> unit
    ; get_step : int
    ; get_rgrad : float32_ba
    ; get_rgrad_sq : float32_ba > =
 fun epsilon beta1 beta2 step rgrad rgrad_sq weights ->
  let beta1 = beta1 |> float in
  let beta2 = beta2 |> float in
  let beta1' = Ops.sub (float 1.) beta1 in
  let beta2' = Ops.sub (float 1.) beta2 in
  let epsilon = float epsilon in

  let step = step |> int |> variable ~dtype:`Int32 in
  let rgrad = tensor_of_ba rgrad |> variable ~trainable:false in
  let rgrad_sq = tensor_of_ba rgrad_sq |> variable ~trainable:false in

  object
    method update lr (grad : tensor Js.t) =
      let open Ops in
      step##assign (step + int 1);
      let correction1 = float 1. - (beta1 ** step) in
      let correction2 = float 1. - (beta2 ** step) in
      rgrad##assign ((rgrad * beta1) + (grad * beta1'));
      rgrad_sq##assign ((rgrad_sq * beta2) + (grad * grad * beta2'));
      weights##assign
        ( rgrad / correction1 / (sqrt (rgrad_sq / correction2) + epsilon)
        |> mul (float lr)
        |> neg |> add weights )

    method get_step = to_int step

    method get_rgrad = ba_of_tensor Bigarray.Float32 rgrad

    method get_rgrad_sq = ba_of_tensor Bigarray.Float32 rgrad_sq
  end

(* MISC. **************************************************************************************** *)
let variable_grads : (unit -> tensor Js.t) -> tensor Js.t * tensor Js.t Named_tensor_map.t =
 fun f ->
  let open Js.Unsafe in
  let ret = fun_call global##.tf##.variableGrads [| f |> Js.wrap_callback |> inject |] in
  (* What if multiple losses? would `value` be a tensor array ? *)
  let value : tensor Js.t = ret##.value in
  let grads : tensor Js.t Named_tensor_map.js_t Js.t = ret##.grads in
  (value, Named_tensor_map.of_js grads)

let engine_registered_variables : unit -> variable Js.t Named_tensor_map.t =
 fun () ->
  let open Js.Unsafe in
  let e = fun_call global##.tf##.engine [||] in
  let map_js = get e "registeredVariables" in
  Named_tensor_map.of_js map_js

(* Backend manipulations ************************************************************************ *)
let setup_backend : backend -> unit Lwt.t =
 fun b ->
  (* WASM backend is not mature yet (some bug and lack of implemented functions)
   * https://cdn.jsdelivr.net/npm/@tensorflow/tfjs-backend-wasm/dist/
   * https://github.com/tensorflow/tfjs/tree/master/tfjs-backend-wasm *)
  let open Js.Unsafe in
  let open Lwt.Infix in
  let b = Js.string (match b with `Webgl -> "webgl" | `Cpu -> "cpu" | `Wasm -> "wasm") in
  fun_call global##.tf##.setBackend [| b |> inject |] |> Ft_js.wrap_promise >>= fun _ ->
  fun_call global##.tf##.ready [||] |> Ft_js.wrap_promise

let memory : unit -> memory =
 fun () ->
  let open Js.Unsafe in
  fun_call global##.tf##.memory [||]

let disposeVariables : unit -> unit =
 fun () ->
  (* Dereference the `variables` inside the engine.
   * Always use it. And use in conjunction with a `tensor` dereferencing scheme below. *)
  let open Js.Unsafe in
  fun_call global##.tf##.disposeVariables [||]

let engine_reset : unit -> unit =
 fun () ->
  (* Seems to be the only reliable way to fully get rid of all the `tensor` memory leaks, when no
   * scoping was used. But it should not be called too often because the webgl initialization
   * takes ~3 seconds *)
  let open Js.Unsafe in
  let e = fun_call global##.tf##.engine [||] in
  meth_call e "reset" [||] |> ignore

let engine_start_scope : unit -> unit =
 fun () ->
  (* Start a tensor garbage collection scope *)
  let open Js.Unsafe in
  let e = fun_call global##.tf##.engine [||] in
  meth_call e "startScope" [||] |> ignore

let engine_end_scope : unit -> unit =
 fun () ->
  (* End a tensor garbage collection scope, dereferencing all `tensors` referenced since the
   * matching `engine_start_scope` *)
  let open Js.Unsafe in
  let e = fun_call global##.tf##.engine [||] in
  meth_call e "endScope" [||] |> ignore

let tidy : (unit -> unit) -> unit =
 fun f ->
  (* Wrap a synchronous computation in order to dispose the `tensors` referenced inside the engine.
   *)
  let open Js.Unsafe in
  fun_call global##.tf##.tidy [| f |> Js.wrap_callback |> inject |]

let tidy_lwt : (unit -> 'a Lwt.t) -> 'a Lwt.t =
 fun f ->
  (* Wrap an asynchronous computation in order to dispose the `tensors` referenced inside the
     engine.
  *)
  let f () =
    engine_start_scope ();
    f ()
  and f' () =
    engine_end_scope ();
    Lwt.return ()
  in
  Lwt.finalize f f'

let keep : tensor Js.t -> tensor Js.t =
 fun x ->
  let open Js.Unsafe in
  fun_call global##.tf##.keep [| x |> inject |]

let dispose_tensor : tensor Js.t -> unit =
 fun x ->
  let open Js.Unsafe in
  fun_call global##.tf##.dispose_tensor [| x |> inject |]
