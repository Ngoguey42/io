module Firebug = Js_of_ocaml.Firebug

type absolute_shape = (Pshape.Length.tag, Pshape.Size.tag, [ `Idx of int ]) Pshape.t

type storable_shape = Pshape.Axis.symbolic list option * absolute_shape

type storable_layer =
  [ `Input of string option * storable_shape * Fnn.dtype
  | `Astype of string option * Fnn.dtype
  | `Concatenate of string option * Pshape.Axis.t
  | `Conv2d of string option * int * (int * int) * (int * int) * Fnn.boundary_mode
  | `Maxpool2d of string option * Fnn.boundary_mode * (int * int) * (int * int)
  | `Parameter32 of
    string option
    * Fnn__.Init.float32
    * [ `Adam of float * float * float * int * Fnn.float32_tensor * Fnn.float32_tensor | `Sgd ]
    * Fnn.float32_tensor
  | `Normalisation of
    string option
    * Pshape.Axis.t list
    * [ `Exp_moving32 of float * float * Fnn.float32_tensor * Fnn.float32_tensor
      | `Global32 of float * int * Fnn.float32_tensor * Fnn.float32_tensor
      | `Local of float ]
  | `Relu of string option
  | `Softmax of string option * Pshape.Axis.t
  | `Sum of string option
  | `Prod of string option
  | `Padding of
    string option
    * [ `Constant of float | `Reflection | `Replication ]
    * (Pshape.Axis.t * int list) list
  | `Tensordot of
    string option
    * (Pshape.Axis.t * Pshape.Axis.t option) list
    * (Pshape.Axis.t * Pshape.Axis.t option) list
  | `Transpose of string option * int * (Pshape.Axis.t * Pshape.Axis.t) list ]

type storable_nn = int list * (int, int list) Hashtbl.t * (int, storable_layer) Hashtbl.t

let repair = function None -> None | Some s -> Some (String.sub s 0 (String.length s))

let storable_of_pshape : _ -> storable_shape =
 fun s ->
  let open Pshape in
  let s = to_any s in
  if is_symbolic s then
    let s = to_symbolic s in
    let axs = axes s in
    (Some axs, desymbolize axs s)
  else (None, s |> to_absolute)

let pshape_of_storable : storable_shape -> Pshape.any =
 fun s ->
  let open Pshape in
  match s with None, s -> s |> to_any | Some axs, s -> symbolize axs s |> to_any

let _storable_of_fnn : Fnn.network -> storable_layer =
 fun nn ->
  match nn#classify_layer with
  | `Input nn -> `Input (nn#id, storable_of_pshape nn#out_shape, nn#out_dtype)
  | `Parameter32 nn -> `Parameter32 (nn#id, nn#init, nn#optimizer, nn#tensor)
  | `Normalisation nn -> `Normalisation (nn#id, nn#axes, nn#algorithm)
  | `Sum nn -> `Sum nn#id
  | `Prod nn -> `Prod nn#id
  | `Concatenate nn -> `Concatenate (nn#id, nn#axis)
  | `Softmax nn -> `Softmax (nn#id, nn#axis)
  | `Relu nn -> `Relu nn#id
  | `Astype nn -> `Astype (nn#id, nn#dtype)
  | `Conv2d nn -> `Conv2d (nn#id, nn#group_count, nn#stride, nn#dilation, nn#boundary_mode)
  | `Transpose nn -> `Transpose (nn#id, Pshape.ndim nn#out_shape, nn#mapping)
  | `Maxpool2d nn -> `Maxpool2d (nn#id, nn#boundary_mode, nn#stride, nn#kernel_size)
  | `Padding nn ->
      `Padding
        ( nn#id,
          nn#value,
          List.map
            (fun ax ->
              let a, b = nn#paddings_of_axis ax in
              (ax, [ a; b ]))
            nn#axes )
  | `Tensordot nn -> `Tensordot (nn#id, nn#mapping0, nn#mapping1)

let storable_of_fnn : Fnn.network -> storable_nn =
 fun nn ->
  let ids = ref [] in
  let graph = Hashtbl.create 100 in
  let layers = Hashtbl.create 100 in
  let aux nn =
    ids := Oo.id nn :: !ids;
    Hashtbl.add graph (Oo.id nn) (List.map Oo.id nn#upstreams);
    Hashtbl.add layers (Oo.id nn) (_storable_of_fnn nn)
  in
  Fnn.iter_top_down aux [ nn ];
  (List.rev !ids, graph, layers)

let _fnn_of_storable builder store upstreams : Fnn.network =
  let module Builder = (val builder : Fnn.BUILDER) in
  match (store, upstreams) with
  | `Input (id, shape, dtype), [] ->
      Builder.input ~id:(repair id) (pshape_of_storable shape) dtype |> Fnn.downcast
  | `Input _, _ -> failwith "corrupted upstreams"
  | `Parameter32 (id, init, optimizer, tensor), [] ->
      let o = match optimizer with `Sgd -> `Sgd | `Adam (a, b, c, _, _, _) -> `Adam (a, b, c) in
      let nn = Builder.parameter32 ~id:(repair id) (Bigarray.Genarray.dims tensor) init o in
      let nn = nn#replicate tensor optimizer in
      nn |> Fnn.downcast
  | `Parameter32 _, _ -> failwith "corrupted upstreams"
  | `Normalisation (id, axes, algorithm), [ up ] ->
      let stats =
        match algorithm with
        | `Local eps -> `Local eps
        | `Global32 (eps, _, _, _) -> `Global eps
        | `Exp_moving32 (eps, mom, _, _) -> `Exp_moving (eps, mom)
      in
      let nn = Builder.normalisation ~id:(repair id) axes ~stats up in
      let nn = nn#replicate algorithm up in
      nn |> Fnn.downcast
  | `Normalisation _, _ -> failwith "corrupted upstreams"
  | `Sum id, ups -> Builder.sum ~id:(repair id) ups |> Fnn.downcast
  | `Prod id, ups -> Builder.prod ~id:(repair id) ups |> Fnn.downcast
  | `Concatenate (id, axis), ups -> Builder.concatenate ~id:(repair id) axis ups |> Fnn.downcast
  | `Softmax (id, axis), [ up ] -> Builder.softmax ~id:(repair id) axis up |> Fnn.downcast
  | `Softmax _, _ -> failwith "corrupted upstreams"
  | `Relu id, [ up ] -> Builder.relu ~id:(repair id) up |> Fnn.downcast
  | `Relu _, _ -> failwith "corrupted upstreams"
  | `Astype (id, dtype), [ up ] -> Builder.astype ~id:(repair id) dtype up |> Fnn.downcast
  | `Astype _, _ -> failwith "corrupted upstreams"
  | `Conv2d (id, g, s, d, b), [ w; up ] ->
      Builder.conv2d2 ~id:(repair id) ~g ~s ~d ~b w up |> Fnn.downcast
  | `Conv2d _, _ -> failwith "corrupted upstreams"
  | `Transpose (id, ndim, mapping), [ up ] ->
      Builder.transpose ~id:(repair id) ~ndim ~mapping up |> Fnn.downcast
  | `Transpose _, _ -> failwith "corrupted upstreams"
  | `Maxpool2d (id, b, s, k), [ up ] -> Builder.maxpool2d ~id:(repair id) ~b ~s k up |> Fnn.downcast
  | `Maxpool2d _, _ -> failwith "corrupted upstreams"
  | `Padding (id, v, l), [ up ] -> Builder.padding ~id:(repair id) ~v l up |> Fnn.downcast
  | `Padding _, _ -> failwith "corrupted upstreams"
  | `Tensordot (id, l, l'), [ up; up' ] ->
      Builder.tensordot ~id:(repair id) l l' up up' |> Fnn.downcast
  | `Tensordot _, _ -> failwith "corrupted upstreams"

let fnn_of_storable : _ -> storable_nn -> Fnn.network =
 fun builder (ids, graph, layers) ->
  let nns = Hashtbl.create 100 in
  let aux i =
    let upstreams = Hashtbl.find graph i in
    let upstreams = List.map (Hashtbl.find nns) upstreams in
    let nn = Hashtbl.find layers i in
    let nn = _fnn_of_storable builder nn upstreams in
    Hashtbl.add nns i nn
  in
  List.iter aux ids;
  Hashtbl.find nns (List.rev ids |> List.hd)

let (_ : _ -> 'a -> 'a) = fun builder s -> fnn_of_storable builder s |> storable_of_fnn
