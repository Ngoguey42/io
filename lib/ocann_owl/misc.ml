module type ALGODIFF = sig
  type ba_elt

  type ba = (float, ba_elt, Bigarray.c_layout) Bigarray.Genarray.t

  include Owl_algodiff_generic_sig.Sig with type A.elt = float and type A.arr = ba
end

module Make (Algodiff : ALGODIFF) (Nn : Ocann.NETWORK) = struct
  module Ndarray = Algodiff.A

  type optimization = float -> unit

  module OptiMap = struct
    module Key = Stdlib.String
    include Map.Make (Key)
    module StringSet = Set.Make (Key)

    let union_exn : optimization t -> optimization t -> optimization t =
      union (fun name _ _ -> Printf.sprintf "variable name clash: <%s>" name |> failwith)

    let union_silent : optimization t -> optimization t -> optimization t =
      union (fun _ a _ -> Some a)

    let union_list_exn l = List.fold_left union_exn empty l

    let union_list_silent l = List.fold_left union_silent empty l

    let key_disjunction m m' =
      let keys = to_seq m |> Seq.map (fun (key, _) -> key) |> StringSet.of_seq in
      let keys' = to_seq m' |> Seq.map (fun (key, _) -> key) |> StringSet.of_seq in
      ( StringSet.diff keys keys' |> StringSet.elements,
        StringSet.diff keys' keys |> StringSet.elements )
  end

  type optimization_map = optimization OptiMap.t

  let arr_kind = Ndarray.create [||] 0. |> Bigarray.Genarray.kind

  let arr_of_ba : type a. (float, a, Bigarray.c_layout) Bigarray.Genarray.t -> Ndarray.arr =
   fun a ->
    match (Bigarray.Genarray.kind a, arr_kind) with
    | Bigarray.Float32, Bigarray.Float32 -> a
    | Bigarray.Float64, Bigarray.Float64 -> a
    | Bigarray.Float64, Bigarray.Float32 -> failwith "Can't use f64 tensor with a f32 Algodiff"
    | Bigarray.Float32, Bigarray.Float64 -> failwith "Can't use f32 tensor with a f64 Algodiff"

  let ba_of_arr :
      type a.
      (float, a) Bigarray.kind -> Ndarray.arr -> (float, a, Bigarray.c_layout) Bigarray.Genarray.t =
   fun dst_kind a ->
    match (arr_kind, dst_kind) with
    | Bigarray.Float32, Bigarray.Float32 -> a
    | Bigarray.Float64, Bigarray.Float64 -> a
    | Bigarray.Float64, Bigarray.Float32 -> failwith "Can't use f32 tensor with a f64 Algodiff"
    | Bigarray.Float32, Bigarray.Float64 -> failwith "Can't use f64 tensor with a f32 Algodiff"

  let update_primal v primal =
    match v with
    | Algodiff.DR (_, adjoint, op, fanout, tag, tracker) ->
        Algodiff.DR (Algodiff.pack_arr primal, adjoint, op, fanout, tag, tracker)
    | Algodiff.DF (_, tangent, tag) -> Algodiff.DF (Algodiff.pack_arr primal, tangent, tag)
    | Algodiff.Arr _ -> Algodiff.Arr primal
    | Algodiff.F _ -> failwith "in update_primal: can't update scalar"

  let rec tofloat v =
    match Algodiff.primal' v with
    | Algodiff.F _ -> Algodiff.unpack_flt v
    | v -> v |> Algodiff.Maths.mean |> tofloat

  let channel_last_axes = function
    | 5 -> [ `N; `S2; `S1; `S0; `C ]
    | 4 -> [ `N; `S1; `S0; `C ]
    | 3 -> [ `N; `S0; `C ]
    | 2 -> [ `N; `C ]
    | 1 -> [ `N ]
    | 0 -> []
    | _ -> invalid_arg "In channel_last_axes: Invalid ndim"

  let index_in_list l x = List.mapi (fun i y -> (i, y)) l |> List.find (fun (_, y) -> x = y) |> fst

  let axes_of_shape shape =
    let ndim = Pshape.ndim shape in
    let is_sym = Pshape.is_symbolic shape in
    if is_sym then (channel_last_axes ndim :> Pshape.Axis.t list)
    else (Pshape.Axis.absolute_axes_of_ndim ndim :> Pshape.Axis.t list)

  let tensor_axis_of_shape_axis shape ax =
    List.mapi (fun i ax -> (i, ax)) (axes_of_shape shape) |> List.find (fun (_, x) -> x = ax) |> fst

  let derive_configuration_of_transpose_layer (net : Nn.transpose) =
    let mapping = net#mapping in
    let shape0 = net#upstream#out_shape in
    let axes0 = axes_of_shape shape0 in
    let axes1 = axes_of_shape net#out_shape in
    let mapping = Pshape.Axis.transpose ~mapping axes0 axes1 in
    let mapping = List.map (List.map (tensor_axis_of_shape_axis shape0)) mapping in
    let transpose_axes = List.concat mapping in
    let dims1_of_dims0 dims =
      List.map
        (fun tensor_axs0 -> tensor_axs0 |> List.map (Array.get dims) |> List.fold_left ( * ) 1)
        mapping
      |> Array.of_list
    in
    (transpose_axes, dims1_of_dims0)

  let derive_configuration_of_tensordot_layer net =
    (* Extract shape0 infos *)
    let shape0 = net#upstream0#out_shape in
    let axes0 = axes_of_shape shape0 in
    let caxes0 = net#contracted_axes0 in
    let kaxes0 = List.filter (fun ax -> not (List.mem ax caxes0)) axes0 in
    let kcount0 = List.length kaxes0 in
    (* Extract shape1 infos *)
    let shape1 = net#upstream1#out_shape in
    let axes1 = axes_of_shape shape1 in
    let caxes1 = net#contracted_axes1 in
    let kaxes1 = List.filter (fun ax -> not (List.mem ax caxes1)) axes1 in
    (* Compute tensordot argument *)
    let caxes01 =
      List.combine (List.map (index_in_list axes0) caxes0) (List.map (index_in_list axes1) caxes1)
    in
    (* Compute permute argument *)
    let shape2 = net#out_shape in
    let axes2 = axes_of_shape shape2 in
    let perm =
      List.map
        (fun ax ->
          match net#input_axis_of_output_axis ax with
          | `Left ax -> 0 + index_in_list kaxes0 ax
          | `Right ax -> kcount0 + index_in_list kaxes1 ax)
        axes2
    in
    (caxes01, perm)

  let validate_output_tensor net tensor =
    let net = Nn.downcast net in

    (* Printf.eprintf "output of %s has mean value %.17e\n%!" net#to_string (tofloat tensor); *)
    let out_shape = net#out_shape in
    let out_shape =
      if Pshape.is_symbolic out_shape then
        out_shape |> Pshape.to_symbolic
        |> Pshape.desymbolize (channel_last_axes (Pshape.ndim out_shape))
        |> Pshape.to_any
      else out_shape
    in
    let net_dims = out_shape |> Pshape.to_list |> List.map snd in
    let tensor_dims = Algodiff.primal' tensor |> Algodiff.Arr.shape |> Array.to_list in
    let sizes_invalid = function
      | Pshape.Size.U, _ -> false
      | Pshape.Size.K j, i when i = j -> false
      | _, _ -> true
    in
    if
      List.length net_dims <> List.length tensor_dims
      || List.combine net_dims tensor_dims |> List.exists sizes_invalid
    then
      Printf.sprintf "Output tensor of net %s has shape (%s) but was expected to have shape %s=(%s)"
        net#to_string
        (List.map string_of_int tensor_dims |> String.concat ", ")
        (Pshape.to_string net#out_shape)
        (List.map Pshape.Size.to_string net_dims |> String.concat ", ")
      |> failwith;
    tensor

  let _unpack_optimizer net var =
    match net#optimizer with
    | `Sgd ->
        let update lr =
          let w = !var |> Algodiff.primal' |> Algodiff.unpack_arr in
          let g = !var |> Algodiff.adjval |> Algodiff.unpack_arr in
          var :=
            g
            |> Ndarray.mul (Ndarray.create [||] lr)
            |> Ndarray.neg |> Ndarray.add w |> update_primal !var
        in
        let pack () = net#optimizer in
        (update, pack)
    | `Adam (epsilon, beta1, beta2, step, rgrad, rgrad_sq) ->
        let rgrad = rgrad |> Nn.Tensor.to_ba |> arr_of_ba |> ref in
        let rgrad_sq = rgrad_sq |> Nn.Tensor.to_ba |> arr_of_ba |> ref in
        let step = ref step in
        let update lr =
          incr step;
          let w = !var |> Algodiff.primal' |> Algodiff.unpack_arr in
          let g = !var |> Algodiff.adjval |> Algodiff.unpack_arr in
          let correction1 = 1. -. (beta1 ** float_of_int !step) |> Ndarray.create [||] in
          let correction2 = 1. -. (beta2 ** float_of_int !step) |> Ndarray.create [||] in
          let beta1' = Ndarray.create [||] (1. -. beta1) in
          let beta2' = Ndarray.create [||] (1. -. beta2) in
          let beta1 = Ndarray.create [||] beta1 in
          let beta2 = Ndarray.create [||] beta2 in
          let epsilon = Ndarray.create [||] epsilon in
          let lr = Ndarray.create [||] lr in

          Ndarray.(
            let ( / ) = div in
            let ( + ) = add in
            let ( * ) = mul in
            rgrad := (!rgrad * beta1) + (g * beta1');
            rgrad_sq := (!rgrad_sq * beta2) + (g * g * beta2');
            var :=
              !rgrad / correction1
              |> Fun.flip div (sqrt (!rgrad_sq / correction2) + epsilon)
              |> mul lr |> neg |> add w |> update_primal !var)
        in
        let pack () =
          let rgrad = !rgrad |> ba_of_arr Bigarray.Float32 |> Nn.Tensor.of_ba in
          let rgrad_sq = !rgrad_sq |> ba_of_arr Bigarray.Float32 |> Nn.Tensor.of_ba in
          `Adam (epsilon, beta1, beta2, !step, rgrad, rgrad_sq)
        in
        (update, pack)
end
