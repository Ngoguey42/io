open struct
  module Algodiff = Owl_algodiff_generic.Make (Owl_base_algodiff_primal_ops.S)
end

type optimization = float -> unit

module OptiMap = struct
  include Map.Make (Stdlib.String)
  module StringSet = Set.Make (Stdlib.String)

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

let derive_configuration_of_transpose_layer (net : Fnn.transpose) =
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
  let net = Fnn.downcast net in
  Printf.eprintf "validating output of %s\n%!" net#to_string;

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
  Printf.eprintf "ok\n%!";
  tensor

let _unpack_optimizer net var =
  match net#optimizer with
  | `Sgd ->
      let update lr =
        Printf.eprintf "SGD update with lr=%e\n%!" lr;
        let w = !var in
        let g = Algodiff.adjval w in
        let w = w |> Algodiff.primal' in
        let w =
          g
          |> Algodiff.Maths.mul (Algodiff.pack_flt lr)
          |> Algodiff.Maths.neg |> Algodiff.Maths.add w |> Fun.flip Algodiff.make_reverse 42
        in
        var := w;
        ()
      in
      let pack () = net#optimizer in
      (update, pack)
  | `Adam _ -> failwith "ADAM not yet implemented"

(* | `Adam (epsilon, beta1, beta2, step, rgrad, rgrad_sq) -> *)

(*   (\*     let updater = Tfjs_api.create_adam32_updater epsilon beta1 beta2 step rgrad rgrad_sq var in *\) *)
(*   (\*     let pack () = *\) *)
(*   (\*       `Adam (epsilon, beta1, beta2, updater#get_step, updater#get_rgrad, updater#get_rgrad_sq) *\) *)
(*   (\*     in *\) *)
(*   (\*     (updater#update, pack) *\) *)

type optimization_map = optimization OptiMap.t
