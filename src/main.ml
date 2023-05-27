open CCResult.Infix
open Modulectomy

let debug = false

type ty =
  | Elf

let get_file (file, ty) = match ty with
  | Elf ->
    let f = function
      | `Invalid_file ->
        `Msg (Format.sprintf "The file %s is not a valid ELF binary." file)
    in
    CCResult.map_err f @@ Elf.get file

let print_debug ~size ~info =
  Printf.eprintf "treemap size: %Ld \n" size;
  let ranges = Info.find_ranges info in
  let compute_range_size acc (start, stop, _) =
    Int64.add acc (Int64.sub stop start)
  in
  let size = List.fold_left compute_range_size 0L ranges in
  Printf.eprintf "ranges size: %Lu\n" size;
  Printf.eprintf "ranges:\n";
  let x = ref 0L in
  List.iter (fun (start, stop, v) ->
    Printf.eprintf "0x%08Lx - 0x%08Lx %s\n" start stop v;
    if start > Int64.add !x 16L then
      Printf.eprintf "  GAP before: %Ld\n"  (Int64.sub start !x);
    x := stop)
    ranges;
  Printf.eprintf "\n"

module Robur_defaults = struct

  let css_overrides = "\
    .treemap-module {\
      fill: rgb(60, 60, 87);\
    }\
    .treemap-functor > text, .treemap-module > text {\
      fill: bisque;\
    }\
  "

  let filter_small = 0.004

  let with_scale () = failwith "You need to pass --with-scale=<ELF-SIZE>"
  
end

(*> Note: this heuristic fails if one has many subtrees of equal size*)
let is_node_big_enough ~filter_small ~size subtree =
  match filter_small, Info.(subtree.T.value.size) with
  | _, None | None, _ -> true 
  | Some min_pct, Some subtree_size ->
    let pct = Int64.(to_float subtree_size /. to_float size) in
    pct > min_pct 

let prepare_info_tree ~filter_small info_data =
  let info = info_data |> Info.import in
  let size, info = Info.diff_size_tree info in
  if debug then print_debug ~size ~info;
  info
  |> Info.prefix_filename
  |> Info.cut 2
  |> Info.partition_subtrees (is_node_big_enough ~size ~filter_small)
  |> fst

let prepare_main_info_tree ~filter_small ~infos_anim info_data =
  let info = info_data |> Info.import in
  let size, info = Info.diff_size_tree info in
  if debug then print_debug ~size ~info;
  let info, excluded = 
    info
    |> Info.prefix_filename
    |> Info.cut 2
    |> Info.partition_subtrees (is_node_big_enough ~size ~filter_small)
  in
  let info =
    let choose_left x _ = x in
    let union_left = Info.T.union ~union_value:choose_left in
    let infos_anim_merged =
      infos_anim
      |> List.fold_left union_left Info.T.empty
      |> Info.T.map (fun data ->
        let size = data.size |> Option.map (fun _ -> Int64.zero) in
        { data with size } 
      )
    in
    union_left info infos_anim_merged
  in
  info, excluded

(*goto also animate scale*)
let squarify
    robur_defaults
    robur_css
    filter_small
    with_scale
    info_data1
    info_data2
    info_data3
    info_data4
  =
  let default_css_overrides =
    if robur_defaults || robur_css then
      Some Robur_defaults.css_overrides
    else None
  and default_filter_small = 
    if robur_defaults then
      Some Robur_defaults.filter_small
    else None
  and default_with_scale () = 
    if robur_defaults then
      Some (Robur_defaults.with_scale ())
      (*< todo can this param dependency be represented in Cmdliner DSL?*)
    else None
  in
  let filter_small = filter_small |> CCOption.or_ ~else_:default_filter_small
  and with_scale = with_scale |> CCOption.or_lazy ~else_:default_with_scale
  in
  let override_css = default_css_overrides in
  let infos_anim =
    [ info_data2; info_data3; info_data4 ]
    |> CCList.flat_map (fun v ->
      if Iter.is_empty v then [] else
        [ prepare_info_tree ~filter_small v ]
    )
  in
  let infos1, excluded_minors =
    prepare_main_info_tree ~filter_small ~infos_anim info_data1 in
  let trees = infos1 :: infos_anim in
  (*> goto this should become default when it works (i.e. remove 'Treemap.of_trees')*)
  let treemap = Treemap.Animated.of_trees trees in
  let html = match with_scale with
    | None -> Treemap.to_html ?override_css treemap
    | Some elf_size ->
      let scale_chunks =
        let excluded_minors_size =
          excluded_minors
          |> List.map Info.compute_area
          |> List.fold_left Int64.add 0L
        in
        [ "Smaller excluded entries", excluded_minors_size ]
      in
      Treemap.to_html_with_scale
        ~binary_size:elf_size
        ~scale_chunks
        ?override_css
        treemap
  in
  Tyxml.Html.pp () Format.std_formatter html

let guess file =
  match Fpath.get_ext @@ Fpath.v file with
  | _ -> Elf
  | exception _ -> Elf

module Arg_aux = struct

  open Cmdliner 

  let elf_doc = "Native ELF binaries. Requires the $(b,owee) library. \
                 For better results, the binary file should have been compiled \
                 with debug information."

  let elfN_doc = "The same as for `--elf`, but lets you pass ELFs that the \
                  treemap is animated with. Each --elf<N> argument signifies \
                  a keyframe in animation. Usecase is to see changes over time \
                  in similar ELF files."
  
  let make_programs_arg ?(doc=`Elf) ~required name =
    let doc = match doc with
      | `Elf -> elf_doc
      | `ElfN -> elfN_doc
    in
    let flatten x = Term.(const List.flatten $ x) in
    let annot f t =
      let g l = List.map (fun x -> (x, f x)) l in
      Term.(const g $ t) in
    let elf_args =
      let i = Arg.info ~doc ~docs:"FORMATS" ~docv:"BIN,..." [ name ] in
      annot (fun _ -> Elf) @@ flatten Arg.(value & opt_all (list file) [] i)
    in
    let take_all = function
      | [] when required -> `Help (`Auto, None)
      | l -> `Ok l
    in
    Term.(ret (const take_all $ elf_args))

  let filter_small =
    let doc = "Remove subtrees that are smaller than PCT" in
    let docv = "PCT" in
    Arg.(value & opt (some float) None & info [ "filter-small" ] ~doc ~docv)

  let with_scale =
    let doc = "Include an additional scale-SVG in the HTML, \
               given the size in bytes of the non-debug ELF file." in
    let docv = "BYTES" in
    Arg.(value & opt (some int) None & info [ "with-scale" ] ~doc ~docv)

  let robur_css = 
    let doc = "Use Robur CSS styling in HTML" in
    Arg.(value & flag & info [ "robur-css" ] ~doc)
  
  let robur_defaults = 
    let doc = "Use Robur default values for every configuration option. \
               You need to pass --with-scale too." in
    Arg.(value & flag & info [ "robur-defaults" ] ~doc)

end

let squarify_files
    robur_defaults
    robur_css
    filter_small
    with_scale
    files1
    files2
    files3
    files4
  =
  let rec get_all = function
    | [] -> Ok Iter.empty
    | h :: t ->
      get_file h >>= fun i ->
      get_all t >|= fun i' ->
      Iter.append i i'
  in
  try
    get_all files1 >>= fun infos1 ->
    get_all files2 >>= fun infos2 ->
    get_all files3 >>= fun infos3 ->
    get_all files4 >|= fun infos4 ->
    squarify robur_defaults robur_css filter_small with_scale
      infos1 infos2 infos3 infos4
  with exn ->
    Printexc.print_backtrace stderr;
    Format.eprintf "%s\n%!" (Printexc.to_string exn);
    exit 1

let main_term =
  let open Cmdliner in
  Printexc.record_backtrace true;
  let doc = "Dissect OCaml compiled programs, and weight their content." in
  let version = Fmt.str "%d" Treemap.visualization_version in
  let info = Cmd.info ~doc ~version "modulectomy" in
  let term = Term.(term_result (
    const squarify_files
    $ Arg_aux.robur_defaults
    $ Arg_aux.robur_css
    $ Arg_aux.filter_small
    $ Arg_aux.with_scale
    $ Arg_aux.make_programs_arg ~required:true "elf"
    $ Arg_aux.make_programs_arg ~required:false ~doc:`ElfN "elf2"
    $ Arg_aux.make_programs_arg ~required:false ~doc:`ElfN "elf3"
    $ Arg_aux.make_programs_arg ~required:false ~doc:`ElfN "elf4"
  )) in
  Cmd.v info term

let () =
  Cmdliner.Cmd.eval main_term
  |> exit
