open Common

let _threshold = 0.00000001
let _equal_float f1 f2 = abs_float (f1 -. f2) < _threshold
let _equal_pos p1 p2 = _equal_float p1.x p2.x && _equal_float p1.y p2.y

module Squarify = struct

  type dir = Horizontal | Vertical

  let length dir rect = match dir with
    | Horizontal -> rect.w
    | Vertical -> rect.h
  let opp = function Horizontal -> Vertical | Vertical -> Horizontal

  let compute_dir rect =
    if rect.h > rect.w then Horizontal else Vertical

  type 'a state = {
    rect : rectangle ;
    dir : dir ;
    elements : 'a list ;
    area : float ;
    smallest : float ;
    biggest : float ;
  }

  let add ~area ~elem sol =
    let a = area elem in {
      rect = sol.rect ;
      dir = sol.dir ;
      elements = elem :: sol.elements ;
      area = a +. sol.area ;
      smallest = min a sol.smallest ;
      biggest = max a sol.biggest ;
    }

  let init rect = {
    rect ;
    dir = compute_dir rect ;
    elements = [] ;
    area = 0. ;
    smallest = max_float ;
    biggest = 0. ;
  }

  (** Return the worst aspect ratio *) 
  let worst sol =
    let s = sol.area and w = length sol.dir sol.rect
    and rp = sol.biggest and rm = sol.smallest in
    max ((w*.w*.rp)/.(s*.s)) ((s*.s)/.(w*.w*.rm))

  (** Utility functions for computing layout *)
  let mv_pos dir { x ; y } len = match dir with
    | Horizontal -> { y ; x = x +. len }
    | Vertical -> { x ; y = y +. len }
  let mk_rect dir side p len = match dir with
    | Horizontal -> { p ; w = len ; h = side }
    | Vertical -> { p ; h = len ; w = side }
  let cut_rect dir { p ; w ; h } side =
    let p = mv_pos dir p side in
    match dir with
    | Horizontal -> { p ; h ; w = w -. side }
    | Vertical -> { p ; w ; h = h -. side }

  module IMap = Map.Make(Int)

  (** Layout a solution in a given rectangle.
      Iterate on the list of laid out elements (by continuation [k])
      and return the new state. *)
  let layout ~areas ~states k =
    let init =
      let outlaid_rects = IMap.empty in
      let new_rects = [] in
      new_rects, outlaid_rects
    in
    let new_rects, outlaid_rects = 
      List.combine states areas
      |> List.fold_left (fun (new_rects, outlaid_rects) (state, area) ->
        let total_len = length state.dir state.rect in
        let side = state.area /. total_len in
        let new_rect = cut_rect (opp state.dir) state.rect side in

        let layout_elem (i, pos, rects) elem = 
          let len = total_len *. area elem /. state.area in
          let rect = mk_rect state.dir side pos len in
          let pos = mv_pos state.dir pos len in
          let rects = rects |> IMap.update i (function
            | None -> Some (elem, [ rect ])
            | Some (_elem, rects) ->
              (*> Note: elem = _elem, as index of elements are the same across
                  states (the primary state contains all possible elements)*)
              Some (elem, rect :: rects)
          ) in
          (succ i, pos, rects)
        in
        let _, _pos, outlaid_rects =
          let init = 0, state.rect.p, outlaid_rects in
          state.elements
          |> List.rev (*< Note: this is done to lay out in the right direction*)
          |> List.fold_left layout_elem init
        in
        (* assert (_equal_pos _pos @@ mv_pos state.dir state.rect.p total_len); *)
        let new_rects = new_rect :: new_rects in
        new_rects, outlaid_rects
      ) init
    in
    let new_rects = List.rev new_rects in
    let () =
      outlaid_rects
      |> IMap.iter (fun _i (elem, rects) ->
        let rects = List.rev rects in
        match rects with
        | rect :: anim_rects -> k (elem, (rect, anim_rects))
        | [] -> failwith "Treemaps.layout: There should always be an outlaid \
                          rect"
      )
    in
    new_rects

  let layout_remaining ~areas states k =
    match states with
    | [] -> ()
    | main_state :: _ -> 
      begin match main_state.elements with
        | [] -> ()
        | _ -> begin
            let _s = layout ~areas ~states k in
            (*assert (_s.w *. _s.h >= -. _threshold);*)
            ()
          end
      end

  (* let update_anim_states ~anim_states ~animate_areas ~elem action = *)
  (*   List.combine anim_states animate_areas *)
  (*   |> List.map (fun (state, area) -> *)
  (*     let state = match action with *)
  (*       | `Append -> state *)
  (*       | `Layout -> *)
  (*         (\*> goto what to do about k?*\) *)
  (*         let k = failwith "todo" in *)
  (*         let new_rect = layout ~area state k in *)
  (*         init new_rect *)
  (*     in *)
  (*     add ~area ~elem state  *)
  (*   ) *)

  let squarify ?animate_areas ~area rect l : _ Iter.t =
    let animate_areas =
      Option.to_list animate_areas |> List.flatten
    in
    let areas = area :: animate_areas in
    let place_rect k states elem =
      match states with
      | state :: anim_states -> 
        let new_state = add ~area ~elem state in
        if worst new_state <= worst state then
          let anim_states = anim_states |> List.map (add ~area ~elem) in
          new_state :: anim_states
        else
          let new_rects = layout ~areas ~states k in
          let new_states = new_rects |> List.map init in
          new_states |> List.map (add ~area ~elem)
      | [] -> assert false (*Won't happen*)
    in
    let init_state = init rect in
    let init_animation_states = List.map (fun _ -> init_state) animate_areas in
    let init_states = init_state :: init_animation_states in
    fun k ->
      let states_final = Iter.fold (place_rect k) init_states l in
      (*> goto also do this for anim_states*)
      layout_remaining ~areas states_final k ;
      ()

end

let squarify = Squarify.squarify

let layout ?(sub=fun x -> x) ?animate_areas ~area ~children rect0 l0 : _ Iter.t =
  let rec go_level k (v, (rect, anim_rects)) =
    k (v, (rect, anim_rects)) ;
    let rect = sub rect in
    let cl = children v in
    let l = squarify ?animate_areas ~area rect cl in 
    Iter.iter (go_level k) l
  in
  fun k ->
    let l = squarify ?animate_areas ~area rect0 l0 in
    Iter.iter (go_level k) l

(*
 * Copyright (c) 2019 Gabriel Radanne <drupyog@zoho.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
