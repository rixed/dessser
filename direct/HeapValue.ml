(* We might want to serialize into a value allocated on the heap.
 * The form of which depends on the backend of course, but every backend
 * must allow construction of arbitrarily nested data structures.
 * Of course, it is then also possible to serialize from a heap value
 * into anything else.
 *
 * For such a format, the pointers used to write/read during resp.
 * serialization/deserialization are unused. *)
open Dessser

module Ser (BE : BACKEND) : SER with module BE = BE =
struct
  module BE = BE

  type frame =
    { (* The type of the opened tuple/record/vector/wtv: *)
      typ : Types.t ;
      (* The index of the current field within that record/wtv.
       * Note that although record fields can be accessed by name, they can
       * still be accessed by number as long as we keep the type around. *)
      mutable index : int }

  type state =
    { (* Currently opened compound types: *)
      mutable frames : frame list ;
      (* In case the outermost type is not a compound type: *)
      outermost_typ : Types.t ;
      (* Is the current field nullable? *)
      mutable nullable : bool ;
      (* The identifier of the outermost value: *)
      id : [`Any] id }

  let init_state typ oc p =
    (* For backends that can alloc uninitialized values:
     *   Will define the type, then alloc an uninitialized  value of that
     *   type and return an identifier for it. Fields will be initialized
     *   as serialization progresses.
     * For backends that can not alloc uninitialized values:
     *   Will define the type, then make up a name and return it. Construction
     *   and allocation will happen as the serialize progresses. *)
    let id = BE.alloc_value oc typ in
    { outermost_typ = typ ; frames = [] ; nullable = false ; id }, p

  let push_frame st frame =
    st.frames <- frame :: st.frames

  let pop_frame st =
    st.nullable <- false ;
    st.frames <- List.tl st.frames

  let incr_frame_index st =
    let frame = List.hd st.frames in
    frame.index <- frame.index + 1

  type 'a ser = BE.output -> state -> 'a -> [`Pointer] id -> [`Pointer] id

  let set_field oc st id =
    let typ, index =
      match st.frames with
      | [] ->
          (* The outermost value is not a compound type, retrieve the
           * type that was saved in [init_state]: *)
          st.outermost_typ, 0
      | frame :: _ ->
          frame.typ, frame.index
    in
    if st.nullable then
      BE.set_nullable_field oc typ index (Some id)
    else
      BE.set_field oc typ index id

  let sfloat oc st id p = set_field oc st id ; p
  let sstring oc st id p = set_field oc st id ; p
  let sbool oc st id p = set_field oc st id ; p
  let si8 oc st id p = set_field oc st id ; p
  let si16 oc st id p = set_field oc st id ; p
  let si32 oc st id p = set_field oc st id ; p
  let si64 oc st id p = set_field oc st id ; p
  let si128 oc st id p = set_field oc st id ; p
  let su8 oc st id p = set_field oc st id ; p
  let su16 oc st id p = set_field oc st id ; p
  let su32 oc st id p = set_field oc st id ; p
  let su64 oc st id p = set_field oc st id ; p
  let su128 oc st id p = set_field oc st id ; p

  let tup_opn typs oc st p =
    let frame =
      { typ = Types.make ~nullable:st.nullable (Types.TTup typs) ;
        index = 0 } in
    push_frame st frame ;
    BE.begin_tup oc frame.typ frame.index ;
    p

  let tup_cls _typs oc st p =
    pop_frame st ;
    BE.end_tup oc ;
    p

  let tup_sep _typs _idx _oc st p =
    incr_frame_index st ;
    p

  let rec_opn typs oc st p =
    let frame =
      { typ = Types.make ~nullable:st.nullable (Types.TRec typs) ;
        index = 0 } in
    push_frame st frame ;
    BE.begin_rec oc frame.typ frame.index ;
    p

  let rec_cls _typs oc st p =
    pop_frame st ;
    BE.end_rec oc ;
    p

  let rec_sep _typs _fname _oc st p =
    incr_frame_index st ;
    p

  let vec_opn dim typ oc st p =
    let frame =
      { typ = Types.make ~nullable:st.nullable (Types.TVec (dim, typ)) ;
        index = 0 } in
    push_frame st frame ;
    BE.begin_vec oc frame.typ frame.index ;
    p

  let vec_cls _dim _typ oc st p =
    pop_frame st ;
    BE.end_vec oc ;
    p

  let vec_sep _dim _typ _idx _oc st p =
    incr_frame_index st ;
    p

  let nullable _typs _oc st p =
    (* Just record the fact that the next field value will be nullable *)
    st.nullable <- true ;
    p

  let snull oc st p =
    let frame = List.hd st.frames in
    BE.set_nullable_field oc frame.typ frame.index None ;
    p

  let snotnull _oc _st p = p
end
