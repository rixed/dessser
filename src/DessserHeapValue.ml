(* Dessser API is optimised to convert from one format into another without
 * materializing complex values in memory.
 * But when we do want to materialize the value in the heap, another API is
 * more natural.
 * The two following modules are designed to unserialize into a materialized
 * values and to then serialize that materialized value. *)
open Batteries
open Stdint
open Dessser
open DessserTools
open DessserMiscTypes
module E = DessserExpressions
module Mask = DessserMasks
module StdLib = DessserStdLib
module T = DessserTypes
module U = DessserCompilationUnit
open E.Ops

(* The generated code can use a runtime fieldmask or just encode everything.
 * Of course, we could define a function A that calls the runtime mask version
 * of some other function B with a constant copy-field mask, and the partial
 * evaluation would inline B in A, removing all the code dealing with the
 * fieldmask, as long as B is not recursive. Also, it would not inline
 * externally defined functions that might be called by B. So it's more
 * efficient to have two versions of B: one that accept a runtime fieldmask
 * and one that does not. *)
type fieldmask = CompTimeMask | RunTimeMask of E.t

let fieldmask_get i = function
  | CompTimeMask -> CompTimeMask
  | RunTimeMask ma -> RunTimeMask (mask_get i ma)

let fieldmask_copy = function
  | CompTimeMask -> CompTimeMask
  | RunTimeMask _ -> RunTimeMask copy_field

module type MATERIALIZE =
sig
  module Des : DES

  (* TODO: This should also accept a runtime fieldmask as parameter. *)

  val make : ?config:Des.config ->
             (* The type name (as used with TThis): *)
             ?type_name:string ->
             T.mn ->
             U.t ->
             (* Same return type than U.add_identifier_of_expression: *)
             U.t * E.t * string
end

module Materialize (Des : DES) :
  MATERIALIZE with module Des = Des =
struct
  module Des = Des

  let local_des_for type_name =
    (if type_name = "t" then "" else type_name ^"-")^
    string_of_type_method (DesNoMask Des.id)

  let rec dvec dim mn dstate mn0 path src =
    let src = Des.vec_opn dim mn dstate mn0 path src in
    let rec loop ids i src =
      if i >= dim then
        make_pair
          (make_vec (List.rev ids))
          (Des.vec_cls mn dstate mn0 path src)
      else
        let src = if i = 0 then src else
                    Des.vec_sep dstate mn0 path src in
        let subpath = Path.(append (CompTime i) path) in
        let v_src = make1 dstate mn0 subpath mn src in
        E.with_sploded_pair "dvec" v_src (fun v src ->
          loop (v :: ids) (i + 1) src) in
    loop [] 0 src

  (* Arrs and sets are serialized the same, so here the arr is desserialized
   * into a list, and then the final operation converts it into an arr or a
   * set: *)

  and darr mn dstate mn0 path src =
    dlist arr_of_lst_rev mn dstate mn0 path src

  and dset mn dstate mn0 path src =
    dlist set_of_lst mn dstate mn0 path src

  and dlst mn dstate mn0 path src =
    dlist identity mn dstate mn0 path src

  and dlist of_list mn dstate mn0 path src =
    match Des.arr_opn dstate with
    | KnownSize list_opn ->
        let dim_src = list_opn mn mn0 path src in
        let inits_src =
          E.with_sploded_pair "dlist1" dim_src (fun dim src ->
            let inits_src_ref = make_ref (make_pair (end_of_list mn) src) in
            let_ ~name:"inits_src_ref" inits_src_ref (fun inits_src_ref ->
              let inits_src = get_ref inits_src_ref in
              seq [
                StdLib.repeat ~from:(i32 0l) ~to_:(to_i32 dim) (fun n ->
                  E.with_sploded_pair "dlist2" inits_src (fun inits src ->
                    let src =
                      if_ (eq n (i32 0l))
                        ~then_:src
                        ~else_:(Des.arr_sep dstate mn0 path src) in
                    let subpath = Path.(append (RunTime n) path) in
                    let v_src = make1 dstate mn0 subpath mn src in
                    E.with_sploded_pair "dlist3" v_src (fun v src ->
                      make_pair (cons v inits) src) |>
                    set_ref inits_src_ref)) ;
                inits_src ])) in
        E.with_sploded_pair "dlist4" inits_src (fun inits src ->
          let v = of_list inits
          and src = Des.arr_cls mn dstate mn0 path src in
          make_pair v src)
    | UnknownSize (list_opn, is_end_of_list) ->
        let src = list_opn mn mn0 path src in
        let_ ~name:"inits_ref" (make_ref (end_of_list mn)) (fun inits_ref ->
          let_ ~name:"src_ref" (make_ref src) (fun src_ref ->
            let_ ~name:"n_ref" (make_ref (u32_of_int 0)) (fun n_ref ->
              seq [
                while_ (not_ (is_end_of_list mn0 path (get_ref src_ref))) (
                  let src = get_ref src_ref
                  and n = get_ref n_ref
                  and inits = get_ref inits_ref in
                  let subpath = Path.(append (RunTime n) path) in
                  let src =
                    if_ (eq n (u32_of_int 0))
                      ~then_:src
                      ~else_:(Des.arr_sep dstate mn0 subpath src) in
                  let v_src = make1 dstate mn0 subpath mn src in
                  E.with_sploded_pair "dlist7" v_src (fun v src ->
                    seq [
                      set_ref inits_ref (cons v inits) ;
                      set_ref src_ref src ;
                      set_ref n_ref (add n (u32_of_int 1)) ])) ;
                (
                  let v = of_list (get_ref inits_ref)
                  and src =
                    Des.arr_cls mn dstate mn0 path (get_ref src_ref) in
                  make_pair v src
                ) ])))

  and dtup mns dstate mn0 path src =
    let src = Des.tup_opn mns dstate mn0 path src in
    let rec loop ids i src =
      if i >= Array.length mns then
        make_pair
          (make_tup (List.rev ids))
          (Des.tup_cls dstate mn0 path src)
      else
        let src = if i = 0 then src else
                    Des.tup_sep dstate mn0 path src in
        let subpath = Path.(append (CompTime i) path) in
        let v_src = make1 dstate mn0 subpath mns.(i) src in
        E.with_sploded_pair "dtup" v_src (fun v src ->
          loop (v :: ids) (i + 1) src) in
    loop [] 0 src

  and drec mns dstate mn0 path src =
    let src = Des.rec_opn mns dstate mn0 path src in
    let len = Array.length mns in
    let rec loop ids i src =
      if i >= len then
        make_pair
          (make_rec (List.mapi (fun i id ->
             fst mns.(len - i - 1), id) ids))
          (Des.rec_cls dstate mn0 path src)
      else
        let src = if i = 0 then src else
                    Des.rec_sep dstate mn0 path src in
        let subpath = Path.(append (CompTime i) path) in
        let v_src = make1 dstate mn0 subpath (snd mns.(i)) src in
        E.with_sploded_pair "drec" v_src (fun v src ->
          loop (v :: ids) (i + 1) src) in
    loop [] 0 src

  and dsum mns dstate mn0 path src =
    let cstr_src = Des.sum_opn mns dstate mn0 path src in
    let max_lbl = Array.length mns - 1 in
    E.with_sploded_pair "dsum1" cstr_src (fun cstr src ->
      let rec choose_cstr i =
        assert (i <= max_lbl) ;
        let res () =
          let subpath = Path.(append (CompTime i) path) in
          let _, subtyp = mns.(i) in
          let v_src = make1 dstate mn0 subpath subtyp src in
          E.with_sploded_pair "dsum2" v_src (fun v src ->
            make_pair
              (construct mns i v)
              (Des.sum_cls cstr dstate mn0 path src))
        in
        if i = max_lbl then
          seq [
            assert_ (eq cstr (u16 (Uint16.of_int max_lbl))) ;
            res () ]
        else
          if_ (eq (u16 (Uint16.of_int i)) cstr)
            ~then_:(res ())
            ~else_:(choose_cstr (i + 1)) in
      choose_cstr 0)

  and dvoid _ _ _ src =
    make_pair void src

  and dext name =
    Des.dext (fun src ->
      apply (type_method name (DesNoMask Des.id)) [ src ])

  (* Call the decoder for type name [type_name]: *)
  and dthis type_name dstate mn0 path src =
    let f =
      let mn = T.required (T.find_this type_name) in
      if T.eq_mn mn mn0 then
        myself T.(pair mn0 (T.dptr_of_enc Des.id))
      else
        identifier (local_des_for type_name) in
    Des.dext (fun src -> apply f [ src ]) dstate mn0 path src

  and make_mn dstate mn0 path mn src =
    let rec des_of_vt = function
      | T.TNamed (type_name, _) ->
          (* assume this had been defined already with [make_des_for_subtypes]: *)
          dthis type_name
      | TThis type_name -> dthis type_name
      | TExt type_name -> dext type_name
      | TVoid -> dvoid
      | TFloat -> Des.dfloat
      | TString -> Des.dstring
      | TBytes -> Des.dbytes
      | TBool -> Des.dbool
      | TChar -> Des.dchar
      | TI8 -> Des.di8
      | TI16 -> Des.di16
      | TI24 -> Des.di24
      | TI32 -> Des.di32
      | TI40 -> Des.di40
      | TI48 -> Des.di48
      | TI56 -> Des.di56
      | TI64 -> Des.di64
      | TI128 -> Des.di128
      | TU8 -> Des.du8
      | TU16 -> Des.du16
      | TU24 -> Des.du24
      | TU32 -> Des.du32
      | TU40 -> Des.du40
      | TU48 -> Des.du48
      | TU56 -> Des.du56
      | TU64 -> Des.du64
      | TU128 -> Des.du128
      | TUsr vt ->
          (* Deserialize according to vt.def, then make a new user value to
           * keep the user type: *)
          fun state mn0 path ptr ->
            let v_src = des_of_vt vt.def state mn0 path ptr in
            E.with_sploded_pair "des_usr_type" v_src (fun v src ->
              make_pair (make_usr vt.name [ v ]) src)
      | TTup mns -> dtup mns
      | TRec mns -> drec mns
      | TSum mns -> dsum mns
      | TVec (dim, mn) -> dvec dim mn
      | TArr mn -> darr mn
      | TLst mn -> dlst mn
      | TSet (Simple, mn) -> dset mn
      | TSet _ -> todo "Materialization of non simple sets"
      | TMap _ -> assert false (* No value of map type *)
      | _ -> invalid_arg "make1"
    in
    let vt = mn.T.typ in
    if mn.nullable then (
      if_ (Des.is_null dstate mn0 path src)
        ~then_:(make_pair (null vt) (Des.dnull vt dstate mn0 path src))
        ~else_:(
          let src = Des.dnotnull vt dstate mn0 path src in
          let des = des_of_vt vt in
          let v_src = des dstate mn0 path src in
          E.with_sploded_pair "make1_1" v_src (fun v src ->
            make_pair (not_null v) src))
    ) else (
      let des = des_of_vt vt in
      des dstate mn0 path src
    )

  and make1 dstate mn0 path mn src =
    let_pair ~n1:"is_present" ~n2:"src"
      (Des.is_present dstate mn0 path src)
      (fun is_present src ->
        if_ is_present
          ~then_:(make_mn dstate mn0 path mn src)
          ~else_:(
            let def =
              match mn.default with
              | Some def -> def
              | None ->
                  (try E.default_mn mn
                  with Invalid_argument _ -> no_return mn)
            in
            make_pair (convert mn def) src))

  (* Build the deserializer, using augmented pointers: *)
  let rec make_ dstate type_name mn0 compunit =
    (* Pretend first that this desserializer is defined, so that mutually recursive
     * calls can type-check: *)
    let name = local_des_for type_name in
    let dptr = T.dptr_of_enc Des.id in
    let mn = T.(func1 dptr (pair mn0 dptr)) in
    let compunit, _, name = U.add_identifier_of_type compunit ~name mn in
    (* Start by defining all required deserializers for subtypes: *)
    let compunit = make_des_for_subtypes dstate compunit mn0.T.typ in
    let expr =
      func1 dptr (fun src -> make1 dstate mn0 [] mn0 src) in
    U.add_identifier_of_expression compunit ~name expr

  and make_des_for_subtypes dstate compunit = function
    | T.TNamed (type_name, t) ->
        let compunit, _, _ = make_ dstate type_name T.(required t) compunit in
        compunit
        (* No further recursion needed since [make_] have added subtypes
         * already *)
    | TUsr { def ; _ } ->
        make_des_for_subtypes dstate compunit def
    | TVec (_, mn) | TArr mn | TSet (_, mn) | TLst mn ->
        make_des_for_subtypes dstate compunit mn.T.typ
    | TTup mns ->
        Array.fold (fun compunit mn ->
          make_des_for_subtypes dstate compunit mn.T.typ
        ) compunit mns
    | TRec mns | TSum mns ->
        Array.fold (fun compunit (_, mn) ->
          make_des_for_subtypes dstate compunit mn.T.typ
        ) compunit mns
    | TMap (kt, vt) ->
        let compunit = make_des_for_subtypes dstate compunit kt.T.typ in
        make_des_for_subtypes dstate compunit vt.T.typ
    | _ ->
        compunit

  let make ?config ?(type_name="t") mn0 compunit =
    let mn0 = T.develop_this_mn mn0 in
    let dstate = Des.make_state ?config mn0 in
    let compunit, id, name = make_ dstate type_name mn0 compunit in
    (* If that Des uses a custom pointer then we need to return a wrapper.
     * the [local_des_for] name must not be that of the wrapper though, since
     * that's the name recursive calls will use. So in this case we need a
     * new name.
     * And since the [start] and [stop] functions are allowed to emit any
     * code they want (for instance, to append a new line after serialization)
     * this wrapper is needed in any cases:*)
    let wrapper =
      func1 T.ptr (fun src ->
        let src = Des.start mn0 dstate src in
        let v_src = apply id [ src ] in
        E.with_sploded_pair "make" v_src (fun v src ->
          make_pair v (Des.stop mn0 dstate src))) in
    let name' = "wrap-"^ name in
    U.add_identifier_of_expression compunit ~name:name' wrapper
end

(* The other way around: given a heap value of some type and a serializer,
 * serialize that value.
 * The generated function will optionally accept a runtime fieldmask. *)

module type SERIALIZE =
sig
  module Ser : SER

  val serialize :
    ?config:Ser.config ->
    ?with_fieldmask:bool ->
    (* Type name as in TThis: *)
    ?type_name:string ->
    T.mn ->
    U.t ->
    (* Same output type than U.add_identifier_of_expression: *)
    U.t * E.t * string

  val sersize :
    ?config:Ser.config ->
    ?with_fieldmask:bool ->
    (* Type name as in TThis: *)
    ?type_name:string ->
    T.mn ->
    U.t ->
    (* Same output type than U.add_identifier_of_expression: *)
    U.t * E.t * string
end

module Serialize (Ser : SER) :
  SERIALIZE with module Ser = Ser =
struct
  module Ser = Ser

  let local_ser_for ~with_fieldmask type_name =
    (if type_name = "t" then "" else type_name ^"-")^
    string_of_type_method
      (if with_fieldmask then SerWithMask Ser.id else SerNoMask Ser.id)

  let rec svec dim mn ma sstate mn0 path v dst =
    let dst = Ser.vec_opn dim mn sstate mn0 path dst in
    let rec loop i dst =
      let subpath = Path.(append (CompTime i) path) in
      if i >= dim then
        Ser.vec_cls mn sstate mn0 path dst
      else
        let dst = if i = 0 then dst else
                    Ser.vec_sep sstate mn0 subpath dst in
        let_ ~name:"svec_dst" dst (fun dst ->
          let v' = unsafe_nth (u32_of_int i) v in
          (* From now on we copy all fields, as individual vector items cannot
           * be filtered: (TODO: why not BTW?) *)
          let ma = fieldmask_copy ma in
          ser1 sstate mn0 subpath mn v' ma dst |>
          loop (i + 1)) in
    loop 0 dst

  (* Thanks to cardinality and for_each being generic, arrs, lists and sets are
   * serialized the same: *)
  and slst mn ma sstate mn0 path v dst =
    let len = cardinality v in
    let dst = Ser.arr_opn mn (Some len) sstate mn0 path dst in
    let_ ~name:"dst_ref" (make_ref dst) (fun dst_ref ->
      let dst = get_ref dst_ref in
      let_ ~name:"n_ref" (make_ref (i32 0l)) (fun n_ref ->
        let n = get_ref n_ref in
        seq [
          for_each  ~name:"x" v (fun x ->
            let subpath = Path.(append (RunTime n) path) in
            let dst =
              if_ (gt n (i32 0l))
                ~then_:(Ser.arr_sep sstate mn0 subpath dst)
                ~else_:dst in
            (* From now on copy everything, as individual list items cannot
             * be selected - since lists are variable in length *)
            let ma = fieldmask_copy ma in
            seq [
              set_ref dst_ref (ser1 sstate mn0 subpath mn x ma dst) ;
              set_ref n_ref (add (i32 1l) n) ]) ;
          Ser.arr_cls mn sstate mn0 path dst ]))

  and stup mns ma sstate mn0 path v dst =
    let dst = Ser.tup_opn mns sstate mn0 path dst in
    let dst =
      Array.fold_lefti (fun dst i mn ->
        let subpath = Path.(append (CompTime i) path) in
        let_ ~name:"stup_dst"
          (if i = 0 then dst else
                    Ser.tup_sep sstate mn0 subpath dst)
          (fun dst ->
            let ma = fieldmask_get i ma in
            ser1 sstate mn0 subpath mn (get_item i v) ma dst)
      ) dst mns in
    Ser.tup_cls sstate mn0 path dst

  and srec mns ma sstate mn0 path v dst =
    let dst = Ser.rec_opn mns sstate mn0 path dst in
    let dst =
      Array.fold_lefti (fun dst i (fname, mn) ->
        let subpath = Path.(append (CompTime i) path) in
        let_ ~name:"srec_dst"
          (if i = 0 then dst else
                    Ser.rec_sep sstate mn0 subpath dst)
          (fun dst ->
            let ma = fieldmask_get i ma in
            comment ("serialize field "^ fname)
                    (ser1 sstate mn0 subpath mn (get_field fname v) ma dst))
      ) dst mns in
    Ser.rec_cls sstate mn0 path dst

  (* Regarding masks, sum types are like scalars: all or nothing.
   * Indeed, the mask would have to be aware of the constructor to be
   * meaningful. *)
  and ssum mns ma sstate mn0 path v dst =
    let max_lbl = Array.length mns - 1 in
    let_ ~name:"label1" (label_of v) (fun label ->
      let ssum_dst = Ser.sum_opn mns label sstate mn0 path dst in
      let_ ~name:"ssum_dst" ssum_dst (fun dst ->
        let rec choose_cstr i =
          let subpath = Path.(append (CompTime i) path) in
          assert (i <= max_lbl) ;
          let field, mn = mns.(i) in
          let v' = get_alt field v in
          if i = max_lbl then
            seq [
              assert_ (eq label (u16 (Uint16.of_int max_lbl))) ;
              ser1 sstate mn0 subpath mn v' ma dst ]
          else
            if_ (eq (u16 (Uint16.of_int i)) label)
              ~then_:(ser1 sstate mn0 subpath mn v' ma dst)
              ~else_:(choose_cstr (i + 1)) in
        let dst = choose_cstr 0 in
        Ser.sum_cls label sstate mn0 path dst))

  and svoid _ _ _ _ dst = dst

  and sext name ma =
    Ser.sext (fun v dst ->
      match ma with
      | RunTimeMask ma ->
          apply (type_method name (SerWithMask Ser.id)) [ ma ; v ; dst ]
      | CompTimeMask ->
          apply (type_method name (SerNoMask Ser.id)) [ v ; dst ])

  and sthis type_name ma sstate mn0 path v dst =
    let f =
      let mn = T.required (T.find_this type_name) in
      if T.eq_mn mn mn0 then
        myself (T.sptr_of_enc Ser.id)
      else
        let with_fieldmask = ma <> CompTimeMask in
        identifier (local_ser_for ~with_fieldmask type_name) in
    (* Call ourself recursively *)
    Ser.sext (fun v dst ->
      apply f (
        match ma with
        | RunTimeMask ma -> [ ma ; v ; dst ]
        | CompTimeMask -> [ v ; dst ])
    ) sstate mn0 path v dst

  and ser1 sstate mn0 path mn v ma dst =
    let rec ser_of_vt = function
      | T.TNamed (type_name, _) ->
          (* assume this had been defined already with [make_ser_for_subtypes]: *)
          sthis type_name ma
      | TThis type_name -> sthis type_name ma
      | TExt type_name -> sext type_name ma
      | TVoid -> svoid
      | TFloat -> Ser.sfloat
      | TString -> Ser.sstring
      | TBytes -> Ser.sbytes
      | TBool -> Ser.sbool
      | TChar -> Ser.schar
      | TI8 -> Ser.si8
      | TI16 -> Ser.si16
      | TI24 -> Ser.si24
      | TI32 -> Ser.si32
      | TI40 -> Ser.si40
      | TI48 -> Ser.si48
      | TI56 -> Ser.si56
      | TI64 -> Ser.si64
      | TI128 -> Ser.si128
      | TU8 -> Ser.su8
      | TU16 -> Ser.su16
      | TU24 -> Ser.su24
      | TU32 -> Ser.su32
      | TU40 -> Ser.su40
      | TU48 -> Ser.su48
      | TU56 -> Ser.su56
      | TU64 -> Ser.su64
      | TU128 -> Ser.su128
      | TUsr vt -> ser_of_vt vt.def
      | TTup mns -> stup mns ma
      | TRec mns -> srec mns ma
      | TSum mns -> ssum mns ma
      | TVec (dim, mn) -> svec dim mn ma
      | TArr mn -> slst mn ma
      | TLst mn -> slst mn ma
      | TSet (_, mn) -> slst mn ma
      | TMap _ -> assert false (* No value of map type *)
      | _ -> invalid_arg "ser1" in
    let on_copy =
      (* Copy or Recurse are handled the same: *)
      let vt = mn.typ in
      if mn.nullable then
        if_null v
          ~then_:(Ser.snull vt sstate mn0 path dst)
          ~else_:(
            let dst = Ser.snotnull vt sstate mn0 path dst in
            let ser = ser_of_vt vt in
            ser sstate mn0 path (force v) dst)
      else
        let ser = ser_of_vt vt in
        ser sstate mn0 path v dst
    in
    match ma with
    | RunTimeMask ma ->
        if_ (eq ma skip_field)
          ~then_:dst
          ~else_:(
            if_ (eq ma set_field_null)
              ~then_: (
                if mn.nullable then
                  Ser.snull mn.typ sstate mn0 path dst
                else (* Mask has been type checked *)
                  seq [ assert_ false_ ; dst ])
              ~else_:on_copy)
    | CompTimeMask ->
        on_copy

  let rec serialize_ sstate ~with_fieldmask type_name mn0 compunit =
    (* Pretend first that this sersize is defined, so that mutually recursive
     * calls can type-check: *)
    let name = local_ser_for ~with_fieldmask type_name in
    let sptr = T.sptr_of_enc Ser.id in
    let mn =
      if with_fieldmask then
        T.(func3 mask mn0 sptr sptr)
      else
        T.(func2 mn0 sptr sptr) in
    let compunit, _, name = U.add_identifier_of_type compunit ~name mn in
    (* Similarly to Materialize.make, construct all required serializers: *)
    let compunit =
      make_ser_for_subtypes sstate ~with_fieldmask compunit mn0.T.typ in
    let expr =
      if with_fieldmask then
        func3 T.mask mn0 sptr (fun ma v dst ->
          let path = [] in
          ser1 sstate mn0 path mn0 v (RunTimeMask ma) dst)
      else
        func2 mn0 sptr (fun v dst ->
          let path = [] in
          ser1 sstate mn0 path mn0 v CompTimeMask dst) in
    U.add_identifier_of_expression compunit ~name expr

  and make_ser_for_subtypes sstate ~with_fieldmask compunit =
    let make_ser_for_subtypes = make_ser_for_subtypes sstate ~with_fieldmask
    and serialize = serialize_ sstate ~with_fieldmask in
    function
    | T.TNamed (type_name, t) ->
        let compunit, _, _ = serialize type_name T.(required t) compunit in
        compunit
        (* No further recursion needed since [make] have added subtypes
         * already *)
    | TUsr { def ; _ } ->
        make_ser_for_subtypes compunit def
    | TVec (_, mn) | TArr mn | TSet (_, mn) | TLst mn ->
        make_ser_for_subtypes compunit mn.T.typ
    | TTup mns ->
        Array.fold (fun compunit mn ->
          make_ser_for_subtypes compunit mn.T.typ
        ) compunit mns
    | TRec mns | TSum mns ->
        Array.fold (fun compunit (_, mn) ->
          make_ser_for_subtypes compunit mn.T.typ
        ) compunit mns
    | TMap (kt, vt) ->
        let compunit = make_ser_for_subtypes compunit kt.T.typ in
        make_ser_for_subtypes compunit vt.T.typ
    | _ ->
        compunit

  let serialize ?config ?(with_fieldmask=true) ?(type_name="t") mn0 compunit =
    let mn0 = T.develop_this_mn mn0 in
    let sstate = Ser.make_state ?config mn0 in
    let compunit, id, name =
      serialize_ sstate ~with_fieldmask type_name mn0 compunit in
    let wrapper =
      if with_fieldmask then
        func3 T.mask mn0 T.ptr (fun ma v dst ->
          let dst = Ser.start mn0 sstate dst in
          let dst = apply id [ ma ; v ; dst ] in
          Ser.stop mn0 sstate dst)
      else
        func2 mn0 T.ptr (fun v dst ->
          let dst = Ser.start mn0 sstate dst in
          let dst = apply id [ v ; dst ] in
          Ser.stop mn0 sstate dst) in
    let name' = "wrap-"^ name in
    U.add_identifier_of_expression compunit ~name:name' wrapper

  (*
   * Compute the sersize of a expression:
   *)

  let local_ssize_for ~with_fieldmask type_name =
    (if type_name = "t" then "" else type_name ^"-")^
    string_of_type_method
      (if with_fieldmask then SSizeWithMask Ser.id else SSizeNoMask Ser.id)

  let rec ssvec dim mn ma mn0 path v sz =
    let sz =
      Ser.ssize_of_vec mn0 path v |> add sz in
    let rec loop sz i =
      if i >= dim then sz else
      let subpath = Path.(append (CompTime i) path) in
      let v' = unsafe_nth (u32_of_int i) v in
      let_ ~name:"sz" sz (fun sz ->
        let ma = fieldmask_copy ma in
        let sz = sersz1 mn mn0 subpath v' ma sz in
        loop sz (i + 1)) in
    loop sz 0

  (* thanks to cardinality and nth begin generic, arrs, lsts and sets are
   * serialized the same: *)
  and sslst mn ma mn0 path v sz =
    let sz =
      Ser.ssize_of_arr mn0 path v |> add sz in
    let_ ~name:"sz_ref" (make_ref sz) (fun sz_ref ->
      let sz = get_ref sz_ref in
      let len = cardinality v in
      seq [
        StdLib.repeat ~from:(i32 0l) ~to_:(to_i32 len) (fun n ->
          let v' = unsafe_nth n v in
          let subpath = Path.(append (RunTime n) path) in
          let ma = fieldmask_copy ma in
          sersz1 mn mn0 subpath v' ma sz |>
          set_ref sz_ref) ;
        sz ])

  and sstup mns ma mn0 path v sz =
    let sz =
      Ser.ssize_of_tup mn0 path v |> add sz in
    Array.fold_lefti (fun sz i mn ->
      let v' = get_item i v in
      let ma = fieldmask_get i ma in
      let subpath = Path.(append (CompTime i) path) in
      let_ ~name:"sz" sz (fun sz ->
        sersz1 mn mn0 subpath v' ma sz)
    ) sz mns

  and ssrec mns ma mn0 path v sz =
    let sz =
      Ser.ssize_of_rec mn0 path v |> add sz in
    Array.fold_lefti (fun sz i (fname, mn) ->
      let v' = get_field fname v in
      let ma = fieldmask_get i ma in
      let subpath = Path.(append (CompTime i) path) in
      let_ ~name:"sz" sz (fun sz ->
        comment ("sersize of field "^ fname)
                (sersz1 mn mn0 subpath v' ma sz))
    ) sz mns

  and sssum mns ma mn0 path v sz =
    let sz =
      Ser.ssize_of_sum mn0 path v |> add sz in
    let max_lbl = Array.length mns - 1 in
    let_ ~name:"label2"
      (label_of v)
      (fun label ->
        let rec choose_cstr i =
          let name, mn = mns.(i) in
          let v' = get_alt name v in
          let subpath = Path.(append (CompTime i) path) in
          assert (i <= max_lbl) ;
          (* From now on copy everything as individual elements an not be
           * cherry picked, since the actual type is unknown: *)
          let ma = fieldmask_copy ma in
          if i = max_lbl then
            seq [
              assert_ (eq label (u16 (Uint16.of_int max_lbl))) ;
              sersz1 mn mn0 subpath v' ma sz ]
          else
            if_ (eq (u16 (Uint16.of_int i)) label)
              ~then_:(sersz1 mn mn0 subpath v' ma sz)
              ~else_:(choose_cstr (i + 1)) in
        choose_cstr 0)

  and ssvoid _mn0 _path v sz =
    seq [ ignore_ v ; sz ]

  and ssext name ma _ _ v =
    match ma with
    | RunTimeMask ma ->
        apply (type_method name (SSizeWithMask Ser.id)) [ ma ; v ]
    | CompTimeMask ->
        apply (type_method name (SSizeNoMask Ser.id)) [ v ]

  and ssthis type_name ma mn0 _path v =
    let f =
      let mn = T.required (T.find_this type_name) in
      if T.eq_mn mn mn0 then
        myself T.size
      else
        let with_fieldmask = ma <> CompTimeMask in
        identifier (local_ssize_for ~with_fieldmask type_name) in
    apply f (
      match ma with
      | RunTimeMask ma -> [ ma ; v ]
      | CompTimeMask -> [ v ])

  and sersz1 mn mn0 path v ma sz =
    let cumul ssizer mn0 path v sz =
      add sz (ssizer mn0 path v) in
    let rec ssz_of_vt = function
      | T.TNamed (type_name, _)
      | TThis type_name -> cumul (ssthis type_name ma)
      | TExt type_name -> cumul (ssext type_name ma)
      | TVoid -> ssvoid
      | TFloat -> cumul Ser.ssize_of_float
      | TString -> cumul Ser.ssize_of_string
      | TBytes -> cumul Ser.ssize_of_bytes
      | TBool -> cumul Ser.ssize_of_bool
      | TChar -> cumul Ser.ssize_of_char
      | TI8 -> cumul Ser.ssize_of_i8
      | TI16 -> cumul Ser.ssize_of_i16
      | TI24 -> cumul Ser.ssize_of_i24
      | TI32 -> cumul Ser.ssize_of_i32
      | TI40 -> cumul Ser.ssize_of_i40
      | TI48 -> cumul Ser.ssize_of_i48
      | TI56 -> cumul Ser.ssize_of_i56
      | TI64 -> cumul Ser.ssize_of_i64
      | TI128 -> cumul Ser.ssize_of_i128
      | TU8 -> cumul Ser.ssize_of_u8
      | TU16 -> cumul Ser.ssize_of_u16
      | TU24 -> cumul Ser.ssize_of_u24
      | TU32 -> cumul Ser.ssize_of_u32
      | TU40 -> cumul Ser.ssize_of_u40
      | TU48 -> cumul Ser.ssize_of_u48
      | TU56 -> cumul Ser.ssize_of_u56
      | TU64 -> cumul Ser.ssize_of_u64
      | TU128 -> cumul Ser.ssize_of_u128
      | TUsr vt -> ssz_of_vt vt.def
      | TVec (dim, mn) -> ssvec dim mn ma
      | TTup mns -> sstup mns ma
      | TRec mns -> ssrec mns ma
      | TSum mns -> sssum mns ma
      | TArr mn -> sslst mn ma
      | TLst mn -> sslst mn ma
      | TSet (_, mn) -> sslst mn ma
      | TMap _ -> assert false (* No value of map type *)
      | _ -> invalid_arg "sersz1" in
    let on_copy =
      let vt = mn.typ in
      if mn.nullable then
        if_null v
          ~then_:(add sz (Ser.ssize_of_null mn0 path))
          ~else_:(add (ssz_of_vt vt mn0 path (force v) sz)
                      (Ser.ssize_of_notnull mn0 path))
      else
        ssz_of_vt vt mn0 path v sz
    in
    match ma with
    | RunTimeMask ma ->
        if_ (eq ma skip_field)
          ~then_:sz
          ~else_:(
            if_ (eq ma set_field_null)
              ~then_:(
                if mn.nullable then
                  add sz (Ser.ssize_of_null mn0 path)
                else
                  seq [ assert_ false_ ; sz ])
              ~else_:on_copy)
    | CompTimeMask ->
        on_copy

  let rec sersize_ ?config ?(with_fieldmask=true) ?(type_name="t") mn0 compunit =
    (* Pretend first that this sersize is defined, so that mutually recursive
     * calls can type-check: *)
    let name = local_ssize_for ~with_fieldmask type_name in
    let mn =
      if with_fieldmask then
        T.(func2 mask mn0 size)
      else
        T.(func1 mn0 size) in
    let compunit, _, name = U.add_identifier_of_type compunit ~name mn in
    (* Defines all required deserializers for subtypes: *)
    let compunit =
      make_ssize_for_subtypes ?config ~with_fieldmask compunit mn0.T.typ in
    let expr =
      if with_fieldmask then
        func2 T.mask mn0 (fun ma v ->
          let sz = Ser.ssize_start ?config mn0 in
          sersz1 mn0 mn0 [] v (RunTimeMask ma) sz)
      else
        func1 mn0 (fun v ->
          let sz = Ser.ssize_start ?config mn0 in
          sersz1 mn0 mn0 [] v CompTimeMask sz) in
    U.add_identifier_of_expression compunit ~name expr

  and make_ssize_for_subtypes ?config ~with_fieldmask compunit =
    let make_ssize_for_subtypes =
      make_ssize_for_subtypes ?config ~with_fieldmask
    and sersize = sersize_ ?config ~with_fieldmask in
    function
    | T.TNamed (type_name, t) ->
        let compunit, _, _ = sersize ~type_name T.(required t) compunit in
        compunit
        (* No further recursion needed since [make] have added subtypes
         * already *)
    | TUsr { def ; _ } ->
        make_ssize_for_subtypes compunit def
    | TVec (_, mn) | TArr mn | TSet (_, mn) | TLst mn ->
        make_ssize_for_subtypes compunit mn.T.typ
    | TTup mns ->
        Array.fold (fun compunit mn ->
          make_ssize_for_subtypes compunit mn.T.typ
        ) compunit mns
    | TRec mns | TSum mns ->
        Array.fold (fun compunit (_, mn) ->
          make_ssize_for_subtypes compunit mn.T.typ
        ) compunit mns
    | TMap (kt, vt) ->
        let compunit = make_ssize_for_subtypes compunit kt.T.typ in
        make_ssize_for_subtypes compunit vt.T.typ
    | _ ->
        compunit

  let sersize ?config ?with_fieldmask ?type_name mn0 compunit =
    let mn0 = T.develop_this_mn mn0 in
    sersize_ ?config ?with_fieldmask ?type_name mn0 compunit
end
