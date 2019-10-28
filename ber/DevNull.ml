(* A special serializer that drops everything on the floor. Useful to time
 * desserializers. *)
open Batteries
open Stdint
open Dessert
open Helpers

module Ser : SER =
struct
  module SerData = IntRepr.SerData
  type pointer = SerData.pointer
  type 'a ser = ('a * pointer -> pointer) code

  let sfloat = .< snd >.
  let sstring = .< snd >.
  let sbool = .< snd >.
  let si8 = .< snd >.
  let su8 = .< snd >.
  let si16 = .< snd >.
  let su16 = .< snd >.
  let si32 = .< snd >.
  let su32 = .< snd >.
  let si64 = .< snd >.
  let su64 = .< snd >.
  let si128 = .< snd >.
  let su128 = .< snd >.
  let tup_opn _ pc = pc
  let tup_cls _ pc = pc
  let tup_sep _ _ pc = pc
  let vec_opn _ _ pc = pc
  let vec_cls _ _ pc = pc
  let vec_sep _ _ _ pc = pc
  let snull = identity
  let snotnull = identity
end
