-- | fixed point number implementation
--
-- Implements 32-bit fixed point number with 8 fractional bits.
--
-- Inspiration: github.com/MikeLankamp/fpm
--
-- exposes:
-- module fixedpoint = mk_fixedpoint abstract_i32
-- module vec2fp = mk_vspace2 fixedpoint
-- module vec3fp = mk_vspace3 fixedpoint
-- module vec4fp = mk_vspace4 fixedpoint

import "vec"

local
module type abstract_i32_spec = {
  type t

  val raw : i32 -> t
  val to_raw : t -> i32

  val neg : t -> t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val (*) : t -> t -> t
  val (/) : t -> t -> t
  val (%) : t -> t -> t

  val (>) : t -> t -> bool
  val (<) : t -> t -> bool
  val (>=) : t -> t -> bool
  val (<=) : t -> t -> bool
  val min: t -> t -> t
  val max: t -> t -> t
  val abs: t -> t

  val to_i32 : t -> i32
  val to_i64 : t -> i64
  val to_f32 : t -> f32
  val f32 : f32 -> t
  val i32 : i32 -> t
  val i64 : i64 -> t
}

local
module abstract_i32 : abstract_i32_spec = {
  def raw = id
  def to_raw = id

  def to_f32 = f32.i32
  def to_i32 = id

  open i32
}

local
module type mk_fixedpoint_spec = {
  type t
  val num_fractional_bits : i64

  val raw : i32 -> t
  val to_raw : t -> i32

  val zero : t
  val one : t

  val neg : t -> t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val (*) : t -> t -> t
  val (/) : t -> t -> t
  val (>) : t -> t -> bool
  val (<) : t -> t -> bool
  val (>=) : t -> t -> bool
  val (<=) : t -> t -> bool
  val min: t -> t -> t
  val max: t -> t -> t
  val abs: t -> t

  val to_i32 : t -> i32
  val to_i64 : t -> i64
  val to_f32 : t -> f32
  val f32 : f32 -> t
  val i32 : i32 -> t
  val i64 : i64 -> t
}

local
module mk_fixedpoint (V: abstract_i32_spec) : mk_fixedpoint_spec = {
  type t = V.t

  def num_fractional_bits : i64 = 8

  def raw = (V.raw)
  def to_raw = (V.to_raw)

  def neg = (V.neg)
  def (+) = (V.+)
  def (-) = (V.-)

  def (*) (lhs: V.t) (rhs: V.t) =
    let (lhs', rhs') = (i64.i32 <| V.to_raw lhs, i64.i32 <| V.to_raw rhs)
    in (i64.>>) ((i64.*) lhs' rhs') num_fractional_bits |> i32.i64 |> V.raw

  def (/) (lhs: V.t) (rhs: V.t) =
    let (lhs', rhs') = (i64.i32 <| V.to_raw lhs, i64.i32 <| V.to_raw rhs)
    in ((i64./) ((i64.<<) lhs' num_fractional_bits) rhs') |> i32.i64 |> V.raw

  def (<=) = (V.<=)
  def (>=) = (V.>=)
  def (<) = (V.<)
  def (>) = (V.>)
  def min = (V.min)
  def max = (V.max)
  def abs = (V.abs)

  def to_f32 (x: V.t) : f32 =
    f32.from_fraction (i64.i32 <| V.to_raw x) (1 << num_fractional_bits)

  def to_i32 (x: V.t) : i32 =
    (i32.>>) (V.to_raw x) (i32.i64 num_fractional_bits)

  def to_i64 = to_i32 >-> i64.i32

  def i32 (x: i32) : V.t = (i64.<<) (i64.i32 x) num_fractional_bits |> V.i64
  def i64 (x: i64) : V.t = (i64.<<) x num_fractional_bits |> V.i64

  def f32 (x: f32) : V.t =
    (f32.round ((f32.*) x (f32.i64 (1 << num_fractional_bits))))
    |> f32.to_i64
    |> V.i64

  def zero = i32 0
  def one = i32 1
}

module fixedpoint = mk_fixedpoint abstract_i32
module vec2fp = mk_vspace2 fixedpoint
module vec3fp = mk_vspace3 fixedpoint
module vec4fp = mk_vspace4 fixedpoint
