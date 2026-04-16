-- | tiny bitmask implementation
--
-- Inspiration:
-- github.com/athas/vector
-- github.com/diku-dk/containers
--
-- exposed modules:
-- module bitmask_64 = bitmask_1 u64 {def find_ith_set_bit = find_ith_set_bit_u64}
-- module bitmask_128 = cat_bitmask bitmask_64 bitmask_64
-- module bitmask_256 = cat_bitmask bitmask_128 bitmask_128

local
def find_ith_set_bit_u8 (b: u8) (i: i64) : i64 =
  let f b = b & (b - 1)
  let s0 = b
  let s1 = f s0
  let s2 = f s1
  let s3 = f s2
  let s4 = f s3
  let s5 = f s4
  let s6 = f s5
  let s7 = f s6
  let w =
    match i
    case 0 -> s0
    case 1 -> s1
    case 2 -> s2
    case 3 -> s3
    case 4 -> s4
    case 5 -> s5
    case 6 -> s6
    case 7 -> s7
    case _ -> 0u8
  in u8.ctz w |> i64.i32

local
def find_ith_set_bit_u16 (b: u16) (i: i64) : i64 =
  let lower = u8.u16 b
  let upper = b >> 8 |> u8.u16
  let low_count = u8.popc lower |> i64.i32
  in if i < low_count
     then find_ith_set_bit_u8 lower i
     else find_ith_set_bit_u8 upper (i - low_count) + 8

local
def find_ith_set_bit_u32 (b: u32) (i: i64) : i64 =
  let lower = u16.u32 b
  let upper = b >> 16 |> u16.u32
  let low_count = u16.popc lower |> i64.i32
  in if i < low_count
     then find_ith_set_bit_u16 lower i
     else find_ith_set_bit_u16 upper (i - low_count) + 16

local
def find_ith_set_bit_u64 (b: u64) (i: i64) : i64 =
  let lower = u32.u64 b
  let upper = b >> 32 |> u32.u64
  let low_count = u32.popc lower |> i64.i32
  in if i < low_count
     then find_ith_set_bit_u32 lower i
     else find_ith_set_bit_u32 upper (i - low_count) + 32

local
module type bitmask = {
  type t
  val num_bits : i64
  val empty : t
  val is_empty : t -> bool
  val size : t -> i64

  val union : t -> t -> t
  val intersection : t -> t -> t
  val difference : t -> t -> t
  val complement : t -> t
  val is_subset : t -> t -> bool
  val (==) : t -> t -> bool

  val member : t -> i64 -> bool
  val set : t -> i64 -> bool -> t
  val from_single_bit : i64 -> bool -> t
  val find_ith_set_bit : t -> i64 -> i64

  val from_pred : (i64 -> bool) -> t
  val from_pred_par : (i64 -> bool) -> t
  val to_array : t -> [num_bits]bool
}

local
module bitmask_1 (T: integral) (F: {val find_ith_set_bit : T.t -> i64 -> i64}) : bitmask = {
  type t = T.t

  def num_bits = i64.i32 T.num_bits
  def empty = T.i32 0
  def is_empty = (T.==) empty
  def size (b: t) = T.popc b |> i64.i32

  def complement (x: t) = (T.not) x
  def union (l: t) (r: t) = (T.|) l r
  def intersection (l: t) (r: t) = (T.&) l r
  def difference (l: t) (r: t) = l `intersection` complement r
  def is_subset (l: t) (r: t) = (l `difference` r) T.== empty
  def (==) = (T.==)

  def member (b: t) (pos: i64) = T.get_bit (i32.i64 pos) b i32.== 1
  def set (b: t) (pos: i64) (v: bool) = T.set_bit (i32.i64 pos) b (i32.bool v)
  def from_single_bit (pos: i64) (v: bool) : t = (T.<<) (T.bool v) (T.i64 pos)

  def find_ith_set_bit = F.find_ith_set_bit

  def from_pred (f: i64 -> bool) : t =
    loop b = empty
    for pos in 0..<num_bits do
      set b pos (f pos)

  def from_pred_par (f: i64 -> bool) : t =
    iota num_bits
    |> map f
    |> zip (iota num_bits)
    |> map (uncurry from_single_bit)
    |> reduce_comm union empty

  def to_array (b: t) : []bool =
    tabulate num_bits (member b)
}

local
module cat_bitmask (L: bitmask) (R: bitmask) : bitmask = {
  type t = (L.t, R.t)

  def num_bits = L.num_bits + R.num_bits
  def empty = (L.empty, R.empty)
  def is_empty (l, r) = L.is_empty l && R.is_empty r
  def size (l, r) = L.size l + R.size r

  def complement (l, r) = (L.complement l, R.complement r)
  def union (l0, r0) (l1, r1) = (l0 `L.union` l1, r0 `R.union` r1)
  def intersection (l0, r0) (l1, r1) = (l0 `L.intersection` l1, r0 `R.intersection` r1)
  def difference (l0, r0) (l1, r1) = (l0 `L.difference` l1, r0 `R.difference` r1)
  def is_subset (l0, r0) (l1, r1) = (l0 `L.is_subset` l1) && (r0 `R.is_subset` r1)
  def (==) (l0, r0) (l1, r1) = (l0 L.== l1) && (r0 R.== r1)

  def member (l, r) (pos: i64) =
    if pos < L.num_bits
    then L.member l pos
    else R.member r (pos - L.num_bits)

  def set (l, r) (pos: i64) (v: bool) =
    if pos < L.num_bits
    then (L.set l pos v, r)
    else (l, R.set r (pos - L.num_bits) v)

  def from_single_bit (pos: i64) (v: bool) : t =
    if pos < L.num_bits
    then (L.from_single_bit pos v, R.empty)
    else (L.empty, R.from_single_bit (pos - L.num_bits) v)

  def find_ith_set_bit (l, r) (i: i64) : i64 =
    let l_size = L.size l
    in if i < l_size
       then L.find_ith_set_bit l i
       else R.find_ith_set_bit r (i - l_size) + L.num_bits

  def from_pred (f: i64 -> bool) : t =
    let l_pred i = f i
    let r_pred i = f (i + L.num_bits)
    in (L.from_pred l_pred, R.from_pred r_pred)

  def from_pred_par (f: i64 -> bool) : t =
    let l_pred i = f i
    let r_pred i = f (i + L.num_bits)
    in (L.from_pred l_pred, R.from_pred r_pred)

  def to_array (b: t) : []bool =
    tabulate num_bits (member b)
}

module bitmask_64 = bitmask_1 u64 {def find_ith_set_bit = find_ith_set_bit_u64}
module bitmask_128 = cat_bitmask bitmask_64 bitmask_64
module bitmask_256 = cat_bitmask bitmask_128 bitmask_128
