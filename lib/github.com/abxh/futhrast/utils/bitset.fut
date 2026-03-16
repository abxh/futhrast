-- | bitset
--
-- makes use of the ctz, clz and popc instructions for further efficiency
-- and assumes relatively small-sized bitsets

import "../../../diku-dk/segmented/segmented"

-- | bitset specification
module type bitset_spec = {
  -- | underlying type.
  type t

  -- | initialize empty bitset
  val empty : t

  -- | initalize bitset with size and predicate
  val init : i64 -> (i64 -> bool) -> t

  -- | check if index is a member
  val member : i64 -> t -> bool

  -- | population count.
  val size : t -> i64

  -- | convert to array of indices (of bits set to true)
  val to_array : t -> *[]i64
}

-- | bitset implementation
module bitset (M: integral) (S: {val max_bits : i64}) : bitset_spec = {
  def word_size = i64.i32 M.num_bits
  def max_words = (S.max_bits + word_size - 1) / word_size

  type t = [max_words]M.t

  def empty = replicate max_words (M.i32 0)

  def init (n: i64) (pred: (i64 -> bool)) : t =
    let f (word_i: i64) =
      loop w = M.i32 0
      for bit_i < i64.i32 M.num_bits do
        let idx = word_i * word_size + bit_i
        let flag = idx < n && pred idx
        in M.set_bit (i32.i64 bit_i) w (i32.bool flag)
    in tabulate max_words f

  def member (i: i64) (b: t) : bool =
    let word_i = i / word_size
    let bit_i = i % word_size |> i32.i64
    in if word_i >= max_words
       then false
       else M.get_bit bit_i b[word_i] |> bool.i32

  def size (b: t) : i64 =
    map (i64.i32 <-< M.popc) b
    |> i64.sum

  def is_lsb : bool = M.ctz (M.i32 1) == 0

  def set_bit_positions (word: M.t): *[word_size]i64 =
    let (_, res) =
      loop (w, res) = (word, replicate word_size (-1i64))
      for i in 0..<M.popc word do
        let pos = if is_lsb then M.ctz w else M.clz w
        let mask = (M.<<) (M.i32 1) (M.i32 pos)
        in ((M.&) w (M.not mask), res with [i] = i64.i32 pos)
    in res

  def to_array (b: t) : *[]i64 =
    let szs = map (i64.i32 <-< M.popc) b
    let (idxs, iotas) = repl_segm_iota szs
    let lookup = tabulate max_words (\i -> set_bit_positions b[i])
    in map2 (\word_i i -> word_i * word_size + lookup[word_i, i]) idxs iotas
}
