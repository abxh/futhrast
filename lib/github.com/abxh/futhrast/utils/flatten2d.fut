-- | flatten 2d with different 1d indexing patterns
--
-- based on:
-- https://www.tugraz.at/fileadmin/user_upload/Institute/ICG/Downloads/team_steinberger/Publications/EffectiveStaticBinPatternsForSortMiddleRendering.pdf
-- https://fgiesen.wordpress.com/2011/01/17/texture-tiling-and-swizzling/

module type index_pattern = {
  type~ t
  val setup : {w: i64, h: i64} -> t
  val flatten : t -> (i64, i64) -> i64
  val unflatten : t -> i64 -> (i64, i64)
}

-- linear indexing pattern implementation
module linear_pattern : index_pattern = {
  type t = {w: i64}

  def setup {w = w, h = _} =
    {w}

  def flatten {w = w: i64} (y: i64, x: i64) =
    y * w + x

  def unflatten {w = w: i64} (i: i64) =
    let y = i / w
    let x = i %% w
    in (y, x)
}

-- diagonal pattern implementation
module diagonal_pattern : index_pattern = {
  type t = {w: i64, h: i64}

  def setup = id

  def flatten (s: t) (y: i64, x: i64) =
    let y' = s.h - y - 1
    let n = s.w
    let x' = (x - y - 1 + n) %% n
    in y' * n + x'

  def unflatten (s: t) (i: i64) =
    let n = s.w
    let y' = i / n
    let x' = i %% n
    let y = s.h - y' - 1
    let x = (x' + y + 1) %% n
    in (y, x)
}

-- x-shift+offset pattern implementation
module xshift_offset_pattern : index_pattern = {
  type t = {w: i64, h: i64, k: i64}

  def setup {w = w: i64, h = h: i64} =
    {w, h, k = i64.f32 <| f32.sqrt (f32.i64 w)}

  def flatten (s: t) (y: i64, x: i64) =
    let y' = s.h - y - 1
    let n = s.w
    let shift = ((y * (n + 1)) / s.k) %% n
    let x' = (x + shift) %% n
    in y' * n + x'

  def unflatten (s: t) (i: i64) =
    let n = s.w
    let y' = i / n
    let x' = i %% n
    let y = s.h - y' - 1
    let shift = ((y * (n + 1)) / s.k) %% n
    let x = (x' - shift + n) %% n
    in (y, x)
}

-- 16-bit morton pattern implementation
module morton_u16_pattern : index_pattern with t = () = {
  type t = ()

  def is_pow2 (n: i64) : bool = n & (n - 1) == 0

  def setup {w = w: i64, h = h: i64} =
    assert (w <= i64.u8 u8.highest
            && h <= i64.u8 u8.highest
            && is_pow2 w
            && is_pow2 h)
    ()

  def mask = i64.u16 u16.highest

  def part1by1 (x: i64) =
    let x = x & mask
    let x = (x | (x << 4)) & 0b0000111100001111
    let x = (x | (x << 2)) & 0b0011001100110011
    let x = (x | (x << 1)) & 0b0101010101010101
    in x

  def compact1by1 (x: i64) =
    let x = x & 0b0101010101010101
    let x = (x ^ (x >> 1)) & 0b0011001100110011
    let x = (x ^ (x >> 2)) & 0b0000111100001111
    let x = (x ^ (x >> 4)) & 0b0000000011111111
    in x

  def flatten () (y: i64, x: i64) =
    let x16 = x & mask
    let y16 = y & mask
    let i = (part1by1 (y16) << 1) | part1by1 (x16)
    in i

  def unflatten () (i: i64) =
    let x = compact1by1 (i)
    let y = compact1by1 (i >> 1)
    in (y, x)
}

-- 32-bit morton pattern implementation
module morton_u32_pattern : index_pattern with t = () = {
  type t = ()

  def is_pow2 (n: i64) : bool = n & (n - 1) == 0

  def setup {w = w: i64, h = h: i64} =
    assert (w <= i64.u16 u16.highest
            && h <= i64.u16 u16.highest
            && is_pow2 w
            && is_pow2 h)
    ()

  def mask = i64.u32 u32.highest

  def part1by1 (x: i64) =
    let x = x & mask
    let x = (x | (x << 8)) & 0b00000000111111110000000011111111
    let x = (x | (x << 4)) & 0b00001111000011110000111100001111
    let x = (x | (x << 2)) & 0b00110011001100110011001100110011
    let x = (x | (x << 1)) & 0b01010101010101010101010101010101
    in x

  def compact1by1 (x: i64) =
    let x = x & 0b01010101010101010101010101010101
    let x = (x ^ (x >> 1)) & 0b00110011001100110011001100110011
    let x = (x ^ (x >> 2)) & 0b00001111000011110000111100001111
    let x = (x ^ (x >> 4)) & 0b00000000111111110000000011111111
    let x = (x ^ (x >> 8)) & 0b00000000000000001111111111111111
    in x

  def flatten () (y: i64, x: i64) =
    let x16 = x & mask
    let y16 = y & mask
    let i = (part1by1 (y16) << 1) | part1by1 (x16)
    in i

  def unflatten () (i: i64) =
    let x = compact1by1 i
    let y = compact1by1 (i >> 1)
    in (y, x)
}

def w : i64 = 8
def h : i64 = 8
def num_rasterizers : i64 = 8

def visualize_linear =
  let s = linear_pattern.setup {w, h}
  let xs = tabulate_2d h w (\y x -> 1 + ((y * w + x) % num_rasterizers))
  let is =
    tabulate_2d h w (\y x ->
                       let i = linear_pattern.flatten s (y, x)
                       let y' = i / w
                       let x' = i %% w
                       let y' = h - y' - 1
                       in (y', x'))
    |> reverse
    |> flatten
  in scatter_2d (copy xs) is (flatten xs)

def visualize_diagonal =
  let s = diagonal_pattern.setup {w, h}
  let xs = tabulate_2d h w (\y x -> 1 + ((y * w + x) % num_rasterizers))
  let is =
    tabulate_2d h w (\y x ->
                       let i = diagonal_pattern.flatten s (y, x)
                       let y' = i / w
                       let x' = i %% w
                       let y' = h - y' - 1
                       in (y', x'))
    |> reverse
    |> flatten
  in scatter_2d (copy xs) is (flatten xs)

def visualize_xshift_offset =
  let s = xshift_offset_pattern.setup {w, h}
  let xs = tabulate_2d h w (\y x -> 1 + ((y * w + x) % num_rasterizers))
  let is =
    tabulate_2d h w (\y x ->
                       let i = xshift_offset_pattern.flatten s (y, x)
                       let y' = i / w
                       let x' = i %% w
                       let y' = h - y' - 1
                       in (y', x'))
    |> reverse
    |> flatten
  in scatter_2d (copy xs) is (flatten xs)

def visualize_morton_u16 =
  let w = 4
  let h = 4
  let s = morton_u16_pattern.setup {w, h}
  in tabulate_2d h w (\y x -> morton_u16_pattern.flatten s (y, x)) |> flatten

def visualize_morton_u32 =
  let w = 4
  let h = 4
  let s = morton_u32_pattern.setup {w, h}
  in tabulate_2d h w (\y x -> morton_u32_pattern.flatten s (y, x)) |> flatten
