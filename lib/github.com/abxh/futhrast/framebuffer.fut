import "config"

-- | 2D axis-aligned bounding box
type aabb2D 'a =
  { xmin: i64
  , xmax: i64
  , ymin: i64
  , ymax: i64
  }

-- | tile type
type tile [tile_size] 'target =
  { buffer: [tile_size * tile_size](target, f32)
  , bbox: aabb2D i64
  }

-- | tile bin type
type tile_bin [tile_bin_size] [tile_size] 'target =
  { tiles: [tile_bin_size * tile_bin_size](tile [tile_size] target)
  , bbox: aabb2D i64
  }

-- | Tiled framebuffer specification
module type FramebufferSpec = {
  -- config module
  module Config: ConfigSpec

  -- | the framebuffer type
  type~ t

  -- | the target type
  type target

  -- | initialize a new framebuffer
  val init : {h: i64, w: i64} -> target -> t

  -- | retrieve width
  val w : t -> i64

  -- | retrieve height
  val h : t -> i64

  -- | retrieve number of tile bins in horizontal direction
  val bins_w : t -> i64

  -- | retrieve number of tile bins in vertical direction
  val bins_h : t -> i64

  -- | get target buffer
  val target_buf : (fb: t) -> *[h fb][w fb]target

  -- | get depth buffer with [0;1] corresponding to [far;near] and (-1) as neutral element.
  val depth_buf : (fb: t) -> *[h fb][w fb]f32

  -- | read tile bins
  val get_bins : (fb: t) -> [bins_h fb * bins_w fb](tile_bin [Config.tile_bin_size] [Config.tile_size] target)

  -- | write tile bins
  val set_bins : (fb: t) -> [bins_h fb * bins_w fb](tile_bin [Config.tile_bin_size] [Config.tile_size] target) -> t
}

-- | Tiled framebuffer implementation
module Framebuffer (Config: ConfigSpec) (Target: {type t})
  : FramebufferSpec
    with target = Target.t = {
  module Config = Config

  type target = Target.t

  type~ t =
    { w: i64
    , h: i64
    , bins_w: i64
    , bins_h: i64
    , bins: [](tile_bin [Config.tile_bin_size] [Config.tile_size] target)
    }

  def init {w = w: i64, h = h: i64} (target_default: Target.t) : t =
    let bin_extent = Config.tile_size * Config.tile_bin_size
    let total_tiles_w = (w + (Config.tile_size - 1)) / Config.tile_size
    let total_tiles_h = (h + (Config.tile_size - 1)) / Config.tile_size
    let bins_w = (total_tiles_w + (Config.tile_bin_size - 1)) / Config.tile_bin_size
    let bins_h = (total_tiles_h + (Config.tile_bin_size - 1)) / Config.tile_bin_size
    let mk_tile (ty: i64) (tx: i64) =
      { buffer = replicate Config.tile_size (replicate Config.tile_size (target_default, -1f32)) |> flatten
      , bbox =
          { xmin = Config.tile_size * tx
          , ymin = Config.tile_size * ty
          , xmax = Config.tile_size * (tx + 1) `i64.min` w
          , ymax = Config.tile_size * (ty + 1) `i64.min` h
          }
      }
    let mk_bin (by: i64) (bx: i64) =
      { tiles =
          tabulate_2d Config.tile_bin_size
                      Config.tile_bin_size
                      (\y x ->
                         let ty = by * Config.tile_bin_size + y
                         let tx = bx * Config.tile_bin_size + x
                         in mk_tile ty tx)
          |> flatten
      , bbox =
          { xmin = bin_extent * bx
          , ymin = bin_extent * by
          , xmax = bin_extent * (bx + 1) `i64.min` w
          , ymax = bin_extent * (by + 1) `i64.min` h
          }
      }
    in { w
       , h
       , bins_w
       , bins_h
       , bins = tabulate_2d bins_h bins_w mk_bin |> flatten
       }

  def w (fb: t) = fb.w
  def h (fb: t) = fb.h

  def bins_w (fb: t) = fb.bins_w
  def bins_h (fb: t) = fb.bins_h

  def target_buf (fb: t) : *[h fb][w fb]target =
    tabulate_2d (h fb)
                (w fb)
                (\y x ->
                   let bin_extent = Config.tile_size * Config.tile_bin_size
                   let by = y / bin_extent
                   let bx = x / bin_extent
                   let iy = y % bin_extent
                   let ix = x % bin_extent
                   let ty = iy / Config.tile_size
                   let tx = ix / Config.tile_size
                   let py = iy % Config.tile_size
                   let px = ix % Config.tile_size
                   in fb.bins[by * fb.bins_w + bx].tiles[ty * Config.tile_bin_size + tx].buffer[py * Config.tile_size + px].0)

  def depth_buf (fb: t) : *[h fb][w fb]f32 =
    tabulate_2d (h fb)
                (w fb)
                (\y x ->
                   let bin_extent = Config.tile_size * Config.tile_bin_size
                   let by = y / bin_extent
                   let bx = x / bin_extent
                   let iy = y % bin_extent
                   let ix = x % bin_extent
                   let ty = iy / Config.tile_size
                   let tx = ix / Config.tile_size
                   let py = iy % Config.tile_size
                   let px = ix % Config.tile_size
                   in fb.bins[by * fb.bins_w + bx].tiles[ty * Config.tile_bin_size + tx].buffer[py * Config.tile_size + px].1)

  def get_bins (fb: t) = fb.bins |> sized (bins_h fb * bins_w fb)

  def set_bins (fb: t)
               (bins: [bins_h fb * bins_w fb](tile_bin [Config.tile_bin_size] [Config.tile_size] target)) : t =
    fb with bins = bins
}
