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

-- | Tiled framebuffer specification
module type FramebufferSpec = {
  -- config module
  module Config: ConfigSpec

  -- | the framebuffer type
  type~ t

  -- | the target type
  type target

  -- | the tile size
  val tile_size : i64

  -- | initialize a new framebuffer
  val init : {h: i64, w: i64} -> target -> t

  -- | retrieve width
  val w : t -> i64

  -- | retrieve height
  val h : t -> i64

  -- | retrieve number of tiles in horizontal direction
  val tiles_w : t -> i64

  -- | retrieve number of tiles in vertical direction
  val tiles_h : t -> i64

  -- | get target buffer
  val target_buf : (fb: t) -> *[h fb][w fb]target

  -- | get depth buffer with [0;1] corresponding to [far;near] and (-1) as neutral element.
  val depth_buf : (fb: t) -> *[h fb][w fb]f32

  -- | read tiles
  val tiles : (fb: t) -> [tiles_h fb * tiles_w fb](tile [tile_size] target)

  -- | default tile, such that merging it with any other tile with result in the other tile
  val default_tile : aabb2D i64 -> tile [tile_size] target

  -- | non-commutative tile merging operation
  val merge_tiles : tile [tile_size] target -> tile [tile_size] target -> tile [tile_size] target

  -- | write tiles
  val set_tiles : (fb: t) -> [tiles_h fb * tiles_w fb](tile [tile_size] target) -> t
}

module type TargetSpec = {
  type t
  val dummy : t
}

-- | Tiled framebuffer implementation
module Framebuffer (Config: ConfigSpec) (Target: TargetSpec)
  : FramebufferSpec
    with target = Target.t = {
  module Config = Config

  type target = Target.t

  def tile_size = Config.tile_size

  type~ t =
    { w: i64
    , h: i64
    , tiles_w: i64
    , tiles_h: i64
    , tiles: [](tile [tile_size] target)
    }

  def init {w = w: i64, h = h: i64} (target_default: Target.t) : t =
    let tiles_w = (w + (tile_size - 1)) / tile_size
    let tiles_h = (h + (tile_size - 1)) / tile_size
    let mk_tile (ty: i64) (tx: i64) =
      { buffer = replicate tile_size (replicate tile_size (target_default, -1f32)) |> flatten
      , bbox =
          { xmin = tile_size * tx
          , ymin = tile_size * ty
          , xmax = tile_size * (tx + 1) `i64.min` w
          , ymax = tile_size * (ty + 1) `i64.min` h
          }
      }
    in { w
       , h
       , tiles_w
       , tiles_h
       , tiles = tabulate_2d tiles_h tiles_w mk_tile |> flatten
       }

  def w (fb: t) = fb.w
  def h (fb: t) = fb.h
  def tiles_w (fb: t) = fb.tiles_w
  def tiles_h (fb: t) = fb.tiles_h

  def target_buf (fb: t) : *[h fb][w fb]target =
    tabulate_2d (h fb)
                (w fb)
                (\y x ->
                   let ty = y / tile_size
                   let tx = x / tile_size
                   let py = y % tile_size
                   let px = x % tile_size
                   in fb.tiles[ty * fb.tiles_w + tx].buffer[py * tile_size + px].0)

  def depth_buf (fb: t) : *[h fb][w fb]f32 =
    tabulate_2d (h fb)
                (w fb)
                (\y x ->
                   let ty = y / tile_size
                   let tx = x / tile_size
                   let py = y % tile_size
                   let px = x % tile_size
                   in fb.tiles[ty * fb.tiles_w + tx].buffer[py * tile_size + px].1)

  def tiles (fb: t) = fb.tiles |> sized (tiles_h fb * tiles_w fb)

  def default_tile (bbox: aabb2D i64) : tile [tile_size] target =
    { buffer = replicate tile_size (replicate tile_size (Target.dummy, -1f32)) |> flatten
    , bbox
    }

  def merge_tiles (t0: tile [tile_size] target) (t1: tile [tile_size] target) : tile [tile_size] target =
    let merge_pixel (a: (target, f32)) (b: (target, f32)): (target, f32) =
      if b.1 > a.1
      then b
      else a
    in { buffer = map2 merge_pixel t0.buffer t1.buffer
       , bbox = t1.bbox
       }

  def set_tiles (fb: t) (tiles: [tiles_h fb * tiles_w fb](tile [tile_size] target)) : t = fb with tiles = tiles
}
