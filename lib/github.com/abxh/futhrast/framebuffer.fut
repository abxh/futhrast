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
  { buffer: [tile_size][tile_size](target, f32)
  , bbox: aabb2D i64
  }

-- | Framebuffer specification
module type FramebufferSpec = {
  -- | the framebuffer type
  type~ t

  -- | the target type
  type target

  -- | the tile size
  val tile_size : i64

  -- | initialize a new framebuffer
  val init : {h: i64, w: i64} -> target -> t

  -- | retrieve width
  val get_width : t -> i64

  -- | retrieve height
  val get_height : t -> i64

  -- | get target buffer
  val get_target_buffer : t -> *[][]target

  -- | get depth buffer with [0;1] corresponding to [far;near] and (f32.lowest) for undefined.
  val get_depth_buffer : t -> *[][]f32

  -- | read tiles
  val get_tiles : t -> [][](tile [tile_size] target)

  -- | write tiles
  val set_tiles [tiles_h] [tiles_w] : t -> *[tiles_h][tiles_w](tile [tile_size] target) -> t
}

-- -- | get bin bbox'es
-- val get_bin_bboxes : [][]AABB2D i64

module Framebuffer (Config: ConfigSpec) (Target: {type t})
  : FramebufferSpec
    with target = Target.t = {
  def tile_size = Config.tile_size

  type target = Target.t

  type~ t =
    { w: i64
    , h: i64
    , target_default: Target.t
    , tiles: [][](tile [tile_size] target)
    }

  def init {w = w: i64, h = h: i64} (target_default: Target.t) : t =
    let tiles_w = (w + (tile_size - 1)) / tile_size
    let tiles_h = (h + (tile_size - 1)) / tile_size
    let tiles_f =
      (\y x ->
         { buffer = replicate tile_size (replicate tile_size (target_default, f32.lowest))
         , bbox =
             { xmin = tile_size * x
             , ymin = tile_size * y
             , xmax = i64.min w (tile_size * (x + 1))
             , ymax = i64.min h (tile_size * (y + 1))
             }
         })
    in { w
       , h
       , target_default
       , tiles = tabulate_2d tiles_h tiles_w tiles_f
       }

  def get_width (fb: t) : i64 = fb.w

  def get_height (fb: t) : i64 = fb.h

  def get_target_buffer (fb: t) : *[][]target =
    tabulate_2d fb.h
                fb.w
                (\y x ->
                   let uy = y / tile_size
                   let ux = x / tile_size
                   let ly = y % tile_size
                   let lx = x % tile_size
                   in fb.tiles[uy, ux].buffer[ly, lx].0)

  def get_depth_buffer (fb: t) : *[][]f32 =
    tabulate_2d fb.h
                fb.w
                (\y x ->
                   let uy = y / tile_size
                   let ux = x / tile_size
                   let ly = y % tile_size
                   let lx = x % tile_size
                   in fb.tiles[uy, ux].buffer[ly, lx].1)

  def get_tiles (fb: t) = fb.tiles

  def set_tiles (fb: t) tiles : t = fb with tiles = tiles
}
