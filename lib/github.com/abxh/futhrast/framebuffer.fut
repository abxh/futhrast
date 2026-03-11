import "config"

-- | framebuffer tile with target and depth textures
type framebuffer_tile [tile_size] 'target =
  { target: [tile_size][tile_size]target
  , depth: [tile_size][tile_size]f32
  , bbox: { xmin: i64
          , xmax: i64
          , ymin: i64
          , ymax: i64
          }
  }

-- | framebuffer type
type~ framebuffer [tile_size] 'target =
  { w: i64
  , h: i64
  , tiles_w: i64
  , tiles_h: i64
  , tiles: [][](framebuffer_tile [tile_size] target)
  }

local
-- | framebuffer module specfication
module type mk_framebuffer_spec = {
  -- | the framebuffer type
  type~ t

  -- | the target type
  type target_t

  -- | initialize a new framebuffer with target default value
  val init : {w: i64, h: i64} -> target_t -> t

  -- | get a fresh copy of the target values
  val get_target : t -> *[][]target_t
}

-- | module for making framebuffer
module mk_framebuffer (Config: ConfigSpec) (Target: {type t})
  : mk_framebuffer_spec
    with t = framebuffer [Config.tile_size] Target.t
    with target_t = Target.t = {
  local open Config

  type target_t = Target.t
  type~ t = framebuffer [Config.tile_size] Target.t

  def init {w = w: i64, h = h: i64} (target_default: target_t) : t =
    let div_rounded_up_f (q: i64) (r: i64) = (q + r - 1) / r
    let tiles_h = div_rounded_up_f (div_rounded_up_f h tile_size) 64 * 64
    let tiles_w = div_rounded_up_f w tile_size
    let tiles_f =
      (\y x ->
         { target = replicate tile_size (replicate tile_size target_default)
         , depth = replicate tile_size (replicate tile_size (-f32.inf))
         , bbox =
             { xmin = tile_size * x
             , ymin = tile_size * y
             , xmax = i64.min w (tile_size * (x + 1))
             , ymax = i64.min h (tile_size * (y + 1))
             }
         })
    in { w
       , h
       , tiles_w = tiles_w
       , tiles_h = tiles_h
       , tiles = tabulate_2d tiles_h tiles_w tiles_f
       }

  def get_target (s: t) : *[][]Target.t =
    tabulate_2d s.h
                s.w
                (\y x ->
                   let uy = y / tile_size
                   let ux = x / tile_size
                   let ly = y % tile_size
                   let lx = x % tile_size
                   in s.tiles[uy, ux].target[ly, lx])
}
