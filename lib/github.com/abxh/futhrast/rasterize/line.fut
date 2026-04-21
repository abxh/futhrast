import "../../../diku-dk/segmented/segmented"

import "../types"

local
-- | line rasterizer specfication
module type LineRasterizerSpec =
  (V: VaryingSpec)
  -> {
    -- | rasterize line given plot function, depth selection function,
    -- line fragments, a neutral value for the target/depth buffers and
    -- the target/depth buffers themselves
    val rasterize 'target [n] [h] [w] :
      (plot: fragment V.t -> target)
      -> (depth_select: f32 -> f32 -> f32)
      -> (ne: (target, f32))
      -> ([h][w]target, [h][w]f32)
      -> [n](fragment V.t, fragment V.t)
      -> ([h][w]target, [h][w]f32)
  }

-- | line rasterizer
module LineRasterizer : LineRasterizerSpec = \(V: VaryingSpec) ->
  {
    -- based on:
    -- futhark-book.readthedocs.io/en/latest/irregular-flattening.html#drawing-lines

    local
    module V = VaryingExtensions (V)

    local
    module F32 = VaryingExtensions (
      {
        open f32
        def one = 1f32
      })

    type fragment_generic 'a 'varying =
      {pos: {x: a, y: a}, depth: f32, Z_inv: f32, attr: varying}

    local
    type line = (fragment_generic i32 V.t, fragment_generic i32 V.t)

    def lerp_affine_f32 = F32.lerp_perspective_corrected_w_Z_inv_t
    def lerp_affine_attr = V.lerp_perspective_corrected_w_Z_inv_t

    def compare_i32 (l: i32) (r: i32) : i32 =
      i32.bool (l < r) - i32.bool (l > r)

    def get_line_slope {x = x0: i32, y = y0: i32} {x = x1: i32, y = y1: i32} : f32 =
      if x1 == x0
      then if y1 > y0 then 1 else -1
      else f32.i32 (y1 - y0) / f32.i32 (i32.abs (x1 - x0))

    def get_line_size ((v0, v1): line) =
      let {x = x0, y = y0} = v0.pos
      let {x = x1, y = y1} = v1.pos
      let size = i64.i32 (i32.max (i32.abs (x1 - x0)) (i32.abs (y1 - y0)))
      in i64.bool (size != 0) + size

    def get_point_in_line ((v0, v1): line) (i: i64) : fragment_generic i32 V.t =
      let (p0, p1) = (v0.pos, v1.pos)
      in if i32.abs (p0.x - p1.x) > i32.abs (p0.y - p1.y)
         then let dir = compare_i32 (p0.x) (p1.x)
              let sl = get_line_slope p0 p1
              let pos =
                { x = p0.x + dir * i32.i64 i
                , y = p0.y + i32.f32 (f32.round (sl * f32.i64 i))
                }
              let t = f32.from_fraction i (get_line_size (v0, v1) - 1)
              let Z_inv = f32.lerp v0.Z_inv v1.Z_inv t
              let depth = lerp_affine_f32 Z_inv (v0.depth, v0.Z_inv) (v1.depth, v1.Z_inv) t
              let attr = lerp_affine_attr Z_inv (v0.attr, v0.Z_inv) (v1.attr, v1.Z_inv) t
              in {pos, Z_inv, depth, attr}
         else let dir = compare_i32 (p0.y) (p1.y)
              let sl = get_line_slope {x = p0.y, y = p0.x} {x = p1.y, y = p1.x}
              let pos =
                { x = p0.x + i32.f32 (f32.round (sl * f32.i64 i))
                , y = p0.y + dir * i32.i64 i
                }
              let t = f32.from_fraction i (get_line_size (v0, v1) - 1)
              let Z_inv = f32.lerp v0.Z_inv v1.Z_inv t
              let depth = lerp_affine_f32 Z_inv (v0.depth, v0.Z_inv) (v1.depth, v1.Z_inv) t
              let attr = lerp_affine_attr Z_inv (v0.attr, v0.Z_inv) (v1.attr, v1.Z_inv) t
              in {pos, depth, Z_inv, attr}

    def round_fragment (f: fragment_generic f32 V.t) : fragment_generic i32 V.t =
      { pos = {x = i32.f32 (f.pos.x + 0.5), y = i32.f32 (f.pos.y + 0.5)}
      , depth = f.depth
      , Z_inv = f.Z_inv
      , attr = f.attr
      }

    def unpack_fragment (f: fragment_generic i32 V.t) =
      let y = i64.i32 f.pos.y
      let x = i64.i32 f.pos.x
      let f' =
        { pos = {x = 0.5 + f32.i32 f.pos.x, y = 0.5 + f32.i32 f.pos.y}
        , depth = f.depth
        , Z_inv = f.Z_inv
        , attr = f.attr
        }
      in ((y, x), f', f.depth)

    def rasterize 'target [n] [h] [w]
                  (plot: (fragment V.t -> target))
                  (depth_select: f32 -> f32 -> f32)
                  ((ne_target, ne_depth): (target, f32))
                  ((target_buffer, depth_buffer): ([h][w]target, [h][w]f32))
                  (frags: [n](fragment V.t, fragment V.t)) : ([h][w]target, [h][w]f32) =
      let (is, frag_values, depth_values) =
        frags
        |> map (\l -> (round_fragment l.0, round_fragment l.1))
        |> expand get_line_size get_point_in_line
        |> map unpack_fragment
        |> unzip3
      let depth_buffer = reduce_by_index_2d (copy depth_buffer) depth_select ne_depth is depth_values
      let (is, target_values) =
        zip is frag_values
        |> map (\((y, x), f) ->
                  if (0 <= x && x < w) && (0 <= y && y < h) && depth_buffer[y, x] == f.depth
                  then ((y, x), plot f)
                  else ((-1, -1), ne_target))
        |> unzip2
      let target_buffer = scatter_2d (copy target_buffer) is target_values
      in (target_buffer, depth_buffer)
  }

-- line rasterizer for testing purposes. can use the REPL for this
module LineRasterizerTest = {
  local
  module V : VaryingSpec with t = bool = {
    type t = bool
    def one = true
    def (+) = (||)
    def (*) s x = if bool.f32 s then x else false
  }

  -- note: above do not satisfy all the algrebraic properties required for varying,
  -- but is defined such for testing purposes

  local module M = LineRasterizer (V)

  def rasterize_line_demo [n] (h: i64) (w: i64) (vs: [n]((f32, f32), (f32, f32))) : [h][w]i32 =
    let target_buffer = replicate h (replicate w false)
    let depth_buffer = replicate h (replicate w (-f32.inf))
    let frags =
      vs
      |> map (\(f0, f1) ->
                ( {pos = {x = f0.0, y = f0.1}, depth = 1, Z_inv = 1, attr = true}
                , {pos = {x = f1.0, y = f1.1}, depth = 1, Z_inv = 1, attr = true}
                ))
    let plot = (\(f: fragment bool) -> f.attr)
    let depth_select (lhs: f32) (rhs: f32) = if lhs > rhs then lhs else rhs
    in M.rasterize plot depth_select (false, -f32.inf) (target_buffer, depth_buffer) frags
       |> (.0)
       |> map (map i32.bool)
}

open LineRasterizerTest
