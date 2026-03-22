import "../../../diku-dk/segmented/segmented"

import "../fragment"
import "../varying"

local
-- | line rasterizer specfication
module type mk_line_rasterizer_spec =
  (V: VaryingSpec)
  -> {
    -- | rasterize line given plot function, depth comparision function,
    -- line fragments, a neutral value for the target/depth buffers and
    -- the target/depth buffers themselves
    val rasterize 'target [n] [h] [w] :
      (plot: pfragment V.t -> target)
      -> (depth_cmp: f32 -> f32 -> #left | #right)
      -> [n](pfragment V.t, pfragment V.t)
      -> (ne: (target, f32))
      -> ([h][w]target, [h][w]f32)
      -> ([h][w]target, [h][w]f32)
  }

-- | line rasterizer
module mk_line_rasterizer : mk_line_rasterizer_spec = \(V: VaryingSpec) ->
  {
    -- based on:
    -- futhark-book.readthedocs.io/en/latest/irregular-flattening.html#drawing-lines

    module V = VaryingExtensions (V)
    module F32 = VaryingExtensions (f32)

    local
    type line = (pfragment_generic i32 V.t, pfragment_generic i32 V.t)

    def lerp_affine_f32 = F32.lerp_perspective_corrected_w_Z_inv_t
    def lerp_affine_attr = V.lerp_perspective_corrected_w_Z_inv_t

    def round_fragment (f: pfragment_generic f32 V.t) : pfragment_generic i32 V.t =
      { pos = {x = i32.f32 (f.pos.x + 0.5), y = i32.f32 (f.pos.y + 0.5)}
      , depth = f.depth
      , Z_inv = f.Z_inv
      , attr = f.attr
      }

    def compare_i32 (l: i32) (r: i32) : i32 =
      i32.bool (l < r) - i32.bool (l > r)

    def plot_fragment 'target (plot: (pfragment V.t -> target)) (f: pfragment_generic i32 V.t) =
      let f' =
        { pos = {x = f32.i32 f.pos.x, y = f32.i32 f.pos.y}
        , depth = f.depth
        , Z_inv = f.Z_inv
        , attr = f.attr
        }
      in ((i64.i32 f.pos.y, i64.i32 f.pos.x), plot f', f.depth)

    def get_line_slope {x = x0: i32, y = y0: i32} {x = x1: i32, y = y1: i32} : f32 =
      if x1 == x0
      then if y1 > y0 then 1 else -1
      else f32.i32 (y1 - y0) / f32.i32 (i32.abs (x1 - x0))

    def get_line_size ((v0, v1): line) =
      let {x = x0, y = y0} = v0.pos
      let {x = x1, y = y1} = v1.pos
      in 1 + i64.i32 (i32.max (i32.abs (x1 - x0)) (i32.abs (y1 - y0)))

    def get_point_in_line ((v0, v1): line) (i: i64) : pfragment_generic i32 V.t =
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

    def rasterize 'target [n] [h] [w]
                  (plot: (pfragment V.t -> target))
                  (depth_cmp: f32 -> f32 -> #left | #right)
                  (frags: [n](pfragment V.t, pfragment V.t))
                  (ne: (target, f32))
                  (target_buf: [h][w]target, depth_buf: [h][w]f32) : ([h][w]target, [h][w]f32) =
      let (is, target_values, depth_values) =
        frags
        |> map (\l -> (round_fragment l.0, round_fragment l.1))
        |> filter (\l -> get_line_size l > 1)
        |> expand get_line_size get_point_in_line
        |> map (plot_fragment plot)
        |> unzip3
      let as = zip target_values depth_values
      let dest = zip (flatten target_buf) (flatten depth_buf) |> unflatten
      let cmp = (\f0 f1 -> match depth_cmp f0.1 f1.1 case #left -> f0 case #right -> f1)
      in reduce_by_index_2d (copy dest) cmp ne is as
         |> flatten
         |> unzip
         |> (\(x, y) -> (unflatten x, unflatten y))
  }

-- line rasterizer for testing purposes. can use the REPL for this
module line_rasterizer_test = {
  local
  module V : VaryingSpec with t = bool = {
    type t = bool
    def (+) = (||)
    def (*) s x = if bool.f32 s then x else false
  }

  -- note: above do not satisfy all the algrebraic properties required for varying,
  -- but is defined such for testing purposes

  local module M = mk_line_rasterizer (V)

  def rasterize_line_demo [n] (h: i64) (w: i64) (vs: [n]((f32, f32), (f32, f32))) : [h][w]i32 =
    let target_buf = replicate h (replicate w false)
    let depth_buf = replicate h (replicate w (-f32.inf))
    let in_fb v = 0 <= v.0 && v.0 < f32.i64 w && 0 <= v.1 && v.1 < f32.i64 h
    let frags =
      vs
      |> filter (\(v0, v1) -> in_fb v0 && in_fb v1)
      |> map (\(f0, f1) ->
                ( {pos = {x = f0.0, y = f0.1}, depth = 1, Z_inv = 1, attr = true}
                , {pos = {x = f1.0, y = f1.1}, depth = 1, Z_inv = 1, attr = true}
                ))
    let plot = (\(f: pfragment bool) -> f.attr)
    let depth_cmp (x: f32) (y: f32) = if x > y then #left else #right
    in M.rasterize plot depth_cmp frags (false, -f32.inf) (target_buf, depth_buf) |> (.0)
       |> map (map i32.bool)
}

open line_rasterizer_test
