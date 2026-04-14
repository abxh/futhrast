import "../../../diku-dk/segmented/segmented"

import "../types"

local
-- | line rasterizer specfication
module type LineRasterizerSpec =
  (V: VaryingSpec)
  -> {
    -- | rasterize line given plot function, depth comparision function,
    -- line fragments, a neutral value for the target/depth buffers and
    -- the target/depth buffers themselves
    val rasterize 'target [n] [h] [w] :
      (plot: fragment V.t -> target)
      -> (depth_cmp: f32 -> f32 -> #left | #right)
      -> (ne: (target, f32))
      -> [h][w](target, f32)
      -> [n](fragment V.t, fragment V.t)
      -> [h][w](target, f32)
  }

-- | line rasterizer
module LineRasterizer : LineRasterizerSpec = \(V: VaryingSpec) ->
  {
    -- based on:
    -- futhark-book.readthedocs.io/en/latest/irregular-flattening.html#drawing-lines

    module V = VaryingExtensions (V)
    module F32 = VaryingExtensions (f32)

    type fragment_generic 'a 'varying =
      {pos: {x: a, y: a}, depth: f32, Z_inv: f32, attr: varying}

    local
    type line = (fragment_generic i32 V.t, fragment_generic i32 V.t)

    def lerp_affine_f32 = F32.lerp_perspective_corrected_w_Z_inv_t
    def lerp_affine_attr = V.lerp_perspective_corrected_w_Z_inv_t

    def round_fragment (f: fragment_generic f32 V.t) : fragment_generic i32 V.t =
      { pos = {x = i32.f32 (f.pos.x + 0.5), y = i32.f32 (f.pos.y + 0.5)}
      , depth = f.depth
      , Z_inv = f.Z_inv
      , attr = f.attr
      }

    def compare_i32 (l: i32) (r: i32) : i32 =
      i32.bool (l < r) - i32.bool (l > r)

    def get_line_slope {x = x0: i32, y = y0: i32} {x = x1: i32, y = y1: i32} : f32 =
      if x1 == x0
      then if y1 > y0 then 1 else -1
      else f32.i32 (y1 - y0) / f32.i32 (i32.abs (x1 - x0))

    def get_line_size ((v0, v1): line) =
      let {x = x0, y = y0} = v0.pos
      let {x = x1, y = y1} = v1.pos
      in 1 + i64.i32 (i32.max (i32.abs (x1 - x0)) (i32.abs (y1 - y0)))

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

    def transform_fragment {h = _: i64, w = w: i64} (f: fragment_generic i32 V.t) =
      let y = i64.i32 f.pos.y
      let x = i64.i32 f.pos.x
      let i = y * w + x
      let f' =
        { pos = {x = f32.i32 f.pos.x, y = f32.i32 f.pos.y}
        , depth = f.depth
        , Z_inv = f.Z_inv
        , attr = f.attr
        }
      in (i, f', f.depth)

    def rasterize 'target [n] [h] [w]
                  (plot: (fragment V.t -> target))
                  (depth_cmp: f32 -> f32 -> #left | #right)
                  ((ne_target, ne_depth): (target, f32))
                  (dest: [h][w](target, f32))
                  (frags: [n](fragment V.t, fragment V.t)) : ([h][w](target, f32)) =
      let depth_cmp (d0: f32) (d1: f32) =
        match depth_cmp d0 d1
        case #left -> d0
        case #right -> d1
      let (is, frag_values, depth_values) =
        frags
        |> map (\l -> (round_fragment l.0, round_fragment l.1))
        |> filter (\l -> get_line_size l > 1)
        |> expand get_line_size get_point_in_line
        |> map (transform_fragment {h, w})
        |> unzip3
      let depth_buffer = flatten dest |> map (.1)
      let depth_buffer = reduce_by_index (copy depth_buffer) depth_cmp ne_depth is depth_values
      let (is, target_values) =
        zip3 is frag_values depth_values
        |> map (\(i, f, d) -> if depth_buffer[i] == d then (i, plot f) else (-1, ne_target))
        |> unzip2
      let target_buffer = flatten dest |> map (.0)
      let target_buffer = scatter (copy target_buffer) is target_values
      in zip target_buffer depth_buffer |> unflatten
  }

-- line rasterizer for testing purposes. can use the REPL for this
module LineRasterizerTest = {
  local
  module V : VaryingSpec with t = bool = {
    type t = bool
    def (+) = (||)
    def (*) s x = if bool.f32 s then x else false
  }

  -- note: above do not satisfy all the algrebraic properties required for varying,
  -- but is defined such for testing purposes

  local module M = LineRasterizer (V)

  def rasterize_line_demo [n] (h: i64) (w: i64) (vs: [n]((f32, f32), (f32, f32))) : [h][w]i32 =
    let dest = replicate h (replicate w (false, -f32.inf))
    let frags =
      vs
      |> map (\(f0, f1) ->
                ( {pos = {x = f0.0, y = f0.1}, depth = 1, Z_inv = 1, attr = true}
                , {pos = {x = f1.0, y = f1.1}, depth = 1, Z_inv = 1, attr = true}
                ))
    let plot = (\(f: fragment bool) -> f.attr)
    let depth_cmp (x: f32) (y: f32) = if x > y then #left else #right
    let (target_buf, _) =
      M.rasterize plot depth_cmp (false, -f32.inf) dest frags
      |> flatten
      |> unzip
    in target_buf
       |> unflatten
       |> map (map i32.bool)
}

open LineRasterizerTest
