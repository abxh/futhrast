import "../types"

local
-- | point rasterizer specfication
module type PointRasterizerSpec =
  (V: VaryingSpec)
  -> {
    -- | rasterize point given plot function, depth comparision function,
    -- point fragments, a neutral value for the target/depth buffers and
    -- the target/depth buffers themselves
    val rasterize 'target [n] [h] [w] :
      (plot: fragment V.t -> target)
      -> (depth_cmp: f32 -> f32 -> #left | #right)
      -> (ne: (target, f32))
      -> [h][w](target, f32)
      -> [n]fragment V.t
      -> [h][w](target, f32)
  }

-- | point rasterizer
module PointRasterizer : PointRasterizerSpec = \(V: VaryingSpec) ->
  {
    def plot_fragment 'target (plot: (fragment V.t -> target)) (f: fragment V.t) =
      ((i64.f32 (f.pos.y + 0.5), i64.f32 (f.pos.x + 0.5)), plot f, f.depth)

    def rasterize 'target [n] [h] [w]
                  (plot: (fragment V.t -> target))
                  (depth_cmp: f32 -> f32 -> #left | #right)
                  (ne: (target, f32))
                  (dest: [h][w](target, f32))
                  (frags: [n]fragment V.t) : [h][w](target, f32) =
      let (is, target_values, depth_values) =
        frags
        |> map (plot_fragment plot)
        |> unzip3
      let as = zip target_values depth_values
      let cmp f0 f1 = match depth_cmp f0.1 f1.1 case #left -> f0 case #right -> f1
      in reduce_by_index_2d (copy dest) cmp ne is as
  }

-- point rasterizer for testing purposes. can use the REPL for this
module PointRasterizerTest = {
  local
  module V : VaryingSpec with t = bool = {
    type t = bool
    def (+) = (||)
    def (*) s x = if bool.f32 s then x else false
  }

  -- note: above do not satisfy all the algrebraic properties required for varying,
  -- but is defined such for testing purposes

  local module M = PointRasterizer (V)

  def rasterize_point_test [n] (h: i64) (w: i64) (vs: [n](f32, f32)) : [h][w]i32 =
    let dest = replicate h (replicate w (false, -f32.inf))
    let in_fb v = 0 <= v.0 && v.0 < f32.i64 w && 0 <= v.1 && v.1 < f32.i64 h
    let frags =
      vs
      |> filter in_fb
      |> map (\(x, y) -> {pos = {x, y}, depth = 1, Z_inv = 1, attr = true})
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

open PointRasterizerTest
