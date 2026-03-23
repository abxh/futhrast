import "../fragment"
import "../varying"

local
-- | point rasterizer specfication
module type mk_point_rasterizer_spec =
  (V: VaryingSpec)
  -> {
    -- | rasterize point given plot function, depth comparision function,
    -- point fragments, a neutral value for the target/depth buffers and
    -- the target/depth buffers themselves
    val rasterize 'target [n] [h] [w] :
      (plot: fragment V.t -> target)
      -> (depth_cmp: f32 -> f32 -> #left | #right)
      -> [n]fragment V.t
      -> (ne: (target, f32))
      -> ([h][w]target, [h][w]f32)
      -> ([h][w]target, [h][w]f32)
  }

-- | point rasterizer
module mk_point_rasterizer : mk_point_rasterizer_spec = \(V: VaryingSpec) ->
  {
    def plot_fragment 'target (plot: (fragment V.t -> target)) (f: fragment V.t) =
      ((i64.f32 (f.pos.y + 0.5), i64.f32 (f.pos.x + 0.5)), plot f, f.depth)

    def rasterize 'target [n] [h] [w]
                  (plot: (fragment V.t -> target))
                  (depth_cmp: f32 -> f32 -> #left | #right)
                  (frags: [n]fragment V.t)
                  (ne: (target, f32))
                  (target_buf: [h][w]target, depth_buf: [h][w]f32) : ([h][w]target, [h][w]f32) =
      let (is, target_values, depth_values) =
        frags
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

-- point rasterizer for testing purposes. can use the REPL for this
module point_rasterizer_test = {
  local
  module V : VaryingSpec with t = bool = {
    type t = bool
    def (+) = (||)
    def (*) s x = if bool.f32 s then x else false
  }

  -- note: above do not satisfy all the algrebraic properties required for varying,
  -- but is defined such for testing purposes

  local module M = mk_point_rasterizer (V)

  def rasterize_point_test [n] (h: i64) (w: i64) (vs: [n](f32, f32)) : [h][w]i32 =
    let target_buf = replicate h (replicate w false)
    let depth_buf = replicate h (replicate w (-f32.inf))
    let frags =
      vs
      |> filter (\v -> 0 <= v.0 && v.0 < f32.i64 w && 0 <= v.1 && v.1 < f32.i64 h)
      |> map (\(x, y) -> {pos = {x, y}, depth = 1, Z_inv = 1, attr = true})
    let plot = (\(f: fragment bool) -> f.attr)
    let depth_cmp (x: f32) (y: f32) = if x > y then #left else #right
    in M.rasterize plot depth_cmp frags (false, -f32.inf) (target_buf, depth_buf) |> (.0)
       |> map (map i32.bool)
}

open point_rasterizer_test
