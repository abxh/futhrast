-- | setup vertices to pass to rasterizer

import "../../diku-dk/segmented/segmented"

import "types"
import "math/vec"
import "rasterize/point"
import "rasterize/line"
import "rasterize/triangle_imm"
import "rasterize/triangle_tiled"

-- | configuration options
module type ConfigSpec = {
  val triangle_winding_order : #clockwise | #counterclockwise | #neither
  val depth_type : #normal_z | #reversed_z
}

-- | model data. indices can be (0..<length vertices) by default
type~ model_data 'vertex =
  { primitive_type: #points | #lines | #triangles
  , vertices: []vertex
  , indices: []i64
  }

-- | vertex shader function
type^ vertex_shader 'uniform 'varying 'vertex = uniform -> vertex -> vertex_out varying

-- | fragment shader function
type^ fragment_shader 'uniform 'varying 'target = uniform -> fragment varying -> target

-- | rendering setup specification
module type RenderSetupSpec =
  (V: VaryingSpec)
  -> {
    -- initialize a framebuffer
    val init 'target : {w: i64, h: i64} -> (ne: target) -> [][](target, f32)

    -- unpack framebuffer
    val unpack [w] [h] 'target : [h][w](target, f32) -> ([h][w]target, [h][w]f32)

    -- render primitives onto framebuffer
    val render 'uniform 'vertex 'target [w] [h] [v] [i] :
      (u: uniform)
      -> (d: { primitive_type: #points | #lines | #triangles
             , vertices: [v]vertex
             , indices: [i]i64
             })
      -> (on_vert: vertex_shader uniform V.t vertex)
      -> (on_frag: fragment_shader uniform V.t target)
      -> (ne: target)
      -> [h][w](target, f32)
      -> [h][w](target, f32)

    -- render primitive wireframes onto framebuffer
    val render_wireframe 'uniform 'vertex 'target [w] [h] [v] [i] :
      (u: uniform)
      -> (d: { primitive_type: #points | #lines | #triangles
             , vertices: [v]vertex
             , indices: [i]i64
             })
      -> (on_vert: vertex_shader uniform V.t vertex)
      -> (on_frag: fragment_shader uniform V.t target)
      -> (ne: target)
      -> [h][w](target, f32)
      -> [h][w](target, f32)
  }

-- | rendering setup implementation
module RenderSetup (C: ConfigSpec) : RenderSetupSpec = \(V: VaryingSpec) ->
  {
    local module Point = PointRasterizer V
    local module Line = LineRasterizer V
    local module Triangle = TriangleTiledRasterizer V

    local
    def map_screen_to_window 'varying
                             {w = w: i64, h = h: i64}
                             ({pos = {x, y}, depth, Z_inv, attr}: fragment varying) : fragment varying =
      let x' = (x + 1) / 2
      let y' = (y + 1) / 2
      let x'' = x' * f32.i64 (w - 1)
      let y'' = (1 - y') * f32.i64 (h - 1)
      in {pos = {x = x'', y = y''}, depth, Z_inv, attr}

    local
    def winding_order_check (f0: fragment V.t, f1: fragment V.t, f2: fragment V.t) : bool =
      let (v0, v1, v2) = (f0.pos, f1.pos, f2.pos)
      let v0v1 = v1 vec2f.- v0
      let v0v2 = v2 vec2f.- v0
      let signed_area_2 = v0v1 `vec2f.cross` v0v2
      in signed_area_2 > 1e-6 && C.triangle_winding_order == #counterclockwise
         || signed_area_2 < -1e-6 && C.triangle_winding_order == #clockwise
         || f32.abs (signed_area_2) >= 1e-6 && C.triangle_winding_order == #neither

    local
    def depth_cmp lhs rhs : #left | #right =
      if (lhs f32.> rhs) && C.depth_type == #reversed_z
      || (lhs f32.< rhs) && C.depth_type == #normal_z
      then #left
      else #right

    def init 'target
             {w = w: i64, h = h: i64}
             (ne: target) : [][](target, f32) =
      let ne = if C.depth_type == #reversed_z then (ne, -f32.inf) else (ne, f32.inf)
      in replicate h (replicate w (ne))

    def unpack [w] [h] 'target (fb: [h][w](target, f32)) : ([h][w]target, [h][w]f32) =
      let (target_buf, depth_buf): ([h * w]target, [h * w]f32) =
        fb
        |> flatten
        |> unzip
      in (unflatten target_buf, unflatten depth_buf)

    def render 'uniform 'vertex 'target [w] [h]
               (u: uniform)
               (d: model_data vertex)
               (on_vert: vertex_shader uniform V.t vertex)
               (on_frag: fragment_shader uniform V.t target)
               (ne: target)
               (fb: [h][w](target, f32)) : [h][w](target, f32) =
      let (stw, ne) =
        let stw = map_screen_to_window {h = length fb, w = length fb[0]}
        let ne = if C.depth_type == #reversed_z then (ne, -f32.inf) else (ne, f32.inf)
        in (stw, ne)
      let vs = map (on_vert u) d.vertices
      let vs = map (\i -> vs[i]) d.indices
      in match d.primitive_type
         case #points ->
           vs
           |> map (\(v0) -> proj v0)
           |> map (\(v0) -> stw v0)
           |> Point.rasterize (on_frag u) depth_cmp ne fb
         case #lines ->
           (iota (length vs / 2))
           |> map (\i -> (vs[2 * i], vs[2 * i + 1]))
           |> map (\(v0, v1) -> (proj v0, proj v1))
           |> map (\(v0, v1) -> (stw v0, stw v1))
           |> Line.rasterize (on_frag u) depth_cmp ne fb
         case #triangles ->
           (iota (length vs / 3))
           |> map (\i -> (vs[3 * i], vs[3 * i + 1], vs[3 * i + 2]))
           |> map (\(v0, v1, v2) -> (proj v0, proj v1, proj v2))
           |> map (\(v0, v1, v2) -> (stw v0, stw v1, stw v2))
           |> filter (winding_order_check)
           |> Triangle.rasterize (on_frag u) depth_cmp ne fb

    def render_wireframe 'uniform 'vertex 'target [w] [h]
                         (u: uniform)
                         (d: model_data vertex)
                         (on_vert: vertex_shader uniform V.t vertex)
                         (on_frag: fragment_shader uniform V.t target)
                         (ne: target)
                         (fb: [h][w](target, f32)) : [h][w](target, f32) =
      let (stw, ne) =
        let stw = map_screen_to_window {h = length fb, w = length fb[0]}
        let ne = if C.depth_type == #reversed_z then (ne, -f32.inf) else (ne, f32.inf)
        in (stw, ne)
      let vs = map (on_vert u) d.vertices
      let vs = map (\i -> vs[i]) d.indices
      in match d.primitive_type
         case #triangles ->
           (iota (length vs / 3))
           |> map (\i -> (vs[3 * i], vs[3 * i + 1], vs[3 * i + 2]))
           |> expand (\_ -> 3) (\t j -> if j == 0 then (t.0, t.1) else if j == 1 then (t.1, t.2) else (t.2, t.0))
           |> map (\(v0, v1) -> (proj v0, proj v1))
           |> map (\(v0, v1) -> (stw v0, stw v1))
           |> Line.rasterize (on_frag u) depth_cmp ne fb
         case _ -> assert false fb
  }

-- todo: cull/clip primitives.
