-- | setup vertices to pass to rasterizer

-- todo: cull/clip primitives.
-- todo: add quad support
-- todo: render in quads
-- todo: add texture support

import "../../diku-dk/segmented/segmented"

import "math/vec"

open import "types"
open import "rasterize/point"
open import "rasterize/line"
open import "rasterize/triangle_imm"
open import "rasterize/triangle_imm_barycentric"
open import "rasterize/triangle_tiled"

-- | renderer configuration options
type render_config =
  { triangle_winding_order: #clockwise | #counterclockwise | #neither
  , depth_type: #normal_z | #reversed_z
  , flip_y: -- flip screen y (e.g. for SDL)
            bool
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

-- initialize a framebuffer for rendering
def init_framebuffer 'target
                     {w = w: i64, h = h: i64}
                     (( default_target: target
                      , default_depth: f32
                      )
                     ) : { target_buffer: [][]target
                         , depth_buffer: [][]f32
                         , ne_target: target
                         , ne_depth: f32
                         } =
  { target_buffer = replicate h (replicate w default_target)
  , depth_buffer = replicate h (replicate w default_depth)
  , ne_target = default_target
  , ne_depth = default_depth
  }

-- | rendering setup specification
module type RenderSetupSpec =
  (V: VaryingSpec)
  -> {
    -- render primitives onto framebuffer
    val render 'uniform 'vertex 'target [w] [h] [v] [i] :
      (c: render_config)
      -> (u: uniform)
      -> (d: { primitive_type: #points | #lines | #triangles
             , vertices: [v]vertex
             , indices: [i]i64
             })
      -> (on_vert: vertex_shader uniform V.t vertex)
      -> (on_frag: fragment_shader uniform V.t target)
      -> { target_buffer: [h][w]target
         , depth_buffer: [h][w]f32
         , ne_target: target
         , ne_depth: f32
         }
      -> { target_buffer: [h][w]target
         , depth_buffer: [h][w]f32
         , ne_target: target
         , ne_depth: f32
         }

    -- render primitive wireframes onto framebuffer
    val render_wireframe 'uniform 'vertex 'target [w] [h] [v] [i] :
      (c: render_config)
      -> (u: uniform)
      -> (d: { primitive_type: #points | #lines | #triangles
             , vertices: [v]vertex
             , indices: [i]i64
             })
      -> (on_vert: vertex_shader uniform V.t vertex)
      -> (on_frag: fragment_shader uniform V.t target)
      -> { target_buffer: [h][w]target
         , depth_buffer: [h][w]f32
         , ne_target: target
         , ne_depth: f32
         }
      -> { target_buffer: [h][w]target
         , depth_buffer: [h][w]f32
         , ne_target: target
         , ne_depth: f32
         }
  }

-- | rendering setup implementation
module CustomRenderSetup (T: TriangleRasterizerSpec) : RenderSetupSpec = \(V: VaryingSpec) ->
  {
    local module Point = PointRasterizer V
    local module Line = LineRasterizer V
    local module Triangle = T V

    local
    def map_screen_to_window 'varying
                             (c: render_config)
                             {w = w: i64, h = h: i64}
                             ({pos = {x, y}, depth, Z_inv, attr}: fragment varying) : fragment varying =
      let x' = (x + 1) / 2
      let y' = (y + 1) / 2
      let x'' = x' * f32.i64 (w - 1)
      let y'' = (if c.flip_y then 1 - y' else y') * f32.i64 (h - 1)
      in {pos = {x = x'', y = y''}, depth, Z_inv, attr}

    local
    def winding_order_check (c: render_config) (f0: fragment V.t, f1: fragment V.t, f2: fragment V.t) : bool =
      if c.triangle_winding_order == #neither
      then true
      else let (v0, v1, v2) = (f0.pos, f1.pos, f2.pos)
           let v0v1 = v1 vec2f.- v0
           let v0v2 = v2 vec2f.- v0
           let signed_area_2 = v0v1 `vec2f.cross` v0v2
           in signed_area_2 > 0 && c.triangle_winding_order == #counterclockwise
              || signed_area_2 < 0 && c.triangle_winding_order == #clockwise

    local
    def line_bbox_check {w = w: i64, h = h: i64} (f0: fragment V.t, f1: fragment V.t) : bool =
      let (p0, p1) = (f0.pos, f1.pos)
      let xmin = (p0.x `f32.min` p1.x) |> (f32.floor >-> i64.f32)
      let ymin = (p0.y `f32.min` p1.y) |> (f32.floor >-> i64.f32)
      let xmax = (p0.x `f32.max` p1.x) |> (f32.ceil >-> i64.f32)
      let ymax = (p0.y `f32.max` p1.y) |> (f32.ceil >-> i64.f32)
      let area_check = (xmax - xmin > 1) && (ymax - ymin > 1)
      let overlap_check = !(w <= xmin || 0 >= xmax || h <= ymin || 0 >= ymax)
      in area_check && overlap_check

    local
    def tri_bbox_area_check (f0: fragment V.t, f1: fragment V.t, f2: fragment V.t) : bool =
      let (p0, p1, p2) = (f0.pos, f1.pos, f2.pos)
      let xmin = (p0.x `f32.min` p1.x `f32.min` p2.x) |> (f32.floor >-> i64.f32)
      let ymin = (p0.y `f32.min` p1.y `f32.min` p2.y) |> (f32.floor >-> i64.f32)
      let xmax = (p0.x `f32.max` p1.x `f32.max` p2.x) |> (f32.ceil >-> i64.f32)
      let ymax = (p0.y `f32.max` p1.y `f32.max` p2.y) |> (f32.ceil >-> i64.f32)
      let area_check = (xmax - xmin > 1) && (ymax - ymin > 1)
      in area_check

    local
    def depth_select (c: render_config) lhs rhs : f32 =
      if c.depth_type == #reversed_z
      then f32.max lhs rhs
      else f32.min lhs rhs

    def render 'uniform 'vertex 'target [w] [h]
               (c: render_config)
               (u: uniform)
               (d: model_data vertex)
               (on_vert: vertex_shader uniform V.t vertex)
               (on_frag: fragment_shader uniform V.t target)
               { target_buffer = target_buffer: [h][w]target
               , depth_buffer = depth_buffer: [h][w]f32
               , ne_target = ne_target: target
               , ne_depth = ne_depth: f32
               } : { target_buffer: [h][w]target
                   , depth_buffer: [h][w]f32
                   , ne_target: target
                   , ne_depth: f32
                   } =
      let stw = map_screen_to_window c {h, w}
      let winding_order_check = winding_order_check c
      let vs = map (on_vert u) d.vertices
      let vs = map (\i -> vs[i]) d.indices
      let (target_buffer, depth_buffer) =
        match d.primitive_type
        case #points ->
          vs
          |> map (\(v0) -> proj v0)
          |> map (\(v0) -> stw v0)
          |> Point.rasterize (on_frag u)
                             (depth_select c)
                             (ne_target, ne_depth)
                             (target_buffer, depth_buffer)
        case #lines ->
          (iota (length vs / 2))
          |> map (\i -> (vs[2 * i], vs[2 * i + 1]))
          |> map (\(v0, v1) -> (proj v0, proj v1))
          |> map (\(v0, v1) -> (stw v0, stw v1))
          |> filter (\tri -> line_bbox_check {w, h} tri)
          |> Line.rasterize (on_frag u)
                            (depth_select c)
                            (ne_target, ne_depth)
                            (target_buffer, depth_buffer)
        case #triangles ->
          (iota (length vs / 3))
          |> map (\i -> (vs[3 * i], vs[3 * i + 1], vs[3 * i + 2]))
          |> map (\(v0, v1, v2) -> (proj v0, proj v1, proj v2))
          |> map (\(v0, v1, v2) -> (stw v0, stw v1, stw v2))
          |> filter (\tri -> winding_order_check tri && tri_bbox_area_check tri)
          |> Triangle.rasterize (on_frag u)
                                (depth_select c)
                                (ne_target, ne_depth)
                                (target_buffer, depth_buffer)
      in {target_buffer, depth_buffer, ne_target, ne_depth}

    def render_wireframe 'uniform 'vertex 'target [w] [h]
                         (c: render_config)
                         (u: uniform)
                         (d: model_data vertex)
                         (on_vert: vertex_shader uniform V.t vertex)
                         (on_frag: fragment_shader uniform V.t target)
                         { target_buffer = target_buffer: [h][w]target
                         , depth_buffer = depth_buffer: [h][w]f32
                         , ne_target = ne_target: target
                         , ne_depth = ne_depth: f32
                         } : { target_buffer: [h][w]target
                             , depth_buffer: [h][w]f32
                             , ne_target: target
                             , ne_depth: f32
                             } =
      let stw = map_screen_to_window c {h, w}
      let vs = map (on_vert u) d.vertices
      let vs = map (\i -> vs[i]) d.indices
      let (target_buffer, depth_buffer) =
        match d.primitive_type
        case #triangles ->
          (iota (length vs / 3))
          |> map (\i -> (vs[3 * i], vs[3 * i + 1], vs[3 * i + 2]))
          |> expand (\_ -> 3) (\t j -> if j == 0 then (t.0, t.1) else if j == 1 then (t.1, t.2) else (t.2, t.0))
          |> map (\(v0, v1) -> (proj v0, proj v1))
          |> map (\(v0, v1) -> (stw v0, stw v1))
          |> filter (\tri -> line_bbox_check {w, h} tri)
          |> Line.rasterize (on_frag u)
                            (depth_select c)
                            (ne_target, ne_depth)
                            (target_buffer, depth_buffer)
        case _ -> assert false (target_buffer, depth_buffer)
      in {target_buffer, depth_buffer, ne_target, ne_depth}
  }

-- | Default renderer setup
module RenderSetup = CustomRenderSetup ImmTriangleRasterizer
