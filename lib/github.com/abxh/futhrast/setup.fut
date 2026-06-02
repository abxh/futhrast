-- | setup vertices to pass to rasterizer

-- todo: add texture support

import "../../diku-dk/segmented/segmented"

import "math/vec"
import "clip"

open import "varying"
open import "fragment"
open import "misc"
open import "rasterize/point"
open import "rasterize/line"
open import "rasterize/triangle_imm_pineda"
open import "rasterize/triangle_tiled_pineda"
open import "rasterize/triangle_imm_scanline"
open import "rasterize/triangle_hybrid_pineda"

-- | renderer configuration options
type render_config =
  { triangle_winding_order: #clockwise | #counterclockwise | #neither
  }

-- | model data. indices can be (0..<length vertices) by default
type~ model_data 'vertex =
  { primitive_type: #points | #lines | #triangles
  , vertices: []vertex
  , indices: []i64
  }

-- | vertex shader function
type^ vertex_shader 'uniform 'varying 'vertex =
  uniform -> vertex -> vertex_out varying

-- | fragment shader function
type^ fragment_shader 'uniform 'varying 'target =
  uniform -> fragment varying -> target

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
      -> ([h][w]target, [h][w]f32)
      -> ([h][w]target, [h][w]f32)

    -- render primitive wireframe onto framebuffer
    val render_wireframe 'uniform 'vertex 'target [w] [h] [v] [i] :
      (c: render_config)
      -> (u: uniform)
      -> (d: { primitive_type: #points | #lines | #triangles
             , vertices: [v]vertex
             , indices: [i]i64
             })
      -> (on_vert: vertex_shader uniform V.t vertex)
      -> (on_frag: fragment_shader uniform V.t target)
      -> ([h][w]target, [h][w]f32)
      -> ([h][w]target, [h][w]f32)

    -- render primitive normals onto framebuffer
    val render_normals 'uniform 'target [w] [h] [v] [n] [i] :
      (c: render_config)
      -> (u: uniform)
      -> (d: { primitive_type: #points | #lines | #triangles
             , vertices: [v](f32, f32, f32)
             , indices: [i]i64
             })
      -> (normals: [n](f32, f32, f32))
      -> (normal_scale: f32)
      -> (on_vert: vertex_shader uniform V.t (f32, f32, f32))
      -> (on_frag: fragment_shader uniform V.t target)
      -> ([h][w]target, [h][w]f32)
      -> ([h][w]target, [h][w]f32)
  }

-- | rendering setup implementation
module CustomRenderSetup (T: TriangleRasterizerSpec)
  : RenderSetupSpec = \(V: VaryingSpec) ->
  {
    local module Point = PointRasterizer
    local module Line = LineRasterizer V
    local module Triangle = T V

    local module V = VaryingExtensions V
    local module Vec4f = VaryingExtensions vec4f

    local
    def lerp_vertex_fragment (f0: vertex_out V.t)
                             (f1: vertex_out V.t)
                             t =
      { pos = Vec4f.lerp f0.pos f1.pos t
      , attr = V.lerp f0.attr f1.attr t
      }

    local
    def map_screen_to_window 'varying
                             {w = w: i64, h = h: i64}
                             ({ pos = {x, y}
                              , depth
                              , Z_inv
                              , attr
                              }: fragment varying
                             ) : fragment varying =
      -- following SDL y-up convention (flipping the y)
      let x' = (x + 1) / 2
      let y' = (y + 1) / 2
      let x'' = x' * f32.i64 (w - 1)
      let y'' = (1 - y') * f32.i64 (h - 1)
      in {pos = {x = x'', y = y''}, depth, Z_inv, attr}

    local
    def winding_order_test (c: render_config)
                           ( f0: fragment V.t
                           , f1: fragment V.t
                           , f2: fragment V.t
                           ) : bool =
      let (v0, v1, v2) = (f0.pos, f1.pos, f2.pos)
      let v0v1 = v1 vec2f.- v0
      let v0v2 = v2 vec2f.- v0
      let signed_area_2 = v0v1 `vec2f.cross` v0v2
      in c.triangle_winding_order == #neither
         || signed_area_2 > 0 && c.triangle_winding_order == #counterclockwise
         || signed_area_2 < 0 && c.triangle_winding_order == #clockwise

    def render 'uniform 'vertex 'target [w] [h]
               (c: render_config)
               (u: uniform)
               (d: model_data vertex)
               (on_vert: vertex_shader uniform V.t vertex)
               (on_frag: fragment_shader uniform V.t target)
               ( target_buffer: [h][w]target
               , depth_buffer: [h][w]f32
               ) : ([h][w]target, [h][w]f32) =
      let f = proj >-> map_screen_to_window {h, w}
      let vs = map (on_vert u) d.vertices
      let vs = map (\i -> vs[i]) d.indices
      in match d.primitive_type
         case #points ->
           vs
           |> filter test_point_bounds
           |> map (\(v0) -> f v0)
           |> Point.rasterize (on_frag u) (target_buffer, depth_buffer)
         case #lines ->
           (iota (length vs / 2))
           |> map (\i -> (vs[2 * i], vs[2 * i + 1]))
           |> clip_lines lerp_vertex_fragment
           |> map (\(v0, v1) -> (f v0, f v1))
           |> Line.rasterize (on_frag u) (target_buffer, depth_buffer)
         case #triangles ->
           (iota (length vs / 3))
           |> map (\i -> (vs[3 * i], vs[3 * i + 1], vs[3 * i + 2]))
           |> map (\(v0, v1, v2) ->
                     -- note: following SDL convention and flipping the y swaps the triangle winding order
                     (v0, v2, v1))
           |> clip_triangles lerp_vertex_fragment
           |> map (\(v0, v1, v2) -> (f v0, f v1, f v2))
           |> filter (\tri -> winding_order_test c tri)
           |> Triangle.rasterize (on_frag u) (target_buffer, depth_buffer)

    def render_wireframe 'uniform 'vertex 'target [w] [h]
                         (_: render_config)
                         (u: uniform)
                         (d: model_data vertex)
                         (on_vert: vertex_shader uniform V.t vertex)
                         (on_frag: fragment_shader uniform V.t target)
                         ( target_buffer: [h][w]target
                         , depth_buffer: [h][w]f32
                         ) : ([h][w]target, [h][w]f32) =
      let f = proj >-> map_screen_to_window {h, w}
      let vs = map (on_vert u) d.vertices
      let vs = map (\i -> vs[i]) d.indices
      in match d.primitive_type
         case #triangles ->
           (iota (length vs / 3))
           |> map (\i -> (vs[3 * i], vs[3 * i + 1], vs[3 * i + 2]))
           |> map (\(v0, v1, v2) -> (v0, v2, v1))
           |> expand (\_ -> 3) (\t j -> (j, t))
           |> map (\(j, t) ->
                     match j
                     case 0 -> (t.0, t.1)
                     case 1 -> (t.1, t.2)
                     case _ -> (t.2, t.0))
           |> clip_lines lerp_vertex_fragment
           |> map (\(v0, v1) -> (f v0, f v1))
           |> Line.rasterize (on_frag u) (target_buffer, depth_buffer)
         case _ -> assert false (target_buffer, depth_buffer)

    def render_normals 'uniform 'target [w] [h] [v] [i] [n]
                       (_: render_config)
                       (u: uniform)
                       (d: { primitive_type: #points | #lines | #triangles
                           , vertices: [v](f32, f32, f32)
                           , indices: [i]i64
                           }
                       )
                       (normals: [n](f32, f32, f32))
                       (normal_scale: f32)
                       (on_vert: vertex_shader uniform V.t (f32, f32, f32))
                       (on_frag: fragment_shader uniform V.t target)
                       ( target_buffer: [h][w]target
                       , depth_buffer: [h][w]f32
                       ) : ([h][w]target, [h][w]f32) =
      let f = proj >-> map_screen_to_window {h, w}
      let indexed_verts = map (\i -> d.vertices[i]) d.indices
      let normal_lines =
        map2 (\v n ->
                let p0 = v
                let p1 =
                  ( v.0 + n.0 * normal_scale
                  , v.1 + n.1 * normal_scale
                  , v.2 + n.2 * normal_scale
                  )
                in (p0, p1))
             indexed_verts
             (normals |> sized i)
      in match d.primitive_type
         case #triangles ->
           normal_lines
           |> map (\(v0, v1) -> (on_vert u v0, on_vert u v1))
           |> clip_lines lerp_vertex_fragment
           |> map (\(v0, v1) -> (f v0, f v1))
           |> Line.rasterize (on_frag u)
                             (target_buffer, depth_buffer)
         case _ -> assert false (target_buffer, depth_buffer)
  }

module RenderSetup : RenderSetupSpec =
  CustomRenderSetup HybridPinedaTriangleRasterizer
