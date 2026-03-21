-- | setup vertices to pass to rasterizer

import "fragment"

-- user-defined configuration
module type ConfigSpec = {
  -- triangle winding order
  val triangle_winding_order : #clockwise | #counterclockwise | #neither
}

-- user-specified program specification (to be used to render)
module type ProgramSpec = {
  -- | user-specified varying module
  module Varying: VaryingSpec

  -- | user-specified uniform type (which remains constant in shader functions)
  type uniform

  -- | user-specified vertex type
  type vertex

  -- | user-specified target type
  type target

  -- | on vertex function (vertex shader function)
  val on_vert : uniform -> vertex -> fragment Varying.t

  -- | on quad-fragment function (fragment shader function)
  val on_qfrag : uniform -> pfragment Varying.t ->target
}

type~ vertex_data 'vertex =
  { primitive_type: #triangles
  , vertices: []vertex
  , indices: []i64
  }

module mk_render (Program: ProgramSpec) = {
  local module Fragment = derive_fragment_ops (Program.Varying)
  local type uniform_t = Program.uniform
  local type vertex_t = Program.vertex
  local type varying_t = Program.Varying.t
  local type^ on_vert_t = uniform_t -> vertex_t -> fragment varying_t
  local type^ on_qfrag_t 'target = uniform_t -> [4]pfragment varying_t -> [4]target

  def map_screen_to_window {w = w: i64, h = h: i64}
                           ({pos = {x, y}, depth, Z_inv, attr}: Fragment.pfragment) : Fragment.pfragment =
    -- Map screen [-1;1]x[-1;1] to [0;1]x[0;1]
    let x' = (x + 1) / 2
    let y' = (y + 1) / 2
    -- Map [0;1]x[0;1] to window [0;w]x[-h;0]
    let x'' = x' * f32.i64 w
    let y'' = (-y') * f32.i64 h
    in {pos = {x = x'', y = y''}, depth, Z_inv, attr}

  def render 'target [w] [h]
             ((on_vert, on_qfrag): (on_vert_t, on_qfrag_t target))
             (u: uniform_t)
             (d: vertex_data vertex_t)
             (fb: [h][w]target) : [h][w]target =
    let vertices = map (\i -> d.vertices[i]) d.indices
    let fragments = map (on_vert u) vertices
    in match d.primitive_type
       case #triangles ->
         let NDC_triangles =
           map (\i -> (fragments[3 * i], fragments[3 * i + 1], fragments[3 * i + 2]))
               (iota (length fragments / 3))
         -- todo: clip triangles if they are not already. this may generate new triangles or cull (remove) existing triangles
         let screen_triangles = map (\t -> (Fragment.proj t.0, Fragment.proj t.1, Fragment.proj t.2)) NDC_triangles
         let stw_f = map_screen_to_window {w, h}
         let window_triangles = map (\t -> (stw_f t.0, stw_f t.1, stw_f t.2)) screen_triangles
         in fb
}
