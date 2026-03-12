import "fragment"
import "framebuffer"
import "config"

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
  val on_qfrag : uniform -> [4]pfragment Varying.t -> [4]target
}

type~ vertex_data 'vertex =
  { primitive_type: #triangles
  , vertices: []vertex
  , indices: []i64
  }

module mk_render (Config: ConfigSpec) (Program: ProgramSpec) (Framebuffer: FramebufferSpec) = {
  local module Fragment = derive_fragment_ops (Program.Varying)
  local type uniform_t = Program.uniform
  local type vertex_t = Program.vertex
  local type target_t = Program.target
  local type varying_t = Program.Varying.t
  local type^ on_vert_t = uniform_t -> vertex_t -> fragment varying_t
  local type^ on_qfrag_t = uniform_t -> [4]pfragment varying_t -> [4]target_t

  def render ((on_vert, on_qfrag): (on_vert_t, on_qfrag_t))
             (u: uniform_t)
             (d: vertex_data vertex_t)
             (fb: Framebuffer.t) : Framebuffer.t =
    let vertices = map (\i -> d.vertices[i]) d.indices
    let fragments = map (on_vert u) vertices
    in match d.primitive_type
       case #triangles ->
         let NDC_triangles =
           map (\i -> (fragments[3 * i], fragments[3 * i + 1], fragments[3 * i + 2]))
               (iota (length fragments / 3))
         -- todo: clip triangles if they are not already. this may generate new triangles or cull (remove) existing triangles
         let screen_triangles = map (\t -> (Fragment.proj t.0, Fragment.proj t.1, Fragment.proj t.2)) NDC_triangles
         let stw_f = Fragment.map_screen_to_window {w = Framebuffer.get_width fb, h = Framebuffer.get_height fb}
         let window_triangles = map (\t -> (stw_f t.0, stw_f t.1, stw_f t.2)) screen_triangles
         in fb
}
