-- | setup vertices to pass to rasterizer

import "fragment"
import "varying"

-- | supported primitive types (TODO: quad)
type primitive = #points | #lines | #triangles

-- | basic model data information
type~ model_data 'vertex =
  { primitive_type: primitive
  , vertices: []vertex
  , indices: []i64
  }

-- | configuration options
type setup_config =
  { triangle_winding_order: #clockwise | #counterclockwise | #neither
  }

local
-- | vertex program shader
type^ on_vertex_function 'uniform 'varying 'vertex = uniform -> vertex -> vertex_out varying

local
-- | fragment program shader
type^ on_fragment_function 'uniform 'varying 'target = uniform -> fragment varying -> target

def setup 'uniform 'vertex 'varying 'target [w] [h]
          (c: setup_config)
          (on_vertex: on_vertex_function uniform varying vertex)
          (on_fragment: on_fragment_function uniform varying target)
          (u: uniform)
          (d: model_data vertex)
          (fb: [h][w]target) : [h][w]target =
  let vertices = map (\i -> d.vertices[i]) d.indices
  let fragments = map (on_vertex u) vertices
  -- match d.primitive_type
  --    case #triangles ->
  -- let NDC_triangles =
  --   map (\i -> (fragments[3 * i], fragments[3 * i + 1], fragments[3 * i + 2]))
  --       (iota (length fragments / 3))
  -- -- todo: clip triangles if they are not already. this may generate new triangles or cull (remove) existing triangles
  -- let screen_triangles = map (\t -> (Fragment.proj t.0, Fragment.proj t.1, Fragment.proj t.2)) NDC_triangles
  -- let stw_f = map_screen_to_window {w, h}
  -- let window_triangles = map (\t -> (stw_f t.0, stw_f t.1, stw_f t.2)) screen_triangles
  in fb
