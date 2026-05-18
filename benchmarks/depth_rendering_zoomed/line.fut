-- ==
-- compiled input @ bunny.in
-- compiled input @ monkey.in
-- compiled input @ african_head.in
-- compiled input @ penger.in
-- compiled input @ dragon.in
-- compiled input @ lucy.in
-- compiled input @ armadillo.in

import "../../lib/github.com/abxh/futhrast/math/vec"
import "../../lib/github.com/abxh/futhrast/math/transform"
import "../../lib/github.com/abxh/futhrast/setup"

import "../../lib/github.com/athas/matte/colour"

local
module Varying : VaryingSpec with t = argb.colour = {
  type t = argb.colour
  def (+) = argb.add_linear
  def (*) = flip argb.scale
}

module R = RenderSetup Varying

type state =
  { h: i64
  , w: i64
  , zoom: f32
  , zmin: f32
  , zmax: f32
  }

def render_config : render_config =
  { triangle_winding_order = #counterclockwise
  , depth_type = #reversed_z
  , flip_y = true
  }

local
def on_vertex (s: state) (v: (f32, f32, f32)) : vertex_out Varying.t =
  let aspect_ratio = f32.i64 s.w / f32.i64 s.h
  let t =
    transform.identity
    |> (transform.*) (transform.scale s.zoom s.zoom 1)
    |> (transform.*) (make_orthographic s.zmin s.zmax aspect_ratio #reversed_z)
  let v = vec3f.from_tuple v
  let pos = transform.apply_to_pos v t
  in { pos
     , attr = argb.scale argb.white pos.z
     }

local
def on_fragment (_: state) (f: fragment Varying.t) : argb.colour =
  f.attr

def main [n] (vx: [n]f32, vy: [n]f32, vz: [n]f32, inds: []i64) =
  let s: state =
    { w = 1024
    , h = 1024
    , zoom = 1.6
    , zmin = 0
    , zmax = 0
    }
  let verts = zip3 vx vy vz
  let ne_depth = if render_config.depth_type == #reversed_z then f32.lowest else f32.highest
  let s =
    s with zmin = reduce f32.min f32.highest (map (.2) verts)
      with zmax = reduce f32.max f32.lowest (map (.2) verts) + 1
  in init_framebuffer {w = 1024, h = 1024} (argb.black, ne_depth)
     |> R.render_wireframe render_config
                           s
                           { primitive_type = #triangles
                           , vertices = verts
                           , indices = inds
                           }
                           on_vertex
                           on_fragment
     |> (.target_buffer)
