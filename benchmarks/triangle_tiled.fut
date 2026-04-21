-- ==
-- compiled input @ bunny.in
-- compiled input @ monkey.in
-- compiled input @ african_head.in
-- compiled input @ penger.in

import "../lib/github.com/abxh/futhrast/types"
import "../lib/github.com/abxh/futhrast/setup"
import "../lib/github.com/abxh/futhrast/math/vec"

import "../lib/github.com/abxh/futhrast/rasterize/triangle_tiled"

import "../lib/github.com/athas/matte/colour"

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
  , pos: (f32, f32, f32)
  , pos_delta: (f32, f32, f32)
  , zoom: f32
  , angle: f32
  , angle_delta: f32
  }

def s : state =
  { w = 1024
  , h = 1024
  , zoom = 1
  , pos = (0, 0, 0)
  , pos_delta = (0, 0, 0)
  , angle = 0
  , angle_delta = 0
  }

def render_config : render_config =
  { triangle_winding_order = #counterclockwise
  , depth_type = #reversed_z
  , flip_y = true
  }

local
def on_vertex (s: state) (v: (f32, f32, f32)) : vertex_out Varying.t =
  let v = v |> vec3f.from_tuple
  let v = v with z = -v.z
  let angle = s.angle
  let cos_a = f32.cos angle
  let sin_a = f32.sin angle
  let x' = v.x * cos_a + v.z * sin_a
  let z' = -v.x * sin_a + v.z * cos_a
  in { pos = {x = (x' * s.zoom) + s.pos.0, y = (v.y * s.zoom) + s.pos.1, z = z', w = 1}
     , attr = argb.scale argb.white ((z' + 1) * 0.5)
     }

local
def on_fragment (_: state) (f: fragment Varying.t) : argb.colour =
  f.attr

def main [n] (vx: [n]f32, vy: [n]f32, vz: [n]f32, inds: []i64) =
  let verts = zip3 vx vy vz
  let ne_depth = if render_config.depth_type == #reversed_z then -f32.inf else f32.inf
  in init_framebuffer {w = 1024, h = 1024} (argb.black, ne_depth)
     |> R.render render_config
                 s
                 { primitive_type = #triangles
                 , vertices = verts
                 , indices = inds
                 }
                 on_vertex
                 on_fragment
     |> (.target_buffer)
