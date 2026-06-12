-- ==
-- compiled input @ bunny.in
-- compiled input @ monkey.in
-- compiled input @ penger.in
-- compiled input @ dragon.in
-- compiled input @ lucy.in
-- compiled input @ armadillo.in

-- > :img main ($loaddata "dragon.in")

import "../../lib/github.com/abxh/futhrast/setup"
import "../../lib/github.com/abxh/futhrast/math/vec"
import "../../lib/github.com/abxh/futhrast/math/transform"

import "../../lib/github.com/athas/matte/colour"
import "../../lib/github.com/abxh/futhrast/rasterize/triangle_tiled_pineda"

local
module Varying : VaryingSpec with t = argb.colour = {
  type t = argb.colour
  def (+) = argb.add_linear
  def (*) = flip argb.scale
}

module R = CustomRenderSetup TiledPinedaTriangleRasterizer Varying

type state =
  { h: i64
  , w: i64
  , pos: (f32, f32, f32)
  , zmin: f32
  , zmax: f32
  }

local
def on_vertex (s: state) (v: (f32, f32, f32)) : vertex_out Varying.t =
  let aspect_ratio = f32.i64 s.w / f32.i64 s.h
  let model =
    transform.identity
    |> (transform.*) (transform.reflect_z)
  let view =
    transform.identity
    |> (transform.*) (transform.translate_tup s.pos)
  let proj =
    transform.identity
    |> (transform.*) (make_orthographic s.zmin s.zmax aspect_ratio)
  let mvp =
    model
    |> (transform.*) view
    |> (transform.*) proj
  let v = vec3f.from_tuple v
  let pos = transform.apply v mvp
  in { pos
     , attr = argb.scale argb.white ((pos.z / pos.w) ** 3)
     }

local
def on_fragment (_: state) (f: fragment Varying.t) : argb.colour =
  f.attr

def main [n] (vx: [n]f32, vy: [n]f32, vz: [n]f32, inds: []i64) =
  let render_config: render_config =
    { triangle_winding_order = #counterclockwise
    }
  let s: state =
    { w = 8192
    , h = 8192
    , pos = (0, 0, 0)
    , zmin = 0.001
    , zmax = 10
    }
  let verts = zip3 vx vy vz
  let s = s with pos.2 = s.zmin + (f32.abs (reduce f32.max f32.lowest (map (.2) verts) - reduce f32.min f32.highest (map (.2) verts)))
  in (tabulate_2d s.h s.w (const (const (argb.black))), tabulate_2d s.h s.w (const (const 0)))
     |> R.render render_config
                 s
                 { primitive_type = #triangles
                 , vertices = verts
                 , indices = inds
                 }
                 on_vertex
                 on_fragment
     |> (.0)
