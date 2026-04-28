import "../../lib/github.com/abxh/lys/lys"
import "../../lib/github.com/athas/matte/colour"

import "../../lib/github.com/abxh/futhrast/setup"

type~ lys_state =
  { time: f32
  , h: i64
  , w: i64
  }

module lys : lys = {
  open lys_no_text
  open lys_no_file

  type~ state = lys_state

  def grab_mouse = false

  def init (_: u32) (h: i64) (w: i64) : lys_state =
    { time = 0
    , w
    , h
    }

  def resize (h: i64) (w: i64) (s: lys_state) =
    s with h = h with w = w

  def keydown (_: i32) (s: lys_state) = s

  def keyup (_: i32) (s: lys_state) = s

  def event (_: event) (s: lys_state) = s

  local
  module Varying : VaryingSpec with t = argb.colour = {
    type t = argb.colour
    def (+) = argb.add_linear
    def (*) = flip argb.scale
  }

  local module R = RenderSetup Varying

  def render (s: lys_state) : [][]argb.colour =
    let render_config: render_config =
      { triangle_winding_order = #counterclockwise
      , flip_y = false
      , depth_type = #normal_z
      }
    let ne_depth = f32.inf
    let v0 = ({x = 2 / 4, y = 1 / 4}, argb.red)
    let v1 = ({x = 3 / 4, y = 3 / 4}, argb.blue)
    let v2 = ({x = 1 / 4, y = 3 / 4}, argb.green)
    let on_vertex () (({x, y}, colour): ({x: f32, y: f32}, argb.colour)) =
      let pos = {x = (x - 0.5) * 2, y = (y - 0.5) * 2, z = 0, w = 1}
      in {pos, attr = colour}
    let on_fragment () f: argb.colour = f.attr
    in init_framebuffer {w = s.w, h = s.h} (argb.black, ne_depth)
       |> R.render render_config
                   ()
                   { primitive_type = #triangles
                   , vertices = [v0, v1, v2]
                   , indices = iota 3
                   }
                   on_vertex
                   on_fragment
       |> (.target_buffer)
}
