import "../lib/github.com/abxh/lys/lys"

import "../../lib/github.com/abxh/futhrast/setup"
import "../../lib/github.com/abxh/futhrast/math/vec"
import "../../lib/github.com/abxh/futhrast/math/rotation"
import "../../lib/github.com/abxh/futhrast/math/transform"

type~ lys_state =
  { h: i64
  , w: i64
  , pos: (f32, f32, f32)
  , pos_delta: (f32, f32, f32)
  , orientation: rotation.t
  , orientation_delta: rotation.t
  , zmin: f32
  , verts: [](f32, f32, f32)
  , inds: []i64
  , normals: [](f32, f32, f32)
  }

module lys_text_content = {
  type text_content = (i64, i64)

  def text_format () =
    "FPS: %ld\n"
    ++ "number of triangles: %ld\n"
    ++ "\n"
    ++ "0: snap into position\n"
    ++ "w|a|s|d|q|e|<arrow keys>: movement\n"

  def text_content (render_duration: f32) (s: lys_state) : text_content =
    let num_triangles = length s.inds / 3
    in (i64.f32 render_duration, num_triangles)

  def text_colour = const argb.black
}

module lys_file = {
  def input_file_names () =
    ""
    ++ "../../models/teapot.obj,"

  def load_bin _ _ s = s

  def load_obj_vertex_indices [n] (i: i64) (is: [n]i64) (s: lys_state) : lys_state =
    match i
    case 0 ->
      s with inds = is
    case _ ->
      s

  def load_obj_vertices [n] (i: i64) (vs: [n](f32, f32, f32)) (s: lys_state) : lys_state =
    match i
    case 0 ->
      s with verts = vs
    case _ ->
      s

  def load_obj_normals [n] (i: i64) (vs: [n](f32, f32, f32)) (s: lys_state) : lys_state =
    match i
    case 0 ->
      s with normals = vs
    case _ ->
      s

  def load_obj_texcoords _ _ s = s
}

module lys : lys with text_content = lys_text_content.text_content = {
  open lys_text_content
  open lys_file

  type~ state = lys_state

  def grab_mouse = false

  def init (_: u32) (h: i64) (w: i64) : state =
    { w
    , h
    , zmin = 0.001
    , pos = (0, 2, -5)
    , pos_delta = (0, 0, 0)
    , orientation = rotation.identity |> (rotation.*) (rotation.rotate_x (-15 / 180 * 3.14))
    , orientation_delta = rotation.identity
    , inds = replicate 0 (-1)
    , verts = replicate 0 (0, 0, 0)
    , normals = replicate 0 (0, 0, 0)
    }

  def resize (h: i64) (w: i64) (s: state) =
    s with h = h with w = w

  def keydown (key: i32) (s: state) =
    if key == SDLK_w
    then s with pos_delta.2 = 1
    else if key == SDLK_s
    then s with pos_delta.2 = -1
    else if key == SDLK_d
    then s with pos_delta.0 = 1
    else if key == SDLK_a
    then s with pos_delta.0 = -1
    else if key == SDLK_q
    then s with pos_delta.1 = 1
    else if key == SDLK_e
    then s with pos_delta.1 = -1
    else if key == SDLK_LEFT
    then s with orientation_delta = rotation.rotate_y 0.02
    else if key == SDLK_RIGHT
    then s with orientation_delta = rotation.rotate_y (-0.02)
    else if key == SDLK_UP
    then s with orientation_delta = rotation.rotate_x 0.02
    else if key == SDLK_DOWN
    then s with orientation_delta = rotation.rotate_x (-0.02)
    else if key == SDLK_0
    then s with pos = (0, 0, -3)
           with orientation = rotation.identity
    else s

  def keyup (key: i32) (s: state) =
    if key == SDLK_w || key == SDLK_s || key == SDLK_a || key == SDLK_d || key == SDLK_q || key == SDLK_e
    then s with pos_delta = (0, 0, 0)
    else if key == SDLK_LEFT || key == SDLK_RIGHT || key == SDLK_UP || key == SDLK_DOWN
    then s with orientation_delta = rotation.identity
    else s

  def event (e: event) (s: state) =
    match e
    case #step td ->
      let local_velocity = vec3f.from_tuple s.pos_delta
      let world_velocity = rotation.apply s.orientation local_velocity
      let orientation = s.orientation_delta rotation.* s.orientation |> rotation.normalize
      in s with orientation = orientation
           with pos = vec3f.to_tuple <| (vec3f.from_tuple s.pos) vec3f.+ (td vec3f.* world_velocity)
    case #keydown {key} -> keydown key s
    case #keyup {key} -> keyup key s
    case _ -> s

  local module RD = RenderSetup DefaultVarying

  local
  def on_vertex (s: state) (v: (f32, f32, f32)) : vertex_out DefaultVarying.t =
    let aspect_ratio = f32.i64 s.w / f32.i64 s.h
    let model =
      transform.identity
    let view =
      transform.identity
      |> (transform.*) (rotation.to_mat s.orientation)
      |> (transform.*) (transform.translate_vec (s.pos |> vec3f.from_tuple))
      |> transform.inverse
    let proj =
      transform.identity
      |> (transform.*) (make_perspective_inf s.zmin (45 * 3.14 / 180) aspect_ratio)
    let mvp =
      transform.identity
      |> (transform.*) model
      |> (transform.*) view
      |> (transform.*) proj
    let v = vec3f.from_tuple v
    in {pos = transform.apply v mvp, attr = ()}

  def render (s: state) : [][]argb.colour =
    let render_config: render_config =
      { triangle_winding_order = #neither
      }
    in (tabulate_2d s.h s.w (const (const (argb.white))), tabulate_2d s.h s.w (const (const 0)))
       |> RD.render_wireframe render_config
                              s
                              { primitive_type = #triangles
                              , vertices = s.verts
                              , indices = s.inds
                              }
                              on_vertex
                              (\_ _ -> argb.black)
       |> RD.render_normals render_config
                            s
                            { primitive_type = #triangles
                            , vertices = s.verts
                            , indices = s.inds
                            }
                            s.normals
                            0.1
                            on_vertex
                            (\_ _ -> argb.red)
       |> (.0)
}
