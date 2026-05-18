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
  , zmax: f32
  , zmin: f32
  , verts: [](f32, f32, f32)
  , inds: []i64
  , normals: [](f32, f32, f32)
  , inner_mode: #yes | #no
  , show_normals: #yes | #no
  }

module lys_text_content = {
  type text_content = (i64, i64)

  def text_format () =
    "FPS: %ld\n"
    ++ "number of triangles: %ld\n"
    ++ "\n"
    ++ "0: snap into position\n"
    ++ "w|a|s|d: movement\n"
    ++ "n: show normals\n"
    ++ "i: see inner/outer\n"

  def text_content (render_duration: f32) (s: lys_state) : text_content =
    let num_triangles = length s.inds / 3
    in (i64.f32 render_duration, num_triangles)

  def text_colour = const argb.white
}

module lys_file = {
  def input_file_names () =
    ""
    ++ "../../models/african_head.obj,"

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
    , zmin = 0
    , zmax = 0
    , pos = (0, 0, -3)
    , pos_delta = (0, 0, 0)
    , orientation = rotation.identity
    , orientation_delta = rotation.identity
    , inds = replicate 0 (-1)
    , verts = replicate 0 (0, 0, 0)
    , normals = replicate 0 (0, 0, 0)
    , inner_mode = #no
    , show_normals = #no
    }

  def resize (h: i64) (w: i64) (s: state) =
    s with h = h with w = w

  def keydown (key: i32) (s: state) =
    if key == SDLK_i
    then s with inner_mode = match s.inner_mode
           case #no -> #yes
           case #yes -> #no
    else if key == SDLK_n
    then s with show_normals = match s.show_normals
           case #no -> #yes
           case #yes -> #no
    else if key == SDLK_w
    then s with pos_delta.2 = 1
    else if key == SDLK_s
    then s with pos_delta.2 = -1
    else if key == SDLK_a
    then s with orientation_delta = rotation.rotate_y 0.02
    else if key == SDLK_d
    then s with orientation_delta = rotation.rotate_y (-0.02)
    else if key == SDLK_0
    then s with pos = (0, 0, -3)
           with orientation = rotation.identity
    else s

  def keyup (key: i32) (s: state) =
    if key == SDLK_w || key == SDLK_s
    then s with pos_delta.2 = 0
    else if key == SDLK_a || key == SDLK_d
    then s with orientation_delta = rotation.identity
    else s

  def event (e: event) (s: state) =
    match e
    case #step td ->
      let local_velocity = vec3f.from_tuple s.pos_delta
      let world_velocity = rotation.apply s.orientation local_velocity
      in s with orientation = s.orientation_delta rotation.* s.orientation
           with pos = vec3f.to_tuple <| (vec3f.from_tuple s.pos) vec3f.+ (td vec3f.* world_velocity)
    case #keydown {key} ->
      keydown key s
    case #keyup {key} ->
      keyup key s
    case _ -> s

  local
  module Varying : VaryingSpec with t = (argb.colour, vec3f.t) = {
    type t = (argb.colour, vec3f.t)
    def (+) (lhs: t) (rhs: t) = (argb.add_linear lhs.0 rhs.0, lhs.1 vec3f.+ rhs.1)
    def (*) s (rhs: t) = (argb.scale rhs.0 s, s vec3f.* rhs.1)
  }

  local module R = RenderSetup Varying

  local
  def on_vertex (s: state) (v: (f32, f32, f32)) : vertex_out Varying.t =
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
      |> (transform.*) (make_perspective s.zmin s.zmax (45 * 3.14 / 180) aspect_ratio #reversed_z)
    let mvp =
      model
      |> (transform.*) view
      |> (transform.*) proj
    let v = vec3f.from_tuple v
    let pos = transform.apply_to_pos v mvp
    in { pos
       , attr = (argb.scale argb.white pos.z, v)
       }

  local
  def on_wireframe_vertex (s: state) (v: (f32, f32, f32)) : vertex_out Varying.t =
    let aspect_ratio = f32.i64 s.w / f32.i64 s.h
    let model =
      transform.identity
      |> (transform.*) (transform.scale_tup (1.05, 1.05, 1.05))
    let view =
      transform.identity
      |> (transform.*) (rotation.to_mat s.orientation)
      |> (transform.*) (transform.translate_vec (s.pos |> vec3f.from_tuple))
      |> transform.inverse
    let proj =
      transform.identity
      |> (transform.*) (make_perspective s.zmin s.zmax (45 * 3.14 / 180) aspect_ratio #reversed_z)
    let mvp =
      model
      |> (transform.*) view
      |> (transform.*) proj
    let v = vec3f.from_tuple v
    let pos = transform.apply_to_pos v mvp
    in { pos
       , attr = (argb.scale argb.white pos.z, v)
       }

  local
  def on_model_fragment (_: state) (f: fragment Varying.t) : argb.colour =
    f.attr.0

  local
  def on_checkerboard_fragment (_: state) (f: fragment Varying.t) : argb.colour =
    let p = f.attr.1
    let scale = 1
    let xi = i32.f32 (f32.floor (p.x * scale))
    let zi = i32.f32 (f32.floor (p.z * scale))
    let checker = ((xi + zi) % 2) == 0
    in if checker then argb.white else argb.black

  def render (s: state) : [][]argb.colour =
    let render_config: render_config =
      { triangle_winding_order =
          match s.inner_mode
          case #no -> #counterclockwise
          case #yes -> #clockwise
      , depth_type = #reversed_z
      , flip_y = true
      }
    let ne_depth = if render_config.depth_type == #reversed_z then f32.lowest else f32.highest
    let s =
      s with zmin = reduce f32.min f32.highest (map (.2) s.verts)
        with zmax = reduce f32.max f32.lowest (map (.2) s.verts) + 30
    let fb =
      init_framebuffer {w = s.w, h = s.h} (argb.gray 0.3, ne_depth)
      |> R.render render_config
                  s
                  { primitive_type = #triangles
                  , vertices =
                      [ (-100, -2, -100)
                      , (100, -2, -100)
                      , (100, -2, 100)
                      , (-100, -2, 100)
                      ]
                  , indices = [0, 1, 2, 0, 2, 1, 0, 2, 3, 0, 3, 2]
                  }
                  on_vertex
                  on_checkerboard_fragment
      |> R.render render_config
                  s
                  { primitive_type = #triangles
                  , vertices = s.verts
                  , indices = s.inds
                  }
                  on_vertex
                  on_model_fragment
      |> R.render_wireframe render_config
                            s
                            { primitive_type = #triangles
                            , vertices = s.verts
                            , indices = s.inds
                            }
                            on_wireframe_vertex
                            (\_ _ -> argb.black)
    in if s.show_normals == #yes
       then fb
            |> R.render_normals render_config
                                s
                                { primitive_type = #triangles
                                , vertices = s.verts
                                , indices = s.inds
                                }
                                s.normals
                                0.1
                                on_vertex
                                (\_ _ -> argb.red)
            |> (.target_buffer)
       else fb
            |> (.target_buffer)
}
