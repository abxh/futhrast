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
  , zoom: f32
  , angle: f32
  , angle_delta: f32
  , zmax: f32
  , zmin: f32
  , model_zmin: f32
  , model_zmax: f32
  , verts_armadillo: [](f32, f32, f32)
  , verts_bunny: [](f32, f32, f32)
  , verts_monkey: [](f32, f32, f32)
  , verts_head: [](f32, f32, f32)
  , verts_dragon: [](f32, f32, f32)
  , verts_lucy: [](f32, f32, f32)
  , inds_armadillo: []i64
  , inds_bunny: []i64
  , inds_monkey: []i64
  , inds_head: []i64
  , inds_dragon: []i64
  , inds_lucy: []i64
  , render_model:   #bunny
                  | #monkey
                  | #head
                  | #dragon
                  | #lucy
                  | #armadillo
  , inner_mode: bool
  }

module lys_text_content = {
  type text_content = (i64, i64)

  def text_format () =
    "FPS: %ld\n"
    ++ "number of triangles: %ld\n"
    ++ "\n"
    ++ "b: bunny\n"
    ++ "m: monkey\n"
    ++ "h: african head\n"
    ++ "r: dragon\n"
    ++ "l: lucy\n"
    ++ "o: armadillo\n"
    ++ "\n"
    ++ "0: snap into position\n"
    ++ "w|a|s|d: movement\n"
    ++ "i: inner mode\n"
    ++ "+|-: zoom\n"
    ++ "left|right: rotation\n"

  def text_content (render_duration: f32) (s: lys_state) : text_content =
    let num_triangles =
      match s.render_model
      case #bunny -> length s.inds_bunny / 3
      case #monkey -> length s.inds_monkey / 3
      case #head -> length s.inds_head / 3
      case #dragon -> length s.inds_dragon / 3
      case #lucy -> length s.inds_lucy / 3
      case #armadillo -> length s.inds_armadillo / 3
    in (i64.f32 render_duration, num_triangles)

  def text_colour = const argb.white
}

module lys_file = {
  def input_file_names () =
    ""
    ++ "../../models/bunny.obj,"
    ++ "../../models/monkey.obj,"
    ++ "../../models/african_head.obj,"
    ++ "../../models/dragon.obj,"
    ++ "../../models/lucy.obj,"
    ++ "../../models/armadillo.obj,"

  def load_bin _ _ s = s

  def load_obj_vertex_indices [n] (i: i64) (is: [n]i64) (s: lys_state) : lys_state =
    match i
    case 0 ->
      s with inds_bunny = is
    case 1 ->
      s with inds_monkey = is
    case 2 ->
      s with inds_head = is
    case 3 ->
      s with inds_dragon = is
    case 4 ->
      s with inds_lucy = is
    case 5 ->
      s with inds_armadillo = is
    case _ ->
      s

  def load_obj_vertices [n] (i: i64) (vs: [n](f32, f32, f32)) (s: lys_state) : lys_state =
    match i
    case 0 ->
      s with verts_bunny = vs
    case 1 ->
      s with verts_monkey = vs
    case 2 ->
      s with verts_head = vs
    case 3 ->
      s with verts_dragon = vs
    case 4 ->
      s with verts_lucy = vs
    case 5 ->
      s with verts_armadillo = vs
    case _ ->
      s

  def load_obj_normals _ _ s = s
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
    , zoom = 1
    , zmin = 0.001
    , zmax = 10
    , model_zmin = 0
    , model_zmax = 1
    , pos = (0, 0, 0)
    , pos_delta = (0, 0, 0)
    , angle = 0
    , angle_delta = 0
    , inds_armadillo = replicate 0 (-1)
    , inds_bunny = replicate 0 (-1)
    , inds_monkey = replicate 0 (-1)
    , inds_head = replicate 0 (-1)
    , inds_dragon = replicate 0 (-1)
    , inds_lucy = replicate 0 (-1)
    , verts_armadillo = replicate 0 (0, 0, 0)
    , verts_bunny = replicate 0 (0, 0, 0)
    , verts_monkey = replicate 0 (0, 0, 0)
    , verts_head = replicate 0 (0, 0, 0)
    , verts_dragon = replicate 0 (0, 0, 0)
    , verts_lucy = replicate 0 (0, 0, 0)
    , render_model = #bunny
    , inner_mode = false
    }

  def resize (h: i64) (w: i64) (s: state) =
    s with h = h with w = w

  def keydown (key: i32) (s: state) =
    if key == SDLK_b
    then s with render_model = #bunny
    else if key == SDLK_m
    then s with render_model = #monkey
    else if key == SDLK_h
    then s with render_model = #head
    else if key == SDLK_r
    then s with render_model = #dragon
    else if key == SDLK_l
    then s with render_model = #lucy
    else if key == SDLK_o
    then s with render_model = #armadillo
    else if key == SDLK_a
    then s with pos_delta.0 = -1
    else if key == SDLK_d
    then s with pos_delta.0 = 1
    else if key == SDLK_w
    then s with pos_delta.1 = 1
    else if key == SDLK_s
    then s with pos_delta.1 = -1
    else if key == SDLK_RIGHT
    then s with angle_delta = 1
    else if key == SDLK_LEFT
    then s with angle_delta = -1
    else if key == SDLK_PLUS
    then s with zoom = s.zoom * 1.1
    else if key == SDLK_MINUS
    then s with zoom = s.zoom / 1.1
    else if key == SDLK_0
    then s with pos = (0, 0, 0)
           with zoom = 1
           with angle = 0
    else if key == SDLK_i
    then s with inner_mode = !s.inner_mode
    else s

  def keyup (key: i32) (s: state) =
    if key == SDLK_RIGHT
    then s with angle_delta = 0
    else if key == SDLK_LEFT
    then s with angle_delta = 0
    else if key == SDLK_a
    then s with pos_delta.0 = 0
    else if key == SDLK_d
    then s with pos_delta.0 = 0
    else if key == SDLK_w
    then s with pos_delta.1 = 0
    else if key == SDLK_s
    then s with pos_delta.1 = 0
    else s

  def event (e: event) (s: state) =
    match e
    case #step td ->
      s with angle = s.angle + s.angle_delta * td
        with pos = vec3f.from_tuple s.pos vec3f.+ (td vec3f.* (vec3f.from_tuple s.pos_delta)) |> vec3f.to_tuple
    case #keydown {key} ->
      keydown key s
    case #keyup {key} ->
      keyup key s
    case _ -> s

  local
  module Varying : VaryingSpec with t = argb.colour = {
    type t = argb.colour
    def (+) = argb.add_linear
    def (*) = flip argb.scale
  }

  local module R = RenderSetup Varying

  local
  def on_vertex (s: state) (v: (f32, f32, f32)) : vertex_out Varying.t =
    let aspect_ratio = f32.i64 s.w / f32.i64 s.h
    let model =
      transform.identity
      |> (transform.*) (transform.reflect_z)
    let view =
      transform.identity
      |> (transform.*) (rotation.identity
                        |> (rotation.*) (rotation.rotate_y s.angle)
                        |> rotation.to_mat)
      |> (transform.*) (transform.scale s.zoom s.zoom 1)
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

  def render (s: state) : [][]argb.colour =
    let render_config: render_config = {triangle_winding_order = if s.inner_mode then #clockwise else #counterclockwise}
    let (verts, inds) =
      match s.render_model
      case #bunny -> (s.verts_bunny, s.inds_bunny)
      case #monkey -> (s.verts_monkey, s.inds_monkey)
      case #head -> (s.verts_head, s.inds_head)
      case #dragon -> (s.verts_dragon, s.inds_dragon)
      case #lucy -> (s.verts_lucy, s.inds_lucy)
      case #armadillo -> (s.verts_armadillo, s.inds_armadillo)
    let s =
      s with pos.2 = s.zmin + (f32.abs (reduce f32.max f32.lowest (map (.2) verts) - reduce f32.min f32.highest (map (.2) verts)))
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
}
