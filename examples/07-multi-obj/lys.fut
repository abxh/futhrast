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
  , verts_avg_pos: (f32, f32, f32)
  , orientation: rotation.t
  , orientation_delta: rotation.t
  , zmin: f32
  , body_verts: [](f32, f32, f32)
  , body_inds: []i64
  , body_normals: [](f32, f32, f32)
  , facial_verts: [](f32, f32, f32)
  , facial_inds: []i64
  , facial_normals: [](f32, f32, f32)
  , quils_verts: [](f32, f32, f32)
  , quils_inds: []i64
  , quils_normals: [](f32, f32, f32)
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
    let num_triangles =
      length s.body_inds / 3
      + length s.facial_inds / 3
      + length s.quils_inds / 3
    in (i64.f32 render_duration, num_triangles)

  def text_colour = const argb.black
}

module lys_file = {
  def input_file_names () =
    ""
    ++ "../../models/hedgehog/body.obj,"
    ++ "../../models/hedgehog/eyes_and_nose.obj,"
    ++ "../../models/hedgehog/quils.obj,"

  def load_bin _ _ s = s

  def load_obj_vertex_indices [n] (i: i64) (is: [n]i64) (s: lys_state) : lys_state =
    match i
    case 0 ->
      s with body_inds = is
    case 1 ->
      s with facial_inds = is
    case 2 ->
      s with quils_inds = is
    case _ ->
      s

  def load_obj_vertices [n] (i: i64) (vs: [n](f32, f32, f32)) (s: lys_state) : lys_state =
    match i
    case 0 ->
      s with body_verts = vs
        with verts_avg_pos.0 = f32.sum (map (.0) vs) / f32.i64 (length vs)
        with verts_avg_pos.1 = reduce f32.min f32.highest (map (.1) vs)
        with verts_avg_pos.2 = f32.sum (map (.2) vs) / f32.i64 (length vs)
    case 1 ->
      s with facial_verts = vs
    case 2 ->
      s with quils_verts = vs
    case _ ->
      s

  def load_obj_normals [n] (i: i64) (vs: [n](f32, f32, f32)) (s: lys_state) : lys_state =
    match i
    case 0 ->
      s with body_normals = vs
    case 1 ->
      s with facial_normals = vs
    case 2 ->
      s with quils_normals = vs
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
    , verts_avg_pos = (0, 0, 0)
    , orientation = rotation.identity
    , orientation_delta = rotation.identity
    , body_verts = replicate 0 (0, 0, 0)
    , body_inds = replicate 0 (-1)
    , body_normals = replicate 0 (0, 0, 0)
    , facial_verts = replicate 0 (0, 0, 0)
    , facial_inds = replicate 0 (-1)
    , facial_normals = replicate 0 (0, 0, 0)
    , quils_verts = replicate 0 (0, 0, 0)
    , quils_inds = replicate 0 (-1)
    , quils_normals = replicate 0 (0, 0, 0)
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

  local
  module vec3f_tup : VaryingSpec with t = (vec3f.t, vec3f.t) = {
    type t = (vec3f.t, vec3f.t)
    def (+) (lhs: t) (rhs: t) = (lhs.0 vec3f.+ rhs.0, lhs.1 vec3f.+ rhs.1)
    def (*) (s: f32) (rhs: t) = (s vec3f.* rhs.0, s vec3f.* rhs.1)
  }

  local module RC = RenderSetup vec3f
  local module RV = RenderSetup vec3f_tup

  local
  def on_grass_vertex (s: state) (v: (f32, f32, f32)) : vertex_out vec3f.t =
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
      model
      |> (transform.*) view
      |> (transform.*) proj
    let v = vec3f.from_tuple v
    in { pos = transform.apply v mvp
       , attr = v
       }

  local
  def on_vertex (s: state) ((v, n): ((f32, f32, f32), (f32, f32, f32))) : vertex_out vec3f_tup.t =
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
    let pos_mat =
      transform.identity
      |> (transform.*) model
      |> (transform.*) view
    let mvp =
      pos_mat
      |> (transform.*) proj
    let normal_mat =
      pos_mat
      |> transform.inverse
      |> transform.transpose
    let v = vec3f.from_tuple v
    let n = vec3f.from_tuple n
    let pos = transform.apply_to_pos v pos_mat
    let normal = transform.apply_to_dir n normal_mat
    in { pos = transform.apply v mvp
       , attr = (pos, normal)
       }

  def light_dir = vec3f.from_tuple (0, 1, -1) |> vec3f.normalize

  def ambient_color = argb.from_rgba (235 / 255) (210 / 255) (140 / 255) 1
  def diffuse_color = argb.from_rgba (255 / 255) (235 / 255) (160 / 255) 1
  def specular_color = argb.from_rgba (255 / 255) (250 / 255) (210 / 255) 1

  local
  def on_body_fragment (_: state) (f: fragment vec3f_tup.t) : argb.colour =
    let ka = 0.45
    let kb = 0.45
    let kc = 0.35
    let (vertex_pos, vertex_normal) = f.attr
    let vertex_normal = vertex_normal |> vec3f.normalize
    let diffuse_raw = f32.max 0 (vertex_normal `vec3f.dot` light_dir)
    let diffuse_factor = 0.35 + 0.65 * diffuse_raw
    let specular_factor =
      let shininess = 48
      let view_dir = (vec3f.neg vertex_pos) |> vec3f.normalize
      let halfway = (light_dir vec3f.+ view_dir) |> vec3f.normalize
      in (f32.max 0 (vertex_normal `vec3f.dot` halfway)) ** shininess
    in argb.from_rgba 0 0 0 0
       `argb.add_linear` argb.scale ambient_color (ka)
       `argb.add_linear` argb.scale diffuse_color (kb * diffuse_factor)
       `argb.add_linear` argb.scale specular_color (kc * specular_factor)

  def eye_nose_color = argb.from_rgba (20 / 255) (20 / 255) (20 / 255) 1
  def eye_specular = argb.from_rgba (255 / 255) (255 / 255) (255 / 255) 1

  local
  def on_eye_nose_fragment (_: state) (f: fragment vec3f_tup.t) : argb.colour =
    let kb = 0.25
    let kc = 0.8
    let (vertex_pos, vertex_normal) = f.attr
    let vertex_normal = vertex_normal |> vec3f.normalize
    let diffuse_raw = f32.max 0 (vertex_normal `vec3f.dot` light_dir)
    let diffuse_factor = 0.4 + 0.6 * diffuse_raw
    let specular_factor =
      let shininess = 80
      let view_dir = (vec3f.neg vertex_pos) |> vec3f.normalize
      let halfway = (light_dir vec3f.+ view_dir) |> vec3f.normalize
      in (f32.max 0 (vertex_normal `vec3f.dot` halfway)) ** shininess
    in argb.from_rgba 0 0 0 0
       `argb.add_linear` argb.scale eye_nose_color (kb * diffuse_factor)
       `argb.add_linear` argb.scale eye_specular (kc * specular_factor)

  def grass_center = argb.from_rgba (70 / 255) (120 / 255) (55 / 255) 1
  def grass_mid = argb.from_rgba (110 / 255) (160 / 255) (80 / 255) 1
  def grass_outer = argb.from_rgba (20 / 255) (60 / 255) (25 / 255) 1

  local
  def on_grass_fragment (s: state) (f: fragment vec3f.t) : argb.colour =
    let p = f.attr
    let dx = p.x - s.verts_avg_pos.0
    let dz = p.z - s.verts_avg_pos.2
    let dist = f32.sqrt (dx * dx + dz * dz)
    let radius = 2.0
    let edge = 0.25
    let mask = f32.min (f32.max ((radius - dist) / edge) 0.0) 1.0
    let t = f32.max 0 (1 - dist / radius)
    let t2 = t * t * (3 - 2 * t)
    let noise = 0.08 * (0.5 + 0.5 * f32.sin (p.x * 8.0 + p.z * 11.0))
    let g = f32.min 1 (t2 * (0.75 + noise))
    let t = (f32.min 1 (g / 0.6)) ** 1.6
    let col =
      argb.from_rgba 0 0 0 0
      `argb.add_linear` argb.scale grass_outer (1 - t)
      `argb.add_linear` argb.scale grass_mid (1 - t)
      `argb.add_linear` argb.scale grass_center t
    in if mask <= 0.0
       then argb.from_rgba 1 1 1 1
       else col

  def quils = argb.from_rgba (10 / 255) (25 / 255) (60 / 255) 1

  def render (s: state) : [][]argb.colour =
    let render_config: render_config =
      { triangle_winding_order = #neither
      }
    let num_body_verts = length s.body_inds
    let body_verts' =
      zip (sized num_body_verts (map (\i -> s.body_verts[i]) s.body_inds))
          (sized num_body_verts s.body_normals)
    let num_facial_verts = length s.facial_inds
    let facial_verts' =
      zip (sized num_facial_verts (map (\i -> s.facial_verts[i]) s.facial_inds))
          (sized num_facial_verts s.facial_normals)
    let num_quils_verts = length s.quils_inds
    let quils_verts' =
      zip (sized num_quils_verts (map (\i -> s.quils_verts[i]) s.quils_inds))
          (sized num_quils_verts s.quils_normals)
    in (tabulate_2d s.h s.w (const (const (argb.white))), tabulate_2d s.h s.w (const (const 0)))
       |> RV.render render_config
                    s
                    { primitive_type = #triangles
                    , vertices = body_verts'
                    , indices = iota num_body_verts
                    }
                    on_vertex
                    on_body_fragment
       |> RV.render render_config
                    s
                    { primitive_type = #triangles
                    , vertices = facial_verts'
                    , indices = iota num_facial_verts
                    }
                    on_vertex
                    on_eye_nose_fragment
       |> RV.render render_config
                    s
                    { primitive_type = #triangles
                    , vertices = quils_verts'
                    , indices = iota num_quils_verts
                    }
                    on_vertex
                    (\_ _ -> quils)
       |> RC.render render_config
                    s
                    { primitive_type = #triangles
                    , vertices =
                        [ (-20, s.verts_avg_pos.1, -20)
                        , (20, s.verts_avg_pos.1, -20)
                        , (20, s.verts_avg_pos.1, 20)
                        , (-20, s.verts_avg_pos.1, 20)
                        ]
                    , indices = [0, 1, 2, 0, 2, 3]
                    }
                    on_grass_vertex
                    on_grass_fragment
       |> (.0)
  }
