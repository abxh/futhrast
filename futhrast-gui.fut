import "lib/github.com/abxh/lys/lys"
import "lib/github.com/abxh/futhrast/types"
import "lib/github.com/abxh/futhrast/setup"
import "lib/github.com/abxh/futhrast/math/vec"
import "lib/github.com/diku-dk/segmented/segmented"

type~ lys_state =
  { h: i64
  , w: i64
  , pos: (f32, f32, f32)
  , pos_delta: (f32, f32, f32)
  , zoom: f32
  , angle: f32
  , angle_delta: f32
  , verts0: [](f32, f32, f32)
  , verts1: [](f32, f32, f32)
  , verts2: [](f32, f32, f32)
  , inds0: []i64
  , inds1: []i64
  , inds2: []i64
  , render_model: #bunny | #monkey | #head
  , render_kind: #points | #lines | #triangles
  }

module lys_text_content = {
  type text_content = (i64)

  def text_format () =
    "FPS: %ld\n"
    ++ "\n"
    ++ "b: bunny\n"
    ++ "m: monkey\n"
    ++ "h: african head\n"
    ++ "\n"
    ++ "0: snap into position\n"
    ++ "1|2|3: point|line|triangle\n"
    ++ "w|a|s|d: movement\n"
    ++ "+|-: zoom\n"
    ++ "left|right: rotation\n"

  def text_content (render_duration: f32) (_: lys_state) : text_content =
    (i64.f32 render_duration)

  def text_colour = const argb.white
}

module lys_file = {
  def input_file_names () =
    ""
    ++ "bunny.obj,"
    ++ "monkey.obj,"
    ++ "african_head.obj,"

  def load_bin _ _ s = s

  def load_obj_vertex_indices [n] (i: i64) (is: [n]i64) (s: lys_state) : lys_state =
    match i
    case 0 ->
      s with inds0 = is
    case 1 ->
      s with inds1 = is
    case 2 ->
      s with inds2 = is
    case _ ->
      s

  def load_obj_vertices [n] (i: i64) (vs: [n](f32, f32, f32)) (s: lys_state) : lys_state =
    match i
    case 0 ->
      s with verts0 = vs
    case 1 ->
      s with verts1 = vs
    case 2 ->
      s with verts2 = vs
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
    , pos = (0, 0, 0)
    , pos_delta = (0, 0, 0)
    , angle = 0
    , angle_delta = 0
    , inds0 = replicate 0 (-1)
    , inds1 = replicate 0 (-1)
    , inds2 = replicate 0 (-1)
    , verts0 = replicate 0 (0, 0, 0)
    , verts1 = replicate 0 (0, 0, 0)
    , verts2 = replicate 0 (0, 0, 0)
    , render_model = #bunny
    , render_kind = #triangles
    }

  def resize (h: i64) (w: i64) (s: state) =
    s with h = h with w = w

  def keydown (key: i32) (s: state) =
    if key == SDLK_1
    then s with render_kind = #points
    else if key == SDLK_2
    then s with render_kind = #lines
    else if key == SDLK_3
    then s with render_kind = #triangles
    else if key == SDLK_b
    then s with render_model = #bunny
    else if key == SDLK_m
    then s with render_model = #monkey
    else if key == SDLK_h
    then s with render_model = #head
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

  local
  module Config : ConfigSpec = {
    def triangle_winding_order : #clockwise | #counterclockwise | #neither = #counterclockwise
    def depth_type : #normal_z | #reversed_z = #reversed_z
  }

  local module R = RenderSetup Config Varying

  local
  def on_vertex (s: state) (v: (f32, f32, f32)) : vertex_out Varying.t =
    let v = v |> vec3f.from_tuple
    let v = v with z = -v.z
    let angle = s.angle
    let cos_a = f32.cos angle
    let sin_a = f32.sin angle
    let x' = v.x * cos_a + v.z * sin_a
    let z' = -v.x * sin_a + v.z * cos_a
    let z_norm = (z' + 1) * 0.5
    in { pos = {x = (x' * s.zoom) + s.pos.0, y = (v.y * s.zoom) + s.pos.1, z = z', w = 1}
       , attr = argb.scale argb.white z_norm
       }

  local
  def on_fragment (_: state) (f: fragment Varying.t) : argb.colour =
    f.attr

  def render (s: state) : [][]argb.colour =
    let (verts, inds) =
      match s.render_model
      case #bunny -> (s.verts0, s.inds0)
      case #monkey -> (s.verts1, s.inds1)
      case #head -> (s.verts2, s.inds2)
    in match s.render_kind
       case #points ->
         R.init {w = s.w, h = s.h} argb.black
         |> R.render s
                     { primitive_type = s.render_kind
                     , vertices = verts
                     , indices = inds
                     }
                     on_vertex
                     on_fragment
                     argb.black
         |> R.unpack
         |> (.0)
       case #lines ->
         R.init {w = s.w, h = s.h} argb.black
         |> R.render_wireframe s
                               { primitive_type = #triangles
                               , vertices = verts
                               , indices = inds
                               }
                               on_vertex
                               on_fragment
                               argb.black
         |> R.unpack
         |> (.0)
       case #triangles ->
         R.init {w = s.w, h = s.h} argb.black
         |> R.render s
                     { primitive_type = s.render_kind
                     , vertices = verts
                     , indices = inds
                     }
                     on_vertex
                     on_fragment
                     argb.black
         |> R.unpack
         |> (.0)
}

-- let ts = iota ((length fs)) |> map (\i -> (fs[2 * i], fs[2 * i + 1]))
-- {v0=(0,s.h-1), v1=(s.w-1,0), v2=(s.w-1,s.h-1)},
