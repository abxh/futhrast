import "lib/github.com/abxh/lys/lys"
import "lib/github.com/abxh/futhrast/rasterize/imm_triangle"
import "lib/github.com/abxh/futhrast/rasterize/line"
import "lib/github.com/abxh/futhrast/rasterize/point"
import "lib/github.com/abxh/futhrast/varying"
import "lib/github.com/abxh/futhrast/math/vec"
import "lib/github.com/abxh/futhrast/fragment"
import "lib/github.com/diku-dk/segmented/segmented"

type~ lys_state =
  { h: i64
  , w: i64
  , time: f32
  , verts: [](f32, f32, f32)
  , inds: []i64
  , render_kind: #point | #line | #triangle
  }

module lys_text_content = {
  type text_content = (i64)

  def text_format () =
    "FPS: %ld\n"
    ++ "point (key 1)\n"
    ++ "line (key 2)\n"
    ++ "triangle (key 3)\n"

  def text_content (render_duration: f32) (_: lys_state) : text_content =
    (i64.f32 render_duration)

  def text_colour = const argb.white
}

module lys_file = {
  def input_file_names () =
    ""
    ++ "blender_monkey.obj,"

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
    , time = 3.14 / 2
    , verts = replicate 0 (0, 0, 0)
    , inds = replicate 0 (-1)
    , render_kind = #triangle
    }

  def resize (h: i64) (w: i64) (s: state) =
    s with h = h with w = w

  def keydown (key: i32) (s: state) =
    if key == SDLK_1
    then s with render_kind = #point
    else if key == SDLK_2
    then s with render_kind = #line
    else if key == SDLK_3
    then s with render_kind = #triangle
    else s

  def keyup (_: i32) (s: state) = s

  def event (e: event) (s: state) =
    match e
    case #step td ->
      s with time = s.time + td
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

  -- local
  -- module Config = {
  --                   def tile_size : i64 = 8
  --                   def tile_bin_size : i64 = 32
  --                   def tri_block_size : i64 = 1024
  --                   def triangle_winding_order : #clockwise | #counterclockwise | #neither = #neither
  --                 }:
  --                 ConfigSpec

  local
  module Target = {
    type t = argb.colour
    def dummy = argb.black
  }

  local module P = mk_point_rasterizer Varying
  local module L = mk_line_rasterizer Varying
  local module T = mk_imm_triangle_rasterizer Varying

  def render (s: state) : [][]argb.colour =
    let time = s.time
    let screen_to_window_f ({x = x: f32, y = y: f32}) =
      let v = {x = (x + 1) / 2, y = (y + 1) / 2}
      in {x = v.x * f32.i64 (s.w - 1), y = (1 - v.y) * f32.i64 (s.h - 1)}
    let transform_f t =
      t
      |> (\t ->
            let angle = time
            let cos_a = f32.cos angle
            let sin_a = f32.sin angle
            let x' = t.x * cos_a + t.z * sin_a
            let z' = -t.x * sin_a + t.z * cos_a
            let z_norm = (z' + 1) * 0.5
            in { pos = {x = x', y = t.y, z = z', w = 1}
               , attr = argb.scale argb.white z_norm
               })
      |> proj
      |> (\(pf: pfragment Varying.t) -> pf with pos = screen_to_window_f pf.pos)
    let fs =
      s.verts
      |> map (\(v0, v1, v2) -> {x = v0, y = v1, z = v2})
      |> map (\t -> transform_f t)
    let fs =
      s.inds
      |> map (\i -> fs[i])
    let common_args =
      ( (\f -> f.attr)
      , (\x y -> if x > y then #left else #right)
      , (argb.black, -f32.inf)
      , (replicate s.h (replicate s.w (argb.black)), replicate s.h (replicate s.w (-f32.inf)))
      )
    in match s.render_kind
       case #point ->
         let ps = fs
         in P.rasterize common_args.0 common_args.1 ps common_args.2 common_args.3
            |> (.0)
       case #line ->
         let ls =
           iota ((length fs) / 3) |> map (\i -> (fs[3 * i], fs[3 * i + 1], fs[3 * i + 2]))
           |> expand (\_ -> 3) (\t j -> if j == 0 then (t.0, t.1) else if j == 1 then (t.1, t.2) else (t.2, t.0))
         in L.rasterize common_args.0 common_args.1 ls common_args.2 common_args.3
            |> (.0)
       case #triangle ->
         let ts = iota ((length fs) / 3) |> map (\i -> (fs[3 * i], fs[3 * i + 1], fs[3 * i + 2]))
         in T.rasterize common_args.0 common_args.1 ts common_args.2 common_args.3
            |> (.0)
}

-- let ts = iota ((length fs)) |> map (\i -> (fs[2 * i], fs[2 * i + 1]))
-- {v0=(0,s.h-1), v1=(s.w-1,0), v2=(s.w-1,s.h-1)},
