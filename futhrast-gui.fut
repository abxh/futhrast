import "lib/github.com/abxh/lys/lys"
import "lib/github.com/abxh/futhrast/rasterize/imm_triangle"
import "lib/github.com/abxh/futhrast/rasterize/line"
import "lib/github.com/abxh/futhrast/rasterize/point"
import "lib/github.com/abxh/futhrast/varying"
import "lib/github.com/abxh/futhrast/math/vec"
import "lib/github.com/abxh/futhrast/fragment"

type~ lys_state =
  { h: i64
  , w: i64
  , time: f32
  , verts: [](f32, f32, f32)
  , inds: []i64
  }

module lys_text_content = {
  type text_content = (i64)

  def text_format () =
    ""

  def text_content (render_duration: f32) (_: lys_state) : text_content =
    (i64.f32 render_duration)

  def text_colour = const argb.white
}

module lys_file = {
  def input_file_names () =
    ""
    ++ "stanford_bunny.obj,"

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
    }

  def resize (h: i64) (w: i64) (s: state) =
    s with h = h with w = w

  def keydown (_: i32) (s: state) = s

  def keyup (_: i32) (s: state) = s

  def event (e: event) (s: state) =
    match e
    case #step td ->
      s with time = s.time + td
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

  local module Rasterizer = mk_imm_triangle_rasterizer Varying

  def render (s: state) : [][]argb.colour =
    let screen_to_window_f ({x = x: f32, y = y: f32}) =
      let v = {x = (x + 1) / 2, y = (y + 1) / 2}
      in {x = v.x * f32.i64 (s.w - 1), y = (1 - v.y) * f32.i64 (s.h - 1)}
    -- let near_z = 0.1
    -- let z_dist = f32.abs ((map (.1) s.verts |> f32.maximum) - (map (.1) s.verts |> f32.minimum))
    -- let far_z = near_z + z_dist + 4
    let transform_f t =
      t
      |> (\t ->
            { pos = {x = t.x, y = t.y, z = t.z, w = 1}
            , attr = argb.scale argb.white t.z
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
    let ts =
      iota ((length fs) / 3)
      |> map (\i -> (fs[3 * i], fs[3 * i + 1], fs[3 * i + 2]))
    in Rasterizer.rasterize (\f -> f.attr)
                            (\x y -> if x > y then #left else #right)
                            ts
                            (argb.black, -f32.inf)
                            (replicate s.h (replicate s.w (argb.black)), replicate s.h (replicate s.w (-f32.inf)))
       |> (.0)
}

-- {v0=(0,s.h-1), v1=(s.w-1,0), v2=(s.w-1,s.h-1)},

-- let fb = replicate s.h (replicate s.w default)
-- let ps = imm_rasterizer.expand_triangles (\_ -> (argb.red, 1)) ts
-- let fb' = imm_rasterizer.rasterize_fragments fb default (f32.>) ps
-- in fb'
--    |> flatten
--    |> map (.0)
--    |> unflatten
