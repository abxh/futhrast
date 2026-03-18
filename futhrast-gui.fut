import "lib/github.com/abxh/lys/lys"
import "lib/github.com/abxh/futhrast/rasterize"
import "lib/github.com/abxh/futhrast/framebuffer"
import "lib/github.com/abxh/futhrast/fragment"
import "lib/github.com/abxh/futhrast/config"

type lys_state =
  { h: i64
  , w: i64
  , time: f32
  }

module lys_text_content = {
  type text_content = (i64)

  def text_format () =
    "FPS: %ld\n"

  def text_content (render_duration: f32) (_: lys_state) : text_content =
    (i64.f32 render_duration)

  def text_colour = const argb.white
}

module lys : lys with text_content = lys_text_content.text_content = {
  open lys_text_content
  open lys_no_file

  type state = lys_state

  def grab_mouse = false

  def init (_: u32) (h: i64) (w: i64) : state =
    { w
    , h
    , time = 3.14 / 2
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
    def zero = argb.black
    def (+) = argb.add_linear
    def (*) = flip argb.scale
    def (-) x y = x + (-1 * y)
    def (/) x s = (1 / s) * x
  }

  local
  module Config = {
                    def tile_size : i64 = 8
                    def tile_bin_size : i64 = 32
                    def tri_batch_size : i64 = 256
                    def triangle_winding_order : #clockwise | #counterclockwise | #neither = #neither
                  }:
                  ConfigSpec

  local
  module Target = {
    type t = argb.colour
    def dummy = argb.black
  }

  local module Framebuffer = Framebuffer Config Target
  local module Rasterizer = mk_rasterizer Varying Framebuffer

  def render (s: state) : [][]argb.colour =
    let t = f32.abs (f32.sin s.time * f32.sin s.time)
    let f =
      \f ->
        { pos =
            { x = f32.i64 f.pos.0
            , y = f32.i64 f.pos.1
            }
        , Z_inv = 1
        , depth = if f.order == 1 then 1 else 0.8
        , attr = f.colour
        }
    let ts =
      [ ( { pos = (0, 0)
          , colour = argb.green
          , order = 1
          }
        , { pos =
              ( (s.w - 1) / 2 + i64.f32 (f32.i64 (s.w - 1) / 2 * t)
              , (s.h - 1) / 2 + i64.f32 (f32.i64 (s.h - 1) / 2 * t)
              )
          , colour = argb.blue
          , order = 1
          }
        , { pos = (0, (s.h - 1) / 2 + i64.f32 (f32.i64 (s.h - 1) / 2 * t))
          , colour = argb.red
          , order = 1
          }
        )
      , ( { pos = ((s.w - 1) / 2, s.h - 1)
          , colour = argb.blue
          , order = 0
          }
        , { pos = (s.w - 1, (s.h - 1) / 2)
          , colour = argb.orange
          , order = 0
          }
        , { pos = (s.w - 1, s.h - 1)
          , colour = argb.magenta
          , order = 0
          }
        )
      ]
      |> map (\(v0, v1, v2) -> (f v0, f v1, f v2))
    in Framebuffer.init {w = s.w, h = s.h} argb.black
       |> Rasterizer.rasterize (\v -> v.attr) ts
       |> Framebuffer.target_buf
}

-- {v0=(0,s.h-1), v1=(s.w-1,0), v2=(s.w-1,s.h-1)},

-- let fb = replicate s.h (replicate s.w default)
-- let ps = imm_rasterizer.expand_triangles (\_ -> (argb.red, 1)) ts
-- let fb' = imm_rasterizer.rasterize_fragments fb default (f32.>) ps
-- in fb'
--    |> flatten
--    |> map (.0)
--    |> unflatten
