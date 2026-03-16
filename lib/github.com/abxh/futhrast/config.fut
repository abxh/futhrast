-- user-defined configuration
module type ConfigSpec = {
  -- framebuffer tile size set to e.g. 8.
  val tile_size : i64

  -- coarse rasterizer work block size set to e.g. 256
  val tri_block_size : i64

  -- triangle winding order
  val triangle_winding_order : #clockwise | #counterclockwise | #neither
}
