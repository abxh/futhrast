-- user-defined configuration
module type ConfigSpec = {
  -- framebuffer tile size set to e.g. 8.
  val tile_size : i64

  -- coarse rasterizer mask type. set this equal to tri_block_size or u8
  module tri_block_mask_type : integral

  -- coarse rasterizer work block size set to e.g. 32. optimum size depends on on-chip memory
  val tri_block_size : i64

  -- triangle winding order
  val triangle_winding_order : #clockwise | #counterclockwise | #neither
}
