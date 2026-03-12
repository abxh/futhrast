-- user-defined configuration
module type ConfigSpec = {
  -- framebuffer tile size set to e.g. 16
  val tile_size : i64

  -- triangle winding order
  val triangle_winding_order : #clockwise | #counterclockwise | #neither
}
