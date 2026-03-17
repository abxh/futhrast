-- user-defined configuration
module type ConfigSpec = {
  -- tile_size * tile_size defines number of pixels in a tile
  val tile_size : i64

  -- tile_bin_size * tile_bin_size defines number of tiles in a bin
  val tile_bin_size : i64

  -- triangle winding order
  val triangle_winding_order : #clockwise | #counterclockwise | #neither
}
