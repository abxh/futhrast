-- user-defined configuration
module type ConfigSpec = {
  -- tile_size * tile_size defines number of pixels in a tile. set to e.g. 8
  val tile_size : i64

  -- tile_bin_size * tile_bin_size defines number of tiles in a bin. set to e.g. 64
  val tile_bin_size : i64

  -- number of triangles to process in batches in a tile. set to e.g. 256
  val tri_batch_size : i64

  -- triangle winding order
  val triangle_winding_order : #clockwise | #counterclockwise | #neither
}
