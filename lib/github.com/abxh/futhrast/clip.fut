import "fragment"

-- | check whether to cull point, depending on whether it is in NDC space
def point_in_frustum 'varying (f: vertex_out varying) =
  let p = f.pos
  in (-p.w <= p.x && p.x <= p.w)
     && (-p.w <= p.y && p.y <= p.w)
     && (0 <= p.z && p.z <= p.w)
