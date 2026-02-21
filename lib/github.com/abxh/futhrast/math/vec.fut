-- | tiny vector implementation
--
-- Inspiration:
-- github.com/athas/vector
--
-- The exposed modules are at the end of the file:
-- module vec2 = mk_vspace2 f32
-- module vec3 = mk_vspace3 f32
-- module vec4 = mk_vspace4 f32


local module type ScalarSpec = {
    --- | the scalar type
    type t

    val +: t -> t -> t
    val -: t -> t -> t
    val *: t -> t -> t
    val /: t -> t -> t

    val i32: i32 -> t
    val sqrt : t -> t
}

local module type VSpaceSpec = {
    -- | the vector type
    type t
    -- | the vector value type
    type value_t

    -- | zero vector
    val zero: t
    -- | one vector
    val one: t

    -- | map a vector component-wise
    val map: (value_t -> value_t) -> t -> t
    -- | map two vectors component-wise
    val map2: (value_t -> value_t -> value_t) -> t -> t -> t

    -- | element-wise addition
    val +: t -> t -> t
    -- | element-wise subtraction
    val -: t -> t -> t
    -- | element-wise scaling
    val *: value_t -> t -> t
    -- | element-wise inverse scaling
    val /: t -> value_t -> t
    -- | element-wise inverse scaled by some scalar
    val /.: value_t -> t -> t
    -- | element-wise multiplication (hadamard product)
    val hadamard: t -> t -> t

    -- | dot product
    val dot: t -> t -> value_t
    -- | vector norm
    val norm: t -> value_t
    -- | normalize vector
    val normalize: t -> t
}

local module type VSpaceSpec2 = {
    type value_t

    include VSpaceSpec 
        with value_t = value_t 
        with t = {x: value_t, y: value_t}

    -- | "2d" cross product (area of parallelogram spanned by the two vectors)
    val cross: t -> t -> value_t
}

local module type VSpaceSpec3 = {
    type value_t

    include VSpaceSpec 
        with value_t = value_t
        with t = {x: value_t, y: value_t, z: value_t}

    -- | cross product
    val cross: t -> t -> t
}

local module type VSpaceSpec4 = {
    type value_t

    include VSpaceSpec 
        with value_t = value_t
        with t = {x: value_t, y: value_t, z: value_t, w: value_t}
}

local module mk_vspace2 (scalar: ScalarSpec): VSpaceSpec2 with value_t = scalar.t = {
    type value_t = scalar.t
    type t = {x: value_t, y: value_t}

    def zero: t = {x = scalar.i32 0, y = scalar.i32 0}
    def one: t  = {x = scalar.i32 1, y = scalar.i32 1} 

    def map (f: value_t -> value_t) (v: t): t =
        let x = f v.x
        let y = f v.y
        in {x, y}
    def map2 (f: value_t -> value_t -> value_t) (lhs: t) (rhs: t): t =
        let x = f lhs.x rhs.x
        let y = f lhs.y rhs.y
        in {x, y}

    def (+) = map2 (scalar.+)
    def (-) = map2 (scalar.-)
    def (*) (s: value_t) (v: t)  = map (\e -> (scalar.*) s e) v
    def (/) (v: t) (s: value_t)  = map (\e -> (scalar./) e s) v
    def (/.) (s: value_t) (v: t) = map (\e -> (scalar./) s e) v
    def hadamard = map2 (scalar.*)

    def dot (lhs: t) (rhs: t): value_t =
        let x = (scalar.*) lhs.x rhs.x
        let y = (scalar.*) lhs.y rhs.y
        in (scalar.+) x y
    def norm (v: t) = scalar.sqrt (dot v v)
    def normalize (v: t) = v / norm v
    def cross (lhs: t) (rhs: t): value_t =
        let XY = (scalar.-) ((scalar.*) lhs.x rhs.y) ((scalar.*) rhs.x lhs.y)
        in XY
}

local module mk_vspace3 (scalar: ScalarSpec): VSpaceSpec3 with value_t = scalar.t = {
    type value_t = scalar.t
    type t = {x: value_t, y: value_t, z: value_t}

    def zero: t = {x = scalar.i32 0, y = scalar.i32 0, z = scalar.i32 0}
    def one: t  = {x = scalar.i32 1, y = scalar.i32 1, z = scalar.i32 1} 

    def map (f: value_t -> value_t) (v: t): t =
        let x = f v.x
        let y = f v.y
        let z = f v.z
        in {x, y, z}
    def map2 (f: value_t -> value_t -> value_t) (lhs: t) (rhs: t): t =
        let x = f lhs.x rhs.x
        let y = f lhs.y rhs.y
        let z = f lhs.z rhs.z
        in {x, y, z}

    def (+) = map2 (scalar.+)
    def (-) = map2 (scalar.-)
    def (*) (s: value_t) (v: t)  = map (\e -> (scalar.*) s e) v
    def (/) (v: t) (s: value_t)  = map (\e -> (scalar./) e s) v
    def (/.) (s: value_t) (v: t) = map (\e -> (scalar./) s e) v
    def hadamard = map2 (scalar.*)

    def dot (lhs: t) (rhs: t): value_t =
        let x = (scalar.*) lhs.x rhs.x
        let y = (scalar.*) lhs.y rhs.y
        in (scalar.+) x y
    def norm (v: t) = scalar.sqrt (dot v v)
    def normalize (v: t) = v / norm v
    def cross (lhs: t) (rhs: t): t =
        let YZ = (scalar.-) ((scalar.*) lhs.y rhs.z) ((scalar.*) lhs.z rhs.y)
        let ZX = (scalar.-) ((scalar.*) lhs.z rhs.x) ((scalar.*) lhs.x rhs.z)
        let XY = (scalar.-) ((scalar.*) lhs.x rhs.y) ((scalar.*) lhs.y rhs.x)
        in {x = YZ, y = ZX, z = XY}
}

local module mk_vspace4 (scalar: ScalarSpec): VSpaceSpec4 with value_t = scalar.t = {
    type value_t = scalar.t
    type t = {x: value_t, y: value_t, z: value_t, w: value_t}

    def zero: t = {x = scalar.i32 0, y = scalar.i32 0, z = scalar.i32 0, w = scalar.i32 0}
    def one: t  = {x = scalar.i32 1, y = scalar.i32 1, z = scalar.i32 1, w = scalar.i32 1} 

    def map (f: value_t -> value_t) (v: t): t =
        let x = f v.x
        let y = f v.y
        let z = f v.z
        let w = f v.w
        in {x, y, z, w}
    def map2 (f: value_t -> value_t -> value_t) (lhs: t) (rhs: t): t =
        let x = f lhs.x rhs.x
        let y = f lhs.y rhs.y
        let z = f lhs.z rhs.z
        let w = f lhs.w rhs.w
        in {x, y, z, w}

    def (+) = map2 (scalar.+)
    def (-) = map2 (scalar.-)
    def (*) (s: value_t) (v: t)  = map (\e -> (scalar.*) s e) v
    def (/) (v: t) (s: value_t)  = map (\e -> (scalar./) e s) v
    def (/.) (s: value_t) (v: t) = map (\e -> (scalar./) s e) v
    def hadamard = map2 (scalar.*)

    def dot (lhs: t) (rhs: t): value_t =
        let x = (scalar.*) lhs.x rhs.x
        let y = (scalar.*) lhs.y rhs.y
        in (scalar.+) x y
    def norm (v: t) = scalar.sqrt (dot v v)
    def normalize (v: t) = v / norm v
}

module vec2 = mk_vspace2 f32
module vec3 = mk_vspace3 f32
module vec4 = mk_vspace4 f32
