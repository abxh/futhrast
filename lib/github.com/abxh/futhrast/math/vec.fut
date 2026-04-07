-- | tiny vector implementation
--
-- Inspiration:
-- github.com/athas/vector

local
module type scalar = {
  --- | the scalar type
  type t

  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val (*) : t -> t -> t
  val (/) : t -> t -> t

  val zero : t
  val one : t
}

local
module type scalar_float = {
  include scalar

  val sqrt : t -> t
}

local
module type vec_space = {
  -- | the vector type
  type t

  -- | the vector value type
  type value

  -- | zero vector
  val zero : t

  -- | one vector
  val one : t

  -- | element-wise negation
  val neg : t -> t

  -- | element-wise addition
  val (+) : t -> t -> t

  -- | element-wise subtraction
  val (-) : t -> t -> t

  -- | element-wise scaling
  val (*) : value -> t -> t

  -- | element-wise inverse scaling
  val (/) : t -> value -> t

  -- | element-wise inverse scaled by some scalar
  val (/.) : value -> t -> t

  -- | element-wise multiplication (hadamard product)
  val hadamard : t -> t -> t

  -- | dot product
  val dot : t -> t -> value

  -- | perform a left fold over vector elements
  val foldl 'a : (a -> value -> a) -> a -> t -> a

  -- | perform a right fold over vector elements
  val foldr 'a : (value -> a -> a) -> a -> t -> a

  -- | vector from replicated value
  val replicate : value -> t

  -- | get the element at some position
  val get : i64 -> t -> value

  -- | set the element at some position
  val set : i64 -> value -> t -> t
}

local
module type vec_space_float = {
  include vec_space

  -- | vector norm
  val norm : t -> value

  -- | normalize vector
  val normalize : t -> t
}

local
module type vec2 = {
  -- value type
  type value

  -- vector type
  type t = {x: value, y: value}

  -- | map a vector component-wise
  val map 'a : (value -> a) -> t -> {x: a, y: a}

  -- | map two vectors component-wise
  val map2 'a : (value -> value -> a) -> t -> t -> {x: a, y: a}

  -- convert to array
  val to_array : t -> [2]value

  -- convert from array
  val from_array : [2]value -> t

  -- convert to tuple
  val to_tuple : t -> (value, value)

  -- convert from tuple
  val from_tuple : (value, value) -> t

  -- | "2d" cross product (area of parallelogram spanned by the two vectors)
  val cross : t -> t -> value
}

local
module type vec3 = {
  -- | value type
  type value

  -- | vector type
  type t = {x: value, y: value, z: value}

  -- | map a vector component-wise
  val map 'a : (value -> a) -> t -> {x: a, y: a, z: a}

  -- | map two vectors component-wise
  val map2 'a : (value -> value -> a) -> t -> t -> {x: a, y: a, z: a}

  -- convert to array
  val to_array : t -> [3]value

  -- convert from array
  val from_array : [3]value -> t

  -- convert to tuple
  val to_tuple : t -> (value, value, value)

  -- convert from tuple
  val from_tuple : (value, value, value) -> t

  -- | cross product
  val cross : t -> t -> t
}

local
module type vec4 = {
  -- | value type
  type value

  -- | vector type
  type t = {x: value, y: value, z: value, w: value}

  -- | map a vector component-wise
  val map 'a : (value -> a) -> t -> {x: a, y: a, z: a, w: a}

  -- | map two vectors component-wise
  val map2 'a : (value -> value -> a) -> t -> t -> {x: a, y: a, z: a, w: a}

  -- convert to array
  val to_array : t -> [4]value

  -- convert from array
  val from_array : [4]value -> t

  -- convert to tuple
  val to_tuple : t -> (value, value, value, value)

  -- convert from tuple
  val from_tuple : (value, value, value, value) -> t
}

module mk_vspace2 (scalar: scalar)
  : {
      include vec2 with value = scalar.t
      include vec_space with value = value with t = t
    } = {
  type value = scalar.t
  type t = {x: value, y: value}

  def map 'a (f: value -> a) (v: t) : {x: a, y: a} =
    let x = f v.x
    let y = f v.y
    in {x, y}

  def map2 'a (f: value -> value -> a) (lhs: t) (rhs: t) : {x: a, y: a} =
    let x = f lhs.x rhs.x
    let y = f lhs.y rhs.y
    in {x, y}

  def neg = map (\r -> scalar.zero scalar.- r)
  def (+) = map2 (scalar.+)
  def (-) = map2 (scalar.-)
  def (*) (s: value) (v: t) = map (\e -> (scalar.*) s e) v
  def (/) (v: t) (s: value) = map (\e -> (scalar./) e s) v
  def (/.) (s: value) (v: t) = map (\e -> (scalar./) s e) v
  def hadamard = map2 (scalar.*)

  def foldl 'a (f: a -> value -> a) (acc: a) (v: t) : a =
    f (f acc v.x) v.y

  def foldr 'a (f: value -> a -> a) (acc: a) (v: t) : a =
    f v.x (f v.y acc)

  def replicate (e: value) : t =
    {x = e, y = e}

  def to_tuple (v: t) = (v.x, v.y)
  def from_tuple ((x, y): (value, value)) = {x, y}
  def to_array (v: t) = [v.x, v.y]
  def from_array (a: [2]value) = {x = a[0], y = a[1]}

  def get (i: i64) (v: t) : value = (to_array v)[i]
  def set (i: i64) (x: value) (v: t) : t = from_array ([v.x, v.y] with [i] = x)

  def zero : t = replicate scalar.zero
  def one : t = replicate scalar.one

  def dot (lhs: t) (rhs: t) : value =
    foldl (scalar.+) scalar.zero (map2 (scalar.*) lhs rhs)

  def cross (lhs: t) (rhs: t) : value =
    let XY = (scalar.-) ((scalar.*) lhs.x rhs.y) ((scalar.*) rhs.x lhs.y)
    in XY
}

module mk_vspace2f (scalar: scalar_float)
  : {
      include vec2 with value = scalar.t
      include vec_space_float with value = value with t = t
    } = {
  open mk_vspace2 scalar

  def norm (v: t) = scalar.sqrt (dot v v)
  def normalize (v: t) = v / norm v
}

module mk_vspace3 (scalar: scalar)
  : {
      include vec3 with value = scalar.t
      include vec_space with value = value with t = t
    } = {
  type value = scalar.t
  type t = {x: value, y: value, z: value}

  def map 'a (f: value -> a) (v: t) : {x: a, y: a, z: a} =
    let x = f v.x
    let y = f v.y
    let z = f v.z
    in {x, y, z}

  def map2 'a (f: value -> value -> a) (lhs: t) (rhs: t) : {x: a, y: a, z: a} =
    let x = f lhs.x rhs.x
    let y = f lhs.y rhs.y
    let z = f lhs.z rhs.z
    in {x, y, z}

  def neg = map (\r -> scalar.zero scalar.- r)
  def (+) = map2 (scalar.+)
  def (-) = map2 (scalar.-)
  def (*) (s: value) (v: t) = map (\e -> (scalar.*) s e) v
  def (/) (v: t) (s: value) = map (\e -> (scalar./) e s) v
  def (/.) (s: value) (v: t) = map (\e -> (scalar./) s e) v
  def hadamard = map2 (scalar.*)

  def foldl 'a (f: a -> value -> a) (acc: a) (v: t) : a =
    f (f (f acc v.x) v.y) v.z

  def foldr 'a (f: value -> a -> a) (acc: a) (v: t) : a =
    f v.x (f v.y (f v.z acc))

  def replicate (e: value) : t =
    {x = e, y = e, z = e}

  def to_tuple (v: t) = (v.x, v.y, v.z)
  def from_tuple ((x, y, z): (value, value, value)) = {x, y, z}
  def to_array (v: t) = [v.x, v.y, v.z]
  def from_array (a: [3]value) = {x = a[0], y = a[1], z = a[2]}

  def get (i: i64) (v: t) : value = (to_array v)[i]
  def set (i: i64) (x: value) (v: t) : t = from_array ([v.x, v.y, v.z] with [i] = x)

  def zero : t = replicate scalar.zero
  def one : t = replicate scalar.one

  def dot (lhs: t) (rhs: t) : value =
    foldl (scalar.+) scalar.zero (map2 (scalar.*) lhs rhs)

  def cross (lhs: t) (rhs: t) : t =
    let YZ = (scalar.-) ((scalar.*) lhs.y rhs.z) ((scalar.*) lhs.z rhs.y)
    let ZX = (scalar.-) ((scalar.*) lhs.z rhs.x) ((scalar.*) lhs.x rhs.z)
    let XY = (scalar.-) ((scalar.*) lhs.x rhs.y) ((scalar.*) lhs.y rhs.x)
    in {x = YZ, y = ZX, z = XY}
}

module mk_vspace3f (scalar: scalar_float)
  : {
      include vec3 with value = scalar.t
      include vec_space_float with value = value with t = t
    } = {
  open mk_vspace3 scalar

  def norm (v: t) = scalar.sqrt (dot v v)
  def normalize (v: t) = v / norm v
}

module mk_vspace4 (scalar: scalar)
  : {
      include vec4 with value = scalar.t
      include vec_space with value = value with t = t
    } = {
  type value = scalar.t
  type t = {x: value, y: value, z: value, w: value}

  def map 'a (f: value -> a) (v: t) : {x: a, y: a, z: a, w: a} =
    let x = f v.x
    let y = f v.y
    let z = f v.z
    let w = f v.w
    in {x, y, z, w}

  def map2 'a (f: value -> value -> a) (lhs: t) (rhs: t) : {x: a, y: a, z: a, w: a} =
    let x = f lhs.x rhs.x
    let y = f lhs.y rhs.y
    let z = f lhs.z rhs.z
    let w = f lhs.w rhs.w
    in {x, y, z, w}

  def neg = map (\r -> scalar.zero scalar.- r)
  def (+) = map2 (scalar.+)
  def (-) = map2 (scalar.-)
  def (*) (s: value) (v: t) = map (\e -> (scalar.*) s e) v
  def (/) (v: t) (s: value) = map (\e -> (scalar./) e s) v
  def (/.) (s: value) (v: t) = map (\e -> (scalar./) s e) v
  def hadamard = map2 (scalar.*)

  def foldl 'a (f: a -> value -> a) (acc: a) (v: t) : a =
    f (f (f (f acc v.x) v.y) v.z) v.w

  def foldr 'a (f: value -> a -> a) (acc: a) (v: t) : a =
    f v.x (f v.y (f v.z (f v.w acc)))

  def replicate (e: value) : t =
    {x = e, y = e, z = e, w = e}

  def to_tuple (v: t) = (v.x, v.y, v.z, v.w)
  def from_tuple ((x, y, z, w): (value, value, value, value)) = {x, y, z, w}
  def to_array (v: t) = [v.x, v.y, v.z, v.w]
  def from_array (a: [4]value) = {x = a[0], y = a[1], z = a[2], w = a[3]}

  def get (i: i64) (v: t) : value = (to_array v)[i]
  def set (i: i64) (x: value) (v: t) : t = from_array ([v.x, v.y, v.z, v.w] with [i] = x)

  def zero : t = replicate scalar.zero
  def one : t = replicate scalar.one

  def dot (lhs: t) (rhs: t) : value =
    foldl (scalar.+) scalar.zero (map2 (scalar.*) lhs rhs)
}

module mk_vspace4f (scalar: scalar_float)
  : {
      include vec4 with value = scalar.t
      include vec_space_float with value = value with t = t
    } = {
  open mk_vspace4 scalar

  def norm (v: t) = scalar.sqrt (dot v v)
  def normalize (v: t) = v / norm v
}

local
module i64_extended = {
  open i64
  def zero = 0i64
  def one = 1i64
}

local
module i32_extended = {
  open i32
  def zero = 0i32
  def one = 1i32
}

local
module f32_extended = {
  open f32
  def zero = 0f32
  def one = 1f32
}

module vec2i = mk_vspace2 i64_extended
module vec3i = mk_vspace3 i64_extended
module vec4i = mk_vspace4 i64_extended
module vec2i32 = mk_vspace2 i32_extended
module vec3i32 = mk_vspace3 i32_extended
module vec4i32 = mk_vspace4 i32_extended
module vec2f = mk_vspace2f f32_extended
module vec3f = mk_vspace3f f32_extended
module vec4f = mk_vspace4f f32_extended
