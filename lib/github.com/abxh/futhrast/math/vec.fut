-- | tiny vector implementation
--
-- Inspiration:
-- github.com/athas/vector
--
-- exposes:
-- module vec2i = mk_vspace2 i64
-- module vec3i = mk_vspace3 i64
-- module vec4i = mk_vspace4 i64
-- module vec2f = mk_vspace2f f32
-- module vec3f = mk_vspace3f f32
-- module vec4f = mk_vspace4f f32

local
module type VSpaceSpec = {
  -- | the vector type
  type t

  -- | the vector value type
  type value_t

  -- | zero vector
  val zero : t

  -- | one vector
  val one : t

  -- | element-wise addition
  val (+) : t -> t -> t

  -- | element-wise subtraction
  val (-) : t -> t -> t

  -- | element-wise scaling
  val (*) : value_t -> t -> t

  -- | element-wise inverse scaling
  val (/) : t -> value_t -> t

  -- | element-wise inverse scaled by some scalar
  val (/.) : value_t -> t -> t

  -- | element-wise multiplication (hadamard product)
  val hadamard : t -> t -> t

  -- | dot product
  val dot : t -> t -> value_t

  -- | perform a left fold over vector elements
  val foldl 'a : (a -> value_t -> a) -> a -> t -> a

  -- | perform a right fold over vector elements
  val foldr 'a : (value_t -> a -> a) -> a -> t -> a

  -- | vector from a increasing index passed to function, starting from 0.
  val iota : (i64 -> value_t) -> t

  -- | vector from replicated value
  val replicate : value_t -> t

  -- | get the element at some position
  val get : i64 -> t -> value_t

  -- | set the element at some position
  val set : i64 -> value_t -> t -> t
}

local
module type FVSpaceSpec = {
  include VSpaceSpec

  -- | vector norm
  val norm : t -> value_t

  -- | normalize vector
  val normalize : t -> t
}

local
module type ScalarSpec = {
  --- | the scalar type
  type t

  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val (*) : t -> t -> t
  val (/) : t -> t -> t

  val i32 : i32 -> t
}

local
module type FScalarSpec = {
  include ScalarSpec

  val sqrt : t -> t
}

local
module type VSpaceSpec2 = {
  type value_t

  include VSpaceSpec
  with value_t = value_t
  with t = {x: value_t, y: value_t}

  -- | "2d" cross product (area of parallelogram spanned by the two vectors)
  val cross : t -> t -> value_t

  -- | map a vector component-wise
  val map 'a : (value_t -> a) -> t -> {x: a, y: a}

  -- | map two vectors component-wise
  val map2 'a : (value_t -> value_t -> a) -> t -> t -> {x: a, y: a}

  -- convert to array
  val to_array : t -> [2]value_t

  -- convert from array
  val from_array : [2]value_t -> t

  -- convert to tuple
  val to_tuple : t -> (value_t, value_t)

  -- convert from tuple
  val from_tuple : (value_t, value_t) -> t
}

local
module type FVSpaceSpec2 = {
  type value_t

  include FVSpaceSpec
  with value_t = value_t
  with t = {x: value_t, y: value_t}

  -- | map a vector component-wise
  val map 'a : (value_t -> a) -> t -> {x: a, y: a}

  -- | map two vectors component-wise
  val map2 'a : (value_t -> value_t -> a) -> t -> t -> {x: a, y: a}

  -- | "2d" cross product (area of parallelogram spanned by the two vectors)
  val cross : t -> t -> value_t

  -- convert to array
  val to_array : t -> [2]value_t

  -- convert from array
  val from_array : [2]value_t -> t

  -- convert to tuple
  val to_tuple : t -> (value_t, value_t)

  -- convert from tuple
  val from_tuple : (value_t, value_t) -> t
}

local
module type VSpaceSpec3 = {
  type value_t

  include VSpaceSpec
  with value_t = value_t
  with t = {x: value_t, y: value_t, z: value_t}

  -- | map a vector component-wise
  val map 'a : (value_t -> a) -> t -> {x: a, y: a, z: a}

  -- | map two vectors component-wise
  val map2 'a : (value_t -> value_t -> a) -> t -> t -> {x: a, y: a, z: a}

  -- convert to array
  val to_array : t -> [3]value_t

  -- convert from array
  val from_array : [3]value_t -> t

  -- | cross product
  val cross : t -> t -> t

  -- convert to tuple
  val to_tuple : t -> (value_t, value_t, value_t)

  -- convert from tuple
  val from_tuple : (value_t, value_t, value_t) -> t
}

local
module type FVSpaceSpec3 = {
  type value_t

  include VSpaceSpec
  with value_t = value_t
  with t = {x: value_t, y: value_t, z: value_t}

  -- | map a vector component-wise
  val map 'a : (value_t -> a) -> t -> {x: a, y: a, z: a}

  -- | map two vectors component-wise
  val map2 'a : (value_t -> value_t -> a) -> t -> t -> {x: a, y: a, z: a}

  -- convert to array
  val to_array : t -> [3]value_t

  -- convert from array
  val from_array : [3]value_t -> t

  -- | cross product
  val cross : t -> t -> t

  -- convert to tuple
  val to_tuple : t -> (value_t, value_t, value_t)

  -- convert from tuple
  val from_tuple : (value_t, value_t, value_t) -> t
}

local
module type VSpaceSpec4 = {
  type value_t

  include VSpaceSpec
  with value_t = value_t
  with t = {x: value_t, y: value_t, z: value_t, w: value_t}

  -- | map a vector component-wise
  val map 'a : (value_t -> a) -> t -> {x: a, y: a, z: a, w: a}

  -- | map two vectors component-wise
  val map2 'a : (value_t -> value_t -> a) -> t -> t -> {x: a, y: a, z: a, w: a}

  -- convert to array
  val to_array : t -> [4]value_t

  -- convert from array
  val from_array : [4]value_t -> t

  -- convert to tuple
  val to_tuple : t -> (value_t, value_t, value_t, value_t)

  -- convert from tuple
  val from_tuple : (value_t, value_t, value_t, value_t) -> t
}

local
module type FVSpaceSpec4 = {
  type value_t

  include VSpaceSpec
  with value_t = value_t
  with t = {x: value_t, y: value_t, z: value_t, w: value_t}

  -- | map a vector component-wise
  val map 'a : (value_t -> a) -> t -> {x: a, y: a, z: a, w: a}

  -- | map two vectors component-wise
  val map2 'a : (value_t -> value_t -> a) -> t -> t -> {x: a, y: a, z: a, w: a}

  -- convert to array
  val to_array : t -> [4]value_t

  -- convert from array
  val from_array : [4]value_t -> t

  -- convert to tuple
  val to_tuple : t -> (value_t, value_t, value_t, value_t)

  -- convert from tuple
  val from_tuple : (value_t, value_t, value_t, value_t) -> t
}

module mk_vspace2 (scalar: ScalarSpec) : VSpaceSpec2 with value_t = scalar.t = {
  type value_t = scalar.t
  type t = {x: value_t, y: value_t}

  def map 'a (f: value_t -> a) (v: t) : {x: a, y: a} =
    let x = f v.x
    let y = f v.y
    in {x, y}

  def map2 'a (f: value_t -> value_t -> a) (lhs: t) (rhs: t) : {x: a, y: a} =
    let x = f lhs.x rhs.x
    let y = f lhs.y rhs.y
    in {x, y}

  def (+) = map2 (scalar.+)
  def (-) = map2 (scalar.-)
  def (*) (s: value_t) (v: t) = map (\e -> (scalar.*) s e) v
  def (/) (v: t) (s: value_t) = map (\e -> (scalar./) e s) v
  def (/.) (s: value_t) (v: t) = map (\e -> (scalar./) s e) v
  def hadamard = map2 (scalar.*)

  def foldl 'a (f: a -> value_t -> a) (acc: a) (v: t) : a =
    f (f acc v.x) v.y

  def foldr 'a (f: value_t -> a -> a) (acc: a) (v: t) : a =
    f v.x (f v.y acc)

  def iota (f: i64 -> value_t) : t =
    {x = f 0, y = f 1}

  def replicate (e: value_t) : t =
    {x = e, y = e}

  def to_tuple (v: t) = (v.x, v.y)
  def from_tuple ((x, y): (value_t, value_t)) = {x, y}
  def to_array (v: t) = [v.x, v.y]
  def from_array (a: [2]value_t) = {x = a[0], y = a[1]}

  def get (i: i64) (v: t) : value_t = (to_array v)[i]
  def set (i: i64) (x: value_t) (v: t) : t = from_array ([v.x, v.y] with [i] = x)

  def zero : t = replicate (scalar.i32 0)
  def one : t = replicate (scalar.i32 1)

  def dot (lhs: t) (rhs: t) : value_t =
    foldl (scalar.+) (scalar.i32 0) (map2 (scalar.*) lhs rhs)

  def cross (lhs: t) (rhs: t) : value_t =
    let XY = (scalar.-) ((scalar.*) lhs.x rhs.y) ((scalar.*) rhs.x lhs.y)
    in XY
}

module mk_vspace2f (scalar: FScalarSpec) : FVSpaceSpec2 with value_t = scalar.t = {
  open mk_vspace2 scalar

  def norm (v: t) = scalar.sqrt (dot v v)
  def normalize (v: t) = v / norm v
}

module mk_vspace3 (scalar: ScalarSpec) : VSpaceSpec3 with value_t = scalar.t = {
  type value_t = scalar.t
  type t = {x: value_t, y: value_t, z: value_t}

  def map 'a (f: value_t -> a) (v: t) : {x: a, y: a, z: a} =
    let x = f v.x
    let y = f v.y
    let z = f v.z
    in {x, y, z}

  def map2 'a (f: value_t -> value_t -> a) (lhs: t) (rhs: t) : {x: a, y: a, z: a} =
    let x = f lhs.x rhs.x
    let y = f lhs.y rhs.y
    let z = f lhs.z rhs.z
    in {x, y, z}

  def (+) = map2 (scalar.+)
  def (-) = map2 (scalar.-)
  def (*) (s: value_t) (v: t) = map (\e -> (scalar.*) s e) v
  def (/) (v: t) (s: value_t) = map (\e -> (scalar./) e s) v
  def (/.) (s: value_t) (v: t) = map (\e -> (scalar./) s e) v
  def hadamard = map2 (scalar.*)

  def foldl 'a (f: a -> value_t -> a) (acc: a) (v: t) : a =
    f (f (f acc v.x) v.y) v.z

  def foldr 'a (f: value_t -> a -> a) (acc: a) (v: t) : a =
    f v.x (f v.y (f v.z acc))

  def iota (f: i64 -> value_t) : t =
    {x = f 0, y = f 1, z = f 2}

  def replicate (e: value_t) : t =
    {x = e, y = e, z = e}

  def to_tuple (v: t) = (v.x, v.y, v.z)
  def from_tuple ((x, y, z): (value_t, value_t, value_t)) = {x, y, z}
  def to_array (v: t) = [v.x, v.y, v.z]
  def from_array (a: [3]value_t) = {x = a[0], y = a[1], z = a[2]}

  def get (i: i64) (v: t) : value_t = (to_array v)[i]
  def set (i: i64) (x: value_t) (v: t) : t = from_array ([v.x, v.y, v.z] with [i] = x)

  def zero : t = replicate (scalar.i32 0)
  def one : t = replicate (scalar.i32 1)

  def dot (lhs: t) (rhs: t) : value_t =
    foldl (scalar.+) (scalar.i32 0) (map2 (scalar.*) lhs rhs)

  def cross (lhs: t) (rhs: t) : t =
    let YZ = (scalar.-) ((scalar.*) lhs.y rhs.z) ((scalar.*) lhs.z rhs.y)
    let ZX = (scalar.-) ((scalar.*) lhs.z rhs.x) ((scalar.*) lhs.x rhs.z)
    let XY = (scalar.-) ((scalar.*) lhs.x rhs.y) ((scalar.*) lhs.y rhs.x)
    in {x = YZ, y = ZX, z = XY}
}

module mk_vspace3f (scalar: FScalarSpec) : FVSpaceSpec3 with value_t = scalar.t = {
  open mk_vspace3 scalar

  def norm (v: t) = scalar.sqrt (dot v v)
  def normalize (v: t) = v / norm v
}

module mk_vspace4 (scalar: ScalarSpec) : VSpaceSpec4 with value_t = scalar.t = {
  type value_t = scalar.t
  type t = {x: value_t, y: value_t, z: value_t, w: value_t}

  def map 'a (f: value_t -> a) (v: t) : {x: a, y: a, z: a, w: a} =
    let x = f v.x
    let y = f v.y
    let z = f v.z
    let w = f v.w
    in {x, y, z, w}

  def map2 'a (f: value_t -> value_t -> a) (lhs: t) (rhs: t) : {x: a, y: a, z: a, w: a} =
    let x = f lhs.x rhs.x
    let y = f lhs.y rhs.y
    let z = f lhs.z rhs.z
    let w = f lhs.w rhs.w
    in {x, y, z, w}

  def (+) = map2 (scalar.+)
  def (-) = map2 (scalar.-)
  def (*) (s: value_t) (v: t) = map (\e -> (scalar.*) s e) v
  def (/) (v: t) (s: value_t) = map (\e -> (scalar./) e s) v
  def (/.) (s: value_t) (v: t) = map (\e -> (scalar./) s e) v
  def hadamard = map2 (scalar.*)

  def foldl 'a (f: a -> value_t -> a) (acc: a) (v: t) : a =
    f (f (f (f acc v.x) v.y) v.z) v.w

  def foldr 'a (f: value_t -> a -> a) (acc: a) (v: t) : a =
    f v.x (f v.y (f v.z (f v.w acc)))

  def iota (f: i64 -> value_t) : t =
    {x = f 0, y = f 1, z = f 2, w = f 3}

  def replicate (e: value_t) : t =
    {x = e, y = e, z = e, w = e}

  def to_tuple (v: t) = (v.x, v.y, v.z, v.w)
  def from_tuple ((x, y, z, w): (value_t, value_t, value_t, value_t)) = {x, y, z, w}
  def to_array (v: t) = [v.x, v.y, v.z, v.w]
  def from_array (a: [4]value_t) = {x = a[0], y = a[1], z = a[2], w = a[3]}

  def get (i: i64) (v: t) : value_t = (to_array v)[i]
  def set (i: i64) (x: value_t) (v: t) : t = from_array ([v.x, v.y, v.z, v.w] with [i] = x)

  def zero : t = replicate (scalar.i32 0)
  def one : t = replicate (scalar.i32 1)

  def dot (lhs: t) (rhs: t) : value_t =
    foldl (scalar.+) (scalar.i32 0) (map2 (scalar.*) lhs rhs)
}

module mk_vspace4f (scalar: FScalarSpec) : FVSpaceSpec4 with value_t = scalar.t = {
  open mk_vspace4 scalar

  def norm (v: t) = scalar.sqrt (dot v v)
  def normalize (v: t) = v / norm v
}

module vec2i = mk_vspace2 i64
module vec3i = mk_vspace3 i64
module vec4i = mk_vspace4 i64
module vec2f = mk_vspace2f f32
module vec3f = mk_vspace3f f32
module vec4f = mk_vspace4f f32
