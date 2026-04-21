-- | segmented radix sort implementation
--
-- based on the following with some tweaks and fixes:
-- https://github.com/KristianMH/fun-with-futhark/blob/main/segmented/radix_sort/segmented_radix_sort.fut
-- https://github.com/diku-dk/sorts/blob/master/lib/github.com/diku-dk/sorts/radix_sort.fut
--
-- note: zero counts *will* result in bugs. ensure to filter them before passing them on

import "../../../diku-dk/segmented/segmented"

local
def pairwise3 op (a1, b1, c1) (a2, b2, c2) =
  (a1 `op` a2, b1 `op` b2, c1 `op` c2)

local
def pairwise4 op (a1, b1, c1, d1) (a2, b2, c2, d2) =
  (a1 `op` a2, b1 `op` b2, c1 `op` c2, d1 `op` d2)

local
def segmented_radix_sort_step [n] 't
                              (segment_flags: [n]bool)
                              (segment_offsets: [n]i64)
                              (segment_idxs: [n]i64)
                              (xs: [n]t)
                              (get_bit: i32 -> t -> i32)
                              (digit_n: i32) : [n]t =
  let num x = i8.i32 <| ((get_bit (digit_n + 1) x) << 1) | get_bit digit_n x
  let bins = map num xs
  let flags =
    bins
    |> map (\x ->
              ( x == 0b00
              , x == 0b01
              , x == 0b10
              , x == 0b11
              ))
  let flags' =
    map num xs
    |> map (\x ->
              ( i64.bool (x == 0b00)
              , i64.bool (x == 0b01)
              , i64.bool (x == 0b10)
              ))
  let offsets =
    flags
    |> map (\(a, b, c, d) -> (i64.bool a, i64.bool b, i64.bool c, i64.bool d))
    |> segmented_scan (pairwise4 (+)) (0, 0, 0, 0) segment_flags
  let (na, nb, nc) =
    segmented_reduce (pairwise3 (+)) (0, 0, 0) segment_flags flags'
    |> unzip3
  let f bin (a, b, c, d) offset idx =
    offset
    + (-1)
    + a * (i64.bool (bin == 0b00))
    + na[idx] * (i64.bool (bin > 0b00))
    + b * (i64.bool (bin == 0b01))
    + nb[idx] * (i64.bool (bin > 0b01))
    + c * (i64.bool (bin == 0b10))
    + nc[idx] * (i64.bool (bin > 0b10))
    + d * (i64.bool (bin == 0b11))
  let is = map4 f bins offsets segment_offsets segment_idxs
  in scatter (#[scratch] copy xs) is xs

def segmented_radix_sort [n] [m] 't
                         (num_bits: i32)
                         (get_bit: i32 -> t -> i32)
                         (seg_counts: [m]i64)
                         (xs: [n]t) : [n]t =
  let seg_offsets = exscan (+) 0 seg_counts
  let segment_flags = spread n false seg_offsets (map (const true) seg_counts)
  let segment_idxs = map i64.bool segment_flags |> scan (+) 0 |> map (\x -> x - 1)
  let segment_offsets = map (\i -> seg_offsets[i]) segment_idxs
  let iters = if n == 0 then 0 else (num_bits + 2 - 1) / 2
  in loop xs for i < iters do
       segmented_radix_sort_step segment_flags segment_offsets segment_idxs xs get_bit (i * 2)

local
def with_indices [n] 'a (xs: [n]a) : [n](a, i64) =
  zip xs (iota n)

local
def by_key_wrapper [n] 't sorter key num_bits get_bit seg_counts (xs: [n]t) : [n]t =
  map key xs
  |> with_indices
  |> sorter num_bits (\i (k, _) -> get_bit i k) seg_counts
  |> map (\(_, i: i64) -> xs[i])

def segmented_radix_sort_by_key [n] [m] 't 'k
                                (key: t -> k)
                                (num_bits: i32)
                                (get_bit: i32 -> k -> i32)
                                (seg_counts: [m]i64)
                                (xs: [n]t) : [n]t =
  by_key_wrapper segmented_radix_sort key num_bits get_bit seg_counts xs

def segmented_radix_sort_int [n] [m] 't
                             (num_bits: i32)
                             (get_bit: i32 -> t -> i32)
                             (seg_counts: [m]i64)
                             (xs: [n]t) : [n]t =
  let get_bit' i x =
    let b = get_bit i x
    in if i == num_bits - 1 then b ^ 1 else b
  in segmented_radix_sort num_bits get_bit' seg_counts xs

def segmented_radix_sort_int_by_key [n] [m] 't 'k
                                    (key: t -> k)
                                    (num_bits: i32)
                                    (get_bit: i32 -> k -> i32)
                                    (seg_counts: [m]i64)
                                    (xs: [n]t) : [n]t =
  by_key_wrapper segmented_radix_sort_int key num_bits get_bit seg_counts xs

def segmented_radix_sort_float [n] [m] 't
                               (num_bits: i32)
                               (get_bit: i32 -> t -> i32)
                               (seg_counts: [m]i64)
                               (xs: [n]t) : [n]t =
  let get_bit' i x =
    let b = get_bit i x
    in if get_bit (num_bits - 1) x == 1 || i == num_bits - 1
       then b ^ 1
       else b
  in segmented_radix_sort num_bits get_bit' seg_counts xs

def segmented_radix_sort_float_by_key [n] [m] 't 'k
                                      (key: t -> k)
                                      (num_bits: i32)
                                      (get_bit: i32 -> k -> i32)
                                      (seg_counts: [m]i64)
                                      (xs: [n]t) : [n]t =
  by_key_wrapper segmented_radix_sort_float key num_bits get_bit seg_counts xs
