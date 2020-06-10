entry mapscan [k] (n: i32) (xs: [k]i32)
               : [k]i32 =
  let m = k / n
  let xss' = unflatten m n xs
  let xss =
    map2 (\row i ->
            let res1 =
              loop row for _ in 0..<61 do
              let row' = map (+i) row
              in scan (+) 0 row'

            let res2 =
              loop row for _ in 0..<62 do
              let row' = map (+i) row
              in scan (+) 1 row'

            in map2 (+) res1 res2
         ) xss' (iota m)
  in flatten_to k xss
