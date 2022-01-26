let rng (n:int) = 
    List.scan (fun n _ -> n * 1103515245 + 12345) 1 [1..n]

let x = rng 5
printf "%A" x