(List.range_v0 1 100)
|> List.map_v0
     (fun v ->
       if (v % 15 = 0) then "FizzBuzz"
       else if (v % 3 = 0) then "Fizz"
       else if (v % 5 = 0) then "Buzz"
       else toString v)
