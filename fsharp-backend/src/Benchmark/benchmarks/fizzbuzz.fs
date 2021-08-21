(List.range_v0 1 200)
|> List.map_v0
     (fun v ->
       if (v % 15 = 0) then "FizzBuzz"
       elif (v % 3 = 0) then "Fizz"
       elif (v % 5 = 0) then "Buzz"
       else toString v)
