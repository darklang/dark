let log ?(f : 'a -> 'b = fun x -> Obj.magic x) (msg : string) (data : 'a) : 'a
    =
  Js.log2 msg (f data) ;
  data


let loG ?(f : 'a -> 'b = fun x -> Obj.magic x) (msg : string) (data : 'a) :
    unit =
  Js.log2 msg (f data)
