let imm_stack init = object
  val v = init

  method pop =
    match v with
    | [] -> None
    | hd :: tl -> Some (hd, {<v = tl>})

  method push hd =
    {<v = hd :: v>}
end
