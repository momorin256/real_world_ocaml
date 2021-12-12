module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module type Interval_intf = sig
  type t
  type endpoint
  val create : endpoint -> endpoint -> t
  val is_empty : t -> bool
  val contains : t -> endpoint -> bool
  val intersect : t -> t -> t
end

module type Interval_intf_wit_sexp = sig
  type t
  include Interval_intf with type t := t
  include Core.Sexpable with type t := t
end

module Make_interval(Endpoint :
  sig
    type t
    include Comparable with type t := t
    include Core.Sexpable with type t := t
  end)
  : (Interval_intf_wit_sexp with type endpoint := Endpoint.t) = struct
  type t =
    | Empty
    | Interval of Endpoint.t * Endpoint.t
  [@@deriving sexp]

  let create low high =
    if Endpoint.compare low high > 0 then Empty
    else Interval (low, high)

  let t_of_sexp sexp =
    match t_of_sexp sexp with
    | Empty -> Empty
    | Interval (l, h) -> create l h

  let is_empty = function
    | Empty -> true
    | Interval _ -> false

  let contains t x =
    match t with
    | Empty -> false 
    | Interval (l, h) ->
      (Endpoint.compare l x) <= 0 && 0 <= (Endpoint.compare x h)

  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match t1, t2 with
    | Empty, _ | _, Empty -> Empty
    | Interval (l1, h1), Interval (l2, h2) -> create (max l1 l2) (min h1 h2)
end

module Int_interval = Make_interval(Int)
