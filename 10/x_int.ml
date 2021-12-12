module type X_int = sig
  val x : int
end

let to_int m =
  let module M = (val m : X_int) in
  M.x

let to_int (module M : X_int) = M.x

let plus m1 m2 =
  (module struct
      let x = to_int m1 + to_int m2
    end : X_int)

module type Bumpable = sig
  type t
  val bump : t -> t
end

module Int_bumper = struct
  type t = int
  let bump n = n + 1
end

module Float_bumper = struct
  type t = float
  let bump n = n +. 1.0
end

let bump_list (type a) (module Bumper : Bumpable with type t = a) (l : a list) =
  List.map Bumper.bump l

(* let bump_list2 (module Bumper : Bumpable) (l : Bumper.t list) =
  List.map Bumper.bump l *)

let result =
  let int_bumper = (module Int_bumper : Bumpable with type t = int) in
  (* let (module Bumper) = int_bumper in Bumper.bump 3 in *)
  bump_list int_bumper [1; 2; 3]