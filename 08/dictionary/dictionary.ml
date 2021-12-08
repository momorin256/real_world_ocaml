open Base

type ('a, 'b) t =
{
  mutable length: int;
  buckets: ('a * 'b) list array;
  hash: 'a -> int;
  equal: 'a -> 'a -> bool;
}

let buckets_count = 17

let hash_bucket t key = (t.hash key) % buckets_count

let create ~hash ~equal =
{
  length = 0;
  buckets = Array.create ~len:buckets_count [];
  hash;
  equal;
}

let length t = t.length

let find t key =
  List.find_map t.buckets.(hash_bucket t key)
    ~f:(fun (key', data) -> if t.equal key' key then Some data else None)

let iter t ~f =
  for i = 0 to Array.length t.buckets -1 do
    List.iter t.buckets.(i) ~f:(fun (key, data) -> f ~key ~data)
  done

let bucket_has_key t i key =
  List.exists t.buckets.(i) ~f:(fun (key', _) -> t.equal key' key)

let add t ~key ~data =
  let i = hash_bucket t key in
  let replace = bucket_has_key t i key in
  let filtered_bucket =
    if replace then
      List.filter t.buckets.(i) ~f:(fun (key', _) -> not (t.equal key' key))
    else
      t.buckets.(i)
  in
  t.buckets.(i) <- (key, data) :: filtered_bucket;
  if not replace then t.length <- t.length + 1

let remove t key =
  let i = hash_bucket t key in
  if bucket_has_key t i key then (
    t.buckets.(i)
      <- List.filter t.buckets.(i) ~f:(fun (key', _) -> not (t.equal key' key));
    t.length <- t.length - 1;
  )

let result =
  let dict = create ~hash:(fun s -> String.length s) ~equal:(String.(=)) in
  add dict ~key:"pen" ~data:120;
  add dict ~key:"phone" ~data:3400;
  add dict ~key:"example" ~data:17;
  add dict ~key:"example" ~data:10;
  add dict ~key:"structure" ~data:260;
  add dict ~key:"type" ~data:970;
  remove dict "phone";
  let test1 = Option.is_some (find dict "pen") in
  let test2 = Option.is_none (find dict "not found") in
  let test3 = match find dict "example" with
    | Some data -> data = 10
    | None -> false
  in [test1; test2; test3]