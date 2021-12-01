open Core

type service_info =
{
  service_name : string;
  port : int;
  protocol : string;
  comment : string option;
}
[@@deriving fields]

let service_info_of_string line =
  let (line, comment) =
    match String.rsplit2 line ~on:'#' with
    | None -> (line, None)
    | Some (ordinary, comment) -> (ordinary, Some comment)
  in
  let matches =
    let pat = "([a-zA-Z]+)[ \t]+([0-9]+)/([a-zA-Z]+)" in
    Re.exec (Re.Posix.compile_pat pat) line
  in
  {
    service_name = Re.Group.get matches 1;
    port = Int.of_string (Re.Group.get matches 2);
    protocol = Re.Group.get matches 3;
    comment = comment;
  }

type 'a with_line_num =
{
  item : 'a;
  line_num : int
}

let parse_lines parse contens =
  let lines = String.split ~on:'\n' contens in
  List.mapi lines ~f:(fun index line ->
    {
      item = parse line;
      line_num = index + 1;
    }
  )

let show_field field to_string record =
  let name = Field.name field in
  let field_str = to_string (Field.get field record) in
  name ^ ": " ^ field_str

let print_service_info service =
  let print to_string field =
    printf "%s\n" (show_field field to_string service)
  in
  Fields_of_service_info.iter
    ~service_name:(print Fn.id)
    ~port:(print Int.to_string)
    ~protocol:(print Fn.id)
    ~comment:(print (fun x -> match x with None -> "" | Some v -> v))
