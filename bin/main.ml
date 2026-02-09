open Camomile

let win1251_to_utf8 s =
  let enc_1251 = CharEncoding.of_name "CP1251" in
  let enc_utf8 = CharEncoding.of_name "UTF-8" in
  CharEncoding.recode_string ~in_enc:enc_1251 ~out_enc:enc_utf8 s


let char_of_byte b = win1251_to_utf8 (String.make 1 (Char.chr b))
let string_to_bytes s = List.init (String.length s) (fun i -> Char.code s.[i])

let bytes_to_string lst =
  String.init (List.length lst) (fun i -> Char.chr (List.nth lst i))


let most_frequent bytes =
  let freq = Hashtbl.create 65536 in
  let best = (0, 0), 0 in
  let rec aux best = function
    | a :: b :: rest ->
      let v =
        (try Hashtbl.find freq (a, b) with
         | Not_found -> 0)
        + 1
      in
      Hashtbl.replace freq (a, b) v;
      aux (if v > snd best then (a, b), v else best) (b :: rest)
    | _ -> best
  in
  aux best bytes


let string_of_table table =
  let rec aux n = function
    | [] -> ""
    | [ (fir, sec) ] -> Printf.sprintf "%d: (%d, %d)\n" n fir sec
    | (fir, sec) :: t -> Printf.sprintf "%d: (%d, %d)\n" n fir sec ^ aux (n + 1) t
  in
  aux 0 (List.rev table)


let replace (from : int * int) (to' : int) (bytes : int list) =
  let a, b = from in
  let rec aux out = function
    | [] -> List.rev out
    | x :: y :: t when x = a && y = b -> aux (to' :: out) t
    | h :: t -> aux (h :: out) t
  in
  aux [] bytes


let compress filename =
  let as_string = In_channel.with_open_bin filename In_channel.input_all in
  let bytes = string_to_bytes as_string in
  let rec create_pair_table = function
    | (255, -1) :: t -> (255, -1) :: t
    | prev :: table -> create_pair_table ((fst prev + 1, -1) :: prev :: table)
    | [] -> create_pair_table [ 0, -1 ]
  in
  let pair_table = create_pair_table [] in
  let len = List.length bytes in
  let rec loop text pair_table =
    let result = most_frequent text in
    let pair, frequency = result in
    Printf.printf
      "Most frequent pair: '%d' '%d' with frequency: %d. New index: %d\n"
      (fst pair)
      (snd pair)
      frequency
      (List.length pair_table);
    if frequency <= 2
    then (
      print_endline (string_of_table pair_table);
      Printf.printf
        "Old len: %d, New len: %d, New pairs: %d"
        len
        (List.length text)
        (256 - List.length pair_table);
      text)
    else loop (replace pair (List.length pair_table) text) (pair :: pair_table)
  in
  let _result = loop bytes pair_table in
  ()


let () =
  match Sys.argv with
  | [| program_name; filename |] -> compress filename
  | _ ->
    print_endline "Usage: bpeh filename";
    exit 64
