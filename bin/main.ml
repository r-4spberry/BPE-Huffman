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


let replace_pair (from : int * int) (to' : int) (bytes : int list) =
  let a, b = from in
  let rec aux out = function
    | [] -> List.rev out
    | x :: y :: t when x = a && y = b -> aux (to' :: out) t
    | h :: t -> aux (h :: out) t
  in
  aux [] bytes


let replace_to_pair (from : int) (to' : int * int) (bytes : int list) =
  let a, b = to' in
  let rec aux out = function
    | [] -> List.rev out
    | x :: t when x = from -> aux (b :: a :: out) t
    | h :: t -> aux (h :: out) t
  in
  aux [] bytes


let compress filename min_freq =
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
      "Most frequent pair: '%d' '%d' with frequency: %d. New index: %d\n%!"
      (fst pair)
      (snd pair)
      frequency
      (List.length pair_table);
    if frequency <= min_freq
    then (
      print_endline (string_of_table pair_table);
      Printf.printf
        "Old len: %d, New len: %d, New pairs: %d\n%!"
        len
        (List.length text)
        (List.length pair_table - 256);
      text, pair_table)
    else loop (replace_pair pair (List.length pair_table) text) (pair :: pair_table)
  in
  loop bytes pair_table


let decompress bytes pair_table =
  let rec aux bytes pair_table n =
    match pair_table with
    | (255, -1) :: t -> bytes
    | h :: t -> aux (replace_to_pair n h bytes) t (n - 1)
    | _ -> assert false
  in
  aux bytes pair_table (List.length pair_table - 1)
 
let write_file file s =
  Out_channel.with_open_bin file (fun oc ->
    Out_channel.output_string oc s
  )

let () =
  let filename, min_freq = match Sys.argv with
  | [| program_name; filename |] ->
    filename, 2
  | [| program_name; filename; min_freq_str |] ->
    let min_freq = int_of_string min_freq_str in
    filename, min_freq
  | _ ->
    print_endline "Usage: bpeh filename [min_frequency]";
    exit 64
  in 
  let compressed_text, pair_table = compress filename 2 in
  let out = decompress compressed_text pair_table in
  write_file "result.txt" (bytes_to_string out)
