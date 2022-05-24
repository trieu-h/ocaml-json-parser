(* Util *)
let chars_to_string (chars: char list): string =
    chars
    |> List.map Char.escaped
    |> List.fold_left (^) "";;

let string_to_chars (str: string): char list =
    str
    |> String.to_seq
    |> List.of_seq;;

(* Parser implentation *)
type 'a parser = string -> ('a * string) option;;

type json_value =
    | Json_null
    | Json_string of string
    | Json_number of int
    | Json_bool of bool
    | Json_array of json_value list;;

let return (v: 'a): 'a parser =
    fun inp -> Some(v, inp);;

let zero: 'a parser =
    fun _inp -> None;;

let (>>=) (p: 'a parser) (f: 'a -> 'b parser): 'b parser =
    fun inp ->
        match (p inp) with
        | None -> None
        | Some((x, rest)) -> f x rest;;

let (<$>) (f: 'a -> 'b) (p: 'a parser): 'b parser =
    fun inp ->
        match (p inp) with
        | None -> None
        | Some((x, rest)) -> Some((f x, rest));;

let or_o (o1: 'a option) (o2: 'b option) =
    match (o1, o2) with
    | (None, None) -> None
    | (Some(v1), None) -> Some(v1)
    | (None, Some(v2)) -> Some(v2)
    | (Some(v1), Some(v2)) -> Some(v1);;

let (<|>) (p1: 'a parser) (p2: 'b parser) =
    fun inp ->
        or_o (p1 inp) (p2 inp);;

let (>>) (p1: 'a parser) (p2: 'b parser) =
    p1 >>= fun x ->
        p2 >>= fun y ->
            return y;;

let (<<) (p1: 'a parser) (p2: 'b parser) =
    p1 >>= fun x ->
        p2 >>= fun y ->
            return x;;

let item: char parser =
    fun inp ->
        match (inp |> string_to_chars) with
        | [] -> None
        | x :: xs -> let rest = xs |> chars_to_string in Some(x, rest);;

let sat (p: char -> bool): char parser =
    item >>= fun x ->
        if p x then return x else zero;;

let char_parser (c: char): char parser =
    sat (fun x -> x = c);;

let is_digit = fun x -> x >= '0' && x <= '9';;

let parse_while (p: char -> bool): string parser =
    fun inp ->
        let rec apply_pred p acc = function
            | [] -> Some (acc |> List.rev |> chars_to_string , "")
            | x :: xs -> let parsed = acc |> List.rev |> chars_to_string  in
                         let rest = (x :: xs) |> chars_to_string in
                         if p x then apply_pred p (x :: acc) xs else Some(parsed, rest)
        in apply_pred p [] (inp |> string_to_chars);;

let number_parser: string parser =
    (parse_while is_digit) >>= fun x ->
        if x = "" then zero else return x;;

let rec json_number: json_value parser =
    let f = fun x -> Json_number(x) in
    f <$> (int_of_string <$> number_parser);;

let rec char_list_parser (s: string): char list parser =
    match (s |> string_to_chars) with
    | [] -> return []
    | x :: xs -> char_parser x >>= fun _ ->
                 char_list_parser (xs |> chars_to_string) >>= fun _ ->
                 return (x :: xs);;

let string_parser (s: string): string parser =
    chars_to_string <$> (char_list_parser s);;

let json_null: json_value parser =
    (fun _ -> Json_null) <$> string_parser "null";;

let json_bool: json_value parser =
    let f = function
        | "true" -> Json_bool(true)
        | "false" -> Json_bool(false)
        | _ -> failwith "Exception occur"
    in
    f <$> (string_parser "true" <|> string_parser "false");;

let string_literal = parse_while (fun x -> x <> '"');;

let json_string: json_value parser =
    (fun s -> Json_string(s)) <$> (char_parser '"' >> string_literal << char_parser '"');;

let json_parser: json_value parser =
    json_null <|> json_bool <|> json_number <|> json_string;;

let print_json (v: (json_value * string) option): unit =
    match v with
    | None -> print_endline "None"
    | Some(v) -> match v with
        | (Json_null, rest) -> Printf.printf "(%s, [\"%s\"])\n" "Json_null" rest
        | (Json_bool(b), rest) -> Printf.printf "(Json_bool(%s), [\"%s\"])\n" (b |> Bool.to_string) rest
        | (Json_number(n), rest) -> Printf.printf "(Json_number(%s), [\"%s\"])\n" (n |> Int.to_string) rest
        | (Json_string(s), rest) -> Printf.printf "(Json_string(%s), [\"%s\"])\n" s rest
        | _ -> failwith "not implemented yet";;

let () =
    json_parser "123abc" |> print_json;;

let () =
    json_parser "falsetrueabc" |> print_json;;

let () =
    json_parser "nullabc" |> print_json;;

let () =
    json_parser "" |> print_json;;

let () =
    json_parser "\"abc\"" |> print_json;;



