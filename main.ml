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
    fun inp -> Option.some (v, inp);;

let zero: 'a parser =
    fun _inp -> Option.none;;

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

let orO (o1: 'a option) (o2: 'b option) =
    match (o1, o2) with
    | (None, None) -> None
    | (Some(v1), None) -> Some(v1)
    | (None, Some(v2)) -> Some(v2)
    | (Some(v1), Some(v2)) -> Some(v1);;

let (<|>) (p1: 'a parser) (p2: 'b parser) =
    fun inp ->
        orO (p1 inp) (p2 inp);;

let item: char parser =
    fun inp ->
        match (inp |> string_to_chars) with
        | [] -> Option.none
        | (x :: xs) ->
                let rest = xs |> chars_to_string in
                Option.some (x, rest);;

let charParser (c: char): char parser =
    item >>= fun x ->
        if x == c then return x else zero;;

let stringParser (s: string): string parser =
    let rec stringParser' (s: string) (acc: char list) =
        match (s |> string_to_chars) with
        | [] -> return (acc |> List.rev |> chars_to_string)
        | (x :: xs) -> charParser x >>= fun y ->
                            stringParser' (xs |> chars_to_string) (y::acc)
    in stringParser' s [];;

let nullParser: json_value parser =
    (fun _ -> Json_null) <$> stringParser "null";;

let boolParser: json_value parser =
    let f = function
        | "true" -> Json_bool(true)
        | "false" -> Json_bool(false)
        | _ -> failwith "Exception occur"
    in
    f <$> (stringParser "true" <|> stringParser "false");;

let jsonParser: json_value parser = nullParser <|> boolParser;;

let print_json (v: (json_value * string) option): unit =
    match v with
    | None -> print_endline "None"
    | Some(v) -> match v with
        | (Json_null, str) -> Printf.printf "(%s, [\"%s\"])\n" "Json_null" str
        | (Json_bool(b), str) -> Printf.printf "(Json_bool(%s), [\"%s\"])\n" (b |> Bool.to_string) str
        | _ -> failwith "not implemented yet";;

let () =
    jsonParser "true" |> print_json;;





