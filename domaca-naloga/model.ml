(* Pomožni tip, ki predstavlja mrežo *)

type 'a grid = 'a Array.t Array.t

(* Funkcije za prikaz mreže.
   Te definiramo najprej, da si lahko z njimi pomagamo pri iskanju napak. *)

(* Razbije seznam [lst] v seznam seznamov dolžine [size] *)
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

let string_of_list string_of_element sep lst =
  lst |> List.map string_of_element |> String.concat sep

let string_of_nested_list string_of_element inner_sep outer_sep =
  string_of_list (string_of_list string_of_element inner_sep) outer_sep

let string_of_row string_of_cell row =
  let string_of_cells =
    row |> Array.to_list |> chunkify 3
    |> string_of_nested_list string_of_cell "" "│"
  in
  "┃" ^ string_of_cells ^ "┃\n"

let print_grid string_of_cell grid =
  let ln = "───" in
  let big = "━━━" in
  let divider = "┠" ^ ln ^ "┼" ^ ln ^ "┼" ^ ln ^ "┨\n" in
  let row_blocks =
    grid |> Array.to_list |> chunkify 3
    |> string_of_nested_list (string_of_row string_of_cell) "" divider
  in
  Printf.printf "┏%s┯%s┯%s┓\n" big big big;
  Printf.printf "%s" row_blocks;
  Printf.printf "┗%s┷%s┷%s┛\n" big big big

(* Funkcije za dostopanje do elementov mreže *)

let get_row (grid : 'a grid) (row_ind : int) = 
  Array.init 9 (fun col_ind -> grid.(row_ind).(col_ind))

let rows grid = List.init 9 (get_row grid)

let get_column (grid : 'a grid) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind))

let columns grid = List.init 9 (get_column grid)

let get_box (grid : 'a grid) (box_ind : int) = 
  let row_start = 3 * (box_ind / 3) in 
  let column_start = 3 * (box_ind mod 3) in
  let box_list = List.init 3 (fun col_ind -> Array.init 3 (fun row_ind -> grid.(row_ind + row_start).(col_ind + column_start))) in
  Array.concat box_list

let boxes grid = List.init 9 (get_box grid)

(* Funkcije za ustvarjanje novih mrež *)

let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid = 
  Array.map (Array.map f) grid

let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid

let foldi_grid (f : int -> int -> 'a -> 'acc -> 'acc) (grid : 'a grid)
    (acc : 'acc) : 'acc =
  let acc, _ =
    Array.fold_left
      (fun (acc, row_ind) row ->
        let acc, _ =
          Array.fold_left
            (fun (acc, col_ind) cell ->
              (f row_ind col_ind cell acc, col_ind + 1))
            (acc, 0) row
        in
        (acc, row_ind + 1))
      (acc, 0) grid
  in
  acc

let row_of_string cell_of_char str =
  List.init (String.length str) (String.get str) |> List.filter_map cell_of_char

let grid_of_string cell_of_char str =
  let grid =
    str |> String.split_on_char '\n'
    |> List.map (row_of_string cell_of_char)
    |> List.filter (function [] -> false | _ -> true)
    |> List.map Array.of_list |> Array.of_list
  in
  if Array.length grid <> 9 then failwith "Nepravilno število vrstic";
  if Array.exists (fun x -> x <> 9) (Array.map Array.length grid) then
    failwith "Nepravilno število stolpcev";
  grid

(* Model za vhodne probleme *)

type problem = { initial_grid : int option grid }

let print_problem problem : unit =
  let string_of_cell cell =
    match cell with
    | None -> " "
    | Some n -> string_of_int n
  in
  print_grid string_of_cell problem.initial_grid

let problem_of_string str =
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0'))
    | _ -> None
  in
  { initial_grid = grid_of_string cell_of_char str }

(* Model za izhodne rešitve *)

type solution = int grid

let print_solution (solution: solution) = 
  print_grid string_of_int solution

(* Je veljaven, torej ali vsaka vrstica, stolpec, škatla vsebuje vse številke? *)
let check_valid (solution: solution) =
  let sudoku = rows solution @ columns solution @ boxes solution
  in
  let check n =
    List.fold_left (&&) true (List.map (Array.exists ((=)(n+1))) sudoku)
  in
  List.fold_left (&&) true (List.init 9 check)
    
(* Ali pripada pravemu sudokuju? *)
let correct_problem (problem: problem) (solution: solution) =
  let compare (a: int option) (b: int) =
    match a with
    | None -> true
    | Some a -> a = b
  in
  let check n =
    Array.fold_left (&&) true (Array.map2 compare problem.initial_grid.(n) solution.(n))
  in
  List.fold_left (&&) true (List.init 9 check)

let is_valid_solution (problem: problem) (solution: solution) =
  (check_valid solution) && (correct_problem problem solution)
