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

let get_row (grid : 'a grid) (row_ind : int) = failwith "TODO"

let rows grid = failwith "TODO"

let get_column (grid : 'a grid) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind))

let columns grid = List.init 9 (get_column grid)

let get_box (grid : 'a grid) (box_ind : int) = failwith "TODO"

let boxes grid = failwith "TODO"

(* Funkcije za ustvarjanje novih mrež *)

let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid = failwith "TODO"

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
  print_grid (
    fun c -> match c with 
      | None -> " " 
      | Some i -> string_of_int i
  ) problem

let problem_of_string str =
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0'))
    | _ -> None
  in
  { initial_grid = grid_of_string cell_of_char str }

(* Model za izhodne rešitve *)

type solution = int grid

let print_solution solution = failwith "TODO"

let is_valid_solution problem solution = failwith "TODO"


type available = { loc : int * int; possible : int list }

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = { problem : problem; current_grid : int option grid }

let print_state (state : state) : unit =
  print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of solution | Unsolved of state | Fail of state

let initialize_state (problem : problem) : state =
  { current_grid = copy_grid problem.initial_grid; problem }

let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid
  in
  if unsolved then Unsolved state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = map_grid Option.get state.current_grid in
    if is_valid_solution state.problem solution then Solved solution
    else Fail state

let branch_state (state : state) : (state * state) option =
  (* TODO: Pripravite funkcijo, ki v trenutnem stanju poišče hipotezo, glede katere
     se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
     v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
     Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
     za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. *)
  failwith "TODO"

(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  (* TODO: na tej točki je stanje smiselno počistiti in zožiti možne rešitve *)
  match validate_state state with
  | Solved solution ->
      (* če smo našli rešitev, končamo *)
      Some solution
  | Fail fail ->
      (* prav tako končamo, če smo odkrili, da rešitev ni *)
      None
  | Unsolved state' ->
      (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
      explore_state state'

and explore_state (state : state) =
  (* pri raziskovanju najprej pogledamo, ali lahko trenutno stanje razvejimo *)
  match branch_state state with
  | None ->
      (* če stanja ne moremo razvejiti, ga ne moremo raziskati *)
      None
  | Some (st1, st2) -> (
      (* če stanje lahko razvejimo na dve možnosti, poizkusimo prvo *)
      match solve_state st1 with
      | Some solution ->
          (* če prva možnost vodi do rešitve, do nje vodi tudi prvotno stanje *)
          Some solution
      | None ->
          (* če prva možnost ne vodi do rešitve, raziščemo še drugo možnost *)
          solve_state st2 )

let solve_problem (problem : problem) =
  problem |> initialize_state |> solve_state


let read_problem filename =
  let channel = open_in filename in
  let str = really_input_string channel (in_channel_length channel) in
  close_in channel;
  problem_of_string str
  
let find_solution problem =
  let before = Sys.time () in
  let solution = solve_problem problem in
  let after = Sys.time () in
  let elapsed_time = after -. before in
  (solution, elapsed_time)
  
let display_solution = function
  | Some solution ->
      Printf.printf "Končna rešitev:\n";
      print_solution solution
  | None -> Printf.printf "Rešitev ne obstaja.\n"
  
let find_and_display_solution (problem : problem) =
  Printf.printf "Rešujem:\n";
  print_problem problem.problem_grid;
  Printf.printf "\n%!";
  let response, elapsed_time = find_solution problem in
  display_solution response;
  Printf.printf "Čas reševanja: %f s.\n%!" elapsed_time
  
let () =
    (* Če se program sesuje, nam to izpiše klicni sklad. *)
  Printexc.record_backtrace true;
    (* Tabela sistemskih argumentov vsebuje ime klicanega programa ter argumente, ki mu sledijo *)
  Sys.argv
    (* Tabelo pretvorimo v seznam *)
  |> Array.to_list
    (* Odstranimo prvi element (ime klicanega programa), da dobimo seznam imen datotek *)
  |> List.tl
    (* Iz vsake datoteke preberemo problem *)
  |> List.map read_problem
    (* Probleme zaporedoma rešimo *)
  |> List.iter find_and_display_solution
  
  (* Če domačo nalogo rešujete prek spletnega vmesnika, ki ne podpira branja datotek,
     lahko delovanje preizkušate prek spodnjega programa. *)
  
let () = "
  ┏━━━┯━━━┯━━━┓
  ┃483│921│657┃
  ┃967│3 5│821┃
  ┃251│876│493┃
  ┠───┼───┼───┨
  ┃548│132│976┃
  ┃729│ 64│ 38┃
  ┃136│798│ 45┃
  ┠───┼───┼───┨
  ┃372│689│514┃
  ┃814│253│769┃
  ┃695│417│382┃
  ┗━━━┷━━━┷━━━┛"
         |> problem_of_string
         |> find_and_display_solution
  
