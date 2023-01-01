type available = { loc : int * int; mutable possible : int list }

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
(* available_grid je tabela available tipov, ki vsebuje vse mmožne številke na posameznem mestu *)
type state = { problem : Model.problem; current_grid : int option Model.grid; mutable available_list : available list}

let find_available (grid : int option Model.grid ) =
  let available a b cell available_list =
    match cell with
    | None -> available_list @ [{loc = a, b; possible = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]}]
    | Some n -> available_list
  in
  Model.foldi_grid available grid []

let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state

let initialize_state (problem : Model.problem) : state =
  { current_grid = Model.copy_grid problem.initial_grid; problem; available_list = find_available problem.initial_grid }

let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid
  in
  if unsolved then Unsolved state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = Model.map_grid Option.get state.current_grid in
    if Model.is_valid_solution state.problem solution then Solved solution
    else Fail state


let branch_state (state : state) : (state * state) option =
  (* TODO: Pripravite funkcijo, ki v trenutnem stanju poišče hipotezo, glede katere
     se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
     v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
     Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
     za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. : (state * state) option *)
  if state.available_list = [] then None
  else
  
  (* če je v prvi opciji samo ena možnost, sudoku posodobimo in iz available_lista izbrišemo prvo delitev, to nadaljujemo dokler ni možnosti delitve *)
  let remove_first list =
    match list with
    | x :: xs -> xs
    | lst -> failwith "Napaka"
  in
  let update_sudoku state loc n =
    let x, y = loc in
    state.current_grid.(x).(y) <- n;
    state.available_list <- remove_first (state.available_list);
    state
  in
  let one_option (state: state) =
    let avail = (List.nth (state.available_list) 0) in
    if List.length avail.possible = 1 then update_sudoku state avail.loc (Some (List.nth avail.possible 0))
    else state
  in

  (* Imamo dve možnosti, lahko razdelimo *)
  let find_1 (state: state) =
    let avail = (List.nth (state.available_list) 0) in
    let x, y = avail.loc in
    let n = Some (List.nth avail.possible 0) in
    let new_grid = Model.copy_grid state.current_grid in
    new_grid.(x).(y) <- n;
    {problem = state.problem; current_grid = new_grid; available_list = find_available new_grid}
  in
  let new_list (avail: available list) =
    let new_avail = avail in
    (List.nth new_avail 0).possible <- remove_first (List.nth new_avail 0).possible;
    new_avail
  in
  let find_2 (state: state) =
    {problem = state.problem; current_grid = state.current_grid; available_list = new_list state.available_list}
  in
  let branch (state: state) =
    if state.available_list = [] then None
    else
    let state_1 = find_1 state in
    let state_2 = find_2 state in
    Some (state_1, state_2)
  in
  (* združimo *)
  state |> one_option |> branch
      

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

let solve_problem (problem : Model.problem) =
  problem |> initialize_state |> solve_state
