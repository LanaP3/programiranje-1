
(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
(* available_grid je tabela available tipov, ki vsebuje vse mmožne številke na posameznem mestu *)
type state = { problem : Model.problem; current_grid : int option Model.grid; available_grid : bool array Model.grid}

(*let find_available (grid : int option Model.grid ) =
  let rec update c n array =
    match c with
    | c when c < 9 && c <> n -> array.(c) <- false; update (c+1) n array
    | c when c < 9 -> update (c+1) n array
    | c -> array
  in
  let available n =
    match n with
    | None -> Array.make 9 true
    | Some n -> update 0 n (Array.make 9 true)
  in
  Model.map_grid available grid*)
let find_available (grid : int option Model.grid ) =
  let cell = Array.make 9 true in
  let row = Array.make 9 cell in
  let available_grid = Array.make 9 row in 
  available_grid

let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state

let initialize_state (problem : Model.problem) : state =
  { current_grid = Model.copy_grid problem.initial_grid; problem; available_grid = find_available problem.initial_grid }

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
  
  (* vrne None, če so vsa polja ze izpolnjena, sicer vrne lokacijo prvega praznega polja*)
  let find_cell ( state : state ) =
    let rec aux ( pair : int*int)=
      match pair with
      | i, j when i>8 -> None
      | i, j when j=8 -> if state.current_grid.(i).(j) = None then Some(i,j)
        else aux ((i+1), 0)
      | i, j -> if state.current_grid.(i).(j) = None then Some(i,j)
        else aux (i,(j+1))
    in 
    aux (0, 0)
  in

  (* Ce najdemo mozno stevilo na dani lokaciji, vzamemo najmanjse in ga spremenimo na false v kopiji available_grid, current grid pa kopiramo in vrnemo dve stanji
    v prvem je vpisano st., v drugem je current_grid isti *)
  let branch ( i : int )  ( j : int ) : (state * state) option =
    let rec aux n =
      match n with
      | 9 -> None
      | n when state.available_grid.(i).(j).(n) = true -> 
        let grid_1 = Model.copy_grid state.current_grid in
        let available_2 = Model.copy_grid state.available_grid in
        grid_1.(i).(j) <- Some (n+1);
        available_2.(i).(j).(n) <- false;
        print_state { problem = state.problem; current_grid = grid_1; available_grid = state.available_grid };

        Some ({ problem = state.problem; current_grid = grid_1; available_grid = state.available_grid },
        { problem = state.problem; current_grid = state.current_grid; available_grid = available_2 })
      | n -> aux (n+1)
    in 
    aux 0
    
  (* združimo *)
  in
  match find_cell state with
  | None -> None
  | Some (x,y) -> branch x y  

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
