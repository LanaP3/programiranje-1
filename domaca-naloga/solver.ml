
(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
(* available_grid je tabela available tipov, ki vsebuje vse mmožne številke na posameznem mestu *)
type state = { problem : Model.problem; current_grid : int option Model.grid; available_grid : bool array Model.grid}

let update_available available_grid ( i : int ) ( j : int ) ( n : int ) =
  let box_i = 3 * ( i / 3 ) in
  let box_j = 3 * ( j / 3 ) in
  let rec aux k =
    match k with
    | 8 -> 
      available_grid.(k).(j).(n) <- false;
      available_grid.(i).(k).(n) <- false;
      available_grid.( box_i + 2 ).( box_j + 2 ).(n) <- false
    | k ->
      available_grid.(k).(j).(n) <- false;
      available_grid.(i).(k).(n) <- false;
      available_grid.( box_i + ( k / 3 )).( box_j + ( k mod 3 )).(n) <- false;
      aux ( k + 1 )
  in
  aux 0
  
let create_available (grid : int option Model.grid ) =
  let available_grid = Array.init 9 ( fun _ -> Array.init 9 ( fun _ -> ( Array.init 9 ( fun x -> true )))) 
  in
  let rec aux i j =
    match i, j with
    | i, j when ( i > 8 ) -> available_grid
    | i, j when ( j > 8 ) -> aux (i+1) 0
    | i, j -> 
      match grid.(i).(j) with
      | None -> aux i ( j + 1 )
      | Some n -> ( update_available available_grid i j ( n - 1 );
        aux i ( j + 1 ) )
  in aux 0 0


let copy_available available_grid =
  Array.map ( Array.map ( Array.map ( fun x -> x ))) available_grid
  
let print_state ( state : state ) : unit =
  Model.print_grid
    ( function None -> " " | Some digit -> string_of_int digit )
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state

let initialize_state ( problem : Model.problem ) : state =
  { problem = problem; current_grid = Model.copy_grid problem.initial_grid; available_grid = create_available problem.initial_grid }

let validate_state ( state : state ) : response =
  let unsolved =
    Array.exists ( Array.exists Option.is_none ) state.current_grid
  in
  if unsolved then Unsolved state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = Model.map_grid Option.get state.current_grid in
    if Model.is_valid_solution state.problem solution then Solved solution
    else Fail state

let branch_state ( state : state ) : ( state * state ) option =
  (* TODO: Pripravite funkcijo, ki v trenutnem stanju poišče hipotezo, glede katere
     se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
     v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
     Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
     za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. : (state * state) option *)
  
  (* vrne None, ce so vsa polja ze izpolnjena, sicer vrne lokacijo prvega praznega polja*)
  let find_cell ( state : state ) =
    let rec aux ( pair : int * int )=
      match pair with
      | i, j when ( i > 8 ) -> None
      | i, j when ( j = 8 ) -> 
        if state.current_grid.(i).(j) = None then Some( i, j )
        else aux ( ( i + 1 ), 0)
      | i, j -> 
        if state.current_grid.(i).(j) = None then Some( i, j )
        else aux  ( i, ( j + 1 ))
    in 
    aux ( 0, 0 )
  in

  (* preverjemo za st od danega mesta n naprej (brez n), ce je bilo to stevilo se edina moznost na danem mestu, vrnemo true *)
  let rec only_option ( state : state ) ( i : int ) ( j : int ) ( n : int ) =
    match n with
    | 8 -> true
    | n  when (state.available_grid.(i).(j).(n+1) = true) -> false 
    | n -> only_option state i j ( n + 1 )
  in

  let update ( state : state ) ( i : int ) ( j : int ) ( n : int ) =
    state.current_grid.(i).(j) <- Some ( n + 1 )
  in

  (* definiramo rekurzivno funkcijo, ki jo bomo klicali najprej in vedno, ko se posodobi kasna vrednost. V danem stanju izpolni vsa polja z eno samo opcijo *)
  let fill_single_options ( state : state ) =
    let rec option i j n =
      match n with
      | 8 -> Some 8
      | n when ( state.available_grid.(i).(j).(n) = true ) -> 
        ( match ( only_option state i j n ) with
        | true -> Some n
        | false -> None )
      | n -> option i j ( n + 1 )
    in 
    (* naredimo cikel, ce smo kasno polje spremenili, se bo ponovno klical, sicer koncamo *)
    let rec do_cycle ( cycle : bool ) =
      (* premaknemo se cez vsa polja, ko pridemo do zadnjega poklicemo cikel znova *)
      let rec aux ( cycle : bool ) ( i : int ) ( j : int ) =
        match i, j with
        | i, j when ( i > 8 ) -> do_cycle cycle
        | i, j when ( j > 8 ) -> aux cycle (i+1) 0
        | i, j -> (
          match state.current_grid.(i).(j) with
          | None -> (
            match option i j 0 with
              | None -> aux cycle i ( j + 1 )
              | Some n -> (
                update state i j n;
                update_available state.available_grid i j n ;
                aux true i ( j + 1 )
                )
            )
          | Some n ->
            ( aux cycle i ( j + 1 ) )
        )
      in
      if cycle = true then
        aux false 0 0
    in
    do_cycle true
  in

  let rec branch ( state : state ) ( i : int ) ( j : int ) =
    let rec aux n =
      match n with
      | 9 -> Some ( state, state )
      | n when ( state.available_grid.(i).(j).(n) = false ) -> aux ( n + 1 )
      | n ->
        (* stevilo je se zadnja moznost na tem mestu *)
        if only_option state i j n then
          ( update state i j n;
          update_available state.available_grid i j n;
          fill_single_options state;
          match find_cell state with
          | None -> Some (state, state)
          | Some ( i, j ) -> branch state i j )
        else
          let grid_1 = Model.copy_grid state.current_grid in
          let available_1 = copy_available state.available_grid in
          let available_2 = copy_available state.available_grid in

          update_available available_1 i j n;
          grid_1.(i).(j) <- Some ( n + 1 );
          available_2.(i).(j).(n) <- false;

          let state_1 = { problem = state.problem; current_grid = grid_1; available_grid = available_1 } in 
          let state_2 = { problem = state.problem; current_grid = state.current_grid; available_grid = available_2 } in
          fill_single_options state_1;
          Some ( state_1, state_2 )
    in 
    aux 0

  in
  (* zdruzimo *)
  match find_cell state with
  | None -> None
  | Some ( x, y ) -> ( fill_single_options state; branch state x y )

(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state ( state : state ) =
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
