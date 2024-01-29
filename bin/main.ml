[@@@ocaml.warning "-32-37"]

module Math = struct
  let mod_ x y = match x mod y with x when x < 0 -> x + y | x -> x
end

module Direction = struct
  type t = Up | Down | Left | Right

  let move t (x, y) =
    match t with
    | Up -> (x, y - 1)
    | Down -> (x, y + 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)
end

module Snake = struct
  type t = { mutable body : (int * int) list; mutable direction : Direction.t }

  let create () = { body = [ (0, 0) ]; direction = Direction.Right }

  let grow t =
    let new_head = Direction.move t.direction (List.hd t.body) in
    t.body <- new_head :: t.body

  let move t =
    let new_head = Direction.move t.direction (List.hd t.body) in
    t.body <- new_head :: t.body;
    t.body <- List.rev (List.tl (List.rev t.body))

  let change_direction t direction = t.direction <- direction
end

module Board = struct
  type t = {
    cols : int;
    rows : int;
    snake : Snake.t;
    mutable fruit : (int * int) option;
  }

  let create cols rows =
    { cols; rows; snake = Snake.create (); fruit = Some (4, 0) }

  type colision = Snake | Fruit | None

  let move t = Snake.move t.snake

  let verify_colision t =
    let snake = t.snake in

    let body = List.tl snake.body in
    let head = List.hd snake.body in

    if List.mem head body then Snake
    else if Some head = t.fruit then Fruit
    else None

  let change_direction t direction = Snake.change_direction t.snake direction

  let read_player_input t =
    (* TODO: remove blocking and needs to press enter *)
    let input = input_char stdin in
    match input with
    | 'w' -> change_direction t Direction.Up
    | 's' -> change_direction t Direction.Down
    | 'a' -> change_direction t Direction.Left
    | 'd' -> change_direction t Direction.Right
    | _ -> ()

  let play t =
    read_player_input t;
    move t;
    (match verify_colision t with
    | Snake -> failwith "Game Over"
    | Fruit ->
        Snake.grow t.snake;
        t.fruit <- None
    | None -> ());

    match t.fruit with
    | None ->
        let x = Random.int t.cols in
        let y = Random.int t.rows in
        t.fruit <- Some (x, y)
    | Some _ -> ()

  let print t =
    let snake = t.snake in
    for i = 0 to t.rows - 1 do
      for j = 0 to t.cols - 1 do
        if
          List.mem (j, i)
            (List.map
               (fun (x, y) -> (Math.mod_ x t.cols, Math.mod_ y t.rows))
               snake.body)
        then print_string " O "
        else if Some (j, i) = t.fruit then print_string " X "
        else print_string " · "
      done;
      print_newline ()
    done;
    Printf.printf "\027[%dA" t.rows
end

let () =
  print_string "\027[?25l";
  let board = ref (Board.create 10 10) in
  while true do
    Board.print !board;
    Board.play !board;
    Unix.sleepf 1.
  done;

  print_newline ()
