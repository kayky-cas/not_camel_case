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

  let move t max_col max_row =
    let new_head = Direction.move t.direction (List.hd t.body) in
    t.body <-
      (Math.mod_ (fst new_head) max_col, Math.mod_ (snd new_head) max_row)
      :: t.body;
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

  let move t = Snake.move t.snake t.cols t.rows

  let verify_colision t =
    let snake = t.snake in

    let body = List.tl snake.body in
    let head = List.hd snake.body in

    if List.mem head body then Snake
    else if Some head = t.fruit then Fruit
    else None

  let read_player_input t =
    if Raylib.is_key_pressed Raylib.Key.W then
      Snake.change_direction t.snake Direction.Up
    else if Raylib.is_key_pressed Raylib.Key.S then
      Snake.change_direction t.snake Direction.Down
    else if Raylib.is_key_pressed Raylib.Key.A then
      Snake.change_direction t.snake Direction.Left
    else if Raylib.is_key_pressed Raylib.Key.D then
      Snake.change_direction t.snake Direction.Right

  let render t =
    let width = Raylib.get_render_width () in
    let height = Raylib.get_render_height () in

    let cell_width = width / 2 / t.cols in
    let cell_height = height / 2 / t.rows in

    let draw_cell x y color =
      Raylib.draw_rectangle (x * cell_width) (y * cell_height) cell_width
        cell_height color
    in

    for x = 0 to t.cols - 1 do
      for y = 0 to t.rows - 1 do
        if List.mem (x, y) t.snake.body then draw_cell x y Raylib.Color.green
        else if Some (x, y) = t.fruit then draw_cell x y Raylib.Color.red
      done
    done

  let play t =
    (match verify_colision t with
    | Snake -> failwith "Game Over"
    | Fruit ->
        Snake.grow t.snake;
        t.fruit <- None
    | None -> ());

    (match t.fruit with
    | None ->
        let x = Random.int t.cols in
        let y = Random.int t.rows in
        t.fruit <- Some (x, y)
    | Some _ -> ());

    read_player_input t;
    move t;
    render t
end

let setup () =
  Raylib.init_window 800 800 "Snake";
  Raylib.set_target_fps 30

let rec loop b =
  if Raylib.window_should_close () then Raylib.close_window ()
  else (
    Raylib.begin_drawing ();
    Raylib.clear_background Raylib.Color.black;

    Board.play b;

    Raylib.end_drawing ());
  loop b

let () =
  setup ();
  loop (Board.create 20 20)
