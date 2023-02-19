import "../lib/github.com/diku-dk/lys/lys"
import "../flowamok"

type cell = cell ()

type text_content = (i32, i32, i32)
module lys: lys with text_content = text_content = {
  type~ state =
    {h: i64, w: i64,
     gh: i64, gw: i64,
     scale: i64,
     rng: rng,
     time_unused: f32,
     running: bool,
     steps_per_second: i32,
     grid: [][]cell,
     n_steps_total: i64,
     n_leaks: i32}

  type text_content = text_content

  def text_format () = "FPS: %d\nSteps per second: %d\nLeaks: %d"

  def text_content (fps: f32) (s: state): text_content =
    (t32 fps, s.steps_per_second, s.n_leaks)

  def text_colour = const argb.white

  local def init_grid (gh: i64) (gw: i64) (rng: rng): (rng, [gh][gw]cell) =
    let (rng, grid) = create_grid gh gw () rng
    let grid = add_line_horizontal 25 0 10 1 grid
    let grid = add_line_horizontal 35 0 10 (-1) grid
    in (rng, grid)

  def init (seed: u32) (h: i64) (w: i64): state =
    let scale = 10
    let (gh, gw) = (h / scale, w / scale)
    let rng = rnge.rng_from_seed [i32.u32 seed]
    let (rng, grid) = init_grid gh gw rng
    in {h, w, gh, gw, scale, rng, time_unused=0,
        running=true, steps_per_second=30, grid,
        n_steps_total=0, n_leaks=0}

  def grab_mouse = false

  def resize (h: i64) (w: i64) (s: state): state =
    s with h = h
      with w = w
      with gh = h / s.scale
      with gw = w / s.scale
      with grid = s.grid -- FIXME: Extend grid when resized bigger, but don't
                         -- delete grid when resized smaller.

  local def new_frequency = 10i64

  def step (s: state) (n_steps: i64): state =
    let (gh, gw) = (s.gh, s.gw)
    let (rng, grid, n_steps_total, n_leaks) =
      loop (rng, grid, n_steps_total, n_leaks) =
        (s.rng, copy s.grid :> [gh][gw]cell, s.n_steps_total, 0)
      for _i < n_steps
      do let (rng, grid) =
           if n_steps_total % new_frequency == 0
           then let (rng, color) = random_color rng
                in (rng, add_cell 25 0 color {y=0, x=1} grid)
           else (rng, grid)
         let (grid, leaks) = stepper_quick.step gh gw choose_direction_random () grid
         in (rng, grid, n_steps_total + 1, n_leaks + i32.i64 (length leaks))
    in s with rng = rng
         with grid = grid
         with n_steps_total = n_steps_total
         with n_leaks = n_leaks

  local def step (td: f32) (s: state): state =
    if s.running
    then let time_total = td + s.time_unused
         let steps_new = time_total * f32.i32 s.steps_per_second
         let steps_new' = i64.f32 steps_new
         let time_unused = time_total - f32.i64 steps_new' / f32.i32 s.steps_per_second
         let s = if steps_new' > 0
                 then step s steps_new'
                 else s
         in s with time_unused = time_unused
    else s

  local def keydown (key: i32) (s: state): state =
    if key == SDLK_SPACE
    then s with running = !s.running
    else if key == SDLK_r
    then let (rng, grid) = init_grid s.gh s.gw s.rng
         in s with rng = rng
              with grid = grid
    else s

  local def mouse (buttons: i32) (x: i32) (y: i32) (s: state): state =
    let (y, x) = (i64.i32 y, i64.i32 x)
    in if y >= 0 && y < s.h && x >= 0 && x < s.w && buttons == 0b001
       then let (gy, gx) = (y / s.scale, x / s.scale)
            let grid = copy s.grid
            -- FIXME: Detect which direction the road is being constructed and
            -- determine the underlying flow direction.
            let cell' = copy grid[gy, gx] with underlying.direction.x = 1
            let grid[gy, gx] = cell'
            in s with grid = grid
       else s

  def event (e: event) (s: state): state =
    match e
    case #step td -> step td s
    case #keydown {key} -> keydown key s
    case #mouse {buttons, x, y} -> mouse buttons x y s
    case _ -> s

  def render (s: state): [][]argb.colour =
    let (gh, gw) = (s.gh, s.gw)
    in render s.h s.w gh gw s.scale (s.grid :> [gh][gw]cell)
}
