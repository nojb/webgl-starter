open Brr
open Brr_canvas

let compile_shader gl type_ src =
  let shader = Gl.create_shader gl type_ in
  Gl.shader_source gl shader (Jstr.v src);
  Gl.compile_shader gl shader;
  let success = Gl.get_shader_parameter gl shader Gl.compile_status |> Jv.to_bool in
  if success then
    Ok shader
  else
    let info_log = Gl.get_shader_info_log gl shader |> Jstr.to_string in
    Error (Printf.sprintf "Failed to compile shader!\n%s" info_log)

let link_program gl shaders =
  let program = Gl.create_program gl in
  List.iter (Gl.attach_shader gl program) shaders;
  Gl.link_program gl program;
  let success = Gl.get_program_parameter gl program Gl.link_status |> Jv.to_bool in
  if success then
    Ok program
  else
    let info_log = Gl.get_program_info_log gl program |> Jstr.to_string in
    Error (Printf.sprintf "Failed to link program!\n%s" info_log)

let compile_and_link_program gl vertex_shader_source fragment_shader_source =
  match compile_shader gl Gl.vertex_shader vertex_shader_source with
  | Error _ as err -> err
  | Ok vertex_shader ->
    match compile_shader gl Gl.fragment_shader fragment_shader_source with
    | Error _ as err ->
      Gl.delete_shader gl vertex_shader;
      err
    | Ok fragment_shader ->
      let prg = link_program gl [vertex_shader; fragment_shader] in
      Gl.delete_shader gl vertex_shader;
      Gl.delete_shader gl fragment_shader;
      prg

module Vec4 : sig
  type t = float * float * float * float
  val to_float32_array: t -> Tarray.float32
end = struct
  type t = float * float * float * float
  let to_float32_array (x, y, z, w) = Tarray.of_float_array Float32 [|x; y; z; w|]
end

module Vec3 : sig
  type t = float * float * float
  val zero: t
  val scale: float -> t -> t
  val length: t -> float
  val normalize: t -> t
  val dot: t -> t -> float
  val cross: t -> t -> t
  val add: t -> t -> t
  val sub: t -> t -> t
end = struct
  type t = float * float * float

  let zero = (0., 0., 0.)

  let add (x1, y1, z1) (x2, y2, z2) =
    (x1 +. x2, y1 +. y2, z1 +. z2)

  let sub (x1, y1, z1) (x2, y2, z2) =
    (x1 -. x2, y1 -. y2, z1 -. z2)

  let dot (x1, y1, z1) (x2, y2, z2) =
    x1 *. x2 +. y1 *. y2 +. z1 *. z2

  let cross (x1, y1, z1) (x2, y2, z2) =
    (y1 *. z2 -. z1 *. y2, z1 *. x2 -. x1 *. z2, x1 *. y2 -. y1 *. x2)

  let scale a (x, y, z) =
    (a *. x, a *. y, a *. z)

  let length v =
    Float.sqrt (dot v v)

  let normalize v =
    scale (1. /. length v) v
end

module Mat4 : sig
  type t
  val to_float32_array: t -> Tarray.float32
  val rotation_y: float -> t
  val mul: t -> t -> t
  val look_at: Vec3.t -> Vec3.t -> Vec3.t -> t
  val perspective: float -> float -> float -> float -> t
end = struct
  type t =
    {
      m11: float; m12: float; m13: float; m14: float;
      m21: float; m22: float; m23: float; m24: float;
      m31: float; m32: float; m33: float; m34: float;
      m41: float; m42: float; m43: float; m44: float;
    }

  let to_float32_array {m11; m12; m13; m14; m21; m22; m23; m24; m31; m32; m33; m34; m41; m42; m43; m44} =
    Tarray.of_float_array Float32 [|m11; m21; m31; m41; m12; m22; m32; m42; m13; m23; m33; m43; m14; m24; m34; m44|]

  let zero =
    {m11 = 0.; m12 = 0.; m13 = 0.; m14 = 0.;
     m21 = 0.; m22 = 0.; m23 = 0.; m24 = 0.;
     m31 = 0.; m32 = 0.; m33 = 0.; m34 = 0.;
     m41 = 0.; m42 = 0.; m43 = 0.; m44 = 0.}

  let identity =
    {zero with m11 = 1.; m22 = 1.; m33 = 1.; m44 = 1.}

  let mul
      {m11 = a11; m12 = a12; m13 = a13; m14 = a14;
       m21 = a21; m22 = a22; m23 = a23; m24 = a24;
       m31 = a31; m32 = a32; m33 = a33; m34 = a34;
       m41 = a41; m42 = a42; m43 = a43; m44 = a44}
      {m11 = b11; m12 = b12; m13 = b13; m14 = b14;
       m21 = b21; m22 = b22; m23 = b23; m24 = b24;
       m31 = b31; m32 = b32; m33 = b33; m34 = b34;
       m41 = b41; m42 = b42; m43 = b43; m44 = b44}
    =
    {m11 = a11 *. b11 +. a12 *. b21 +. a13 *. b31 +. a14 *. b41;
     m12 = a11 *. b12 +. a12 *. b22 +. a13 *. b32 +. a14 *. b42;
     m13 = a11 *. b13 +. a12 *. b23 +. a13 *. b33 +. a14 *. b43;
     m14 = a11 *. b14 +. a12 *. b24 +. a13 *. b34 +. a14 *. b44;
     m21 = a21 *. b11 +. a22 *. b21 +. a23 *. b31 +. a24 *. b41;
     m22 = a21 *. b12 +. a22 *. b22 +. a23 *. b32 +. a24 *. b42;
     m23 = a21 *. b13 +. a22 *. b23 +. a23 *. b33 +. a24 *. b43;
     m24 = a21 *. b14 +. a22 *. b24 +. a23 *. b34 +. a24 *. b44;
     m31 = a31 *. b11 +. a32 *. b21 +. a33 *. b31 +. a34 *. b41;
     m32 = a31 *. b12 +. a32 *. b22 +. a33 *. b32 +. a34 *. b42;
     m33 = a31 *. b13 +. a32 *. b23 +. a33 *. b33 +. a34 *. b43;
     m34 = a31 *. b14 +. a32 *. b24 +. a33 *. b34 +. a34 *. b44;
     m41 = a41 *. b11 +. a42 *. b21 +. a43 *. b31 +. a44 *. b41;
     m42 = a41 *. b12 +. a42 *. b22 +. a43 *. b32 +. a44 *. b42;
     m43 = a41 *. b13 +. a42 *. b23 +. a43 *. b33 +. a44 *. b43;
     m44 = a41 *. b14 +. a42 *. b24 +. a43 *. b34 +. a44 *. b44}

  let rotation_y angle =
    let c = Float.cos angle and s = Float.sin angle in
    {identity with m11 = c; m31 = -.s; m13 = s; m33 = c}

  let look_at eye target up =
    let (fx, fy, fz) as forward = Vec3.normalize (Vec3.sub eye target) in
    let (rx, ry, rz) as right = Vec3.normalize (Vec3.cross up forward) in
    let (ux, uy, uz) as up = Vec3.cross forward right in
    let tx = Vec3.dot eye right in
    let ty = Vec3.dot eye up in
    let tz = Vec3.dot eye forward in
    {identity with
     m11 = rx; m12 = ry; m13 = rz;
     m21 = ux; m22 = uy; m23 = uz;
     m31 = fx; m32 = fy; m33 = fz;
     m14 = -.tx; m24 = -.ty; m34 = -.tz}

  let perspective fov aspect near far =
    let f = Float.tan (Float.pi *. 0.5 -. 0.5 *. fov) in
    let range_inv = 1. /. (near -. far) in
    {zero with
     m11 = f /. aspect;
     m22 = f;
     m33 =  (near +. far) *. range_inv;
     m43 = -1.;
     m34 = near *. far *. range_inv *. 2.}
end

let vertices =
  [|
    -0.5; 0.5; -0.5;  (* 0 front top left *)
    0.5; 0.5; -0.5;   (* 1 front top right *)
    0.5; -0.5; -0.5;  (* 2 front bottom right *)
    -0.5; -0.5; -0.5; (* 3 front bottom left *)
    -0.5; 0.5; 0.5;   (* 4 back top left *)
    0.5; 0.5; 0.5;    (* 5 back top right *)
    0.5; -0.5; 0.5;   (* 6 back bottom right *)
    -0.5; -0.5; 0.5;  (* 7 back bottom left *)
  |]

let indices =
  [| 0; 1; 1; 2; 2; 3; 3; 0;
     4; 5; 5; 6; 6; 7; 7; 4;
     0; 4; 1; 5; 2; 6; 3; 7 |]

let init_mesh gl prg =
  let vbo = Gl.create_buffer gl in
  let ebo = Gl.create_buffer gl in
  Gl.bind_buffer gl Gl.array_buffer (Some vbo);
  Gl.buffer_data gl Gl.array_buffer (Tarray.of_float_array Float32 vertices) Gl.static_draw;
  Gl.bind_buffer gl Gl.element_array_buffer (Some ebo);
  Gl.buffer_data gl Gl.element_array_buffer (Tarray.of_int_array Uint8 indices) Gl.static_draw;
  let pos_loc = Gl.get_attrib_location gl prg (Jstr.v "pos") in
  Gl.vertex_attrib_pointer gl pos_loc 3 Gl.float false 0 0;
  Gl.enable_vertex_attrib_array gl pos_loc

type camera =
  {
    pos: Vec3.t;
    front: Vec3.t;
    up: Vec3.t;
    yaw: float;
    pitch: float;
  }

let look_at {pos; front; up} =
  Mat4.look_at pos (Vec3.add pos front) up

let draw_mesh gl view_loc view =
  Gl.clear gl Gl.color_buffer_bit;
  Gl.uniform_matrix4fv gl view_loc false (Mat4.to_float32_array view);
  Gl.draw_elements gl Gl.lines (Array.length indices) Gl.unsigned_byte 0

let get_key =
  let handler f ev =
    let code = Jstr.to_string (Ev.Keyboard.code (Ev.as_type ev)) in
    match code with
    | "KeyA" | "KeyW" | "KeyD" | "KeyS" | "Space" | "ShiftLeft" -> Ev.prevent_default ev; f code
    | _ -> ()
  in
  let h = Hashtbl.create 0 in
  let onkeydown code = Hashtbl.replace h code () in
  let onkeyup code = Hashtbl.remove h code in
  ignore (Ev.listen Ev.keydown (handler onkeydown) (Window.as_target G.window));
  ignore (Ev.listen Ev.keyup (handler onkeyup) (Window.as_target G.window));
  Hashtbl.mem h

let onmousemove, get_moved =
  let locked = ref false in
  let onpointerlockchange ev = locked := Option.is_some (Document.pointer_lock_element G.document) in
  ignore (Ev.listen Ev.pointerlockchange onpointerlockchange (Document.as_target G.document));
  let moved_x = ref 0. in
  let moved_y = ref 0. in
  let onmousemove ev =
    if !locked then begin
      let ev = Ev.as_type ev in
      moved_x := Ev.Mouse.movement_x ev +. !moved_x;
      moved_y := Ev.Mouse.movement_y ev +. !moved_y
    end
  in
  onmousemove,
  fun () ->
    let mx = !moved_x and my = !moved_y in
    moved_x := 0.; moved_y := 0.;
    mx, my

let process_input delta {pos; front; up; yaw; pitch} =
  let speed = 1.5 *. delta in
  let pos = if get_key "KeyW" then Vec3.add pos (Vec3.scale speed front) else pos in
  let pos = if get_key "KeyS" then Vec3.sub pos (Vec3.scale speed front) else pos in
  let pos = if get_key "KeyA" then Vec3.sub pos (Vec3.scale speed (Vec3.cross front up)) else pos in
  let pos = if get_key "KeyD" then Vec3.add pos (Vec3.scale speed (Vec3.cross front up)) else pos in
  let pos = if get_key "Space" then Vec3.add pos (Vec3.scale speed up) else pos in
  let pos = if get_key "ShiftLeft" then Vec3.sub pos (Vec3.scale speed up) else pos in
  let dx, dy = get_moved () in
  let sensitivity = 0.001 in
  let yaw = yaw +. sensitivity *. dx and pitch = pitch -. sensitivity *. dy in
  let pitch =
    let limit = Float.pi *. 0.5 -. 0.1 in
    let neg_limit = -.limit in
    if pitch > limit then limit else if pitch < neg_limit then neg_limit else pitch
  in
  let front = (Float.cos yaw *. Float.cos pitch, Float.sin pitch, Float.sin yaw *. Float.cos pitch) in
  {pos; front; up; yaw; pitch}

let run gl prg width height =
  Gl.use_program gl prg;
  Gl.clear_color gl 1. 1. 1. 1.;
  init_mesh gl prg;
  let view_loc = Gl.get_uniform_location gl prg (Jstr.v "view") in
  let proj_loc = Gl.get_uniform_location gl prg (Jstr.v "proj") in
  let proj = Mat4.perspective (Float.pi *. 0.25) (width /. height) 0.1 1000. in
  Gl.uniform_matrix4fv gl proj_loc false (Mat4.to_float32_array proj);
  let rec loop camera angle prev_now now =
    let delta = (now -. prev_now) *. 0.001 in (* seconds *)
    (* Update FPS *)
    Document.set_title G.document (Printf.ksprintf Jstr.v "%.0f" (1. /. delta));
    let angle = angle +. 0.5 *. delta in
    (* Update the camera position *)
    let camera = process_input delta camera in
    (* Render the scene *)
    let view = Mat4.mul (look_at camera) (Mat4.rotation_y angle) in
    draw_mesh gl view_loc view;
    (* Loop *)
    ignore (G.request_animation_frame (loop camera angle now))
  in
  let camera =
    {
      pos = (0., 0., -5.);
      front = (0., 0., 5.);
      up = (0., 1., 0.);
      yaw = Float.pi *. 0.5;
      pitch = 0.;
    }
  in
  ignore (G.request_animation_frame (loop camera 0. 0.))

let vertex_shader_source = {glsl|#version 300 es
in vec3 pos;
uniform mat4 view;
uniform mat4 proj;
void main() {
  gl_Position = proj * view * vec4(pos, 1.0);
}
|glsl}

let fragment_shader_source = {glsl|#version 300 es
precision highp float;
out vec4 fragColor;
void main() {
  fragColor = vec4(0, 0, 0, 1);
}
|glsl}

let main () =
  let w, h = 1200, 800 in
  let canvas =
    let canvas = Canvas.create ~w ~h [] in
    let el = Canvas.to_el canvas in
    El.set_inline_style (Jstr.v "border") (Jstr.v "1px solid red") el;
    El.set_children (Document.body G.document) [el];
    let onclick ev = Fut.await (El.request_pointer_lock el) ignore in
    ignore (Ev.listen Ev.mousemove onmousemove (El.as_target el));
    ignore (Ev.listen Ev.click onclick (El.as_target el));
    canvas
  in
  match Gl.get_context canvas with
  | None ->
    Error "Could not initialize WebGL"
  | Some gl ->
    match compile_and_link_program gl vertex_shader_source fragment_shader_source with
    | Error _ as err -> err
    | Ok prg ->
      run gl prg (float w) (float h);
      Ok ()

let () =
  match main () with
  | Error s -> Printf.eprintf "Error: %s" s
  | Ok () -> ()
