open Graphics
open Base
open Box2d
open Math
open Shape



let despl_point p =
    let delta_pos = {x = 200.; y = 200.} in
    Vec2.(+) p delta_pos

let draw_arrow (p0: vec2) (p1: vec2) =
    let p0 = despl_point p0 in
    let p1 = despl_point p1 in

    (* Draw the line from p0 to p1 *)
    moveto (Int.of_float p0.x) (Int.of_float p0.y);
    lineto (Int.of_float p1.x) (Int.of_float p1.y);

    (* Calculate the arrowhead position *)
    let arrow_size_int = 10 in
    let arrow_size_float = Float.of_int arrow_size_int in
    let dx = p1.x -. p0.x in
    let dy = p1.y -. p0.y in
    let angle = Float.atan2 dy dx in
    let arrow_x = p1.x -. (arrow_size_float *. (Float.cos angle)) in
    let arrow_y = p1.y -. (arrow_size_float *. (Float.sin) angle) in

    (* Draw the arrowhead as a small triangle *)
    fill_poly [|
        (Int.of_float p1.x, Int.of_float p1.y);
        (Int.of_float arrow_x, Int.of_float arrow_y + arrow_size_int / 2);
        (Int.of_float arrow_x, Int.of_float arrow_y - arrow_size_int / 2)
    |]

let draw_line (p0: vec2) (p1: vec2) =
    let p0 = despl_point p0 in
    let p1 = despl_point p1 in
    moveto (Int.of_float p0.x) (Int.of_float p0.y);
    lineto (Int.of_float p1.x) (Int.of_float p1.y)

let draw_point (p: vec2) =
    let p = despl_point p in
    Graphics.draw_circle (Int.of_float p.x) (Int.of_float p.y) 5


(* Draw shapes *)

let draw_aabb (aabb: aabb) =
    let lower = aabb.lower_bound in
    let upper = aabb.upper_bound in
    let p0 = Vec2.create lower.x lower.y in
    let p1 = Vec2.create lower.x upper.y in
    let p2 = Vec2.create upper.x upper.y in
    let p3 = Vec2.create upper.x lower.y in
    draw_line p0 p1;
    draw_line p1 p2;
    draw_line p2 p3;
    draw_line p3 p0

let draw_edge (edge: edge) =
    draw_line edge.p1 edge.p2

let draw_circle (circle: circle) =
    let p = despl_point circle.position in
    Graphics.draw_circle (Int.of_float p.x) (Int.of_float p.y) (Int.of_float circle.radius)

let draw_polygon (polygon: polygon) =
    for i = 0 to Array.length polygon.vertices - 1 do
        let normal = Vec2.mul_value polygon.normals.(i) 1.5 in
        let normal_d = Vec2.(+) polygon.vertices.(i) normal in
        set_color red;
        draw_arrow polygon.vertices.(i) normal_d;
        set_color black;
        if i = (Array.length polygon.vertices) - 1 then
            draw_arrow polygon.vertices.(i) polygon.vertices.(0)
        else 
            draw_arrow polygon.vertices.(i) polygon.vertices.(i+1)
    done;
    draw_point polygon.centroid;
    ()

let%test "compute_convex_hull" =
    open_graph " 600x600";
    set_color black;
    let points = [|
        Vec2.create 0. 0.;
        Vec2.create 0. 150.;
        Vec2.create 100. 100.;
        Vec2.create 100. 0.;
    |] in
    let points = Array.rev points in
    let b = Polygon.create_convex_hull points in
    draw_polygon b;

    let circle = Circle.create {x=200.; y=200.} 20. in
    let aabb = Circle.compute_aabb circle Transform.identity in
    draw_circle circle;
    draw_aabb aabb;

    while true do
        ignore (wait_next_event [Poll]);
    done; 
    true


