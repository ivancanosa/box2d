open Base
open Box2d
open Math
open Shape
open Graphics

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
    let arrow_size = 10. in
    let edge = Vec2.(-) p1 p0 in
    let angle = Vec2.compute_angle edge in
    let tr = {position= Vec2.zero;
       rot = Rotation.create (angle -. 0.5*.Float.pi)}  in
    let arrow_left = Vec2.create (-.arrow_size) (-.arrow_size) in
    let arrow_right = Vec2.create arrow_size (-.arrow_size) in
    let arrow_left = Transform.mul_vec2 tr arrow_left in
    let arrow_right = Transform.mul_vec2 tr arrow_right in
    let arrow_left = Vec2.(+) p1 arrow_left in
    let arrow_right = Vec2.(+) p1 arrow_right in

    fill_poly [|
        (Int.of_float p1.x, Int.of_float p1.y);
        (Int.of_float arrow_left.x, Int.of_float arrow_left.y);
        (Int.of_float arrow_right.x, Int.of_float arrow_right.y)
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
        let normal = Vec2.mul_value polygon.normals.(i) 100.  in
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

    (*

let%test "compute_convex_hull" =
    open_graph " 600x600";
    set_color black;

    let points = [|
        Vec2.create 0. 0.;
        Vec2.create 0. 180.;
        Vec2.create 100. 100.;
        Vec2.create 100. 0.;
    |] in
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
    *)



(* Polygon tests *)

let test_raycast polygon raycast_in is_collision =
    match Polygon.raycast polygon raycast_in Transform.identity with
    | None -> not is_collision
    | Collision _ -> is_collision

    
(* Test 0: Two raycast tests *)
let%test "polygon_raycast_0" =
    let points = [|
        Vec2.create 0. 0.;
        Vec2.create 0. 100.;
        Vec2.create 100. 100.;
        Vec2.create 100. 0.;
    |] in
    let polygon = Polygon.create_convex_hull points in
    let raycast_0 = {
        max_fraction = 1.;
        p1 = Vec2.create (-50.) (-50.);
        p2 = Vec2.create 20. 20.} in
    let raycast_1 = {
        max_fraction = 1.;
        p1 = Vec2.create (-50.) (-50.);
        p2 = Vec2.create (-40.) (-40.);} in
    
    test_raycast polygon raycast_0 true &&
    test_raycast polygon raycast_1 false



(* Test 1: Ray starts inside the polygon, points away *)
let%test "polygon_raycast_1" =
    let points = [|
        Vec2.create 0. 0.;
        Vec2.create 0. 100.;
        Vec2.create 100. 100.;
        Vec2.create 100. 0.;
    |] in
    let polygon = Polygon.create_convex_hull points in
    let raycast = {
        max_fraction = 1.;
        p1 = Vec2.create 50. 50.;
        p2 = Vec2.create 70. 70.;
    } in
    test_raycast polygon raycast false

(* Test 3: Ray starts outside, intersects the polygon *)
let%test "polygon_raycast_3" =
    let points = [|
        Vec2.create 0. 0.;
        Vec2.create 0. 100.;
        Vec2.create 100. 100.;
        Vec2.create 100. 0.;
    |] in
    let polygon = Polygon.create_convex_hull points in
    let raycast = {
        max_fraction = 1.;
        p1 = Vec2.create 50. 150.;
        p2 = Vec2.create 50. (-50.);
    } in
    test_raycast polygon raycast true

(* Test 4: Ray starts outside, does not intersect the polygon *)
let%test "polygon_raycast_4" =
    let points = [|
        Vec2.create 0. 0.;
        Vec2.create 0. 100.;
        Vec2.create 100. 100.;
        Vec2.create 100. 0.;
    |] in
    let polygon = Polygon.create_convex_hull points in
    let raycast = {
        max_fraction = 1.;
        p1 = Vec2.create (-50.) 50.;
        p2 = Vec2.create (-50.) 150.;
    } in
    test_raycast polygon raycast false
    
let%test "polygon_compute_mass" =
    let points = [|
        Vec2.create 0. 0.;
        Vec2.create 0. 100.;
        Vec2.create 100. 100.;
        Vec2.create 100. 0.;
    |] in
    let polygon = Polygon.create_convex_hull points in
    let mass = Polygon.compute_mass polygon 1. in
    Float.compare mass.mass 10000. = 0 &&
    Vec2.equal mass.center (Vec2.create 50. 50.)

