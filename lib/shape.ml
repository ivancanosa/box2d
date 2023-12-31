open Base
open Math
open Stdio

type aabb = {lower_bound: vec2; upper_bound: vec2}

type mass = {mass: float; center: vec2; rot_inertia: float}

type raycast_in = {
    p1: vec2;
    p2: vec2;
    max_fraction: float
}

type raycast_collision = {
    normal: vec2;
    fraction: float}

type raycast_result = 
    | None
    | Collision of raycast_collision

type circle = {radius: float; position: vec2}

(* p1 and p2 are the actual vertices, p0 and p3 are for smooth collision *)
type edge = {
    radius: float; 
    p1: vec2;
    p2: vec2;
    p0: vec2;
    p3: vec2;
    one_sided: bool
}

type polygon = {
    radius: float;
    centroid: vec2;
    vertices: vec2 array;
    normals: vec2 array;
}

type chain = {
    radius: float;
    vertices: vec2 array;
    prev_vertex: vec2;
    next_vertex: vec2;
}

type shape = 
    | Circle of circle
    | Edge of edge
    | Polygon of polygon
    | Chain of chain

let print_mass mass =
    printf "{mass = %s; " (Float.to_string mass.mass);
    printf "center = %s; " (Vec2.to_string mass.center);
    printf "rot_inertia = %s}\n" (Float.to_string mass.rot_inertia);

module Geometry = struct
    let compute_centroid vs =
        let count = Array.length vs in
        let _ = assert (count >= 3) in
        let c = ref {x=0.; y=0.} in
        let area = ref 0. in
        let s = vs.(0) in
        let inv3 = 1. /. 3. in
        for i = 0 to count - 1 do
            let p1 = Vec2.(-) vs.(0) s in
            let p2 = Vec2.(-) vs.(i) s in
            let p3 = ref {x=0.; y=0.} in
            if i + 1 < count then p3 := Vec2.(-) vs.(i+1) s
            else begin p3 := Vec2.(-) vs.(0) s end;

            let e1 = Vec2.(-) p2 p1 in
            let e2 = Vec2.(-) !p3 p1 in

            let d = Vec2.cross_vec2 e1 e2 in

            let triangle_area = 0.5 *. d in
            area := !area +. triangle_area;

            let open Vec2 in
            c := !c + (mul_value (p1 + p2 + !p3) (triangle_area *. inv3))
        done;

        let _ = assert (Float.compare !area  Common.epsilon > 0) in 
        c := Vec2.(+) s (Vec2.mul_value !c (1. /. !area)) ;
        !c
end

module AABB = struct
    let create lower_bound upper_bound = 
        {lower_bound = lower_bound; upper_bound = upper_bound}

    let is_valid aabb =
        let open Vec2 in
        let d = aabb.upper_bound - aabb.lower_bound in
        let valid = (Float.compare d.x 0.) >= 0 && (Float.compare d.y 0.) >= 0 in
        valid && (is_valid aabb.lower_bound) && (is_valid aabb.upper_bound)

    let test_overlap (a: aabb) (b: aabb): bool =
        let open Vec2 in
        let d1 = b.lower_bound - a.upper_bound in
        let d2 = a.lower_bound - b.upper_bound in
        let not_overlap = (Float.compare d1.x 0.) > 0 || (Float.compare d1.y 0.) > 0 ||
           (Float.compare d2.x 0.) > 0 || (Float.compare d2.y 0.) > 0 in
        not not_overlap

    let compute_center (aabb: aabb): vec2 =
        let open Vec2 in
        mul_value (aabb.lower_bound + aabb.upper_bound) 0.5

    let compute_extents (aabb: aabb) =
        let open Vec2 in
        mul_value (aabb.lower_bound - aabb.upper_bound) 0.5

    let compute_perimeter (aabb: aabb): float =
        let wx = aabb.upper_bound.x -. aabb.lower_bound.x in
        let wy = aabb.upper_bound.y -. aabb.lower_bound.y in
        2. *. (wx +. wy)

    let combine (a: aabb) (b: aabb) : aabb =
        let open Vec2 in
        let lower_bound = min a.lower_bound b.lower_bound in
        let upper_bound = min a.upper_bound b.upper_bound in
        {lower_bound = lower_bound; upper_bound = upper_bound}

    let contains (origin: aabb) (contained: aabb) : bool =
        let result = true in
        let result = result && (Float.compare origin.lower_bound.x contained.lower_bound.x) <= 0 in
        let result = result && (Float.compare origin.lower_bound.y contained.lower_bound.y) <= 0 in
        let result = result && (Float.compare contained.upper_bound.x origin.upper_bound.x) <= 0 in
        let result = result && (Float.compare contained.upper_bound.y origin.upper_bound.y) <= 0 in
        result

    let raycast (aabb: aabb) (raycast_in: raycast_in) : raycast_result =
        let open Vec2 in
        let tmin = ref (-.Float.max_value) in
        let tmax = ref Float.max_value in
        let p = raycast_in.p1 in
        let d = raycast_in.p2 - raycast_in.p1 in
        let absD = abs d in
        let normal = ref Vec2.zero in

        let compute (i: int) : bool =
            if Float.compare (nth absD i) Common.epsilon < 0 then
                (* Parallel *)
                if Float.compare (nth p i) (nth aabb.lower_bound i) < 0 || Float.compare (nth p i) (nth aabb.upper_bound i) > 0 then
                    false
                else
                    true
            else
                let inv_d = 1.0 /. (nth d i) in
                let t1 = ((nth aabb.lower_bound i) -. (nth p i)) *. inv_d in
                let t2 = ((nth aabb.upper_bound i) -. (nth p i)) *. inv_d in
                let s = 
                    if Float.compare t1 t2 > 0 then 
                        1.0 
                    else 
                        -1.0 
                in
                let t1, t2 = 
                    if Float.compare t1 t2 > 0 then 
                        t2, t1 
                    else 
                        t1, t2 
                in
                tmax := Float.min !tmax t2;
                if Float.compare t1 !tmin > 0 then begin
                    normal := (nth_set Vec2.zero i s);
                    tmin := t1;
                    if Float.compare !tmin !tmax > 0 then
                        false
                    else
                        true
                end else
                    if Float.compare !tmin !tmax > 0 then
                        false
                    else
                        true
        in

        let rec compute' i max =
            if i < max then
                match compute i with
                | false -> false
                | true  -> compute' (Int.(+) i 1) max
            else
                true
        in

        let result = compute' 0 2 in

        match result with
        | false -> None
        | true -> 
            if Float.compare !tmin 0. < 0 || Float.compare raycast_in.max_fraction !tmin < 0 then
                None
            else
                Collision {fraction = !tmin; normal = !normal}
end


module Circle = struct 
    let create pos radius =
        {position = pos; radius = radius}
    
    let get_child_count (_: edge) =
        1

    let test_point (circle: circle) (transform: transform) (p: vec2): bool =
        let open Vec2 in
        let center = transform.position + (rotate  circle.position transform.rot) in
        let d = p - center in
        Float.compare (dot d d) (circle.radius *. circle.radius) <= 0

    let compute_aabb (circle: circle) (transform: transform): aabb =
        let open Vec2 in
        let p = transform.position + rotate circle.position transform.rot in
        {lower_bound = {
            x = p.x -. circle.radius;
            y = p.y -. circle.radius
        };
         upper_bound = {
             x = p.x +. circle.radius;
             y = p.y +. circle.radius
        }}

    let compute_mass (circle: circle) (density: float): mass =
        let open Vec2 in
        let radius_2 = circle.radius *. circle.radius in
        let mass = density *. Float.pi *. radius_2 in
        let center = circle.position in
        let rot_inertia = mass *. (0.5 *. radius_2 +. (dot circle.position circle.position)) in
        {mass = mass; center = center; rot_inertia = rot_inertia}

    let raycast (circle: circle) (raycast_in: raycast_in) 
        (transform: transform): raycast_result =
        let open Vec2 in
        let position = transform.position + (rotate circle.position transform.rot) in 
        let s = raycast_in.p1 - position in
        let b = (dot s s) -. circle.radius *. circle.radius in

        let r  = raycast_in.p2 - raycast_in.p1 in
        let c  = dot s r in
        let rr = dot r r in
        let sigma = c *. c -. rr *. b in
        if (Float.compare sigma 0.) < 0 && (Float.compare rr Common.epsilon) < 0 then
            None
        else
            let a =  -.(c +. Float.sqrt sigma) in
            if (Float.compare 0. a) <= 0 && (Float.compare a (raycast_in.max_fraction *. rr)) <= 0 then
                let a = a /. rr in
                Collision{fraction = a;
                    normal = s + (Vec2.mul_value r a)}
            else
                None
end

module Edge = struct

    let zero: edge =
        {radius = Common.polygon_radius;
         p0 = Vec2.zero; p1 = Vec2.zero;
         p2 = Vec2.zero; p3 = Vec2.zero;
         one_sided = false}

    let create_one_sided v0 v1 v2 v3 : edge =
        {radius = Common.polygon_radius;
         p0 = v0; p1 = v1;
         p2 = v2; p3 = v3;
         one_sided = true}

    let create_two_sided v1 v2 (edge: edge): edge =
        {radius = edge.radius;
         p0 = edge.p0; p1 = v1;
         p2 = v2; p3 = edge.p3;
         one_sided = false}

    let get_child_count (_: edge) =
        1

    let test_point (_: edge) (_: transform) (_: vec2): bool =
        false
    
    let compute_aabb (edge: edge) (transform: transform): aabb =
        let open Vec2 in
        let v1 = Transform.mul_vec2 transform edge.p1 in
        let v2 = Transform.mul_vec2 transform edge.p2 in

        let lower = min v1 v2 in
        let upper = max v1 v2 in

        let r = {x = edge.radius; y = edge.radius} in
        {lower_bound = lower - r; upper_bound = upper + r}

    let compute_mass (edge: edge) (_: float): mass =
        let open Vec2 in
        let center = mul_value (edge.p1 + edge.p2) 0.5 in
        {mass = 0.; center = center; rot_inertia = 0.}

    let raycast (edge: edge) (raycast_in: raycast_in) (transform: transform) : raycast_result =
        let open Vec2 in
        (* Put the ray into the edge's frame of reference *)
        let p1 = Vec2.rotate_inverse (raycast_in.p1 - transform.position) transform.rot in
        let p2 = Vec2.rotate_inverse (raycast_in.p2 - transform.position) transform.rot in
        let d = p2 - p1 in
        let v1 = edge.p1 in
        let v2 = edge.p2 in
        let e = v2  - v1 in
        let normal = normalize {x = e.y; y =(-.e.x)} in

        let numerator = dot normal (v1 - p1) in
        if edge.one_sided && Float.compare numerator 0. > 0 then
            None
        else
            let denominator = dot normal d in
            if Float.compare denominator 0. = 0 then
                None
            else
                let t = numerator /. denominator in
                if Float.compare t 0. < 0 || Float.compare raycast_in.max_fraction t < 0 then
                    None
                else
                    let q = p1 + (mul_value d t) in
                    let r = v2 - v1 in
                    let rr = dot r r in
                    if Float.compare rr 0. = 0 then
                        None
                    else
                        let s = (dot (q - v1) r) /. rr in
                        if Float.compare s 0. < 0 || Float.compare 1. s < 0 then
                            None
                        else
                            if Float.compare numerator 0. > 0 then
                                Collision {
                                    fraction = t;
                                    normal = mul_value (rotate normal transform.rot) (-.1.)
                                }
                            else
                                Collision {
                                    fraction = t;
                                    normal = rotate normal transform.rot
                                }
end

module Polygon = struct
    let get_child_count (_: polygon) =
        1

    let create_box hx hy =
        {radius = Common.polygon_radius;
         centroid = Vec2.zero;
         vertices = [|
             {x=(-.hx); y=(-.hy)};
             {x=hx; y=(-.hy)};
             {x=hx; y=hy};
             {x=(-.hx); y=hy}
         |];
         normals = [|
             {x=0.; y=(-1.)};
             {x=1.; y=0.};
             {x=0.; y=1.};
             {x=(-1.); y=0.}
         |]}

    let create_box_rotated hx hy center angle =
        let box = {radius = Common.polygon_radius;
         centroid = center;
         vertices = [|
             {x=(-.hx); y=(-.hy)};
             {x=hx; y=(-.hy)};
             {x=hx; y=hy};
             {x=(-.hx); y=hy}
         |];
         normals = [|
             {x=0.; y=(-1.)};
             {x=1.; y=0.};
             {x=0.; y=1.};
             {x=(-1.); y=0.}
         |]} in
        let tr = Transform.create center angle in
        for i = 0 to 3 do
            box.vertices.(i) <- Transform.mul_vec2 tr box.vertices.(i);
            box.normals.(i) <- Transform.mul_vec2 tr box.normals.(i)
        done;
        box

    let test_point (polygon: polygon) (transform: transform) (p: vec2): bool =
        let p_local = Vec2.rotate_inverse (Vec2.(-) p transform.position) transform.rot in
        let rec loop i max =
            if i >= max then true
            else
                let dot = Vec2.dot polygon.vertices.(i) (Vec2.(-) p_local polygon.vertices.(i)) in
                if Float.compare dot 0. > 0 then false
                else loop (i + 1) max 
        in
        loop 0 (Array.length polygon.vertices)

    let raycast (polygon: polygon) (raycast_in: raycast_in) (xf: transform) : raycast_result =
        let p1 = Vec2.rotate_inverse (Vec2.(-) raycast_in.p1 xf.position) xf.rot in
        let p2 = Vec2.rotate_inverse (Vec2.(-) raycast_in.p2 xf.position) xf.rot in
        let d = Vec2.(-) p2 p1 in
        let lower = ref 0. in
        let upper = ref (raycast_in.max_fraction) in
        let index = ref (-1) in
        let m_count = Array.length polygon.vertices in
        let maybe_collision = ref true in
        let i = ref 0 in

        while !i < m_count && !maybe_collision do
            let numerator = Vec2.dot polygon.normals.(!i) (Vec2.(-) polygon.vertices.(!i) p1) in
            let denominator = Vec2.dot polygon.normals.(!i) d in
            if Float.compare denominator 0. = 0 && Float.compare numerator 0. < 0 then
                maybe_collision := false
            else if Float.compare denominator 0. < 0 &&
                Float.compare numerator (!lower *. denominator) < 0 then begin
                
                lower := numerator /. denominator;
                index := !i;
            end else if Float.compare denominator 0. > 0 && 
                Float.compare numerator (!upper *. denominator) < 0 then begin

                upper := numerator /. denominator;
            end;

            if Float.compare !upper !lower < 0 then begin
                maybe_collision := false;
            end;
            i := !i + 1;
        done;


        if !index >= 0 && !maybe_collision then  begin
            assert (Float.compare 0. !lower <= 0 && 
                    Float.compare !lower raycast_in.max_fraction <= 0);
            let normal = Vec2.rotate polygon.normals.(!index) xf.rot in
            Collision {normal = normal; fraction = !lower}
        end else 
            None

    let create_convex_hull (points: vec2 array) =
        let compute_normal current_point next_point =
            let edge = Vec2.(-) next_point current_point in
            let _ = assert (Float.compare (Vec2.norm_squared edge) (Common.epsilon *. Common.epsilon) > 0) in 
            Vec2.normalize (Vec2.cross_float edge 1.) 
        in

        let leftmost_point_index (points: vec2 array) =
            let rec find_leftmost idx min_idx =
                if idx = Array.length points then min_idx
                else
                    let min_point = points.(min_idx) in
                    let current_point = points.(idx) in
                    if Float.compare current_point.x min_point.x < 0 then find_leftmost (idx + 1) idx
                    else find_leftmost (idx + 1) min_idx
            in
            find_leftmost 0 0
        in

        let is_right_of_line p q r =
            let pq = Vec2.(-) q p in
            let pr = Vec2.(-) r p in
            Float.compare (Vec2.cross_vec2 pq pr) 0.0 < 0
        in

        let n = Array.length points in
        let point_on_hull_idx = leftmost_point_index points in
        let p = Array.create ~len:n points.(point_on_hull_idx) in
        let i = ref 0 in
        let endpoint = ref points.(0) in
        let rec loop () =
            p.(!i) <- !endpoint;
            endpoint := points.(0);
            for j = 0 to n - 1 do
                if Vec2.equal !endpoint p.(!i) || is_right_of_line p.(!i) !endpoint points.(j) then
                endpoint := points.(j)
            done;
            i := !i + 1;
            if not (Vec2.equal !endpoint p.(0)) then loop ()
        in
        loop ();
      
        let vertices = Array.sub p ~pos:0 ~len:!i in

        let normals = Array.init !i ~f:(fun idx ->
            let next_idx = (idx + 1) % !i in
            compute_normal p.(idx) p.(next_idx) 
        ) in
      
        {
            radius = Common.polygon_radius;
            centroid = Geometry.compute_centroid vertices;
            vertices = vertices;
            normals = normals
        }

    let compute_aabb (polygon: polygon) (xf: transform): aabb =
        let count = Array.length polygon.vertices in
        let rec loop i lower upper =
            if i >=count then (lower, upper)
            else
                let v = Transform.mul_vec2 xf polygon.vertices.(i) in
                let lower = Vec2.min lower v in
                let upper = Vec2.max upper v in
                loop (i+1) lower upper
        in
        let initial_vec = Transform.mul_vec2 xf polygon.vertices.(0) in
        let lower, upper = loop 1 initial_vec initial_vec in
        let r = Vec2.create polygon.radius polygon.radius in
        AABB.create (Vec2.(-) lower r) (Vec2.(+) upper r)

    let compute_mass (polygon: polygon) (density: float): mass =
        let count = Array.length polygon.vertices in
        let s = polygon.vertices.(0) in
        let k_inv3 = 1. /. 3. in
        let rec loop i center area rot_inertia =
            if i >= count then (center, area, rot_inertia)
            else
                let e1 = Vec2.(-) polygon.vertices.(i) s in
                let next_i = (i + 1) % count in
                let e2 = Vec2.(-) polygon.vertices.(next_i) s in

                let d = Vec2.cross_vec2 e1 e2 in
                let triangle_area = 0.5 *. d in
                let area = area +. triangle_area in
                let center = Vec2.(+) center (Vec2.mul_value (Vec2.(+) e1 e2) (triangle_area *. k_inv3)) in
                let intx2 = e1.x *. e1.x +. e2.x *. e1.x +. e2.x *. e2.x in
                let inty2 = e1.y *. e1.y +. e2.y *. e1.y +. e2.y *. e2.y in
                let rot_inertia = rot_inertia +. (0.25 *. k_inv3 *. d) *. (intx2 +. inty2) in

                loop (i+1) center area rot_inertia
        in
        let center, area, rot_inertia = loop 0 Vec2.zero 0. 0. in

        let mass = density *. area in
        let _ = assert (Float.compare area Common.epsilon > 0) in 
        let center = Vec2.mul_value center (1. /. area) in
        let center = Vec2.(+) center s in
        let rot_inertia = (rot_inertia *. density) *. ((Vec2.dot center center) -. Vec2.dot center center) in
        {mass = mass; center = center; rot_inertia = rot_inertia}
end

module Chain = struct
    let get_child_count (chain: chain) =
        (Array.length chain.vertices) - 1

    let check_unique_points points =
        let count = Array.length points in
        let rec loop_distance_check i =
            if i >= count then ()
            else
                let v1 = points.(i) in
                let i2 = (i + 1) % count in
                let v2 = points.(i2) in
                let distance = Vec2.distance_squared v1 v2 in
                let _ = assert(Float.compare distance (Common.linear_slop *. Common.linear_slop) > 0) in
                loop_distance_check (i + 1)
        in
        loop_distance_check 0

    let create_loop vertices =
        let count = Array.length vertices in
        let _ = assert(count >= 0 ) in
        check_unique_points vertices;
        let new_vertices = Array.create ~len:(count + 1) Vec2.zero in
        Array.blit ~src:vertices ~src_pos:0 ~dst:new_vertices ~dst_pos:0 ~len:count;
        new_vertices.(count) <- new_vertices.(0);
        {radius = Common.polygon_radius;
        vertices = new_vertices;
        prev_vertex = new_vertices.(count - 3);
        next_vertex = new_vertices.(1)}

    let create_chain vertices prev_vertex next_vertex =
        let count = Array.length vertices in
        let _ = assert(count >= 0 ) in
        check_unique_points vertices;
        let new_vertices = Array.create ~len:count Vec2.zero in
        Array.blit ~src:vertices ~src_pos:0 ~dst:new_vertices ~dst_pos:0 ~len:count;
        {radius = Common.polygon_radius;
        vertices = new_vertices;
        prev_vertex = prev_vertex;
        next_vertex = next_vertex}

    let get_child_edge chain index =
        let count = Array.length chain.vertices in
        let _ = assert (0 <= index && index < (count - 1)) in
        let radius = chain.radius in
        let v1 = chain.vertices.(index) in
        let v2 = chain.vertices.(index + 1) in
        let one_sided = true in
        let get_v1 =
            if index > 0 then chain.vertices.(index - 1)
            else chain.prev_vertex
        in
        let get_v3 =
            if index < count - 2 then chain.vertices.(index + 2)
            else chain.next_vertex
        in
        { radius = radius;
        p1 = v1;
        p2 = v2;
        p0 = get_v1;
        p3 = get_v3;
        one_sided = one_sided}

    let test_point (_: chain) (_: transform) (_: vec2): bool =
        false

    let raycast (chain: chain) (raycast_in: raycast_in) (transform: transform) child_index : raycast_result =
        let count = Array.length chain.vertices in
        assert (child_index < count);
        let v1 = chain.vertices.(child_index) in
        let v2 = chain.vertices.((child_index + 1) % count) in
        let edge = Edge.create_one_sided v1 v2 Vec2.zero Vec2.zero in
        Edge.raycast edge raycast_in transform

    let compute_aabb (chain: chain) (xf: transform) (child_index: int): aabb =
        let count = Array.length chain.vertices in
        assert (child_index < count);
        let i1 = child_index in
        let i2 = (i1 + 1) % count in
        let v1 = Transform.mul_vec2 xf chain.vertices.(i1) in
        let v2 = Transform.mul_vec2 xf chain.vertices.(i2) in

        let lower = Vec2.min v1 v2 in
        let upper = Vec2.max v1 v2 in

        let r = Vec2.create chain.radius chain.radius in
        {lower_bound = Vec2.(-) lower r;
         upper_bound = Vec2.(+) upper r}
    
    let compute_mass (_: chain) (_: float) =
        {mass = 0.; center = Vec2.zero; rot_inertia = 0.}
end

module Shape = struct 

    let get_child_count shape =
        match shape with
        | Circle _ -> 1
        | Edge _ -> 1
        | Polygon _ -> 1
        | Chain chain -> Chain.get_child_count chain

    let test_point shape xf point=
        match shape with
        | Circle circle -> Circle.test_point circle xf point
        | Edge _ -> false
        | Polygon polygon -> Polygon.test_point polygon xf point 
        | Chain _ -> false

    let raycast shape raycast_in xf child_index =
        match shape with
        | Circle circle -> Circle.raycast circle raycast_in xf 
        | Edge edge -> Edge.raycast edge raycast_in xf 
        | Polygon polygon -> Polygon.raycast polygon raycast_in xf
        | Chain chain -> Chain.raycast chain raycast_in xf child_index

    let compute_aabb shape xf child_index =
        match shape with
        | Circle circle -> Circle.compute_aabb circle xf 
        | Edge edge -> Edge.compute_aabb edge xf 
        | Polygon polygon -> Polygon.compute_aabb polygon xf
        | Chain chain -> Chain.compute_aabb chain xf child_index

    let compute_mass shape density = 
        match shape with
        | Circle circle -> Circle.compute_mass circle density
        | Edge edge -> Edge.compute_mass edge density
        | Polygon polygon -> Polygon.compute_mass polygon density
        | Chain chain -> Chain.compute_mass chain density

end


    (*

let test_point (circle: edge) (transform: transform) (p: vec2): bool =
    false


let test_point shape transform point =
    let open Math.Vec2 in
    match shape with
    | Circle radious -> 
         let center = transform.position + (rotate transform.position 
         *)



