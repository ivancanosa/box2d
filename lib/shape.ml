open Math

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

type shape = 
    | Circle of circle
    | Edge of edge
    | Polygon of polygon
    | Chain of vec2 list

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

        (* let _ = assert (!area > Float.epsilon) in *)
        c := Vec2.(+) s (Vec2.mul_value !c (1. /. !area)) ;
        !c
end

module AABB = struct
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
        let tmin = ref (-.Float.max_float) in
        let tmax = ref Float.max_float in
        let p = raycast_in.p1 in
        let d = raycast_in.p2 - raycast_in.p1 in
        let absD = abs d in
        let normal = ref Vec2.zero in

        let compute (i: int) : bool =
            if nth absD i < Float.epsilon then
                (* Parallel *)
                if (nth p i) < (nth aabb.lower_bound i) || (nth p i) > (nth aabb.upper_bound i) then
                    false
                else
                    true
            else
                let inv_d = 1.0 /. (nth d i) in
                let t1 = ((nth aabb.lower_bound i) -. (nth p i)) *. inv_d in
                let t2 = ((nth aabb.upper_bound i) -. (nth p i)) *. inv_d in
                let s = 
                    if t1 > t2 then 
                        1.0 
                    else 
                        -1.0 
                in
                let t1, t2 = 
                    if t1 > t2 then 
                        t2, t1 
                    else 
                        t1, t2 
                in
                tmax := Float.min !tmax t2;
                if t1 > !tmin then begin
                    normal := (nth_set Vec2.zero i s);
                    tmin := t1;
                    if !tmin > !tmax then
                        false
                    else
                        true
                end else
                    if !tmin > !tmax then
                        false
                    else
                        true
        in

        let rec compute' i max =
            if i < max then
                match compute i with
                | false -> false
                | true  -> compute' (Int.add i 1) max
            else
                true
        in

        let result = compute' 0 2 in

        match result with
        | false -> None
        | true -> 
            if !tmin < 0. || raycast_in.max_fraction < !tmin then
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
        (dot d d) <= circle.radius *. circle.radius

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
        if (Float.compare sigma 0.) < 0 && (Float.compare rr Float.epsilon) < 0 then
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
        if edge.one_sided && numerator > 0. then
            None
        else
            let denominator = dot normal d in
            if denominator = 0. then
                None
            else
                let t = numerator /. denominator in
                if t < 0. || raycast_in.max_fraction < t then
                    None
                else
                    let q = p1 + (mul_value d t) in
                    let r = v2 - v1 in
                    let rr = dot r r in
                    if rr = 0. then
                        None
                    else
                        let s = (dot (q - v1) r) /. rr in
                        if s < 0. || 1. < s then
                            None
                        else
                            if numerator > 0. then
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
                if dot > 0. then false
                else loop (i + 1) max 
        in
        loop 0 (Array.length polygon.vertices)

    let create_convex_hull (points: vec2 array) =
        let compute_normal current_point next_point =
            let edge = Vec2.(-) next_point current_point in
            Vec2.normalize (Vec2.cross_float edge 1.)
        in

        let leftmost_point_index (points: vec2 array) =
            let rec find_leftmost idx min_idx =
                if idx = Array.length points then min_idx
                else
                    let min_point = points.(min_idx) in
                    let current_point = points.(idx) in
                    if current_point.x < min_point.x then find_leftmost (idx + 1) idx
                    else find_leftmost (idx + 1) min_idx
            in
            find_leftmost 0 0
        in

        let is_left_of_line p q r =
            let pq = Vec2.(-) q p in
            let pr = Vec2.(-) r p in
            Vec2.cross_vec2 pq pr > 0.0
        in

        let n = Array.length points in
        let point_on_hull_idx = leftmost_point_index points in
        let p = Array.make n points.(point_on_hull_idx) in
        let i = ref 0 in
        let endpoint = ref points.(0) in
        let rec loop () =
            p.(!i) <- !endpoint;
            endpoint := points.(0);
            for j = 0 to n - 1 do
                if !endpoint = p.(!i) || is_left_of_line p.(!i) !endpoint points.(j) then
                endpoint := points.(j)
            done;
            i := !i + 1;
            if !endpoint <> p.(0) then loop ()
        in
        loop ();
      
        let normals = Array.init !i (fun idx ->
            let next_idx = (idx + 1) mod !i in
            compute_normal  p.(idx) p.(next_idx)
        ) in
      
        let vertices = Array.sub p 0 !i in
        {
            radius = Common.polygon_radius;
            centroid = Geometry.compute_centroid vertices;
            vertices = vertices;
            normals = normals
        }


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



