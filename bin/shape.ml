open Common
open Math


type aabb = {lower_bound: vec2; upper_bound: vec2}

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
type edge = {radious: float; 
             p1: vec2;
             p2: vec2;
             p0: vec2;
             p3: vec2;
             one_sided: bool}

type shape = 
    | Circle of float
    | Edge of vec2 * vec2
    | Polygon of vec2 list
    | Chain of vec2 list


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

    let compute_extents (aabb: aabb) vec2 =
        let open Vec2 in
        mul_value (aabb.lower_bound - aabb.upper_bound) 0.5

    let compute_perimeter (aabb: aabb): float =
        let open Vec2 in
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

    type raycast_aabb_state = {
        tmin: float;
        tmax: float;
        normal: vec2
    }

    type raycast_aabb_result = 
        | State of raycast_aabb_state
        | Result of raycast_result

        (*
    let raycast (aabb: aabb) (raycast_in: raycast_in) : raycast_result =
        let open Vec2 in
        let tmin = -.Float.max_float in
        let tmax = Float.max_float in
        let p = raycast_in.p1 in
        let d = raycast_in.p2 - raycast_in.p1 in
        let absD = abs d in
        let compute (i: int) state =
            if (Float.compare (nth absD i) Float.epsilon) < 0 then
                (* Parallel *)
                if (Float.compare (nth p i) (nth aabb.lower_bound i)) < 0 ||  
                    (Float.compare (nth aabb.upper_bound i) (nth p i)) < 0 then
                        None
                else
                    state
            else
                let inv_d = 1. /. (nth d i) in
                let t1 = ((nth aabb.lower_bound i) -. (nth p i)) *. inv_d in
                let t2 = ((nth aabb.upper_bound i) -. (nth p i)) *. inv_d in
                let s = -1. in
        in

        None
                    
*)



end


module Circle = struct 
    let test_point (circle: circle) (transform: transform) (p: vec2): bool =
        let open Vec2 in
        let center = transform.position + (rotate  circle.position transform.rot) in
        let d = p - center in
        (dot d d) <= circle.radius *. circle.radius

    (* TODO this *)
    let compute_AABB (circle: circle) (transform: transform) =
        None

    let raycast (circle: circle) (raycast_in: raycast_in) 
        (transform: transform): raycast_result =
        let open Transform in
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

let test_point (circle: edge) (transform: transform) (p: vec2): bool =
    false


    (*
let test_point shape transform point =
    let open Math.Vec2 in
    match shape with
    | Circle radious -> 
         let center = transform.position + (rotate transform.position 
         *)



