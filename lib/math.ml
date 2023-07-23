open Stdio
(* types *)

type vec2 = {x: float; y: float}
type vec3 = { x: float; y: float; z: float }
type mat22 = {ex: vec2; ey: vec2}
type mat33 = {ex: vec3; ey: vec3; ez: vec3}

type rotation = {sine: float; cosine: float}
type transform = {position: vec2; rot: rotation}

type sweep = 
    { local_center: vec2;
      c0: vec2; 
      c: vec2;
      a0: float;
      a: float;
      alpha0: float}


let epsilon = 1e-6

let float_eq (f0: float) (f1: float) =
    (Float.compare (Float.abs (f1 -. f0)) epsilon) <= 0

let next_power_of_two x =
    let x = x lor (x lsr 1) in
    let x = x lor (x lsr 2) in
    let x = x lor (x lsr 4) in
    let x = x lor (x lsr 8) in
    let x = x lor (x lsr 16) in
    x + 1

let is_power_of_two x =
    x > 0 && (x land (x - 1)) = 0

let is_valid (x: float) : bool =
    match Float.classify_float x with
    | FP_normal | FP_subnormal | FP_zero -> true
    | _ -> false

(* vec2 *)

module Vec2 = struct
    let equal (v1: vec2) (v2: vec2) =
        float_eq v1.x v2.x &&
        float_eq v1.y v2.y

    let identity: vec2 =
        {x = 1.; y = 0.}

    let zero: vec2 =
        {x = 0.; y = 0.}

    let nth (v: vec2) (i: int): float =
        match i with
        | 0 -> v.x
        | 1 -> v.y
        | _ -> v.y

    let nth_set (v: vec2) (i: int) (value: float): vec2 =
        match i with
        | 0 -> {x = value; y = v.y}
        | 1 -> {x = v.x; y = value}
        | _ -> {x = v.x; y = value}

    let is_valid (v: vec2) =
        (is_valid v.x) && (is_valid v.y)

    let norm (v: vec2) =
      Float.sqrt (v.x *. v.x +. v.y *. v.y)

    let normalize (v: vec2) =
      if (Float.compare v.x 0.0) <> 0 && (Float.compare v.y 0.0) <> 0 then
        let module_val = norm v in
        { x = v.x /. module_val; y = v.y /. module_val }
      else
        v

    let norm_squared (v: vec2) =
      v.x *. v.x +. v.y *. v.y

    let dot (v1: vec2) (v2: vec2) =
      v1.x *. v2.x +. v1.y *. v2.y

    let cross_vec2 (a: vec2) (b: vec2) =
        a.x *. b.y -. a.y *. b.x

    let cross_float (a: vec2) (s: float) : vec2 =
        {x = s *. a.y; y = (-.s) *. a.x}

    let cross_inv (s: float) (a: vec2) : vec2 =
        {x = (-.s) *. a.y; y = s *. a.x}

    let (+) (v1: vec2) (v2: vec2) =
        { x = v1.x +. v2.x; y = v1.y +. v2.y }

    let (-) (v1: vec2) (v2: vec2) =
        { x = v1.x -. v2.x; y = v1.y -. v2.y }

    let ( * ) (v1: vec2) (v2: vec2) =
        { x = v1.x *. v2.x; y = v1.y *. v2.y }

    let (/) (v1: vec2) (v2: vec2) =
        { x = v1.x /. v2.x; y = v1.y /. v2.y }

    let abs (v: vec2) : vec2 =
        {x = Float.abs v.x; y = Float.abs v.y}

    let min (v0: vec2) (v1: vec2) : vec2 =
        {x = Float.min v0.x v1.x ;
         y = Float.min v0.y v1.y}

    let max (v0: vec2) (v1: vec2) : vec2 =
        {x = Float.max v0.x v1.x ;
         y = Float.max v0.y v1.y}

    let clamp (value: vec2) (low: vec2) (high: vec2): vec2 =
        max low (min value high)

    let distance (a: vec2) (b: vec2) =
        let c = a - b in
        norm c

    let mul_value (v: vec2) (a: float) : vec2 =
        {x = v.x *. a; y = v.y *. a}

    let distance_squared (a: vec2) (b: vec2) =
        let c = a - b in
        dot c c

    let rotate (v: vec2) (q: rotation) : vec2 =
        { x = q.cosine *. v.x -. q.sine *. v.y;
          y = q.sine *. v.x   +. q.cosine *. v.y}

    let rotate_inverse (v: vec2) (q: rotation) : vec2 =
        { x = q.cosine *. v.x   +. q.sine *. v.y;
          y = (-.q.sine) *. v.x +. q.cosine *. v.y}

    let print (v: vec2) =
        printf "{x = %s; " (Float.to_string v.x);
        printf "y = %s}\n " (Float.to_string v.y);

end

(* vec3 *)

module Vec3 = struct

    let equal (v1: vec3) (v2: vec3) : bool =
        float_eq v1.x v2.x 
          && float_eq v1.y v2.y 
          && float_eq v1.z v2.z 

    let identity: vec3 =
        {x = 1.; y = 0.; z = 0.}

    let zero: vec3 =
        {x = 0.; y = 0.; z = 0.}

    let nth (v: vec3) (i: int): float =
        match i with
        | 0 -> v.x
        | 1 -> v.y
        | 2 -> v.z
        | _ -> v.z

    let nth_set (v: vec3) (i: int) (value: float): vec3 =
        match i with
        | 0 -> {x = value; y = v.y; z = v.z}
        | 1 -> {x = v.x; y = value; z = v.z}
        | 2 -> {x = v.x; y = v.y; z = value}
        | _ -> {x = v.x; y = v.y; z = value}

    let is_valid (v: vec3) =
        (is_valid v.x) && (is_valid v.y) && (is_valid v.z)

    let normalize (v: vec3) =
        let module_val = Float.sqrt (v.x *. v.x +. v.y *. v.y +. v.z *. v.z) in
        { x = v.x /. module_val; y = v.y /. module_val; z = v.z /. module_val }

    let norm (v: vec3) =
        Float.sqrt (v.x *. v.x +. v.y *. v.y +. v.z *. v.z)

    let norm_squared (v: vec3) =
        v.x *. v.x +. v.y *. v.y +. v.z *. v.z

    let cross (a: vec3) (b: vec3) =
        {x = (a.y *. b.z -. a.z *. b.y);
         y = (a.z *. b.x -. a.x *. b.z);
         z = (a.x *. b.y -. a.y *. b.x)}

    let dot (v1: vec3) (v2: vec3) =
        v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z

    let (+) (v1: vec3) (v2: vec3) =
        { x = v1.x +. v2.x; y = v1.y +. v2.y; z = v1.z +. v2.z }

    let (-) (v1: vec3) (v2: vec3) =
        { x = v1.x -. v2.x; y = v1.y -. v2.y; z = v1.z -. v2.z }

    let ( * ) (v1: vec3) (v2: vec3) =
        { x = v1.x *. v2.x; y = v1.y *. v2.y; z = v1.z *. v2.z }

    let (/) (v1: vec3) (v2: vec3) =
        { x = v1.x /. v2.x; y = v1.y /. v2.y; z = v1.z /. v2.z }

    let abs (v: vec3) : vec3 =
        {x = Float.abs v.x; y = Float.abs v.y; z = Float.abs v.z}

    let min (v0: vec3) (v1: vec3) : vec3 =
        {x = Float.min v0.x v1.x ;
         y = Float.min v0.y v1.y; 
         z = Float.min v0.z v1.z}

    let max (v0: vec3) (v1: vec3) : vec3 =
        {x = Float.max v0.x v1.x ;
         y = Float.max v0.y v1.y; 
         z = Float.max v0.z v1.z}

    let clamp (value: vec3) (low: vec3) (high: vec3): vec3 =
        max low (min value high)

    let mul_value (v: vec3) (a: float): vec3 =
        {x = v.x *. a; y = v.y *. a; z = v.z *. a}

    let distance (a: vec3) (b: vec3) =
        let c = a - b in
        norm c

    let distance_squared (a: vec3) (b: vec3) =
        let c = a - b in
        dot c c

    let print (v: vec3) =
        printf "{x = %s; " (Float.to_string v.x);
        printf "y = %s; " (Float.to_string v.y);
        printf "z = %s}\n " (Float.to_string v.z)
end

(* mat2 *)

module Mat22 = struct

    let equal (m0: mat22) (m1: mat22) : bool =
        Vec2.equal m0.ex m1.ex &&
        Vec2.equal m0.ey m1.ey 

    let identity : mat22 =
        {ex = {x=1.; y=0.}; 
         ey = {x=0.; y=1.}}

    let zero: mat22 =
        {ex = Vec2.zero; ey = Vec2.zero}

    let inverse (mat22: mat22) =
        let a = mat22.ex.x in
        let b = mat22.ey.x in
        let c = mat22.ex.y in
        let d = mat22.ey.y in
        let det = a *. d -. b *. c in
        if (Float.compare det 0.0) <> 0 then
            let det = 1. /. det in
            {ex = {x=det*.d; y=(-.det)*.c}; ey = {x=(-.det)*.b; y=det*.a}}
        else
            {ex = {x=det*.d; y=(-.det)*.c}; ey = {x=(-.det)*.b; y=det*.a}}

    let solve (mat22: mat22) (b: vec2) : vec2 =
        let a11 = mat22.ex.x in
        let a12 = mat22.ey.x in
        let a21 = mat22.ex.y in
        let a22 = mat22.ey.y in
        let det = a11 *. a22 -. a12 *. a21 in
        if (Float.compare det 0.0) <> 0 then
            let det = 1. /. det in
            let x = det *. (a22 *. b.x -. a12 *. b.y) in
            let y = det *. (a11 *. b.y -. a21 *. b.x) in
            {x = x; y = y}
        else
            let x = det *. (a22 *. b.x -. a12 *. b.y) in
            let y = det *. (a11 *. b.y -. a21 *. b.x) in
            {x = x; y = y}

    let mul_vec2 (a: mat22) (v: vec2) : vec2 =
        {x = a.ex.x *. v.x +. a.ey.x *. v.y;
         y = a.ex.y *. v.x +. a.ey.y *. v.y}

    let (+) (m0: mat22) (m1: mat22) : mat22 =
        {ex = Vec2.(+) m0.ex m1.ex; ey = Vec2.(+) m0.ey m1.ey}
        
    let (-) (m0: mat22) (m1: mat22) : mat22 =
        {ex = Vec2.(-) m0.ex m1.ex; ey = Vec2.(-) m0.ey m1.ey}

    let mul (m0: mat22) (m1: mat22) : mat22 =
        {ex = mul_vec2 m0 m1.ex;
         ey = mul_vec2 m0 m1.ey}

    (* a^T * b *)
    let mul_transpose (a: mat22) (b: mat22) : mat22 =
        let ex = {x = Vec2.dot a.ex b.ex; y = Vec2.dot a.ey b.ex} in
        let ey = {x = Vec2.dot a.ex b.ey; y = Vec2.dot a.ey b.ey} in
        {ex = ex; ey = ey}

    let print (mat: mat22) =
        Printf.printf "{ %f; %f}\n" mat.ex.x mat.ey.x;
        Printf.printf "{ %f; %f}\n" mat.ex.y mat.ey.y;
end

module Mat33 = struct

    let equal (m0: mat33) (m1: mat33) : bool =
        Vec3.equal m0.ex m1.ex &&
        Vec3.equal m0.ey m1.ey &&
        Vec3.equal m0.ez m1.ez  

    let identity : mat33 =
        {ex = {x=1.; y=0.; z=0.}; 
         ey = {x=0.; y=1.; z=0.}; 
         ez = {x=0.; y=0.; z=1.}}

    let zero: mat33 =
        {ex = Vec3.zero; ey = Vec3.zero; ez = Vec3.zero}

    let mul_vec2 (a: mat33) (v: vec2): vec2 =
        {x = a.ex.x *. v.x +. a.ey.x *. v.y;
         y = a.ex.y *. v.x +. a.ey.y *. v.y}

    let mul_vec3 (a: mat33) (v: vec3): vec3 =
        let v1 = Vec3.(+) (Vec3.mul_value a.ex v.x) (Vec3.mul_value a.ey v.y) in
        Vec3.(+) v1 (Vec3.mul_value a.ez v.z) 

    let solve (a: mat33) (b: vec3) : vec3 =
        let computeSolution det =
            {x = det *. Vec3.dot b    (Vec3.cross a.ey a.ez);
             y = det *. Vec3.dot a.ex (Vec3.cross b a.ez);
             z = det *. Vec3.dot a.ex (Vec3.cross a.ey b)}
        in
        let det = Vec3.dot a.ex (Vec3.cross a.ey a.ez) in
        if (Float.compare det 0.0) <> 0 then
            let det = 1. /. det in
            computeSolution det
        else
            computeSolution det

    let solve22 (a: mat33) (b: vec2): vec2 =
        let a11 = a.ex.x in
        let a12 = a.ey.x in
        let a21 = a.ex.y in
        let a22 = a.ey.y in
        let computeSolution det =
            {x = det *. (a22 *. b.x -. a12 *. b.y);
             y = det *. (a11 *. b.y -. a21 *. b.x)}
        in
        let det = a11 *. a22 -. a12 *. a21 in
        if (Float.compare det 0.0) <> 0 then
            let det = 1. /. det in
            computeSolution det
        else
            computeSolution det

    let inverse22 (m0: mat33) : mat33 =
        let a = m0.ex.x in
        let b = m0.ey.x in
        let c = m0.ex.y in
        let d = m0.ey.y in
        let computeSolution det =
            {ex = {x = det *. d; y = (-.det) *. c; z = 0.};
             ey = {x = (-.det) *. b; y = det *. a; z = 0.};
             ez = {x = 0.; y = 0.; z = 0.}}
        in
        let det = a *. d -. b *. c in
        if (Float.compare det 0.0) <> 0 then
            let det = 1. /. det in
            computeSolution det
        else
            computeSolution det

    let sym_inverse (m0: mat33): mat33 =
        let computeSolution det =
            let a11 = m0.ex.x in
            let a12 = m0.ey.x in
            let a13 = m0.ez.x in
            let a22 = m0.ey.y in
            let a23 = m0.ez.y in
            let a33 = m0.ez.z in
            let ex = {
                x = det *. (a22 *. a33 -. a23 *. a23);
                y = det *. (a13 *. a23 -. a12 *. a33);
                z = det *. (a12 *. a23 -. a13 *. a22)
            } in
            let ey = {
                x = ex.y;
                y = det *. (a11 *. a33 -. a13 *. a13);
                z = det *. (a13 *. a12 -. a11 *. a23)
            } in
            let ez = {
                x = ex.z ;
                y = ey.z ;
                z = det *. (a11 *. a22 -. a12 *. a12)
            } in
            {ex = ex; ey = ey; ez = ez}
        in
        let det = Vec3.dot m0.ex (Vec3.cross m0.ey m0.ez) in
        if (Float.compare det 0.0) <> 0 then
            let det = 1. /. det in
            computeSolution det
        else
            computeSolution det
end

(* rotation functions *)

module Rotation = struct

    let equal rot0 rot1 =
        float_eq rot0.sine rot1.sine &&
        float_eq rot0.cosine rot1.cosine

    let create angle =
        let sine = Float.sin angle in
        let cosine = Float.cos angle in
        { sine; cosine }

    let identity =
        { sine = 0.; cosine = 1. }

    let angle { sine; cosine } =
        Float.atan2 sine cosine

    let x_axis { cosine; sine } =
        { x = cosine; y = sine }

    let y_axis { cosine; sine } =
        { x = -.sine; y = cosine }

    let ( * ) q r =
        {sine   = q.sine *. r.cosine +. q.cosine *. r.sine;
         cosine = q.cosine *. r.cosine -. q.sine *. r.sine}

    let mul_transpose q r =
        {sine = q.cosine *. r.sine -. q.sine *. r.cosine;
         cosine = q.cosine *. r.cosine +. q.sine *. r.sine}

    let print (v: rotation) =
        printf "{sine = %s; " (Float.to_string v.sine);
        printf "cosine = %s}\n " (Float.to_string v.cosine);


end

(* transform functions *)

module Transform = struct
    let create position angle =
        {position=position; rot = Rotation.create angle}

    let identity = 
        {position = Vec2.zero; rot = Rotation.identity}

    let mul_vec2 (t: transform) (v: vec2) =
        {x = (t.rot.cosine *. v.x -. t.rot.sine *. v.y) +. t.position.x;
         y = (t.rot.sine   *. v.x +. t.rot.cosine *. v.y) +. t.position.y}

    let mul_vec2_T (t: transform) (v: vec2) =
        let px = v.x -. t.position.x in
        let py = v.y -. t.position.y in
        {x = t.rot.cosine *. px *. t.rot.sine *. py;
         y = (-.t.rot.sine) *. px +. t.rot.cosine *. py}

    let ( * ) (a: transform) (b: transform) : transform =
        let rot = Rotation.( * ) a.rot b.rot in
        let position = Vec2.( + ) (Vec2.rotate b.position a.rot) a.position in
        {position = position; rot = rot}

    let mul_transpose (a: transform) (b: transform) : transform =
        let rot = Rotation.mul_transpose a.rot b.rot in
        let position = Vec2.rotate_inverse (Vec2.(-) b.position a.position) a.rot in
        {position = position; rot = rot}

end

module Sweep = struct
    let to_transform (sweep: sweep) (beta: float) : transform =
        let open Vec2 in
        let position = (mul_value sweep.c0 (1. -. beta)) + (mul_value sweep.c beta) in
        let angle = (1. -. beta) *. sweep.a0 +. beta *. sweep.a in
        let rot = Rotation.create angle in
        let origin = rotate sweep.local_center rot in
        {position =  position - origin; rot = rot}

    let advance (sweep: sweep) (alpha: float): sweep =
        let open Vec2 in
        let beta = (alpha -. sweep.alpha0) /. (1. -. sweep.alpha0) in
        let c0 = Vec2.mul_value (sweep.c - sweep.c0) beta in
        let a0 =  (sweep.a -. sweep.a0) *. beta in
        { local_center = sweep.local_center;
          c0 = c0; 
          c  = sweep.c;
          a0 = a0;
          a  = sweep.a;
          alpha0 = alpha}

    let normalize (sweep: sweep) : sweep =
        let twoPi = 2. *. Float.pi in
        let d = twoPi *. Float.floor(sweep.a0 /. twoPi) in
        { local_center = sweep.local_center;
          c0 = sweep.c0; 
          c  = sweep.c;
          a0 = sweep.a0 -. d;
          a  = sweep.a -. d;
          alpha0 = sweep.alpha0}

end
