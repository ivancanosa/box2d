open Box2d
open Base
open Math

let epsilon = 1e-6

let float_eq (f0: float) (f1: float) =
    (Float.compare (Float.abs (f1 -. f0)) epsilon) <= 0

(* test general math functions *)

let%test "next_power_of_two_test_1" =
    Math.next_power_of_two 3 = 4
let%test "next_power_of_two_test_2" =
    Math.next_power_of_two 16 = 32
let%test "next_power_of_two_test_3" =
    Math.next_power_of_two 1024 = 2048

let%test "is_power_of_two_test_1" =
    Bool.equal (Math.is_power_of_two 16)  true
let%test "is_power_of_two_test_2" =
    Bool.equal (Math.is_power_of_two 15) false
let%test "is_power_of_two_test_3" =
    Bool.equal (Math.is_power_of_two 64) true

let%test "is_valid_test_0" =
    Bool.equal (Math.is_valid 0.) true
let%test "is_valid_test_1" =
    Bool.equal (Math.is_valid 3.14) true
let%test "is_valid_test_2" =
    Bool.equal (Math.is_valid Float.nan)  false
let%test "is_valid_test_3" =
    Bool.equal (Math.is_valid Float.infinity) false

(* test Vec2 *)

let vec2_equal (v1: vec2) (v2: vec2) =
    float_eq v1.x v2.x &&
    float_eq v1.y v2.y

let%test "identity_test" =
    vec2_equal Vec2.identity {x = 1.; y = 0.}

let%test "zero_test" =
    vec2_equal Vec2.zero {x = 0.; y = 0.}

let%test "nth_test" =
    (Float.compare (Vec2.nth {x = 3.14; y = 2.71} 0)  3.14) = 0 &&
    Float.compare (Vec2.nth {x = 3.14; y = 2.71} 1)  2.71 = 0

let%test "nth_set_test" =
    let v = {x = 3.14; y = 2.71} in
    vec2_equal (Vec2.nth_set v 0 1.0) {x = 1.0; y = 2.71} &&
    vec2_equal (Vec2.nth_set v 1 5.0) {x = 3.14; y = 5.0}

let%test "is_valid_test" =
    Vec2.is_valid {x = 1.0; y = 2.0}

let%test "normalize_test_0" =
    let v = {x = 3.0; y = 4.0} in
    vec2_equal (Vec2.normalize v) {x = 0.6; y = 0.8}
let%test "normalize_test_1" =
    let v = {x = 0.0; y = 1.0} in
    vec2_equal (Vec2.normalize v) {x = 0.; y = 1.}
let%test "normalize_test_2" =
    let v = {x = 1.0; y = 0.0} in
    vec2_equal (Vec2.normalize v) {x = 1.; y = 0.}
let%test "normalize_test_2" =
    let v = {x = 0.0; y = 0.0} in
    vec2_equal (Vec2.normalize v) {x = 0.; y = 0.}

let%test "norm_test" =
    Float.compare (Vec2.norm {x = 3.0; y = 4.0})  5.0 = 0

let%test "norm_squared_test" =
    Float.compare (Vec2.norm_squared {x = 3.0; y = 4.0}) 25.0 = 0

let%test "dot_test" =
    Float.compare (Vec2.dot {x = 2.0; y = 3.0} {x = 4.0; y = 5.0})  23.0 = 0

let%test "cross_vec2_test" =
    Float.compare (Vec2.cross_vec2 {x = 2.0; y = 3.0} {x = 4.0; y = 5.0}) (-2.0) = 0

let%test "cross_float_test" =
    let v = {x = 2.0; y = 3.0} in
    vec2_equal (Vec2.cross_float v 2.0) {x = 6.0; y = (-4.0)}

let%test "cross_inv_test" =
    let v = {x = 2.0; y = 3.0} in
    vec2_equal (Vec2.cross_inv 2.0 v) {x = (-6.0); y = 4.0}

let%test "add_test" =
    let open Vec2 in
    let v1 = {x = 2.0; y = 3.0} in
    let v2 = {x = 4.0; y = 5.0} in
    vec2_equal (v1 + v2) {x = 6.0; y = 8.0}

let%test "subtract_test" =
    let open Vec2 in
    let v1 = {x = 4.0; y = 5.0} in
    let v2 = {x = 2.0; y = 3.0} in
    vec2_equal (v1 - v2) {x = 2.0; y = 2.0}

let%test "multiply_test" =
    let open Vec2 in
    let v1 = {x = 2.0; y = 3.0} in
    let v2 = {x = 4.0; y = 5.0} in
    vec2_equal (v1 * v2) {x = 8.0; y = 15.0}

let%test "divide_test" =
    let open Vec2 in
    let v1 = {x = 4.0; y = 6.0} in
    let v2 = {x = 2.0; y = 3.0} in
    vec2_equal (v1 / v2) {x = 2.0; y = 2.0}

let%test "abs_test" =
    let open Vec2 in
    let v = {x = (-2.0); y = 3.0} in
    vec2_equal (abs v) {x = 2.0; y = 3.0}

let%test "min_test" =
    let open Vec2 in
    let v0 = {x = 2.0; y = 3.0} in
    let v1 = {x = 1.0; y = 4.0} in
    vec2_equal (min v0 v1) {x = 1.0; y = 3.0}

let%test "max_test" =
    let open Vec2 in
    let v0 = {x = 2.0; y = 3.0} in
    let v1 = {x = 1.0; y = 4.0} in
    vec2_equal (max v0 v1) {x = 2.0; y = 4.0}

let%test "clamp_test" =
    let open Vec2 in
    let value = {x = 2.0; y = 3.0} in
    let low = {x = 1.0; y = 2.5} in
    let high = {x = 2.5; y = 3.5} in
    vec2_equal (clamp value low high) {x = 2.0; y = 3.0}

let%test "distance_test" =
    let open Vec2 in
    let a = {x = 1.0; y = 2.0} in
    let b = {x = 4.0; y = 6.0} in
    Float.(abs (distance a b - 5.) < epsilon)

let%test "mul_value_test" =
    let v = {x = 2.0; y = 3.0} in
    vec2_equal (Vec2.mul_value v 2.0) {x = 4.0; y = 6.0}

let%test "distance_squared_test" =
    let a = {x = 1.0; y = 2.0} in
    let b = {x = 4.0; y = 6.0} in
    Float.(abs (Vec2.distance_squared a b - 25.0) < epsilon)

let%test "rotate_90_degrees_test" =
    let v = { x = 1.0; y = 0.0 } in
    let q = { cosine = 0.0; sine = 1.0 } in
    let rotated_v = Vec2.rotate v q in
    vec2_equal rotated_v { x = 0.0; y = 1.0 }

let%test "rotate_180_degrees_test" =
    let v = { x = 1.0; y = 0.0 } in
    let q = { cosine = -1.0; sine = 0.0 } in
    let rotated_v = Vec2.rotate v q in
    vec2_equal rotated_v { x = -1.0; y = 0.0 }

let%test "rotate_inverse_90_degrees_test" =
    let v = { x = 0.0; y = 1.0 } in
    let q = { cosine = 0.0; sine = 1.0 } in
    let rotated_v = Vec2.rotate_inverse v q in
    vec2_equal rotated_v { x = 1.0; y = 0.0 }

let%test "rotate_inverse_180_degrees_test" =
    let v = { x = -1.0; y = 0.0 } in
    let q = { cosine = -1.0; sine = 0.0 } in
    let rotated_v = Vec2.rotate_inverse v q in
    vec2_equal rotated_v { x = 1.0; y = 0.0 }

(* test Vec3 *)

let vec3_equal (v1: vec3) (v2: vec3) : bool =
    float_eq v1.x v2.x 
  && float_eq v1.y v2.y 
  && float_eq v1.z v2.z 

let%test "identity_test" =
    let result = Vec3.identity in
    vec3_equal result {x = 1.0; y = 0.0; z = 0.0}

let%test "zero_test" =
    let result = Vec3.zero in
    vec3_equal result {x = 0.0; y = 0.0; z = 0.0}

let%test "nth_test" =
    let v = {x = 3.0; y = 4.0; z = 5.0} in
    Float.compare (Vec3.nth v 0) 3.0 = 0 && 
    Float.compare (Vec3.nth v 1) 4.0 = 0 && 
    Float.compare (Vec3.nth v 2) 5.0 = 0

let%test "nth_set_test" =
    let v = {x = 3.0; y = 4.0; z = 5.0} in
    let result = Vec3.nth_set v 1 10.0 in
    vec3_equal result {x = 3.0; y = 10.0; z = 5.0}

let%test "normalize_test" =
    let v = {x = 3.0; y = 4.0; z = 5.0} in
    let normalized_v = Vec3.normalize v in
    let norm = Vec3.norm normalized_v in
    float_eq norm 1.

let%test "cross_product_test_1" =
    let v1 = {x = 1.0; y = 0.0; z = 0.0} in
    let v2 = {x = 0.0; y = 1.0; z = 0.0} in
    let result = Vec3.cross v1 v2 in
    vec3_equal result {x = 0.0; y = 0.0; z = 1.0}

let%test "cross_product_test_2" =
    let v1 = {x = 1.0; y = 2.0; z = 3.0} in
    let v2 = {x = 4.0; y = 5.0; z = 6.0} in
    let result = Vec3.cross v1 v2 in
    let expected = {x = -3.0; y = 6.0; z = -3.0} in
    vec3_equal result expected

let%test "cross_product_test_3" =
    let v1 = {x = 2.0; y = 3.0; z = 4.0} in
    let v2 = {x = 5.0; y = 6.0; z = 7.0} in
    let result = Vec3.cross v1 v2 in
    let expected = {x = -3.0; y = 6.0; z = -3.0} in
    vec3_equal result expected

let%test "dot_test" =
    let v1 = {x = 1.0; y = 2.0; z = 3.0} in
    let v2 = {x = 4.0; y = 5.0; z = 6.0} in
    let result = Vec3.dot v1 v2 in
    float_eq result 32.0

let%test "add_test" =
    let v1 = {x = 1.0; y = 2.0; z = 3.0} in
    let v2 = {x = 4.0; y = 5.0; z = 6.0} in
    let result = Vec3.(+) v1 v2 in
    vec3_equal result {x = 5.0; y = 7.0; z = 9.0}

let%test "distance_test" =
    let v1 = {x = 1.0; y = 2.0; z = 3.0} in
    let v2 = {x = 4.0; y = 6.0; z = 8.0} in
    let result = Vec3.distance v1 v2 in
    float_eq result 7.0710678118655 

let%test "distance_squared_test" =
    let v1 = {x = 1.0; y = 2.0; z = 3.0} in
    let v2 = {x = 4.0; y = 6.0; z = 8.0} in
    let result = Vec3.distance_squared v1 v2 in
    float_eq result (7.0710678118655 **. 2.)

(* Mat22 *)

let%test "test_identity" =
    let id = Mat22.identity in
    vec2_equal id.ex {x=1.; y=0.} &&
    vec2_equal id.ey {x=0.; y=1.}

let%test "test_zero" =
    let zero = Mat22.zero in
    vec2_equal zero.ex Vec2.zero &&
    vec2_equal zero.ey Vec2.zero

let%test "test_inverse" =
    let mat = {ex = {x=2.; y=3.}; ey = {x=4.; y=5.}} in
    let inv_mat = Mat22.inverse mat in
    let product = Mat22.mul mat inv_mat in
    let identity = Mat22.identity in
    vec2_equal product.ex identity.ex &&
    vec2_equal product.ey identity.ey

let%test "test_inverse_identity" =
    (* The inverse of an identity matrix should be the identity matrix itself *)
    let identity_mat = Mat22.identity in
    let inv_identity_mat = Mat22.inverse identity_mat in
    Mat22.equal inv_identity_mat identity_mat

let%test "test_inverse_mul" =
    (* Test the property of matrix inverse: M * inverse(M) = identity *)
    let mat = {ex = {x=2.; y=3.}; ey = {x=4.; y=5.}} in
    let inv_mat = Mat22.inverse mat in
    let product = Mat22.mul mat inv_mat in
    let expected_product = Mat22.identity in
    Mat22.equal product expected_product

let%test "test_solve" =
    let mat = {ex = {x=2.; y=3.}; ey = {x=4.; y=5.}} in
    let b = {x=6.; y=7.} in
    let x = Mat22.solve mat b in
    let expected_x = {x=(-1.); y=2.} in
    vec2_equal x expected_x

let%test "test_solve_non_invertible_matrix" =
    let mat = {ex = {x=2.; y=3.}; ey = {x=2.; y=3.}} in
    let b = {x=6.; y=7.} in
    let x = Mat22.solve mat b in
    vec2_equal x {x=0.; y=0.}

let%test "test_solve_zero_matrix" =
    let mat = Mat22.zero in
    let b = {x=6.; y=7.} in
    let x = Mat22.solve mat b in
    (* Since the matrix is the zero matrix, the result should be (nan, nan) *)
    vec2_equal x {x=Float.nan; y=Float.nan}

let%test "test_add" =
    let mat1 = {ex = {x=1.; y=2.}; ey = {x=3.; y=4.}} in
    let mat2 = {ex = {x=5.; y=6.}; ey = {x=7.; y=8.}} in
    let sum = Mat22.(+) mat1 mat2 in
    let expected_sum = {ex = {x=6.; y=8.}; ey = {x=10.; y=12.}} in
    Mat22.equal sum expected_sum

let%test "test_subtract" =
    let mat1 = {ex = {x=1.; y=2.}; ey = {x=3.; y=4.}} in
    let mat2 = {ex = {x=5.; y=6.}; ey = {x=7.; y=8.}} in
    let diff = Mat22.(-) mat1 mat2 in
    let expected_diff = {ex = {x=(-4.); y=(-4.)}; ey = {x=(-4.); y=(-4.)}} in
    Mat22.equal diff expected_diff

let%test "test_mul_vec2" =
    let mat = {ex = {x=2.; y=3.}; ey = {x=4.; y=5.}} in
    let vec = {x=6.; y=7.} in
    let result = Mat22.mul_vec2 mat vec in
    let expected_result = {x=6.*.2. +. 7.*.4.; y=6.*.3. +. 7.*.5.} in
    vec2_equal result expected_result

let%test "test_mul" =
    let mat1 = {ex = {x=2.; y=3.}; ey = {x=4.; y=5.}} in
    let mat2 = {ex = {x=6.; y=7.}; ey = {x=8.; y=9.}} in
    let result = Mat22.mul mat1 mat2 in
    let expected_result = {ex = {x=40.; y=53.}; ey = {x=52.; y=69.}} in
    Mat22.equal result expected_result

let%test "test_mul_transpose" =
    let mat1 = {ex = {x=2.; y=3.}; ey = {x=4.; y=5.}} in
    let mat2 = {ex = {x=6.; y=7.}; ey = {x=8.; y=9.}} in
    let result = Mat22.mul_transpose mat1 mat2 in
    let expected_result = {ex = {x=33.; y=59.}; ey = {x=43.; y=77.}} in
    Mat22.equal result expected_result

(* Mat33 *)

let%test "mat33_identity_test" =
    let identity_mat = Mat33.identity in
    let expected_mat = {ex = {x=1.; y=0.; z=0.}; ey = {x=0.; y=1.; z=0.}; ez = {x=0.; y=0.; z=1.}} in
    Mat33.equal identity_mat expected_mat

let%test "mat33_mul_vec2_test" =
    let m = {ex = {x=2.; y=3.; z=3.}; ey = {x=4.; y=5.; z=6.}; ez = {x=7.; y=8.; z=9.}} in
    let v = {x=6.; y=7.} in
    let result = Mat33.mul_vec2 m v in
    vec2_equal result {x=6.*.2. +. 7.*.4.; y=6.*.3. +. 7.*.5.}

let%test "mat33_mul_vec3_test" =
    let m = {ex = {x=1.; y=2.; z=3.}; ey = {x=4.; y=5.; z=6.}; ez = {x=7.; y=8.; z=9.}} in
    let v = {x=1.; y=2.; z=3.} in
    let result = Mat33.mul_vec3 m v in
    vec3_equal result {x=30.; y=36.; z=42.}

let%test "test_solve33" =
    let a = { ex = {x=1.0; y=2.0; z=3.0};
              ey = {x=4.0; y=25.0; z=6.0};
              ez = {x=7.0; y=8.0; z=9.0} } in
    let b = { x=1.0; y=4.0; z=12.0 } in
    let expected = { x=101./.16.; y=(-.1./.8.); z=(-.11./.16.) } in
    vec3_equal (Mat33.solve a b) expected

let%test "test_solve22" =
    let a = { ex = {x=2.0; y=3.0; z=3.0};
              ey = {x=4.0; y=5.0; z=6.0};
              ez = {x=7.0; y=8.0; z=9.0} } in
    let b = { x=6.0; y=7.0} in
    let expected = {x=(-.1.); y = 2.} in
    vec2_equal (Mat33.solve22 a b) expected

let%test "sym_inverse_test" =
    let m = { ex = { x = 1.0; y = 4.0; z = 7.0 };
            ey = { x = 4.0; y = 5.0; z = 8.0 };
            ez = { x = 7.0; y = 8.0; z = 9.0 } } in
    let inverse = Mat33.sym_inverse m in
    let expected = { ex = { x = -19./.40.; y = 0.5; z = -3./.40. };
                   ey = { x = 0.5; y = -1.; z = 0.5 };
                   ez = { x = -3./.40.; y = 0.5; z = -11./.40. } } in
    Mat33.equal inverse expected

(* Rotation *)

let%test "create_test" =
    let angle = Float.pi /. 4. in
    let expected_rotation = { sine = Float.sin angle; cosine = Float.cos angle } in
    let result_rotation = Rotation.create angle in
    Rotation.equal expected_rotation result_rotation

let%test "identity_test" =
    let expected_rotation = { sine = 0.; cosine = 1. } in
    let result_rotation = Rotation.identity in
    Rotation.equal expected_rotation result_rotation

let%test "angle_test" =
    let rotation = { sine = 0.70710678; cosine = 0.70710678 } in
    let expected_angle = Float.pi /. 4. in
    let result_angle = Rotation.angle rotation in
    float_eq expected_angle result_angle

let%test "x_axis_test" =
    let rotation = { sine = 0.70710678; cosine = 0.70710678 } in
    let expected_axis = { x = 0.70710678; y = 0.70710678 } in
    let result_axis = Rotation.x_axis rotation in
    vec2_equal expected_axis result_axis

let%test "y_axis_test" =
    let rotation = { sine = 0.70710678; cosine = 0.70710678 } in
    let expected_axis = { x = -0.70710678; y = 0.70710678 } in
    let result_axis = Rotation.y_axis rotation in
    vec2_equal expected_axis result_axis

