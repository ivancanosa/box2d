open Base
open Stdio
open Box2D

let () =
    let open Box2D in
    let a = Vec3.zero in
    printf "Hello, World! %s\n" (Float.to_string a.x)
