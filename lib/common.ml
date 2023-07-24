let length_units_per_meter = 1.0 
let max_polygon_vertices = 8 
let max_manifold_points  = 2 
let aabb_extension = 0.1 *. length_units_per_meter 
let aabb_multiplier  = 4. 
let linear_slop = 0.005 *. length_units_per_meter 
let angular_slop  = 2.0 /. 180.0 *. Float.pi 
let polygon_radius = 2.0 *. linear_slop 
let max_sub_steps = 8 
let max_TOI_contacts = 32 
let max_linear_correction = 0.2 *. length_units_per_meter 
let max_angular_correction = 8.0 /. 180.0 *. Float.pi 
let max_translation = 2.0 *. length_units_per_meter 
let max_translation_squared = max_translation *. max_translation 
let max_rotation = 0.5 *. Float.pi 
let max_rotation_squared = max_rotation *. max_rotation 
let baumgarte =  0.2 
let toi_baumgarte = 0.75 
let time_to_sleep = 0.5 
let linear_sleep_tolerance = 0.01 *. length_units_per_meter 
let angular_Sleep_tolerance = 2.0 /. 180.0 *. Float.pi 

let epsilon = 1e-6

