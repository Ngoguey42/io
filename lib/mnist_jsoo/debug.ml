let verbose_train = false

let verbose_test = false

let verbose_construct_mount_unmount = true

let verbose_render = false

let on_construct name = if verbose_construct_mount_unmount then Printf.printf "$ C | %s\n%!" name

let on_mount name = if verbose_construct_mount_unmount then Printf.printf "$ M | %s\n%!" name

let on_render name = if verbose_render then Printf.printf "$ r | %s\n%!" name

let on_unmount name = if verbose_construct_mount_unmount then Printf.printf "$ U | %s\n%!" name
