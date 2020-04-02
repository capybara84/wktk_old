
type 'a t = (string * 'a) list

let empty = []

let extend id value env =
    (id, value) :: env

let lookup id env =
    List.assoc id env
