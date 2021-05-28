open! IStd

let in_child = ref false

let update_status = ref (fun _ _ -> ())

let pid = ref (lazy (Unix.getpid ()))

let reset_pid () = pid := lazy (Unix.getpid ())

let get_pid () = Lazy.force !pid

let has_running_children = ref false
