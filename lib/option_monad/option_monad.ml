let return x = Some x
let fail = None
let bind = Option.bind
let ( >>= ) = bind
let ( let* ) = bind
let has_failed = Option.is_none
let run = Option.get
