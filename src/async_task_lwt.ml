type 'a t = 'a Lwt.t

let return = Lwt.return
let bind = Lwt.bind
let catch = Lwt.catch
let sleep = Lwt_unix.sleep
