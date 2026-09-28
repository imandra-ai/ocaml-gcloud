type 'a t = 'a

let return x = x
let bind x f = f x
let catch f handler = try f () with e -> handler e
let sleep seconds = Picos_std_structured.Control.sleep ~seconds
