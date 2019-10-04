module List = struct
  let concat_rev (xss : 'a list list) : 'a list =
    let rec aux acc xss =
      match xss with
      | (x :: xs) :: xss ->
        aux (x :: acc) (xs :: xss)
      | [] :: xss ->
        aux acc xss
      | [] ->
        acc
    in
    aux [] xss
end
