open Base

let is_number ch = Char.to_int '0' <= ch && ch <= Char.to_int '9'

let tokenize str =
  let rec step str idx acc =
    let open Token in
    let integer_of_acc () = Integer (Int.of_string acc) in

    if String.length str <= idx then
      if String.is_empty acc then [] else [ integer_of_acc () ]
    else
      let ch = str.[idx] in
      if is_number (Char.to_int ch) then
        step str (idx + 1) (acc ^ Char.escaped ch)
      else
        let num =
          match String.is_empty acc with
          | true -> None
          | false -> Some (integer_of_acc ())
        in
        let tok = match ch with ' ' -> None | _ -> Some (Token.of_char ch) in
        List.filter_opt [ num; tok ] @ step str (idx + 1) ""
  in
  step str 0 ""
