let balanced string =
    let matches x y =
        let dic = dict[')', '(';']', '[';'}', '{']
        dic.ContainsKey x && dic.[x] = y
    let rec balanced xs ys = 
        match xs with
        | [] -> List.isEmpty ys
        | [x] -> 
            match ys with
            | [y] -> matches x y
            | _ -> false
        | x::xs -> 
            match ys with
            | [] -> balanced xs [x]
            | [y] -> matches x y && balanced xs [] || balanced xs ([x;y])
            | y::ys -> matches x y && balanced xs ys || balanced xs (x::y::ys)
    balanced (Seq.toList string) []

balanced "{{)(}}"       // false
balanced "({)}"         // false
balanced "[({})]"       // true
balanced "{}([])"       // true
balanced "{()}[[{}]]"   // true