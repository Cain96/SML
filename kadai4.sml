use "lib.sml";

fun compute s =
    let
	fun EXP nil = raise SyntaxError
          | EXP (h::t) =
            if isInt h then (toInt h, t)
            else if isOpr h then COMP (h::t)
            else raise SyntaxError

	and COMP nil = raise SyntaxError
          | COMP (h::t) =
            if h = "+" then
                let
                    val (v1,t1) = EXP t
                    val (v2,t2) = EXP t1
                in
                    (v1 + v2, t2)
                end
            else if h = "-" then
                let
                    val (v1,t1) = EXP t
                    val (v2,t2) = EXP t1
                in
                    (v1 - v2, t2)
                end
            else if h = "*" then
                let
                    val (v1,t1) = EXP t
                    val (v2,t2) = EXP t1
                in
                    (v1 * v2, t2)
                end
            else if h = "/" then
                let
                    val (v1,t1) = EXP t
                    val (v2,t2) = EXP t1
                in
                    (v1 div v2, t2)
                end
            else if h = "(" then
                let
                    val (v1, t1) = EXP t
                    val (v2, t2) = EXP t1
                in
                    (v1, t2)
                end
            else if h = ")" then
                (0 ,t)
            else raise SyntaxError
    in
        let
            val (result,rest) = EXP (separate s)
        in
            if rest = nil then result else raise SyntaxError
        end
    end;