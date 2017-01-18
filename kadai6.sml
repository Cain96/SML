use "lib.sml";

fun compute s mapL =
    let
	fun EXP nil = raise SyntaxError
          | EXP (h::t) =
            if isInt h then (toInt h, t)
            else if h = "(" then
                let
                    val (v1, t1) = EXP t
                    val (v2, t2) = EXP t1
                in
                    (v1, t2)
                end
            else if isAlp h then
                if h = "fact" orelse h = "fibo" then
                    FUNC (h::t)
                else
                    let
                        val h = findValue h mapL
                    in
                        (h, t)
                    end
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
            else if h = ")" then
                (0 ,t)
            else raise SyntaxError
    and FUNC nil = raise SyntaxError
          | FUNC (h::t) =
            if h = "fact" then
                let
                    val (v, t) = EXP t
                in
                    (fact v, t)
                end
            else if h = "fibo" then
                let
                    val (v, t) = EXP t
                in
                    (fibo v, t)
                end
            else raise SyntaxError
    and findValue s nil = raise NotDefined 
          | findValue s (h::t : (string*int) list) =
            if s = (#1 h) then
                #2 h
            else findValue s t;
    in
        let
            val (result,rest) = EXP (separate s)
        in
            if rest = nil then result else raise SyntaxError
        end
    end;