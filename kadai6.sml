use "lib.sml";

fun compute s mapL =
    let
        fun EXP (nil, f) = raise SyntaxError
          | EXP (h::t, f) =
            if isInt h then 
                (toInt h, t, f)
            else if h = "(" then
                let
                    val f = f + 1
                    val (v1, t1, f) = EXP (t, f)
                    val (v2, t2, f) = EXP (t1, f)
                    val f = f - 1
                in
                    (v1, t2, f)
                end
            else if isAlp h then
                if h = "fact" orelse h = "fibo" then
                    FUNC (h::t, f)
                else
                    let
                        val h = findValue h mapL
                    in
                        (h, t, f)
                    end
            else if isOpr h then COMP (h::t, f)
            else raise SyntaxError
    and COMP (nil, f) = raise SyntaxError
          | COMP (h::t, f) =
            if f > 0 then
                if h = "+" then
                    let
                        val (v1,t1, f) = EXP (t, f)
                        val (v2,t2, f) = EXP (t1, f)
                    in
                        (v1 + v2, t2, f)
                    end
                else if h = "-" then
                    let
                        val (v1,t1, f) = EXP (t, f)
                        val (v2,t2, f) = EXP (t1, f)
                    in
                        (v1 - v2, t2, f)
                    end
                else if h = "*" then
                    let
                        val (v1,t1, f) = EXP (t, f)
                        val (v2,t2, f) = EXP (t1, f)
                    in
                        (v1 * v2, t2, f)
                    end
                else if h = "/" then
                    let
                        val (v1,t1, f) = EXP (t, f)
                        val (v2,t2, f) = EXP (t1, f)
                    in
                        (v1 div v2, t2, f)
                    end
                else if h = ")" then
                    (0 ,t, f)
                else raise SyntaxError
            else raise SyntaxError
    and FUNC (nil, f) = raise SyntaxError
          | FUNC (h::t, f) =
            if f > 0 then
                if h = "fact" then
                    let
                        val (v, t, f) = EXP (t, f)
                    in
                        (fact v, t, f)
                    end
                else if h = "fibo" then
                    let
                        val (v, t, f) = EXP (t, f)
                    in
                        (fibo v, t, f)
                    end
                else raise SyntaxError
            else raise SyntaxError
    and findValue s nil = raise NotDefined 
          | findValue s (h::t : (string*int) list) =
            if s = (#1 h) then
                #2 h
            else findValue s t;
    in
        let
            val (result,rest, f) = EXP (separate s, 0)
        in
            if rest = nil andalso f = 0 then result else raise SyntaxError
        end
    end;