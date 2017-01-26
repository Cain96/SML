use "lib.sml";

fun compute s mapL =
    let
        fun EXP (nil, f, b) = raise SyntaxError
          | EXP (h::t, f, b) =
            if isInt h then 
                (toInt h, t, f, b)
            else if h = "(" then
                let
                    val f = f + 1
                    val b = true
                    val (v1, t1, f, b) = EXP (t, f, b)
                    val (v2, t2, f, b) = EXP (t1, f, b)
                    val f = f - 1
                in
                    (v1, t2, f, b)
                end
            else if isAlp h then
                if h = "fact" orelse h = "fibo" then
                    if b then
                        FUNC (h::t, f, b)
                    else
                        let
                            val b = false
                            val h = findValue h mapL
                        in
                            (h, t, f, b)
                        end
                else
                    let
                        val b = false
                        val h = findValue h mapL
                    in
                        (h, t, f, b)
                    end
            else if isOpr h then COMP (h::t, f, b)
            else raise SyntaxError
    and COMP (nil, f, b) = raise SyntaxError
          | COMP (h::t, f, b) =
            if f > 0 then
                if h = "+" then
                    let
                        val b = false
                        val (v1,t1, f, b) = EXP (t, f, b)
                        val (v2,t2, f, b) = EXP (t1, f, b)
                    in
                        (v1 + v2, t2, f, b)
                    end
                else if h = "-" then
                    let
                        val b = false
                        val (v1,t1, f, b) = EXP (t, f, b)
                        val (v2,t2, f, b) = EXP (t1, f, b)
                    in
                        (v1 - v2, t2, f, b)
                    end
                else if h = "*" then
                    let
                        val b = false
                        val (v1,t1, f, b) = EXP (t, f, b)
                        val (v2,t2, f, b) = EXP (t1, f, b)
                    in
                        (v1 * v2, t2, f, b)
                    end
                else if h = "/" then
                    let
                        val b = false
                        val (v1,t1, f, b) = EXP (t, f, b)
                        val (v2,t2, f, b) = EXP (t1, f, b)
                    in
                        (v1 div v2, t2, f, b)
                    end
                else if h = ")" then
                    (0 ,t, f, b)
                else raise SyntaxError
            else raise SyntaxError
    and FUNC (nil, f, b) = raise SyntaxError
          | FUNC (h::t, f, b) =
            if f > 0 then
                if h = "fact" then
                    let
                        val b = false
                        val (v, t, f, b) = EXP (t, f, b)
                    in
                        (fact v, t, f, b)
                    end
                else if h = "fibo" then
                    let
                        val b = false
                        val (v, t, f, b) = EXP (t, f, b)
                    in
                        (fibo v, t, f, b)
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
            val (result,rest, f, b) = EXP (separate s, 0, false)
        in
            if rest = nil andalso f = 0 then result else raise SyntaxError
        end
    end;