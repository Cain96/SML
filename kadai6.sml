use "lib.sml";

fun compute s mapL =
    let
    fun EXP nil = raise SyntaxError
          | EXP (h::t) =
            if isInt h then 
                (toInt h, t)
            else if h = "(" then 
                if isOpr (hd t) then 
                    COMP t
                else if isAlp (hd t) then 
                    FUNC t
                else
                    raise SyntaxError
            else if isAlp h then
                (findValue h mapL, t)
            else raise SyntaxError

    and COMP nil = raise SyntaxError
          | COMP (h::t) =
            if h = "+" then
                let
                    val (v1,t1) = EXP t
                    val (v2,t2) = EXP t1
                in
                    if hd t2 = ")" then
                        (v1 + v2, tl t2)
                    else
                        raise SyntaxError
                end
            else if h = "-" then
                let
                    val (v1,t1) = EXP t
                    val (v2,t2) = EXP t1
                in
                    if hd t2 = ")" then
                        (v1 - v2, tl t2)
                    else
                        raise SyntaxError
                end
            else if h = "*" then
                let
                    val (v1,t1) = EXP t
                    val (v2,t2) = EXP t1
                in
                    if hd t2 = ")" then
                        (v1 * v2, tl t2)
                    else
                        raise SyntaxError
                end
            else if h = "/" then
                let
                    val (v1,t1) = EXP t
                    val (v2,t2) = EXP t1
                in
                    if hd t2 = ")" then
                        (v1 div v2, tl t2)
                    else
                        raise SyntaxError
                end
            else raise SyntaxError
    and FUNC nil = raise SyntaxError
      | FUNC (h::t) =
        if h = "fact" then
            let
                val (v, t1) = EXP t
            in
                if hd t1 = ")" then
                    (fact v, tl t1)
                else raise SyntaxError
            end
        else if h = "fibo" then
            let
                val (v, t1) = EXP t
            in
                if hd t1 = ")" then
                    (fibo v, tl t1)
                    else raise SyntaxError
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