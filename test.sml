fun findValue s nil = raise NotDefined 
    | findValue s (h::t : (string*int) list) =
        if s = (#1 h) then
            #2 h
        else findValue s t;
