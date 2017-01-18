fun findValue s nil = raise NotDefined 
    | findValue s (h::t) =
        if [リストの最初の組 h が対象の s に該当する？]
            then [対応する値]
        else [findValue を再帰的に利用してリストの後続を検索];
