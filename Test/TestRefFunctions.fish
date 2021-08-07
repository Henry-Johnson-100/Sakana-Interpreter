fish complicate >(a,b,c)> <(<(fin >(== >(a,b)>,+ >(a, <(+ >(b,c)>)<)>)>)<<(fin >(>= >(a,c)>,<(+ >(b,<(* >(a,c)>  )<)>)<)>)<<(* >(a,<(* >(b,c)>)<)>)<)<

fish factorial >(n)> <(fin >(== <(n,0)<, 1)> <(* >(n, <(factorial >(<(- >(n,1)>)<)>)<)>)<)<