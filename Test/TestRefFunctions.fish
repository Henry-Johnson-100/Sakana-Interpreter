

fish to_bool

  >(x)>

  <(
    fin
    >(x)>
    >(True)>
    >(False)>
  )<



fish or

  >(x)>
  >(y)>

  <(
    fin
    >(x)>
    >(True)>
    >(to_bool >(y)>)>
  )<



fish and

  >(x)>
  >(y)>

  <(
    fin
    >(x)>
    >(to_bool >(y)>)>
    >(False)>
  )<



fish not

  >(x)>

  <(
    fin
    >(to_bool >(x)>)>
    >(False)>
    >(True)>
  )<



fish factorial

  >(n)>
  >(
      fish fact_st

        >(n)>
        >(prod)>
        >(new_n <(- >(n)> >(1)>)<)>
        >(new_prod <(* >(prod)> >(new_n)>)<)>

        <(
          fin
          >(<=
            >(n)>
            >(0)>
          )>
          >(prod)>
          >(fact_st >(new_n)> >(new_prod)>)>
        )<
  )>

  <(fact_st >(n)> >(1)>)<



fish main

  >(main_arg)>
  >(
    _x_ <(
    not >(main_arg)>
    )<
  )>
  >(
    __x__ <(
    not >(_x_)>
    )<
  )>

  <(
    fin
    >(__x__)>
    >("That's right")>
    >("That's wrong")>
  )<



<(
  or >(False)> >(to_bool >(1)>)>
  /*Should return True*/
)<