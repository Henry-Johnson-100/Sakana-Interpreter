fish factorial <( >(n)> <(
        fin >(== >(n >()>,0)>, <(1)< )>
        * >(n >()>, factorial >(- >(n >()>, 1)>  )> )>
    )<
)<


#and here
n = >()> <(:Int)<
#so really, n is called like
n >()>

#so
* == * >(m,n)> <(m * n)<
#where
route * <( >(m,n)> <(m * n)< )<
#so
* >(2,3)> == >(2,3)> >(m,n)> <(m*n)<


#So just calling factorial with no arguments returns this:
<( >(n)> <(fin >(== >(n >()>,0)>, <(1)<)>, * >(n >()>, factorial >(- >(n >()>,1)>)>)>)<)<