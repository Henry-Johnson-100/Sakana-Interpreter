/*
fish id
    >(x)>
    <(x)<
*/
fish not >(x)> <(fin >(x)> >(False)> >(True)>)<
/*
fish and
    >(x)>
    >(y)>
    <(
        fin
            >(x)>
            >(fin
                >(y)>
                >(True)>
                >(False)>
            )>
            >(False)>
    )<

fish or
    >(x)>
    >(y)>
    <(
        fin
        >(x)>
        >(True)>
        >(fin
            >(y)>
            >(True)>
            >(False)>
        )>

    )<

fish to_bool
    >(x)>
    <(
        not >(not >(x)>)>
    )<

*/

<( not >(True)>
)<
