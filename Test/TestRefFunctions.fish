fish variable <(500)< /*The value 500 is bound to the id 'variable' in this way
     therefore: <(variable)< == <(variable >()>)< == <(500)< are all identical in value*/

fish not 
    >(x)> 
    <(
        fin >(
            >(x)>
            >(False)>
            >(True)>
        )>
    )<

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

fish to_bool /*Takes any truthy/falsy value and converts it to an explicit boolean*/
    >(x)>
    <(
        not >(not >(x)>)>
    )<


/*The below are all true statements*/
<(and
    >(1)>
    >(not >(0)>)>
)<

