school Data >()> <(
    school Int >(n)> <(
        route getValue >()> <(n)<
        route like >(compare)> <(
           
        )<
    )<
    school String >(str)> <(
        route getValue >()> <(str)<
    )<
)<

route fromData >(d)> <(
    d.getValue >()>
)<

fish convertToDataString >(d)> <(
    Data >()> String >(<(d.getValue>()>)<)>
)<

fish mapToString >(ds)> <(
    map >(x, convertToDataString >(x)>, ds)>
)<

fish strip >(str)> <(
    reverse >( dropWhile >(isSpace, >(reverse >(dropWhile >(isSpace, str)>)>)>)>)>
)<