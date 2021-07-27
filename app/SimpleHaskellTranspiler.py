import re

function_def_pat = re.compile("(?<=fish )(.*)(?=( |\n))")
fin_arg_pat = re.compile("(?<=fin )(.*)(?=,),(.*)(?=\s)")


def test_files():
    return(open("tests/Complex.fish", "r"), open("tests/Complex.hs", "w"))


def has_fish(line: str) -> bool:
    return "fish " in line


def has_fin(line: str) -> bool:
    return "fin " in line


def is_otherwise(line: str) -> bool:
    return line.endswith("\n")


def has_brackets(line: str) -> bool:
    return "<(" in line and ")<" in line


def get_file():
    read_path = input("Please input a path to the .fish file:\n")
    write_path = input("Please input a path to the destination folder:\n")
    return (open(read_path, "r"), open(write_path, "w"))


def replace_parens(line: str) -> str:
    if has_brackets(line):
        line = line.replace("<(", " ( ", 1).replace(")<", " ) ", 1)
    line = line.replace("<(", "").replace(
        ">(", "").replace(")<", "").replace(")>", "")
    if not has_brackets(line):
        return line
    return replace_parens(line)


def get_func_name(line: str) -> str:
    match = function_def_pat.search(line)
    function_with_args = match.group(0).split(" ")
    func_name = function_with_args[0]
    args = " ".join(function_with_args[1].split(","))
    return f"{func_name} {args}"


def get_guard_statement(line: str) -> str:
    match = fin_arg_pat.search(line)
    fin_args = match.group(0).split(",")
    return f"    | {fin_args[0]} = {fin_args[1]}"


def transpileFunction(fish_lines: list) -> str:
    match = None
    transpiled_haskell = str()
    for line in fish_lines:
        line = replace_parens(line)
        if has_fish(line):
            transpiled_haskell += f"{get_func_name(line)}\n"
        elif has_fin(line):
            transpiled_haskell += f"{get_guard_statement(line)}\n"
        elif line.isspace():
            continue
        elif is_otherwise(line):
            line = line.strip()
            otherwise_statement = f"    | otherwise = {line}"
            transpiled_haskell += f"{otherwise_statement}\n"
    return transpiled_haskell


def main():
    fileTuple = test_files()
    transpiled = transpileFunction(fileTuple[0].readlines())
    fileTuple[1].write(transpiled)
    fileTuple[0].close()
    fileTuple[1].close()


if __name__ == "__main__":
    main()
