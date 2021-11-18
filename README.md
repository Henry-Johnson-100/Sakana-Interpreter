# Sakana - 0.3.0.0

# 0.3.0.0 is a non-functional build. Use it to preview up-coming changes.

A syntax-tree based interpreter implementation for the Sakana programming language.

A functional programming language, heavily inspired by Haskell and subliminally inspired by Scheme.
Based on fish, using fishy words and fish braces `>()>` `<()<`

[TOC]



# Try it

To interpret a Sakana file, make sure you have Haskell's cabal installed
and you can run an executable using:

```cabal run Sakana path/to/your/Sakana/file.skn```

Any file that contains Sakana code can be executed but for posterity,
the file extensions, .skn, or .sakana are preferred.

you can also,

```cabal install```

inside the project directory to put a ```Sakana``` interpreter executable on your path.

I apologize in advance for cabal not building properly. You can always use ghc to compile
the executable files yourself.
The executable is: app/Main.hs

# What is Sakana?

Sakana is a functional programming language that is very (perhaps too) similar to Haskell and Scheme.
All of the syntax is fishy and code blocks are executed and their value returned depending on what kind of fish they reside in.
There are two types of fish, send fish `>()>`, and return fish `<()<`

Send fish provide information to the return fish they are immediately in front of.
A return fish will execute its code and return a value. That's the most fundamental rule of fish,
return fish always execute and return.

# Grammar

Below is a diagram of the grammar of Sakana as currently implemented:

Some of the primitives are defined with regex though this is not how they are actually
implemented, as such, the regex may not be totally accurate.

Following these grammar rules will ensure your program can be parsed but not necessarily
that it will behave in a predictable way.

```
<data-double>                      ::= -?\d+\.?\d*
<data-string>                      ::= \".*\"
<data-boolean>                     ::= True | False
<data-null>                        ::= 
<data>                             ::= <data-double> | <data-string> | <data-boolean> | <data-null>
<Id>                               ::= ([A-z]|_|\')*(\.?([A-z]|_|\'))*
<operator>                         ::= + | - | / | * | ^ | == | /= | < | > | <= | >=
<fish-send>                        ::= >( <Id> <bracket-return-expr> )>
<statement-func-decl-arg-contents> ::= <Id> | <statement-func-decl>
<statement-func-decl-arg>          ::= >( <statement-func-decl-arg-contents> )>
<statement-func-decl-val>          ::= <bracket-return-expr> | <expr-swim>
<statement-func-decl>              ::= fish <Id> <statement-func-decl-arg>* <statement-func-decl-val>
<bracket-send-expr>                ::= >( <expr> )> | >()>
<bracket-return-expr>              ::= <( <expr> )< | <()<
<expr-fin>                         ::= fin <bracket-send-expr>+
<expr-swim-procedure>              ::= <bracket-send-expr> | <fish-send>
<expr-swim>                        ::= swim <expr-swim-procedure>* <bracket-return-expr>
<expr-func-call>                   ::= <Id> <bracket-send-expr>*
<expr-operator>                    ::= <operator> <bracket-send-expr>+
<expr>                             ::= <expr-operator> | <expr-fin> | <expr-swim> | <expr-func-call> | <data>
<statement>                        ::= <satement-func-decl>
<sentence>                         ::= <expr> | <statement> | <bracket-return-expr>
<program>                          ::= <statement>* <bracket-return-expr> | <expr-swim> | <expr>
```

# Keywords

- fish
- swim
- fin

# Primitive Data Types

- Num
- String
- Boolean
- Null

# Operators

- \+
- \-
- \*
- /
- ==
- /= (not equal)
- \^
- \<
- \>
- \<=
- \>=

# Standard Library

- Trout
    - Sakana's print function, prints to stdout.
    - Usage: ```trout >(a)>```
        - Prints ```a``` and returns a null value.
- Herring
    - Like ```trout``` but prints to stderr
- Dolphin
    - Sakana's input function, reads from stdin.
    - Usage: ```dolphin >()>```
        - Where the function call itself will be replaced with a string of whatever the user entered.
        - Analogous to ```dolphin = hGetLine stdin``` in Haskell.
- Read
    - Read a Sakana primitive data type from a string.
        - Will output either a String, Num, or Boolean depending on what the input looks like.
        - Throws an error if the input can not be read to a type.
    - Usage: ```read >(str)>``` 
- Floor
    - Calculate the floor of a Num
    - Usage: ```floor >(n)>```

------


# Basic Arithmetic

```
<(+
    >(1)>
    >(1)>
)<
```
Returns 2.0

```
<(+
    >(1)>
    >(+
        >(2)>
        >(3)>
    )>
)<
```
Returns 6.0

## Boolean operations

```
<(==
    >(1.0)>
    >(1.0)>
)<
```
Returns True

# Control Flow

Control flow is done using the ```fin``` keyword.
fin is actually a function that takes three arguments:
- A Boolean value
- A value to return if True
- A value to return if False

The following code will return ```5.0``` if some variable ```n``` is equal to the string ```\"yes\"```
Otherwise, it will return ```0.0```

```
fin
    >(==
        >(n)>
        >(\"yes\")>
    )>
    >(5.0)>
    >(0.0)>
```

# Function Declaration

Functions are declared with the ```fish``` keyword.

Following the keyword, a function name must be given. After the name comes a list of send fish which contain the function's positional arguments as well as any necessary sub-function or variable declarations.

Here is an example for some simple Boolean logic:

```
fish and
    >(cond_x)>
    >(cond_y)>
    <(
        fin
            >(cond_x)>
            >(cond_y)>
            >(False)>
    )<
```

```
fish or
    >(cond_x)>
    >(cond_y)>
    <(
        fin
            >(cond_x)>
            >(True)>
            >(cond_y)>
    )<
```

So calling,

``` and >(True)> >(False)> ```
returns ```False```

Here is an example definition for factorial with a sub-function defined in the argument list:

```
fish factorial
    >(n)>
    >(
        fish sub_fact
            >(sub_n)>
            >(prod)>
            <(
                fin
                    >(<=
                        >(sub_n)>
                        >(0)>
                    )>
                    >(prod)>
                    >(sub_fact
                        >(-
                            >(sub_n)>
                            >(1)>
                        )>
                        >(*
                            >(prod)>
                            >(sub_n)>
                        )>
                    )>
            )<
    )>
    <(sub_fact
        >(n)>
        >(1)>
    )<
```

so here we can see that calling factorial is essentially just a wrapper for the hidden sub-function ```sub_fact```

It should also be noted that Sakana does not have a loop, so any looping must be defined recursively.



------

# IO

```
<(
    trout >("Printing this line...")>
)<
```

Will print the string ```Printing this line...``` and ouput a null value.

```
<(dolphin)<
```
Will simply return whatever the user enters as a string.

# Executing a Sakana program

So you have written a few functions and would like to call them in an executable Sakana file.

To do so, you must have an execution block in your program.
An execution block is defined as either a return fish ```<()<``` for when you want to perform a single operation.

Or you can use Sakana's new keyword, ```swim```!.

## Swim

This keyword allows you execute code procedurally in Sakana.
If you have worked with Haskell's IO monad, this concept is essentially the same.
There are a few options for calculations in a swim block:

* In a swim block, any value contained in a send fish will be executed 
    but ignored by the program output.

* A swim block will instead return the first value it finds inside a return fish.

* Additionally, swim blocks let you bind values to variables inside the scope of the swim block,
    to call them multiple times.
    * Binding values has the following syntax:
```
>(this_binding <(10)<)>
```

Where the binding is declared with a send fish, an id, and a value returned to that id via a return fish.

#### Example - procedural factorial

```
fish factorial
  >(n)>
  >(
    fish sub_fact
      >(n)>
      >(prod)>
      <(
        swim
        >(new_prod <(* >(prod)> >(n)>)<)>
        >(new_n <(- >(n)> >(1)>)<)>
        <(
          fin
          >(<= >(n)> >(0)>)>
          >(prod)>
          >(sub_fact >(new_n)> >(new_prod)>)>
        )<
      )<
  )>
  <(
    swim
    >(trout >("computing the factorial of: ")>)>
    >(trout >(n)>)>
    <(sub_fact >(n)> >(1)>)<
  )<

swim
>(output <(factorial >(30)>)<)>
>(trout >("The final value is: ")>)>
<(output)<
```

This program will produce output like:
```
computing the factorial of:
30.0
The final value is:
2.652528598121911e32
```

