# Sakana

A syntax-tree based interpreter implementation for the Sakana programming language.

A functional programming language, heavily inspired by Haskell and subliminally inspired by Scheme.
Based on fish, using fishy words and fish braces >()> <()<

# Try it

To interpret a Sakana file, make sure you have Haskell's cabal installed
and you can run an executable using:

```cabal run Sakana path/to/your/Sakana/file.skn```

Any file that contains Sakana code can be executed but for posterity,
the file extensions, .skn, or .sakana are preferred.

You can also peak at the file's tree structure using:

```cabal run SakanaST path/```

I apologize in advance for cabal not building properly. You can always use ghc to compile
the executable files yourself.
The executables are app/Main.hs and app/PTIO.hs

# What is Sakana?

Sakana is a functional programming language that is very (perhaps too) similar to Haskell and Scheme.
All of the syntax is fishy and code blocks are executed and their value returned depending on what kind of fish they reside in.
There are two types of fish, send fish ">()>", and return fish "<()<"

Send fish provide information to the return fish they are immediately in front of.
A return fish will execute its code and return a value. That's the most fundamental rule of fish,
return fish always execute and return.

## Keywords

- fish
- fin

## Primitive Data Types

- Num
- String
- Boolean
- Null

## Operators

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

## Standard Library

- Trout
    - Sakana's print function.
    - Usage: ```trout >(a)> >(b)>```
        - Where ```a``` is the value that is printed and ```b``` is the operation carried out after printing ```a```
        - Analagous to: ```trout a b = hPutStrLn stdout a >> b``` in Haskell.
- Dolphin
    - Sakana's input function
    - Usage: ```dolphin >()>```
        - Where the function all itself will be replaced with a string of whatever the user entered.
        - Analagous to ```dolphin = hGetLine stdin``` in Haskell.
- Encrust
    - A way to expose IO or other values to a scope without defining a function.
    - Usage: ```encrust >(id >(a)>)> >(b)>```
    - Where ```a``` is a value bound to the function ```id``` in the scope where ```b``` is executed.
        - See code examples for usage of ```encrust```.

# Sample Code

## Basic Arithmetic

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

### Boolean operations

```
<(==
    >(1.0)>
    >(1.0)>
)<
```
Returns True

## Control Flow

Control flow is done using the ```fin``` keyword.
fin is actually a function that takes three arguments:
- A boolean value
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

## Function Declaration

Functions are declared with the ```fish``` keyword.

Following the keyword, a function name must be given. After the name comes a list of send fish which contain the function's positional arguments as well as any necessary sub-function or variable declarations.

Here is an example for some simple boolean logic:

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

Hope you enjoyed your quick primer on Sakana!

## IO

```
<(
    trout >("Printing this line...")> >("returning this line")>
)<
```

Will print the string ```Printing this line...``` and ouput the string ```returning this line```.

The output from the second argument can be used in other contexts.
For example, as print flags in a ```fin``` statement:

```
<(
    fin
    >(
        trout >("Is x greater than y?")>
        >(> >(x)> >(y)>)>
    )>
    >(
        trout >("Yes it is!")>
        >(- >(x)> >(y)>)>
    )>
    >(
        trout >("No it's not..")>
        >(+ >(x)> >(y)>)>
    )>

)<
```

Below is a function to concatenate two entered strings with a space in the middle:

```
<(
    +
    >(trout >("Please enter the first string")>
        >(dolphin >()>)>
    )>
    >(trout >("Please enter the second string")>
        >(+ 
            >(" ")>
            dolphin >()>
        )>
    )>
)<
```

Here is a function demonstrating the use of ```encrust``` for binding IO values, 
and perhaps, also demonstrating a hint of the depths of ugliness and depravity you can dive to with Sakana code.
```
<(
  trout >("If you enter a string, I will show how you can use 'encrust' to bind a value to an id in a given scope: ")>
  >(
    encrust >(x >(dolphin >()>)>)>
      >(
        trout >(+ 
          >("You entered: ")> 
          >(x)>
        )>
        >(trout 
            >( + 
              >("You see how I can now reference the encrusted id where I want inside this scope? ")> 
              >(x)>
            )>
          >(trout 
            >(+ 
              >(x)> 
              >(" Here it is again, you can do this as many times as you want in an encrusted scope.")>
            )>
            >(x)>
          )>
        )>
      )>
  )>
)<
```

Here is another example of using encrust (Where you shouldn't be, really) and trout calls to evaluate a fin statement:
```
<(
  encrust >(x >(5)>)>
  >(
    encrust >(y >(3)>)>
    >(
      fin
      >(
          trout >("Is x greater than y?")>
          >(> >(x)> >(y)>)>
      )>
      >(
          trout >("Yes it is!")>
          >(- >(x)> >(y)>)>
      )>
      >(
          trout >("No it's not..")>
          >(+ >(x)> >(y)>)>
      )>
    )>
  )>
)<
```
