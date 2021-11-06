# Structure and Interpretation of Sakana Programs

Here is a write-up of how, generally, the Sakana interpreter works.

Partly as a way to inform whoever wants to write a Sakana program.

But mostly as a way to keep my thoughts in order as I work on it.

[TOC]



------

# Overview

The Sakana interpreter is a syntax-tree based interpreter.
Raw text from a Sakana file, or any file for that matter, is simultaneously parsed into
tokens and a tree structure.

The first step in interpretation of a Sakana program is parsing.
All syntactic elements are placed under the document's root node, `main` with
each node consisting of a `SyntaxUnit`.

## SyntaxUnit

A `SyntaxUnit` is a data structure containing some information relevant to Sakana
interpretation.

this structure consists of three parts:
- Token
    - A lexical token obtained from parsing raw text.
- Line Number
    - The token's line position in the source code, used for error reporting only.
- Context
    - A binary value, either ```Send``` or ```Return```.
    This information is used in executing or binding a value during the execution phase.

## Syntax Tree

Below is an example of some Sakana code and the syntax tree obtained from parsing it.
```
fish add
    >(x)>
    <(
        >(y)>
        <(+
            >(x)>
            >(y)>
        )<
    )<
```
Which is analogous to the following Haskell code:
```haskell
add x = \y -> (+) x y
```
Which yields the following tree:
```
{Data Id "main", 0, Return}
    -> {Keyword Fish, 1, Return}
        -> {Data Id "add", 1, Send}
        -> {Data Id "x", 2, Send}
        -> {Data Id "y", 4, Send}
        -> {Operator Add, 5, Return}
            -> {Data Id "x", 6, Send}
            -> {Data Id "y", 7, Send}
```

The root node `main` is a SyntaxUnit like any other node but it has a line number of 0
because it does not exist in the source code.

------

# Execution

If we wanted to execute a very simple Sakana program, all we would have to do is add an 
expression to the above Sakana source code:
```
<(
    add >(1)> >(1)>
)<
```
The document tree generated would then look like:
```
{Data Id "main", 0, Return}
    -> {Keyword Fish, 1, Return}
        -> {Data Id "add", 1, Send}
        -> {Data Id "x", 2, Send}
        -> {Data Id "y", 4, Send}
        -> {Operator Add, 5, Return}
            -> {Data Id "x", 6, Send}
            -> {Data Id "y", 7, Send}
    -> {Data Id "add", 8, Return}
        -> {Data Num 1.0, 8, Send}
        -> {Data Num 1.0, 8, Send}
```

Following parsing, a reference environment as well as execution tree would have to be 
determined.

## Environment

The environment constitutes a list of symbol tables, which are themselves, lists of 
symbol pairs.

This environment is used to look up the value or tree to execute when any Id is
encountered during the execution of Sakana code.

As such, an environment is created from statements: function declarations, 
or value bindings.

This is where the `context` of a syntax unit is first accessed.
When creating an environment, the interpreter looks at the top-level children of the
`main` node. In this case, the `add` function declaration and the following
`add` function call.

Therefore, the syntactic elements exposed to the interpreter at this stage are these:
```
    -> {Keyword Fish, 1, Return}
    ...
    -> {Data Id "add", 8, Return}
```

When the interpreter encounters a `{Keyword Fish, ...}` of any kind,
it knows that whatever that follows is a function declaration statement and should be stored in the 
environment. Following this, the interpreter then looks at the declaration's children and
pulls out its first child, the function Id `add`. It then stores this id, together
with the whole function declaration tree as a symbol pair:

```
SymbolPair {
    SymbolId {
        Data Id "add", 1, Send
    }
    SymbolVal {
        {Keyword Fish, 1, Return}
        -> {Data Id "add", 1, Send}
        -> {Data Id "x", 2, Send}
        -> {Data Id "y", 4, Send}
        -> {Operator Add, 5, Return}
            -> {Data Id "x", 6, Send}
            -> {Data Id "y", 7, Send}
    }
}
```

The `SymbolId` field is used when the interpreter is looking for a value binding
in the environment, and the `SymbolVal` is the corresponding tree that the 
id is bound to.

After the interpreter binds this function declaration tree to the Id `add` in the 
environment, it moves on to the next tree: `{Data Id "add", 8, Return} -> ...`.
Since this SyntaxUnit is not a `Fish` keyword it then looks to its context: 
`Return`.

This context tells that the interpreter that this tree is meant to be executed so it skips
storing this tree in the environment.

Since there are no more trees to check, the interpreter finishes creating the environment
and yields the following data structure:
```
EnvironmentStack [
    [
        SymbolPair {
            SymbolId {Data Id "add", 1, Send}
            SymbolVal {
                {Keyword Fish, 1, Return}
                -> {Data Id "add", 1, Send}
                -> {Data Id "x", 2, Send}
                -> {Data Id "y", 4, Send}
                -> {Operator Add, 5, Return}
                    -> {Data Id "x", 6, Send}
                    -> {Data Id "y", 7, Send}
            }
        }
    ]
]
```

## Execution Tree

Following the creation of the environment, the interpreter then determines what it will
execute. Determining the execution tree is simple.

The execution tree is either a tree with a ```Swim``` root node, or it is the last
tree in the document tree's children that is not a statement. Meaning, it is not
a function declaration and it has a context of ```return```.

Therefore, for this program, the execution tree is simply:
```
{Data Id "add", 8, Return}
    -> {Data Num 1.0, 8, Send}
    -> {Data Num 1.0, 8, Send}
```

# Executing a Function Call

------

## Current Implementation

Now is the part I'm writing for myself, as I determine how partial function application
and higher order functions may be implemented in Sakana.

Now that the interpreter has its execution state, the environment and execution tree,
it proceeds to execution.

Executing a function call is a relatively complex process. The interpreter first encounters the Id,
`add`, and looks it up in the environment, it retrieves the tree defined in the 
Environment section.

Next comes reconciling the function call's arguments vs. the declaration's parameters.
The interpreter must determine the number of parameters expected, by examining the 
children nodes of the declaration's root `Fish` keyword. It knows that the first
child of a declaration is always the Id of the function so it ignores that node.
Following this, any node that consists of only an Id with a context of ```Send``` is
a parameter.

Then the interpreter examines the arguments, which must be expressions of some sort.
It creates symbol pairs of the parameters bound to the arguments, in order of declaration.
So that the resultant list of symbol pairs is as follows:
```
SymbolTable [
    SymbolPair {
        SymbolId {Data Id "x", 2, Send}
        SymbolVal {Data Num 1.0, 8, Send}
    },
    SymbolPair {
        SymbolId {Data Id "y", 4, Send}
        SymbolVal {Data Num 1.0, 8, Send}
    }
]
```

It then prepends this symbol table onto the environment to yield:
```
EnvironmentStack [
    [
        SymbolPair {
            SymbolId {Data Id "x", 2, Send}
            SymbolVal {Data Num 1.0, 8, Send}
        },
        SymbolPair {
            SymbolId {Data Id "y", 4, Send}
            SymbolVal {Data Num 1.0, 8, Send}
        }
    ],
    [
        SymbolPair {
            SymbolId {
                Data Id "add", 1, Send
            }
            SymbolVal {
                {Keyword Fish, 1, Return}
                -> {Data Id "add", 1, Send}
                -> {Data Id "x", 2, Send}
                -> {Data Id "y", 4, Send}
                -> {Operator Add, 5, Return}
                    -> {Data Id "x", 6, Send}
                    -> {Data Id "y", 7, Send}
            }
        }
    ]
]
```

The interpreter then sends the execution tree of the function declaration, deduced in the
exact same way as the program's main execution tree, back to be executed, with the 
new, updated environment in tow.

Following this, execution is straightforward, the interpreter encounters an `addition`
operator and fetches its arguments `>(x)>` and `>(y)>`. Seeing that these are Id's
and not expressions, it looks to the environment and finds bindings for both of them.
After a few steps of encountering Id's and looking up values in the environment, the 
operator expression looks like: `+ >(1)> >(1)>` which is executed and returns 
`2.0`.

------

# Speculations on Execution

## In the Case of Partial Application

Assuming our source code is:
```
fish add
    >(x)>
    <(
        >(y)>
        <(+
            >(x)>
            >(y)>
        )<
    )<
swim
>(add_one <(add >(1)>)<)>
<(add_one >(2)>)<
```

The resulting document tree is:
```
{Data Id "main", 0, Return}
    -> {Keyword Fish, 1, Return}
        -> {Data Id "add", 1, Send}
        -> {Data Id "x", 2, Send}
        -> {Data Id "y", 4, Send}
        -> {Operator Add, 5, Return}
            -> {Data Id "x", 6, Send}
            -> {Data Id "y", 7, Send}
    -> {Keyword Swim, 10, Return}
        -> {Data Id "add_one", 11, Send}
            -> {Data Id "add", 11, Return}
                -> {Data Num 1.0, 11, Send}
        -> {Data Id "add_one", 12, Return}
            -> {Data Num 2.0, 12, Send}
```

The corresponding environment is:
```
EnvironmentStack [
    [
        SymbolPair {
            SymbolId {
                Data Id "add", 1, Send
            }
            SymbolVal {
                {Keyword Fish, 1, Return}
                -> {Data Id "add", 1, Send}
                -> {Data Id "x", 2, Send}
                -> {Data Id "y", 4, Send}
                -> {Operator Add, 5, Return}
                    -> {Data Id "x", 6, Send}
                    -> {Data Id "y", 7, Send}
            }
        }
    ]
]
```

The execution tree is:
```
{Keyword Swim, 10, Return}
    -> {Data Id "add_one", 11, Send}
        -> {Data Id "add", 11, Return}
            -> {Data Num 1.0, 11, Send}
    -> {Data Id "add_one", 12, Return}
        -> {Data Num 2.0, 12, Send}
```

### Partial Function Call and Binding

In interpretation, a `SendFish` is encountered, so the Id, `add_one`, recieves
the binding of the returned value, `add >(1)>`.

As this function is only partially called, let us examine how this may work.

The interpreter expects two arguments in the function in correspondence with the two
parameters specified in the function declaration, but it receives only one.

So it creates a binding for the argument it has:
```
SymbolPair {
    SymbolId {Data Id "x", 2, Send}
    SymbolVal {Data Num 1.0, 8, Send}
}
```

Then, rather than executing the tree, it must return the remainder of the tree to the
`FishSend` for binding. This is not currently possible, as the execution functions
expect to return a single `Data` rather than an entire tree.

Ideally, a tree would be returned, with the shape of a function declaration, taking one
argument, `y`, and preserving its reference to `x` in the function body.

#### My Initial Proposal

Rather than binding a value to the id `add_one`, it will instead bind the partial
function call, so that the symbol pair would look like:

```
SymbolPair {
    SymbolId {Data Id "add_one", ... , Send}
    SymbolVal {
        {Data Id "add", ... , Return}
            -> {Data Num 1.0, ... , Send}
    }
}
```

Though this would result in the subsequent call:

```
add_one >(2)>
```

having a structure like:

```
{Data Id "add_one", ... , Send}
    -> {Data Id "add", ... , Return}
        -> {Data Num 1.0, ... , Send}
    -> {Data Num 2.0, ... , Send}
```

Where a more correct, or expected, call structure would be:

```
{Data Id "add_one", ... , Send}
	-> {Data Id "add", ... , Return}
		-> {Data Num 1.0, ... , Send}
		-> {Data Num 2.0, ... , Send}
```

So there must be devised, some way to know and appropriately apply multi-level arguments to 
function calls or restructure partial function calls to appear as complete. I will have to think about this to determine if a circumstance
like this is unique to a `FishSend` binding or if it could happen in other syntactic constructs.

Let's examine a more detailed view at the former structure:

```
{Data Id "add_one", ... , Send}
	-> {Keyword Fish, ... , Return}
		-> {Data Id "add", ... , Send}
		-> {Data Id "x", ... , Send}
		-> {Data Id "y", ... , Send}
		-> {Operator Add, ... , Return}
			-> {Data Id "x", ... , Send}
			-> {Data Id "y", ... , Send}
		-> {Data Num 1.0, ... , Send}
	-> {Data Num 2.0, ... , Send}
```

The above is an expanded view of the partial function binding's call structure as it relates to the original function declaration.

In this case, having a partially applied function bound to an id, we can see that the function is only partially applied and then look at the call structure of the id it's bound to to procure the additional arguments needed for execution.

In the case of `FishSend` I think this is fairly sensible.

------

##### Higher Order Functions

Let us say our source code is this:

```
fish apply
	>(f)>
	>(x)>
	<(
		f >(x)>
	)<

fish add
    >(x)>
    <(
        >(y)>
        <(+
            >(x)>
            >(y)>
        )<
    )<

<(
	apply >(add >(1)>)> >(2)>
)<
```

Where the function, `apply`, defines two parameters. One function and one operand. During execution, we bind the partially applied function, `add >(1)>`, to the symbol, `f`, and the value, `2.0`, to the symbol, `x`. The call structure of `f` is as follows:

```
{Keyword Fish, ... , Return}
    -> {Data Id "add", ... , Send}
    -> {Data Id "x", ... , Send}
    -> {Data Id "y", ... , Send}
    -> {Operator Add, ... , Return}
        -> {Data Id "x", ... , Send}
        -> {Data Id "y", ... , Send}
    -> {Data Num 1.0, ... , Send}
```

Here we can see that this call structure appears to be nearly identical to the call structure of the earlier `add_one` binding. Additionally, the structure of the call, `f >(x)>` would again have the same structure. 

This makes sense because function parameter bindings and `FishSend` bindings are created and used in the same way in the interpreter. So this would mean that the same general approach could be used for execution of bound, partially applied functions, and higher order, partially applied functions.

###### Required Development on the Interpreter

My initial thoughts on how this partial function application could be implemented:

- Lazy evaluation of function arguments.

  In the current implementation, arguments are eagerly evaluated when a symbol pair is created at a function call. If I could tweak the interpreter to wait for the full function evaluation to evaluate symbol pair bindings, then I would be able to pass function trees into other functions as arguments.

- Determining complete or partial application.

  A method of determining if a function is completely or partially applied. This would be trivial to implement as it's just counting a filtered list of tree children, but there is one additional concern. 

  If a function is partially applied, it must be possible to reach out one level above the function to search for any additional arguments, since we saw that the call structure of a partially applied function had its remaining arguments in another branch of the root node the function was bound to. This may complicate and obfuscate some of what the interpreter is doing but for now it is just something of which to be aware.

###### Additional Questions / Further Research

- What is the call structure of unbound functions (anonymous functions)?

  ```
  >(x)> <( >(y)> <(+ >(x)> >(y)> )< )<
  ```

- Would passing an anonymous function as an argument work the same way as passing a function binding, a reference to a tree stored in the environment?

- Calling a function results in a new environment, what should the interpreter do with a partial application?

  - Return `D.Null`? Or `Nothing`?

- Trying to execute a Sakana program with a partially applied function should result in an exception, as no value should logically be returned by a function that is not completely applied. So even if the interpreter decides to internally return a `D.Null` value, this is only as a placeholder during interpretation and not as a real value.
