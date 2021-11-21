# Revision history for Sakana

## 0.1.0.0 -- 2021-10-17

* First version. Released on an unsuspecting world.

#### 0.1.0.1 -- 2021-10-17

* Changed name from Fish to Bream to Sakana.

#### 0.1.0.2 -- 2021-10-18

* Some compatibility patches for GHC base 4.8.0 instead of 4.15.0
* minor fixes for some clarity in ExecutionTree.hs

### 0.1.1.0 -- 2021-10-18

* Changed the ```Num``` data type from a Float to a Double.
* Fixed ```String``` data types from printing with ```\" \"``` wrapped around them. 
* Implemented some general collapsible terminal cases.

## 0.2.0.0 -- 2021-10-20

* Implemented standard library functions:
    * trout
    * dolphin
    * encrust
* See the README for usage details.

#### 0.2.0.1 -- 2021-10-21

* Removed the ```encrust``` std lib function because it was awful.
* working on implementing a method of procedurally executing code in a very similar way
    to Haskell's ```do``` notation.

#### 0.2.0.2 -- 2021-10-21

* A more official release of the procedural execution update.
* Tweaked fetching execution trees so that either a ```swim``` keyword could be used or it
    could not be.

#### 0.2.0.3 -- 2021-10-25

* Gave a few options to the Sakana interpreter.
    * Calling ```Sakana -v``` or ```--version``` will display the version number.
    * The ```-h, --help``` flag will display a little help.
* Updated the organization of the program files a little bit.
* Fin arguments can now be executed procedurally with a swim keyword as well.
* Can now pass command line arguments into a Sakana program. Retrieve the arg string
by calling the function ```_args```

### 0.2.1.1 -- 2021-10-27

* Rewrote the parser for converting tokens to trees using parser combinators.
    * I am in awe with how powerful those are.
* Adjusted the syntax of ```swim``` so that, inside a function, or in the main scope,
it does not have to be wrapped in a return fish. This will help cut down on brackets inside
function definitions.
* Error checking in the parser is almost non-existant and the current error messages
do not provide nearly enough information, but I think I will save those for a future update.
* Created a formal grammar diagram in the README describing the current structures that the 
parser looks for when parsing a Sakana program.

#### 0.2.1.2 -- 2021-10-28

* Added more std_lib functions.
    * ```read```
    * ```floor```

### 0.2.2.0 -- 2021-10-30

* Rewrote the parser yet again.
    * Using Haskell's Parsec library.

#### 0.2.2.1 -- 2021-11-02

* Fixed ```trout``` not printing escaped characters (\n,\t,\r) properly.
* Added ```herring``` print function to print to ```stderr```.
* both ```trout``` and ```herring``` will no longer end with a newline, so they will
have to be placed manually.

#### 0.2.2.2 -- YYYY-MM-DD HashMaps