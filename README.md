# vinegar
simple high level language with straightforward rust integration

## usage
to call a function:
```
function(arg2: "hello world", arg1, arg3: "example")
```
or:
```
function:
    arg2: "hello world"
    arg1
    arg3: "example"
```
> NOTE: this is not functional in the current version


to define a function:
```
def function(arg1 (Type), arg2: default, arg3):
    do_something()
    return "result"
```
> NOTE: type annotations and returned values are not functional in the current version

to assign a value to a variable:
```
myvar = "value"
```

variables that are passed to a rust function are retrospectively typed. for example, if:

```
pub fn rust_function(list: Vec<String>) {...}
```
> NOTE: there currently is no typing system, or any type of compile time anlysis. the examples given here would cause runtime errors.

then this:

```
myvar = "value"

mylist = myvar, "something else"

rust_function(mylist)
```

will work.

however, this:

```
myvar = 2

mylist = myvar, "something else"

rust_function(mylist)
```

won't.

vinegar is generous about interpreting the literals. therefore if our rust function took a float, all these would work:

```
rust_function(10)
rust_function(10.0)
rust_function(dec_10)
rust_function(bin_1010)
rust_function(hex_A)
rust_function(hex_a.0)
rust_function(#A)
```

additionally to the type, a wrapper for a rust function can require the value to have some attribute like being in a certain range. this, too is applied to variables that are passed around in the code preceding the function. in the case of a range from 0 to 9, this would work:

```
do_something_with_digit(2)
```

while this wouldn't

```
do_something_with_digit(10)
```

or it could require a valid file path, in which case this might work:
```
modify_file(file.png)
```

while this wouldn't

```
modify_file(nonexistent_file.png)
```

note that any word that is not recognised as an identifier is treated as a string literal. periods also are interpreted as part of a word.

this might be confusing, but is helpful for cases like the above.

for accessing members of a variable, simply use a period:

```
variable.member
```
or, if you want it in the next line:
```
variable. 
    member
```
> NOTE: this is not functional in the current version
