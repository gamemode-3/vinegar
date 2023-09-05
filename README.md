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


to define a function:
```
def function(arg1 (Type), arg2: default, arg3):
    do_something()
    return "result"
```

to assign a value to a variable:
```
myvar = "value"
```

variables that are passed to a rust function are retrospectively typed. for example, if:

```
pub fn rust_function(list: Vec<String>) {...}
```

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

for accessing members of a variable, simply use a space:

```
variable member
```
or, if you want it in the next line:
```
variable 
    member
```

## syntax
$$
\begin{align}
    \text{[code\_body]} &\to \text{[statement] \textbackslash n ... \textbackslash n [statement]}\\
    \text{[statement]} &\to 
        \begin{cases}
            \text{[expression]}\\
            \text{[var\_assignment]}\\
            \text{[func\_definition]}\\
            \text{[condition]}\\
            \text{[while\_loop]}\\
            \text{[for\_loop]}\\
        \end{cases}\\
    \text{[expression]} &\to 
        \begin{cases}
            \text{identifier}\\
            \text{[function\_call]}\\
            \text{[internal\_expr]}\\
            \text{[literal]}\\
            \text{[generator]}\\
            (\text{[expression]})\\
        \end{cases}\\
    \text{[function\_call]} &\to 
        \begin{cases}
            \text{identifier([calling\_arg], ..., [calling\_arg])} \\
            \text{identifier:\textbackslash n <i+> [calling\_arg ]\textbackslash n ... \textbackslash n[\text  calling\_arg]\textbackslash n<i->}
        \end{cases}\\
    \text{[calling\_arg]} &\to 
        \begin{cases}
            \text{[expression]}\\
            \text{[expression]: [expression]}
        \end{cases}
        \\ 
    \text{[internal\_expr]} &\to 
        \begin{cases}
            \text{[expression] + [expression]}\\
            \text{[expression] - [expression]}\\
            \text{[expression] * [expression]}\\
            \text{[expression] / [expression]}\\
            \text{-[expression]}\\
            \text{[expression] or [expression]}\\
            \text{[expression] and [expression]}\\
            \text{not [expression]}\\
        \end{cases}\\
    
\end{align}
$$