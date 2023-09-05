# vinegar
simple high level language with straightforward rust integration

## syntax
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