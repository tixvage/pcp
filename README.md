# Pcp

**THIS LANGUAGE IS A WORK IN PROGRESS**

Pcp is a [Strongly Static Typed](https://en.wikipedia.org/wiki/Strong_and_weak_typing) [C](https://en.wikipedia.org/wiki/C_(programming_language)) like [Programming Language](https://en.wikipedia.org/wiki/Programming_language) for [Computers](https://en.wikipedia.org/wiki/Computer)

## Compiling

```console
$ ./build.sh
$ ls pcp
pcp
```

## Examples

Hello, World:

```pcp
extern fn printf(fmt: cstr, args: va_arg): i32;

fn main(): i32 {
    hello_world := "Hello, World";
    printf("%s\n", hello_world);
    return 0;
}
```
```console
$ ./pcp hello_world.pcp
$ gcc tests/out.c
$ ./a.out
Hello, World
```

## Language Reference

See [demo.pcp](./demo.pcp)

## Milestones

- [] Try To Solve Some Common [C](https://en.wikipedia.org/wiki/C_(programming_language)) Problems
- [] [Self-hosted](https://en.wikipedia.org/wiki/Self-hosting_(compilers))
- [] [JIT Compiler](https://en.wikipedia.org/wiki/Just-in-time_compilation)
- [] Better Compiler Errors

## FAQ

### Why there's not a single **free** in the code?

```console
$ grep -rn "free" *.c *.h
$
```
I am aware that this is a problem that shouldn't be underestimated.
The main reason is that allocations aren't made systematically in the currently used C codebase. ([Allocators](https://en.wikipedia.org/wiki/Allocator_(C%2B%2B)))
I hope self-hosted compiler will fix this issue

### Where does the name come from?

It's just [C++](https://en.wikipedia.org/wiki/C%2B%2B)(cpp) with swapped letters
