# Tizz Programming Language

Not really meant as a serious project, more of an experiment in writing languages without using flex/bison for
lexing/parsing.

The examples/ directory has some "wishlist" items of features I want to implement in the language, but as of right now
none of the examples actually compile (and it may be that way for some time).

Tizz is quite similar to Rust, but with some stuff borrowed from other languages like Zig, C, Lua, Kotlin, etc.

It's principally meant to be used in a functional style, but does implement a heap which can be accessed as well.

I'd like to implement some clever metaprogramming stuff as well, but we'll see how far I get with the preprocessor and
whether that's really going to be possible.  My vision is to use some way of marking a function as being run at compile
time and, if a function is marked as such, it should accept a string which is whatever you pass into the function, and
return a string which is whatever it wants to dump into the source.   So for example:

```tizz
compiler-const func min(String args) -> String {
  let args = args.split(',');
  assert(args.len() == 2, "Min requires a minimum and maximum argument");
  let a = args[0];
  let b = args[1];
  s#"
  if argA.ord() < argB.ord() {
    argA
  } else {
    argB
  }
  ".subst("argA", a).subst("argB", b)
}
```

