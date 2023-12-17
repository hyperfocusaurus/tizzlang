import std::io::stdin;

func main: () -> Result {
  // pattern matching works by splitting an expression across
  // the 'match' keyword, and each branch.
  // for example, you can match a number like this:
  let x = 5;
  match x == {
  	1: { println("one") },
	2: { println("two") },
	3: { println("three") },
	4: { println("four") },
	// asterisk always matches "everything else" (think of it like a 'default' case in a switch)
	*: { println("something else") }
  }
  // or like this:
  match x {
    ==1: { println("one") },
    ==2: { println("two") },
    >3: { println("greater than three") },
    <4: { println("less than four") },
    *: { println("something else") }
  }
  // note that two different branches may match at once.  In this case,
  // both branches are chosen, and the code executed in each branch in the order
  // they are written.
  match x {
    >3: { println("greater than three") },
    >4: { println("greater than four") },
    *: { println("something else") }
  }
  // you can also match on strings:
  let s = "hello";
  match s {
    =="hello": { println("hello") },
    =="world": { println("world") },
    .is_empty(): { println("empty string") },
    .starts_with("h"): { println("starts with h") },
    *: { println("something else") }
  }

  // if you omit the operator in an OR block,
  // we will use the same operator as the previous block.
  // this is especially useful for matching with function calls:
  let y = 5;
  match y {
    ==1|2|3: { println("one, two, or three") },
    >3|<4: { println("greater than three or less than four") },
    .is_positive() |
    is_even(): { println("positive or even") },
    *: { println("something else") }
  }

  Ok()
}
