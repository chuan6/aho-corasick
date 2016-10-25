# aho-corasick

A Clojure library designed to solve set matching (or multi-string matching) problem by applying the classic aho-corasick algorithm.

## Original Algorithm listing

The following algorithm listing is from the original paper "Efficient String Mathcing: An Aid to Bibliographic Search (1975)" by Alfred V. Aho and Margaret J. Corasick.

###Algorithm 1. Pattern matching machine.
__Input.__ A text string x = a1a2...an where each ai is an input symbol and a pattern matching machine M with goto function g, failure function f, and output function output, as described above.

__Output.__ Locations at which keywords occur in x.

__Method.__
```
begin
  state := 0
  for i := 1 until n do
    begin
      while g(state, ai) == fail do state := f(state)
      state := g(state, ai)
      if output(state) != empty then
        begin
          print i
          print output(state)
        end
    end
end
```

### Algorithm 2. Construction of the goto function.
__Input.__ Set of keywords K = {y1, y2, ..., yk}.

__Output.__ Goto function g and a partially computed output function output.

__Method.__ We assume output(s) is empty when state s is first created, and g(s, a) == fail if a is undefined or if g(s, a) has not yet been defined. The procedure enter(y) inserts into the goto graph a path that spells out y.
```
begin
  newstate := 0
  for i := 1 until k do enter(yi)
  for all a such that g(0, a) == fail do g(0, a) := 0
end

procedure enter(a1a2...am):
begin
  state := 0; j := 1
  while g(state, aj) != fail do
    begin
      state := g(state, aj)
      j := j + 1
    end
  for p := j until m do
    begin
      newstate := newstate + 1
      g(state, ap) := newstate
      state := newstate
    end
  output(state) := {a1a2...am}
end
```

### Algorithm 3. Construction of the failure function.
__Input.__ Goto function g and output function output from Algorithm 2.

__Output.__ Failure function f and output function output.

__Method.__
```
begin
  queue := empty
  for each a such that g(0, a) != 0 do
    begin
      s := g(0, a)
      queue := queue U {s}
      f(s) := 0
    end
  while queue != empty do
    begin
      let r be the next state in queue
      queue := queue - {r}
      for each a such that g(r, a) != fail do
        begin
          s := g(r, a)
          queue := queue U {s}
          state := f(r)
          while g(state, a) == fail do state := f(state)
          f(s) := g(state, a)
          output(s) := output(s) U output(f(s))
        end
    end
end
```
FIXME

## License

Copyright © 2016 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
