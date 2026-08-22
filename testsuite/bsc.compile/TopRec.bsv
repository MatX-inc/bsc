import List::*;

function Integer inc(Integer x);
  return (x + 1);
endfunction: inc

function List#(b) recMap(function b f(a x), List#(a) xs);
  return isNull(xs) ? Nil : cons(f(head(xs)), recMap(f, tail(xs)));
endfunction

List#(Integer) evens;
evens = cons(0, recMap(inc, odds));

List#(Integer) odds;
odds = recMap(inc, evens);

