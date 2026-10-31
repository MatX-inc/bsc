function Integer myexp(Integer x, Integer y);
   return x ** y;
endfunction

function Integer pow2(Integer y) = myexp(2, y);
