package Array;

// TODO XXX primArrayNewU could be replaced with bracket syntax
// TODO XXX this file needs more comments
// TODO XXX remove all reference to right and left, and change to 0th and Nth

import List::*;


// Map starting at the Nth element
// (constructed with primArrayGenWith rather than a loop of updates,
// so that elaboration does not build a chain of pending updates whose
// forcing recurses once per element; likewise for the other
// constructors below)
function Array#(res_T) map(function res_T f(any_T x), Array#(any_T) v);

  function res_T gen(Integer i) = f(v[i]);
  return primArrayGenWith(arrayLength(v), gen);

endfunction

// A fold function,  starting at the Nth element
function res_T foldr(function res_T f(any_T x, res_T y), res_T start, Array#(any_T) v);

  Integer ln = arrayLength(v);
  res_T temp = start;

  for(Integer i = (ln - 1); i >= 0; i = i - 1)
    temp = f(v[i], temp);

  return temp;

endfunction

// A scan operation starting the Nth operation
function Array#(res_T) scanr(function res_T f(any_T x, res_T y), res_T start, Array#(any_T) v);

  Integer ln = arrayLength(v);
  Array#(res_T) resv = primArrayNewU(ln + 1);
  resv[ln] = start;

  for(Integer i = (ln - 1); i >= 0; i = i - 1)
    resv[i] = f(v[i], resv[i + 1]);

  return resv;

endfunction

// a fold 1 function starting at Nth element
function any_T foldr1(function any_T f(any_T x, any_T y), Array#(any_T) v);

  Integer ln = arrayLength(v);

  any_T temp = v[ln - 1];

  for(Integer i = (ln - 2); i >= 0; i = i - 1)
    temp = f(v[i], temp);

  return temp;

endfunction

// a fold function starting from 0th index
function res_T foldl(function res_T f(res_T x, any_T y), res_T start, Array#(any_T) v);

  Integer ln = arrayLength(v);
  res_T temp = start;

  for(Integer i = 0; i < ln; i = i + 1)
    temp = f(temp, v[i]);

  return temp;

endfunction


// a fold function starting from 0th index
function any_T fold(function any_T f(any_T x, any_T y), Array#(any_T) v);

  Integer ln = arrayLength(v);

/**/

  // I suspect this is slower.
  if(ln == 0)
     return error("Vector.Fold: Zero length Vector");
  else if (ln == 1)
    return (v[0]);
  else
    begin

      Integer nln = div(ln+1,2);

      function any_T gen(Integer i) =
         ((i == nln - 1) && (nln != div(ln,2))) ? v[ln-1] : f(v[2*i], v[2*i+1]);
      Array#(any_T) temp = primArrayGenWith(nln, gen);
      return fold(f, temp);
    end


/**/
/*
if(ln == 0)
     return error("Vector.Fold: Zero length Vector");
  else if (ln == 1)
    return (v[0]);
  else
    begin
      Integer nln = div(ln+1,2);
      while (nln != 1)
        begin
          for(Integer i = 0; i < div(ln,2); i = i + 1)
            v[i] = f(v[2*i], v[2*i+1]);
          if (nln != div(ln,2))
            v[nln-1] = v[ln-1];
          ln = nln;
          nln = div(nln + 1,2);
        end
      return v[0];
    end
*/

endfunction


// a fold 1 functions, starting at
function any_T foldl1(function any_T f(any_T x, any_T y), Array#(any_T) v);

  Integer ln = arrayLength(v);

  any_T temp = v[0];

  for(Integer i = 1; i < ln; i = i + 1)
    temp = f(temp, v[i]);

  return temp;

endfunction

// a scan 1 from the 0th element
function Array#(res_T) scanl(function res_T f(res_T x, any_T y), res_T start, Array#(any_T) v);

  Integer ln = arrayLength(v) + 1;
  Array#(res_T) resv = primArrayNewU(ln);
  resv[0] = start;

  for(Integer i = 1; i < ln; i = i + 1)
    resv[i] = f(resv[i - 1], v[i - 1]);

  return resv;

endfunction

// Pack
function Bit#(vsz) packArray(Array#(any_T) v)
  provisos
         (Bits#(any_T, sz));

   Integer inferLength = div(valueOf(vsz), valueOf(sz));

   function Bit#(m) flatN(Integer n, Array#(Bit#(k)) b);

     if (n == inferLength)
       return 0;
     else
       return (flatN(n+1, b) << (valueOf(k))) | b[n][(valueOf(k) - 1):0];
   endfunction

   // need to handle 0-bit elements and 0-element arrays
   if(valueOf(vsz) == 0) begin
     if(valueOf(sz) == 0)
       return(0);
     else if (arrayLength(v) == 0)
       return(0);
     else
       return error ("Zero result vector when non-zero size expected!");
   end
   else if (inferLength > 0)
   begin
     function Bit#(sz) genPacked(Integer i) = pack(v[i]);
     Array#(Bit#(sz)) packedArray = primArrayGenWith(inferLength, genPacked);

     return flatN(0, packedArray);
   end
   else
     return error ("Result vector too short for pack!");
   //cannot take length because array might be _, which includes using primArrayMap
   //return flatN(arrayLength(v), map(pack, v));
   //return flatN(arrayLength(v), reverse(map(pack, v)));

endfunction

// Unpack
function Array#(any_T) unpackArray(Bit#(n) bts, Integer ln)
  provisos
          (Bits#(any_T, sz));

 Integer k = valueOf(sz);

 function any_T gen(Integer i) = unpack(bts[((i*k)+k-1):(i*k)]);
 return primArrayGenWith(ln, gen);

endfunction


function Array#(any_T) takeAt(Array#(any_T) v, Integer hi, Integer lo);

  Integer ln = hi - lo + 1;
  if (ln > 0)
  begin
    function any_T gen(Integer x) = v[x + lo];
    return primArrayGenWith(ln, gen);
  end
  else return primArrayNewU(0);

endfunction


function Array#(any_T) genWith(Integer n, function any_T initfun(Integer k));

  return primArrayGenWith(n, initfun);

endfunction


//implicitly reverse list
function List#(any_T) arrayToList(Array#(any_T) v);

   List#(any_T) res = Nil;

   Integer ln = arrayLength(v);

   for (Integer x = (ln - 1); x >= 0; x = x - 1)
     res = cons(v[x], res);

   return res;

endfunction

//implicitly reverse list
function Array#(any_T) listToArray(List#(any_T) l);

   return primArrayFromList(l);

endfunction


function Array#(any_T) append(Array#(any_T) v1, Array#(any_T) v2);

   Integer ln1 = arrayLength(v1);
   Integer ln2 = arrayLength(v2);

   function any_T gen(Integer x) = (x < ln2) ? v2[x] : v1[x - ln2];
   return primArrayGenWith(ln1 + ln2, gen);

endfunction


function Array#(any_T) concat(Array#(Array#(any_T)) v);

   Integer ln1 = arrayLength(v);
   Integer ln2 = (ln1 == 0) ? 0 : arrayLength(v[0]);

   function any_T gen(Integer k) = v[div(k, ln2)][mod(k, ln2)];
   return primArrayGenWith(ln1 * ln2, gen);

endfunction


// [A,B,C] => [A,B,C, _]
function Array#(any_T) growRight(Array#(any_T) v, Integer k);

   Integer ln = arrayLength(v);
   Array#(any_T) vnew = primArrayNewU(k);

   return append(v, vnew);

endfunction


// [A,B,C] => [_, A,B,C]
function Array#(any_T) growLeft(Array#(any_T) v, Integer k);

   Integer ln = arrayLength(v);
   Array#(any_T) vnew = primArrayNewU(k);

   return append(vnew, v);

endfunction


// [A,B,C] => [A,B]
function Array#(any_T) shrinkRight(Array#(any_T) v, Integer k);

   Integer ln = arrayLength(v);
   return takeAt(v, ln - 1, k);

endfunction

// [A,B,C] => [B, C]
function Array#(any_T) shrinkLeft(Array#(any_T) v, Integer k);

   Integer ln = arrayLength(v);
   return takeAt(v, ln - k - 1, 0);

endfunction


// zip function
function Array#(Tuple2#(fst_T, snd_T)) zip(Array#(fst_T) v1, Array#(snd_T) v2);

  function Tuple2#(fst_T, snd_T) gen(Integer i) = tuple2(v1[i], v2[i]);
  return primArrayGenWith(arrayLength(v1), gen);

endfunction

// zip 3
function Array#(Tuple3#(fst_T, snd_T, thd_T)) zip3(Array#(fst_T) v1, Array#(snd_T) v2, Array#(thd_T) v3);

  function Tuple3#(fst_T, snd_T, thd_T) gen(Integer i) = tuple3(v1[i], v2[i], v3[i]);
  return primArrayGenWith(arrayLength(v1), gen);

endfunction

// zip 4
function Array#(Tuple4#(fst_T, snd_T, thd_T, fth_T)) zip4(Array#(fst_T) v1, Array#(snd_T) v2, Array#(thd_T) v3, Array#(fth_T) v4);

  function Tuple4#(fst_T, snd_T, thd_T, fth_T) gen(Integer i) = tuple4(v1[i], v2[i], v3[i], v4[i]);
  return primArrayGenWith(arrayLength(v1), gen);

endfunction

// unzip
function Tuple2#(Array#(fst_T), Array#(snd_T)) unzip(Array#(Tuple2#(fst_T, snd_T)) v);

  Integer ln = arrayLength(v);

  function fst_T genFst(Integer i) = v[i].fst();
  function snd_T genSnd(Integer i) = v[i].snd();

  return tuple2(primArrayGenWith(ln, genFst), primArrayGenWith(ln, genSnd));

endfunction

// zip with
function Array#(res_T) zipWith(function res_T f(fst_T f, snd_T s), Array#(fst_T) v1, Array#(snd_T) v2);

  function res_T gen(Integer i) = f(v1[i], v2[i]);
  return primArrayGenWith(arrayLength(v1), gen);

endfunction

// zip with 3
function Array#(res_T) zipWith3(function res_T f(fst_T f, snd_T s, thd_T t), Array#(fst_T) v1, Array#(snd_T) v2, Array#(thd_T) v3);

  function res_T gen(Integer i) = f(v1[i], v2[i], v3[i]);
  return primArrayGenWith(arrayLength(v1), gen);

endfunction

function Array#(any_T) reverse(Array#(any_T) v);

  Integer ln = arrayLength(v);
  if (ln <= 1) return v;
  else
  begin
    function any_T gen(Integer i) = v[ln - i - 1];
    return primArrayGenWith(ln, gen);
  end

endfunction

function Bool elem(any_T x, Array#(any_T) v)
  provisos
          (Eq#(any_T));

  Integer ln = arrayLength(v);
  Bool res = False;

  for(Integer i = 0; (i < ln) && (res == False); i = i + 1)
    res = x == v[i];

  return res;

endfunction

function Bool any(function Bool f(any_T x), Array#(any_T) v);

  Integer ln = arrayLength(v);
  Bool res = False;

  for(Integer i = 0; (i < ln) && (res == False); i = i + 1)
    res = f(v[i]);

  return res;

endfunction

function Bool all(function Bool f(any_T x), Array#(any_T) v);

  Integer ln = arrayLength(v);
  Bool res = True;

  for(Integer i = 0; (i < ln) && (res == True); i = i + 1)
    res = f(v[i]);

  return res;

endfunction

endpackage
