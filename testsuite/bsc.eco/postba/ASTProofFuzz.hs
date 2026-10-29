-- Differential fuzz of the StrictFourState normalizer: every structural verdict
-- of checkAST over a random reference/candidate pair must agree with Icarus
-- four-state simulation of the printed (pp80) modules. Candidates come from the
-- normalizer's own identity set, Z-unsafe shapes and deliberate mutations. A
-- structural pass with a simulation mismatch is a soundness failure; a rejected
-- identity is incompleteness and is only counted; seeded corners carry explicit
-- expectations, and seeded wrong rewrites must be rejected with a simulation
-- witness. Icarus is the four-state reference: it evaluates x ? z : z to z where
-- IEEE 1364-2005 Table 5-21 gives x, so the equal-arm collapse is checked against
-- Icarus semantics only. Usage: ASTProofFuzz OUT IVERILOG VVP [COUNT [SEED]].
module Main where
import Control.Monad
import Data.Bits
import Data.IORef
import Data.List (intercalate,isInfixOf,isPrefixOf,nub,sort)
import Data.Maybe (isJust)
import qualified Data.Generics as G
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import ECOASTProof
import PPrint (pp80)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Verilog

-- Deterministic generator over a 64-bit splitmix state.
newtype Gen a = Gen { runGen :: Integer -> (a,Integer) }
instance Functor Gen where fmap f (Gen g) = Gen (\s -> let (a,s') = g s in (f a,s'))
instance Applicative Gen where
  pure a = Gen (\s -> (a,s))
  Gen f <*> Gen g = Gen (\s -> let (h,s1) = f s; (a,s2) = g s1 in (h a,s2))
instance Monad Gen where Gen g >>= k = Gen (\s -> let (a,s1) = g s in runGen (k a) s1)
mask64 :: Integer
mask64 = 2^(64::Int) - 1
nextWord :: Gen Integer
nextWord = Gen (\s -> let s' = (s + 0x9E3779B97F4A7C15) .&. mask64
                          z1 = ((s' `xor` (s' `shiftR` 30)) * 0xBF58476D1CE4E5B9) .&. mask64
                          z2 = ((z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB) .&. mask64
                      in (z2 `xor` (z2 `shiftR` 31), s'))
rand :: Int -> Gen Int
rand n = do w <- nextWord; return (fromInteger (w `mod` toInteger (max 1 n)))
randInteger :: Integer -> Gen Integer
randInteger n = do w <- nextWord; return (w `mod` max 1 n)
pick :: [a] -> Gen a
pick xs = do i <- rand (length xs); return (xs !! i)
chance :: Int -> Gen Bool
chance p = do i <- rand 100; return (i < p)

v :: String -> VId
v = mkVId
var :: String -> VExpr
var = VEVar . v
range :: Integer -> Maybe VRange
range w = Just (VEConst (w-1),VEConst 0)
lit :: Integer -> Integer -> VExpr
lit w n = VEWConst (v "") w 2 n
op :: VOp -> VExpr -> VExpr -> VExpr
op k a b = VEOp (v "") a k b
un :: VOp -> VExpr -> VExpr
un k a = VEUnOp (v "") k a

data Var = Var { vName :: String, vWidth :: Integer } deriving (Eq,Show)
data Body = Cont VExpr | Proc VStmt deriving (Eq,Show)
data Prog = Prog { pInputs :: [Var], pOut :: Var, pHelpers :: [(Var,VExpr)], pBody :: Body } deriving (Eq,Show)

toProgram :: String -> Prog -> VProgram
toProgram mname (Prog ins out hs body) = VProgram [VModule (v mname) [] [(ports,[])] items] [] []
  where
    ports = [VAInput (v n) (range w) | Var n w <- ins] ++ [VAOutput (v (vName out)) (range (vWidth out))]
    decls = [VMDecl (VVDecl VDWire (range w) [VVar (v n)]) | (Var n w,_) <- hs] ++
            [VMDecl (VVDecl VDReg (range (vWidth out)) [VVar (v (vName out))]) | Proc _ <- [body]]
    assigns = [VMAssign (VLId (v n)) e | (Var n _,e) <- hs]
    -- Exactly the names the process reads, as bsc prints sensitivity lists.
    event s = foldr1 VEEOr [VEE (var n) | n <- case sort (nub [getVIdString i | VEVar i <- G.listify (const True) s]) of [] -> map vName ins; ns -> ns]
    main' = case body of
      Cont e -> VMAssign (VLId (v (vName out))) e
      Proc s -> VMStmt False (Valways (VAt (event s) s))
    items = decls ++ assigns ++ [main']

-- Expression generator over the shapes bsc prints: context width w, usable
-- variables env. Selects always take an identifier base.
genE :: Int -> Integer -> [Var] -> Gen VExpr
genE d w env
  | d <= 0 = leaf
  | otherwise = do
      k <- rand 100
      if w == 1 && k < 45 then oneBit
      else if k < 18 then leaf
      else if k < 30 then do
        cw <- pick [1,1,1,2,4]
        c <- genE (d-1) cw env
        tw <- pick [w,w,w,max 1 (w-1)]
        t <- genE (d-1) tw env; f <- genE (d-1) w env
        return (VEIf c t f)
      else if k < 42 then do
        o <- pick [VAnd,VOr,VXor]
        aw <- pick [w,w,max 1 (w-1)]
        a <- genE (d-1) aw env; b <- genE (d-1) w env
        return (op o a b)
      else if k < 50 then do
        o <- pick [VAdd,VSub]
        aw <- pick [w,w,max 1 (w-1)]
        a <- genE (d-1) aw env; b <- genE (d-1) w env
        return (op o a b)
      else if k < 56 then do
        o <- pick [VInv,VNeg]
        aw <- pick [w,w,max 1 (w-1)]
        a <- genE (d-1) aw env
        return (un o a)
      else if k < 62 then do
        o <- pick [VShL,VShR]
        n <- randInteger (w+1)
        a <- genE (d-1) w env
        return (op o a (lit 4 n))
      else if k < 74 && w >= 2 then do
        p <- randInteger (w-1)
        let hi = p+1; lo = w-hi
        a <- genE (d-1) hi env; b <- genE (d-1) lo env
        return (VEConcat [a,b])
      else if k < 82 then selectOf
      else if k < 88 && w >= 2 then do
        let ds = [k' | k' <- [2..w], w `mod` k' == 0]
        if null ds then leaf else do
          k' <- pick ds
          a <- genE (d-1) (w `div` k') env
          return (VERepeat (VEConst k') a)
      else if k < 93 then
        case [x | x <- env, vWidth x < w] of
          [] -> leaf
          xs -> do x <- pick xs; return (VEConcat [lit (w - vWidth x) 0, var (vName x)])
      else if k < 96 then triLit
      else leaf
  where
    leaf = do
      k <- rand 10
      if k < 6 then leafVar else if k < 9 then do n <- randInteger (2^w); return (lit w n) else triLit
    leafVar = case [x | x <- env, vWidth x == w] of
      [] -> selectOf
      xs -> do x <- pick xs; return (var (vName x))
    selectOf = case [x | x <- env, vWidth x > w] of
      [] -> case [x | x <- env, vWidth x < w] of
              [] -> do n <- randInteger (2^w); return (lit w n)
              xs -> do x <- pick xs; return (VEConcat [lit (w - vWidth x) 0, var (vName x)])
      xs -> do x <- pick xs
               lo <- randInteger (vWidth x - w + 1)
               return (if w == 1 then VESelect1 (var (vName x)) (VEConst lo)
                       else VESelect (var (vName x)) (VEConst (lo+w-1)) (VEConst lo))
    triLit = do bits <- replicateM (fromInteger w) (pick [V0,V1,Vx,Vz,V0,V1]); return (VETriConst bits)
    oneBit = do
      k <- rand 100
      if k < 30 then do
        o <- pick [VLAnd,VLOr]
        aw <- pick [1,1,1,2,4]; bw <- pick [1,1,1,2,4]
        a <- genE (d-1) aw env; b <- genE (d-1) bw env
        return (op o a b)
      else if k < 45 then do
        aw <- pick [1,1,2,4]
        a <- genE (d-1) aw env
        return (un VNot a)
      else if k < 85 then do
        o <- pick [VULT,VULE,VUGT,VUGE,VEQ,VNE,VEQ,VNE]
        aw <- pick [1,2,3,4]; bw <- pick [aw,aw,aw,4,2]
        a <- genE (d-1) aw env; b <- genE (d-1) bw env
        return (op o a b)
      else if k < 90 then do
        o <- pick [VEQ3,VNE3]
        aw <- pick [1,2,4]
        a <- genE (d-1) aw env; b <- genE (d-1) aw env
        return (op o a b)
      else if k < 96 then do
        o <- pick [VAnd,VOr,VXor]
        aw <- pick [2,3,4]
        a <- genE (d-1) aw env
        return (un o a)
      else leaf

genStmt :: Int -> Var -> [Var] -> Gen VStmt
genStmt d out env = do
  k <- rand 100
  if k < 45 then do
     let cands = [x | x <- env, vWidth x <= 3]
     if null cands then assign else do
       s <- pick cands
       let sw = vWidth s
       nArms <- rand (fromInteger (min 4 (2^sw)))
       labels <- fmap nub (replicateM (nArms+1) (randInteger (2^sw)))
       multi <- chance 20
       arms <- if multi && length labels >= 2
         then do b <- sub; rest <- forM (drop 2 labels) (\l -> do b' <- sub; return (VCaseArm [lit sw l] b'))
                 return (VCaseArm (map (lit sw) (take 2 labels)) b : rest)
         else forM labels (\l -> do b <- sub; return (VCaseArm [lit sw l] b))
       dflt <- sub
       -- A no-default process retains its value: opaque in strict, compared
       -- literally after alias normalization, never through normStmt.
       noDefault <- chance 20
       return (Vcase (var (vName s)) (if noDefault && not (null arms) then arms else arms ++ [VDefault dflt]) False False)
  else if k < 70 then do
     c <- genE 2 1 env
     a <- sub; b <- sub
     return (Vifelse c a b)
  else assign
  where
    assign = do e <- genE 3 (vWidth out) env; return (VAssign (VLId (v (vName out))) e)
    sub = if d <= 0 then assign else genStmt (d-1) out env

genProg :: Gen Prog
genProg = do
  nIn <- pick [2,3,3]
  widths <- replicateM nIn (pick [1,1,2,2,3,4])
  let ins = zipWith Var ["a","b","c"] widths
  ow <- pick [1,1,2,4,8]
  nH <- pick [0,0,1,1,2,3]
  let go _ acc 0 = return (reverse acc)
      go env acc n = do
        hw <- pick [1,2,4,8,3]
        e <- genE 3 hw env
        let h = Var ("h" ++ show (length acc)) hw
        go (env ++ [h]) ((h,e):acc) (n-1 :: Int)
  hs <- go ins [] nH
  let env = ins ++ map fst hs
      out = Var "o" ow
  proc <- chance 35
  body <- if proc then Proc . VSeq . (:[]) <$> genStmt 2 out env else Cont <$> genE 4 ow env
  return (pruneHelpers (Prog ins out hs body))

-- Local rewrites. "s-" rules are intended four-state identities (some only
-- when the operand cannot be Z; the oracle decides), "u-" rules are Z-unsafe
-- shapes on plain wires, "w-" rules change widths, "m-" rules are mutations.
complements :: [(VOp,VOp)]
complements = [(VULT,VUGE),(VULE,VUGT),(VUGT,VULE),(VUGE,VULT),(VEQ,VNE),(VNE,VEQ)]
truthShaped :: VExpr -> Bool
truthShaped e = case e of
  VEOp _ _ k _ -> k `elem` [VLAnd,VLOr,VULT,VULE,VUGT,VUGE,VEQ,VNE,VEQ3,VNE3]
  VEUnOp _ VNot _ -> True
  VEWConst _ 1 _ _ -> True
  _ -> False
boolLit :: Bool -> VExpr
boolLit b = lit 1 (if b then 1 else 0)
localRules :: [(String, VExpr -> Maybe VExpr)]
localRules =
  [ ("s-commute-logical", \e -> case e of VEOp i a k b | k `elem` [VLAnd,VLOr] -> Just (VEOp i b k a); _ -> Nothing)
  , ("s-reassoc-logical", \e -> case e of VEOp i (VEOp j a k b) k' c | k == k', k `elem` [VLAnd,VLOr] -> Just (VEOp i a k (VEOp j b k' c)); _ -> Nothing)
  , ("s-not-relation", \e -> case e of VEUnOp _ VNot (VEOp i a k b) | Just k' <- lookup k complements -> Just (VEOp i a k' b); _ -> Nothing)
  , ("s-relation-to-not", \e -> case e of VEOp i a k b | Just k' <- lookup k complements -> Just (VEUnOp i VNot (VEOp i a k' b)); _ -> Nothing)
  , ("s-absorb-true-or", \e -> case e of VEOp _ (VEWConst _ _ _ n) VLOr _ | n /= 0 -> Just (boolLit True); VEOp _ _ VLOr (VEWConst _ _ _ n) | n /= 0 -> Just (boolLit True); _ -> Nothing)
  , ("s-absorb-false-and", \e -> case e of VEOp _ (VEWConst _ _ _ 0) VLAnd _ -> Just (boolLit False); VEOp _ _ VLAnd (VEWConst _ _ _ 0) -> Just (boolLit False); _ -> Nothing)
  , ("s-drop-false-or", \e -> case e of VEOp _ (VEWConst _ _ _ 0) VLOr b -> Just b; VEOp _ a VLOr (VEWConst _ _ _ 0) -> Just a; _ -> Nothing)
  , ("s-drop-true-and", \e -> case e of VEOp _ (VEWConst _ _ _ n) VLAnd b | n /= 0 -> Just b; VEOp _ a VLAnd (VEWConst _ _ _ n) | n /= 0 -> Just a; _ -> Nothing)
  , ("s-intro-false-or-truth", \e -> if truthShaped e then Just (op VLOr (lit 8 0) e) else Nothing)
  , ("s-intro-true-and-truth", \e -> if truthShaped e then Just (op VLAnd (lit 4 9) e) else Nothing)
  , ("u-intro-false-or-any", \e -> case e of VEVar _ -> Just (op VLOr (lit 1 0) e); VESelect1 _ _ -> Just (op VLOr (lit 1 0) e); _ -> Nothing)
  , ("u-dup-and-any", \e -> case e of VEVar _ -> Just (op VLAnd e e); _ -> Nothing)
  , ("s-dup-if-arm", \e -> case e of VEIf _ t f | t == f -> Just t; _ -> Nothing)
  , ("s-double-not", \e -> case e of VEUnOp _ VNot (VEUnOp _ VNot x) -> Just x; _ -> Nothing)
  , ("s-intro-double-not", \e -> if truthShaped e then Just (un VNot (un VNot e)) else Nothing)
  , ("u-intro-double-not-any", \e -> case e of VEVar _ -> Just (un VNot (un VNot e)); _ -> Nothing)
  , ("w-widen-constant", \e -> case e of VEWConst i w b n | w < 8 -> Just (VEWConst i (w+1) b n); _ -> Nothing)
  , ("w-narrow-constant", \e -> case e of VEWConst i w b n | w > 1, n < 2^(w-1) -> Just (VEWConst i (w-1) b n); _ -> Nothing)
  , ("s-fold-const-relation", \e -> case e of
        VEOp _ (VEWConst _ _ _ a) k (VEWConst _ _ _ b) | Just f <- lookup k folds -> Just (boolLit (f a b))
        _ -> Nothing)
  , ("s-fold-const-not", \e -> case e of VEUnOp _ VNot (VEWConst _ _ _ n) -> Just (boolLit (n == 0)); _ -> Nothing)
  , ("s-select1-to-select", \e -> case e of VESelect1 x i -> Just (VESelect x i i); _ -> Nothing)
  , ("s-select-to-select1", \e -> case e of VESelect x i j | i == j -> Just (VESelect1 x i); _ -> Nothing)
  , ("s-swap-if-negated", \e -> case e of VEIf c t f -> Just (VEIf (un VNot c) f t); _ -> Nothing)
  , ("m-swap-relation", \e -> case e of VEOp i a k b | k `elem` [VULT,VULE,VUGT,VUGE] -> Just (VEOp i b k a); _ -> Nothing)
  , ("m-strictness", \e -> case e of VEOp i a VULT b -> Just (VEOp i a VULE b); VEOp i a VULE b -> Just (VEOp i a VULT b); _ -> Nothing)
  , ("m-flip-const-bit", \e -> case e of VEWConst i w b n -> Just (VEWConst i w b (n `xor` 1)); _ -> Nothing)
  , ("m-and-or", \e -> case e of VEOp i a VLAnd b -> Just (VEOp i a VLOr b); VEOp i a VLOr b -> Just (VEOp i a VLAnd b); VEOp i a VAnd b -> Just (VEOp i a VOr b); _ -> Nothing)
  , ("m-swap-if", \e -> case e of VEIf c t f | t /= f -> Just (VEIf c f t); _ -> Nothing)
  , ("m-select-shift", \e -> case e of VESelect x (VEConst h) (VEConst l) | l > 0 -> Just (VESelect x (VEConst (h-1)) (VEConst (l-1))); _ -> Nothing)
  , ("m-eq-eq3", \e -> case e of VEOp i a VEQ b -> Just (VEOp i a VEQ3 b); VEOp i a VNE b -> Just (VEOp i a VNE3 b); _ -> Nothing)
  , ("m-drop-not", \e -> case e of VEUnOp _ VNot x | truthShaped x -> Just x; _ -> Nothing)
  , ("m-tri-x-to-0", \e -> case e of VETriConst bits | any (`elem` [Vx,Vz]) bits -> Just (VETriConst (map (\b -> if b `elem` [Vx,Vz] then V0 else b) bits)); _ -> Nothing)
  , ("m-logical-to-bitwise", \e -> case e of VEOp i a VLAnd b -> Just (VEOp i a VAnd b); VEOp i a VLOr b -> Just (VEOp i a VOr b); _ -> Nothing)
  , ("m-reduce-kind", \e -> case e of VEUnOp i VAnd x -> Just (VEUnOp i VOr x); VEUnOp i VOr x -> Just (VEUnOp i VAnd x); _ -> Nothing)
  ]
  where folds = [(VULT,(<)),(VULE,(<=)),(VUGT,(>)),(VUGE,(>=)),(VEQ,(==)),(VNE,(/=))]

exprsOf :: Prog -> [VExpr]
exprsOf p = map snd (pHelpers p) ++ (case pBody p of Cont e -> [e]; Proc s -> G.listify (const True) s)
-- Apply a local rule at one randomly chosen hit (all structurally equal
-- occurrences are rewritten, which keeps the rewrite well defined).
applyLocal :: (VExpr -> Maybe VExpr) -> Prog -> Gen (Maybe Prog)
applyLocal rule p = do
  let hits = nub (concatMap (G.listify (isJust . rule)) (exprsOf p))
  if null hits then return Nothing else do
    h <- pick hits
    let f e = if e == h then maybe e id (rule e) else e
        rw :: G.Data a => a -> a
        rw = G.everywhere (G.mkT f)
    return (Just p { pHelpers = [(x,rw e) | (x,e) <- pHelpers p]
                   , pBody = case pBody p of Cont e -> Cont (rw e); Proc s -> Proc (rw s) })

-- Variable occurrences and substitution in expression positions only: the
-- base of a bit/part select stays an identifier.
occE :: VExpr -> [String]
occE e = case e of
  VEVar i -> [getVIdString i]
  VESelect b h l -> baseless b ++ occE h ++ occE l
  VESelect1 b p -> baseless b ++ occE p
  VEUnOp _ _ a -> occE a
  VEOp _ a _ b -> occE a ++ occE b
  VEConcat xs -> concatMap occE xs
  VERepeat n a -> occE n ++ occE a
  VEIf c t f -> occE c ++ occE t ++ occE f
  _ -> []
  where baseless b = case b of VEVar _ -> []; _ -> occE b
occS :: VStmt -> [String]
occS s = case s of
  VSeq xs -> concatMap occS xs
  Vif c a -> occE c ++ occS a
  Vifelse c a b -> occE c ++ occS a ++ occS b
  Vcase c arms _ _ -> occE c ++ concat [occE l | VCaseArm ls _ <- arms, l <- ls] ++ concatMap armOcc arms
  VAssign _ e -> occE e
  _ -> []
  where armOcc (VCaseArm _ b) = occS b
        armOcc (VDefault b) = occS b
substE :: String -> VExpr -> VExpr -> VExpr
substE x r e = case e of
  VEVar i | getVIdString i == x -> r
  VESelect b h l -> VESelect (keepBase b) (go h) (go l)
  VESelect1 b p -> VESelect1 (keepBase b) (go p)
  VEUnOp i o a -> VEUnOp i o (go a)
  VEOp i a o b -> VEOp i (go a) o (go b)
  VEConcat xs -> VEConcat (map go xs)
  VERepeat n a -> VERepeat (go n) (go a)
  VEIf c t f -> VEIf (go c) (go t) (go f)
  _ -> e
  where go = substE x r
        keepBase b = case b of VEVar _ -> b; _ -> go b
substS :: String -> VExpr -> VStmt -> VStmt
substS x r s = case s of
  VSeq xs -> VSeq (map go xs)
  Vif c a -> Vif (ge c) (go a)
  Vifelse c a b -> Vifelse (ge c) (go a) (go b)
  Vcase c arms pf ff -> Vcase (ge c) (map arm arms) pf ff
  VAssign l e -> VAssign l (ge e)
  _ -> s
  where go = substS x r; ge = substE x r
        arm (VCaseArm ls b) = VCaseArm (map ge ls) (go b)
        arm (VDefault b) = VDefault (go b)
varOccurrences :: Prog -> [String]
varOccurrences p = nub (concatMap (occE . snd) (pHelpers p) ++ (case pBody p of Cont e -> occE e; Proc s -> occS s))
substVar :: String -> VExpr -> Prog -> Prog
substVar x r p = p { pHelpers = [(h,substE x r e) | (h,e) <- pHelpers p], pBody = case pBody p of Cont e -> Cont (substE x r e); Proc s -> Proc (substS x r s) }

-- Drop helpers the output cannot observe, so rewrites inside them are not
-- trivially equal on both sides.
pruneHelpers :: Prog -> Prog
pruneHelpers p = p { pHelpers = [h | h@(Var n _,_) <- pHelpers p, n `S.member` live] }
  where
    namesOf :: VExpr -> [String]
    namesOf e = [getVIdString i | VEVar i <- G.listify (const True) e]
    bodyNames = case pBody p of
      Cont e -> namesOf e
      Proc s -> concatMap namesOf (G.listify (const True) s :: [VExpr])
    defs = M.fromList [(n, namesOf e) | (Var n _, e) <- pHelpers p]
    live = go S.empty bodyNames
    go seen [] = seen
    go seen (n:ns) | n `S.member` seen = go seen ns
                   | otherwise = go (S.insert n seen) (M.findWithDefault [] n defs ++ ns)
lookupWidth :: Prog -> String -> Maybe Integer
lookupWidth p n = lookup n ([(vName x,vWidth x) | x <- pInputs p] ++ [(vName x,vWidth x) | (x,_) <- pHelpers p])
freshHelper :: Prog -> String
freshHelper p = head [n | i <- [0::Int ..], let n = "k" ++ show i, n `notElem` map (vName . fst) (pHelpers p)]

programRules :: [(String, Prog -> Gen (Maybe Prog))]
programRules =
  [ ("s-alias-same", aliasWith 0)
  , ("w-alias-wider", \p -> do k <- pick [1,2,3,4]; aliasWith k p)
  , ("s-inline-helper", \p -> case pHelpers p of
        [] -> return Nothing
        hs -> do (h,e) <- pick hs
                 return (Just (substVar (vName h) e p)))
  , ("s-intro-concat-field-low", \p -> intro p (\x y w _ -> (VEConcat [var y,var x], w-1, 0)))
  , ("s-intro-concat-field-high", \p -> intro p (\x y w wy -> (VEConcat [var x,var y], w+wy-1, wy)))
  , ("s-intro-dup-if", \p -> case varOccurrences p of
        [] -> return Nothing
        xs -> do x <- pick xs
                 c <- pick (map vName (pInputs p))
                 return (Just (substVar x (VEIf (var c) (var x) (var x)) p)))
  , ("s-case-fact", \p -> case pBody p of
        Proc (VSeq [Vcase (VEVar s) arms pf ff]) | Just sw <- lookupWidth p (getVIdString s) -> do
          let sn = getVIdString s
              candidates = [(l,i) | (i,VCaseArm [VEWConst _ _ _ l] body) <- zip [0::Int ..] arms, sn `elem` occS body]
          if null candidates then return Nothing else do
            (l,i) <- pick candidates
            let arm' (j,VCaseArm ls body) | j == i = VCaseArm ls (substS sn (lit sw l) body)
                arm' (_,a) = a
            return (Just p { pBody = Proc (VSeq [Vcase (VEVar s) (map arm' (zip [0..] arms)) pf ff]) })
        _ -> return Nothing)
  , ("s-case-redundant-arm", \p -> case pBody p of
        Proc (VSeq [Vcase (VEVar s) arms pf ff]) | [d] <- [b | VDefault b <- arms], Just sw <- lookupWidth p (getVIdString s) -> do
          let used = [l | VCaseArm ls _ <- arms, VEWConst _ _ _ l <- ls]
              free = [l | l <- [0..2^sw-1], l `notElem` used]
          if null free then return Nothing else do
            l <- pick free
            return (Just p { pBody = Proc (VSeq [Vcase (VEVar s) (VCaseArm [lit sw l] d : arms) pf ff]) })
        _ -> return Nothing)
  , ("s-case-drop-default-equal-arm", \p -> case pBody p of
        Proc (VSeq [Vcase (VEVar s) arms pf ff]) | [d] <- [b | VDefault b <- arms] -> do
          let rest = [a | a@(VCaseArm _ b) <- arms, b /= d]
          if length rest == length arms - 1 then return Nothing
            else return (Just p { pBody = Proc (VSeq [Vcase (VEVar s) (rest ++ [VDefault d]) pf ff]) })
        _ -> return Nothing)
  , ("m-case-relabel", \p -> case pBody p of
        Proc (VSeq [Vcase (VEVar s) arms pf ff]) | Just sw <- lookupWidth p (getVIdString s) -> do
          let used = [l | VCaseArm ls _ <- arms, VEWConst _ _ _ l <- ls]
              free = [l | l <- [0..2^sw-1], l `notElem` used]
              singles = [i | (i,VCaseArm [_] _) <- zip [0::Int ..] arms]
          if null free || null singles then return Nothing else do
            l <- pick free; i <- pick singles
            let arm' (j,VCaseArm _ body) | j == i = VCaseArm [lit sw l] body
                arm' (_,a) = a
            return (Just p { pBody = Proc (VSeq [Vcase (VEVar s) (map arm' (zip [0..] arms)) pf ff]) })
        _ -> return Nothing)
  , ("m-swap-variable", \p -> case varOccurrences p of
        [] -> return Nothing
        xs -> do x <- pick xs
                 let w = lookupWidth p x
                     others = [n | n <- map vName (pInputs p) ++ map (vName . fst) (pHelpers p), n /= x, lookupWidth p n == w]
                 if null others then return Nothing else do
                   y <- pick others
                   return (Just (substVar x (var y) p)))
  ]
  where
    aliasWith extra p = case [x | x <- varOccurrences p, isJust (lookupWidth p x)] of
      [] -> return Nothing
      xs -> do
        x <- pick xs
        let Just w = lookupWidth p x
            k = min 8 (w + extra)
            h = freshHelper p
        if k == w && extra /= 0 then return Nothing else
          return (Just (substVar x (var h) p) { pHelpers = pHelpers (substVar x (var h) p) ++ [(Var h k, var x)] })
    intro p shape = case [x | x <- varOccurrences p, isJust (lookupWidth p x)] of
      [] -> return Nothing
      xs -> do
        x <- pick xs
        y <- pick (map vName (pInputs p))
        let Just w = lookupWidth p x
            Just wy = lookupWidth p y
            (packed,hi,lo) = shape x y w wy
            h = freshHelper p
            use = if hi == lo then VESelect1 (var h) (VEConst lo) else VESelect (var h) (VEConst hi) (VEConst lo)
        return (Just (substVar x use p) { pHelpers = pHelpers (substVar x use p) ++ [(Var h (w+wy), packed)] })

-- One rewrite drawn uniformly from the rules applicable to this program.
candidateOf :: Prog -> Gen (Maybe (String,Prog))
candidateOf p = do
  locals <- forM localRules (\(tag,rule) -> fmap (fmap ((,) tag)) (applyLocal rule p))
  progs <- forM programRules (\(tag,rule) -> fmap (fmap ((,) tag)) (rule p))
  case [(t,c) | Just (t,c) <- locals ++ progs, c /= p] of
    [] -> return Nothing
    xs -> Just <$> pick xs

data Expect = MustAccept | MustReject | Oracle deriving (Eq,Show)
-- Seeded corners and observable wrong rewrites (label, expectation, reference, candidate).
seeded :: [(String,Expect,Prog,Prog)]
seeded =
  [ ("seed-wide-const-or-absorb", MustAccept, cont [a8,b1] o1 [] (op VLOr (lit 8 5) (var "b")), cont [a8,b1] o1 [] (lit 1 1))
  , ("seed-wide-zero-or-relation", MustAccept, cont [a8,b8] o1 [] (op VLOr (lit 8 0) (op VULT (var "a") (var "b"))), cont [a8,b8] o1 [] (op VULT (var "a") (var "b")))
  , ("seed-zero-or-plain-wire", MustReject, cont [a8,b1] o1 [] (op VLOr (lit 1 0) (var "b")), cont [a8,b1] o1 [] (var "b"))
  , ("seed-if-equal-arms-x-cond", MustAccept, cont [a8,b1] o8 [] (VEIf (var "b") (var "a") (var "a")), cont [a8,b1] o8 [] (var "a"))
  , ("seed-if-equal-arms-wide-cond", MustAccept, cont [a8,b8] o8 [] (VEIf (var "b") (op VAdd (var "a") (lit 8 1)) (op VAdd (var "a") (lit 8 1))), cont [a8,b8] o8 [] (op VAdd (var "a") (lit 8 1)))
  , ("seed-not-eq-vs-ne", MustAccept, cont [a8,b8] o1 [] (un VNot (op VEQ (var "a") (var "b"))), cont [a8,b8] o1 [] (op VNE (var "a") (var "b")))
  , ("seed-not-lt-vs-ge-mixed-width", MustAccept, cont [a8,b4] o1 [] (un VNot (op VULT (var "a") (var "b"))), cont [a8,b4] o1 [] (op VUGE (var "a") (var "b")))
  , ("seed-inv-through-wider-alias", Oracle, cont [a4,b1] o8 [] (un VInv (var "a")), cont [a4,b1] o8 [(Var "y" 8,var "a")] (un VInv (var "y")))
  , ("seed-neg-through-wider-alias", Oracle, cont [a4,b1] o8 [] (un VNeg (var "a")), cont [a4,b1] o8 [(Var "y" 8,var "a")] (un VNeg (var "y")))
  , ("seed-add-through-wider-alias", Oracle, cont [a4,b8] o8 [] (op VAdd (var "a") (var "b")), cont [a4,b8] o8 [(Var "y" 8,var "a")] (op VAdd (var "y") (var "b")))
  , ("seed-shift-through-wider-alias", Oracle, cont [a4,b1] o8 [] (op VShL (var "a") (lit 4 2)), cont [a4,b1] o8 [(Var "y" 8,var "a")] (op VShL (var "y") (lit 4 2)))
  , ("seed-eq-through-wider-alias", Oracle, cont [a4,b8] o1 [] (op VEQ (var "a") (var "b")), cont [a4,b8] o1 [(Var "y" 8,var "a")] (op VEQ (var "y") (var "b")))
  , ("seed-concat-through-wider-alias", Oracle, cont [a4,b4] o8 [] (VEConcat [var "b",var "a"]), cont [a4,b4] o8 [(Var "y" 8,var "a")] (VEConcat [var "b",var "y"]))
  , ("seed-const-4-vs-8-in-8-context", MustAccept, cont [a8,b1] o8 [] (VEIf (var "b") (lit 4 3) (var "a")), cont [a8,b1] o8 [] (VEIf (var "b") (lit 8 3) (var "a")))
  , ("seed-case-arm-fact", Oracle, proc' [a8,b2] o8 [] (Vcase (var "b") [VCaseArm [lit 2 1] (as (op VAdd (VEConcat [lit 6 0,var "b"]) (var "a"))),VDefault (as (var "a"))] False False)
                           , proc' [a8,b2] o8 [] (Vcase (var "b") [VCaseArm [lit 2 1] (as (op VAdd (lit 8 1) (var "a"))),VDefault (as (var "a"))] False False))
  , ("seed-case-redundant-arm", MustAccept, proc' [a8,b2] o8 [] (Vcase (var "b") [VCaseArm [lit 2 1] (as (var "a")),VDefault (as (un VInv (var "a")))] False False)
                              , proc' [a8,b2] o8 [] (Vcase (var "b") [VCaseArm [lit 2 1] (as (var "a")),VCaseArm [lit 2 2] (as (un VInv (var "a"))),VDefault (as (un VInv (var "a")))] False False))
  , ("seed-retaining-case-alias", MustAccept, proc' [a8,b2] o8 [] (retain (var "b")), proc' [a8,b2] o8 [(Var "y" 2,var "b")] (retain (var "y")))
  -- A reduction's operand is self-determined: through a wider alias it is not
  -- the same function, and the width model must say so.
  , ("seed-reduce-and-through-wider-alias", MustReject, cont [a4,b1] o8 [] (un VAnd (var "a")), cont [a4,b1] o8 [(Var "y" 8,var "a")] (un VAnd (var "y")))
  , ("seed-reduce-and-1bit-context", MustReject, cont [a4,b1] o1 [] (un VAnd (var "a")), cont [a4,b1] o1 [(Var "y" 8,var "a")] (un VAnd (var "y")))
  , ("seed-reduce-or-through-wider-alias", Oracle, cont [a4,b1] o8 [] (un VOr (var "a")), cont [a4,b1] o8 [(Var "y" 8,var "a")] (un VOr (var "y")))
  , ("seed-reduce-same-alias", MustAccept, cont [a4,b1] o8 [] (un VAnd (var "a")), cont [a4,b1] o8 [(Var "y" 4,var "a")] (un VAnd (var "y")))
  , ("neg-sub-commute", MustReject, cont [a4,b4] o4 [] (op VSub (var "a") (var "b")), cont [a4,b4] o4 [] (op VSub (var "b") (var "a")))
  , ("neg-select-shift", MustReject, cont [a4,b1] o1 [] (VESelect1 (var "a") (VEConst 0)), cont [a4,b1] o1 [] (VESelect1 (var "a") (VEConst 1)))
  , ("neg-drop-not", MustReject, cont [a4,b4] o1 [] (un VNot (op VULT (var "a") (var "b"))), cont [a4,b4] o1 [] (op VULT (var "a") (var "b")))
  , ("neg-dup-and-raw", MustReject, cont [a4,b1] o1 [] (op VLAnd (var "b") (var "b")), cont [a4,b1] o1 [] (var "b"))
  , ("neg-const-width", MustReject, cont [a8,b1] o8 [] (lit 8 240), cont [a8,b1] o8 [] (lit 4 15))
  , ("neg-cross-concat-select", MustReject, cont [a4,b4] o4 [(Var "h" 8,VEConcat [var "a",var "b"])] (VESelect (var "h") (VEConst 4) (VEConst 1)), cont [a4,b4] o4 [] (VEConcat [VESelect1 (var "a") (VEConst 1),VESelect (var "b") (VEConst 3) (VEConst 1)]))
  ]
  where
    a8 = Var "a" 8; a4 = Var "a" 4; b1 = Var "b" 1; b2 = Var "b" 2; b4 = Var "b" 4; b8 = Var "b" 8
    o1 = Var "o" 1; o4 = Var "o" 4; o8 = Var "o" 8
    cont ins out hs e = Prog ins out hs (Cont e)
    proc' ins out hs s = Prog ins out hs (Proc (VSeq [s]))
    as e = VAssign (VLId (v "o")) e
    retain sel = Vcase sel [VCaseArm [lit 2 1] (as (var "a")),VCaseArm [lit 2 2] (as (un VInv (var "a")))] False False

data Pair = Pair { pIndex :: Int, pTag :: String, pRef :: Prog, pCand :: Prog, pExpectEqual :: Bool }

-- One Icarus run per batch: every pair drives its own modules from its own
-- vectors (exhaustive 4^T when the inputs hold at most 6 bits, otherwise the
-- four uniform corners plus 1024 sampled vectors) and reports the first
-- mismatching vector.
testbench :: [Pair] -> String
testbench ps = unlines (
  ["module tb;","  integer fails = 0; integer finished = 0;",
   "  function fs; input [1:0] code; begin case (code) 2'd0: fs=1'b0; 2'd1: fs=1'b1; 2'd2: fs=1'bx; default: fs=1'bz; endcase end endfunction"] ++
  concat (zipWith one [0::Int ..] ps) ++
  ["  initial begin wait (finished == "++show (length ps)++"); if (fails == 0) $display(\"PASS: "++show (length ps)++" pairs\"); else $display(\"FAIL: %0d mismatching pairs\", fails); $finish; end",
   "endmodule"])
  where
    one k p =
      let ins = pInputs (pRef p); t = sum (map vWidth ins); w = vWidth (pOut (pRef p))
          offsets = scanl (+) 0 (map vWidth ins)
          conn = intercalate "," ["."++vName x++"(bits_"++s++"["++show (o+vWidth x-1)++":"++show o++"])" | (x,o) <- zip ins offsets]
          exhaustive = t <= 6
          n = if exhaustive then 4^t else 4+1024 :: Integer
          s = show k
      in ["  reg ["++show (t-1)++":0] bits_"++s++"; wire ["++show (w-1)++":0] l_"++s++", r_"++s++"; reg ["++show (2*t-1)++":0] code_"++s++"; integer n_"++s++", b_"++s++", seed_"++s++", bad_"++s++";",
          "  ref_"++s++" ref_"++s++"_i("++conn++",.o(l_"++s++"));",
          "  cand_"++s++" cand_"++s++"_i("++conn++",.o(r_"++s++"));",
          "  initial begin seed_"++s++" = "++show (pIndex p * 7919 + 13)++"; bad_"++s++" = 0;",
          "    for (n_"++s++" = 0; n_"++s++" < "++show n++"; n_"++s++" = n_"++s++" + 1) begin",
          (if exhaustive then "      code_"++s++" = n_"++s++";"
           else "      code_"++s++" = (n_"++s++" < 4) ? {"++show t++"{n_"++s++"[1:0]}} : $random(seed_"++s++");"),
          "      for (b_"++s++" = 0; b_"++s++" < "++show t++"; b_"++s++" = b_"++s++" + 1) bits_"++s++"[b_"++s++"] = fs(code_"++s++"[2*b_"++s++" +: 2]);",
          "      #1; if (l_"++s++" !== r_"++s++" && bad_"++s++" == 0) begin $display(\"MISMATCH pair="++s++" bits=%b left=%b right=%b\", bits_"++s++", l_"++s++", r_"++s++"); bad_"++s++" = 1; fails = fails + 1; end",
          "    end",
          "    finished = finished + 1;",
          "  end"]

rename :: String -> VProgram -> VProgram
rename n (VProgram [m] ds cs) = VProgram [m {vm_name = v n}] ds cs
rename _ p = p

-- Right: for each pair, whether simulation found it equal. Left: compile or run failure.
simulate :: FilePath -> String -> String -> [Pair] -> IO (Either String [Bool])
simulate base iverilog vvp ps = do
  let source = concat [pp80 (rename ("ref_"++show k) (toProgram "m" (pRef p))) ++ "\n" ++
                       pp80 (rename ("cand_"++show k) (toProgram "m" (pCand p))) ++ "\n" | (k,p) <- zip [0::Int ..] ps] ++ testbench ps
      rtl = base ++ ".v"; exe = base ++ ".sim"
  writeFile rtl source
  (compiled,cout,cerr) <- readProcessWithExitCode iverilog ["-g2012","-s","tb","-o",exe,rtl] ""
  if compiled /= ExitSuccess then do
      writeFile (base ++ ".compile.log") (cout ++ cerr)
      return (Left ("compile failure: " ++ take 400 (cout ++ cerr)))
    else do
      (ran,sout,serr) <- readProcessWithExitCode vvp [exe] ""
      writeFile (base ++ ".run.log") (sout ++ serr)
      let bad = S.fromList [read (takeWhile (/= ' ') (drop (length "MISMATCH pair=") l)) :: Int | l <- lines sout, "MISMATCH pair=" `isPrefixOf` l]
      if ran == ExitSuccess && ("PASS:" `isInfixOf` sout || "FAIL:" `isInfixOf` sout)
        then return (Right [k `S.notMember` bad | k <- [0 .. length ps - 1]])
        else return (Left ("run failure: " ++ take 400 (sout ++ serr)))

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  (out,iverilog,vvp,count,seed0) <- case args of
    [o,i,s] -> return (o,i,s,3000,1)
    [o,i,s,c] -> return (o,i,s,read c,1)
    [o,i,s,c,sd] -> return (o,i,s,read c,read sd)
    _ -> error "ASTProofFuzz OUT IVERILOG VVP [COUNT [SEED]]"
  createDirectoryIfMissing True (out </> "batches")
  perRule <- newIORef (M.empty :: M.Map String (Int,Int,Int))  -- rejected, structural, divergent
  failures <- newIORef ([] :: [String])
  queue <- newIORef ([] :: [Pair])
  skipped <- newIORef (0 :: Int)
  let bump tag which = modifyIORef' perRule (M.insertWith (\(a,b,c) (a',b',c') -> (a+a',b+b',c+c')) tag which)
      failed idx tag why ref cand = do
        let dir = out </> ("case-" ++ show idx)
        createDirectoryIfMissing True dir
        writeFile (dir </> "FAILURE.txt") (unlines [tag, why, "REFERENCE:", show ref, "CANDIDATE:", show cand])
        writeFile (dir </> "pair.v") (pp80 (rename "reference" (toProgram "m" ref)) ++ "\n" ++ pp80 (rename "candidate" (toProgram "m" cand)))
        modifyIORef' failures (("case-" ++ show idx ++ " " ++ tag ++ ": " ++ why) :)
        putStrLn ("FAILURE case-" ++ show idx ++ " " ++ tag ++ ": " ++ why)
      examine idx tag expect ref cand = do
        let verdict = checkAST StrictFourState [] (toProgram "m" ref) (toProgram "m" cand)
            accepted = either (const False) (null . proofResidualRoots) verdict
            code = either (\is -> intercalate "," (map issueCode is)) (const "") verdict
        when (either (const False) (not . null . proofResidualRoots) verdict) (failed idx tag "strict bundle with residual roots" ref cand)
        bump tag (if accepted then (0,1,0) else (1,0,0))
        case (expect,accepted) of
          (MustAccept,False) -> failed idx tag ("expected a structural pass, got " ++ code) ref cand
          (MustReject,True) -> failed idx tag "expected rejection, got a structural pass" ref cand
          _ -> return ()
        -- Every structural pass is simulated; a seeded wrong rewrite is
        -- simulated for its mismatch witness.
        when (accepted || expect == MustReject) (modifyIORef' queue (Pair idx tag ref cand accepted :))
  forM_ (zip [1::Int ..] seeded) (\(i,(label,expect,ref,cand)) -> examine (negate i) label expect ref cand)
  let loop i s | i >= count = return ()
               | otherwise = do
        let (prog,s1) = runGen genProg s
            (choice,s2) = runGen (candidateOf prog) s1
        case choice of
          Nothing -> modifyIORef' skipped (+1)
          Just (tag,cand) -> examine i tag Oracle prog cand
        loop (i+1) s2
  loop 0 seed0
  pairs <- reverse <$> readIORef queue
  let batches = chunks 20 pairs
      chunks _ [] = []
      chunks n xs = let (a,b) = splitAt n xs in a : chunks n b
      run label ps = do
        r <- simulate (out </> "batches" </> label) iverilog vvp ps
        case r of
          Right equal -> forM_ (zip ps equal) (\(p,eq) -> do
            when (eq /= pExpectEqual p) (do
              bump (pTag p) (0,0,1)
              failed (pIndex p) (pTag p) (if eq then "rejected rewrite has no simulation witness" else "structural pass but four-state simulation mismatch (see " ++ label ++ ".run.log)") (pRef p) (pCand p)))
          Left err | length ps > 1 -> forM_ (zip [0::Int ..] ps) (\(k,p) -> run (label ++ "-" ++ show k) [p])
                   | otherwise -> forM_ ps (\p -> failed (pIndex p) (pTag p) ("oracle error: " ++ err) (pRef p) (pCand p))
  forM_ (zip [0::Int ..] batches) (\(n,ps) -> run ("batch-" ++ show n) ps)
  rules <- readIORef perRule
  fails <- reverse <$> readIORef failures
  noop <- readIORef skipped
  let totals f = sum (map f (M.elems rules))
  forM_ (M.toAscList rules) (\(tag,(rj,st,dv)) -> putStrLn ("RULE " ++ tag ++ " rejected=" ++ show rj ++ " structural=" ++ show st ++ " divergent=" ++ show dv))
  putStrLn ("SUMMARY attempts=" ++ show count ++ " seeded=" ++ show (length seeded) ++ " inapplicable=" ++ show noop ++
            " rejected=" ++ show (totals (\(a,_,_) -> a)) ++ " structural=" ++ show (totals (\(_,b,_) -> b)) ++
            " simulated=" ++ show (length pairs) ++ " seed=" ++ show seed0)
  if null fails then putStrLn ("PASS: " ++ show (totals (\(_,b,_) -> b)) ++ " strict structural verdicts agree with four-state simulation; " ++
                               show (length [() | (_,MustReject,_,_) <- seeded]) ++ " seeded wrong rewrites rejected with witnesses")
    else do putStrLn ("FAILURES:\n" ++ unlines fails); exitFailure
