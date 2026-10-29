-- The printer emits identifier and comment strings verbatim, so a proved AST
-- is emittable only when no string can escape its syntactic position and no
-- name is used before its declaration. Both predicates are pure, position
-- aware and shared textually with src/comp/ECO/VerilogInfo.hs (native emitter):
-- the body below this header is the same text as that module's predicate
-- section, and testsuite/bsc.eco/verilog-info/verilog_info.exp checks it.
module ECOASTLexical (lexicallyEmittable, declaredBeforeUse) where

import Control.Monad (foldM, foldM_, forM_, unless, when)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.Set as S
import Verilog

-- Printed identifier positions, in print order. The annotation VIds of
-- VEOp/VEUnOp/VEWConst and the inlined item inside a VId are never printed;
-- constants, tri-constants, unknowns and quoted strings print no identifier.
data Printed = Ident String | Macro String

printedText :: Printed -> String
printedText (Ident s) = s
printedText (Macro s) = s

ident :: VId -> Printed
ident = Ident . getVIdString

exprP :: VExpr -> [Printed]
exprP e = case e of
    VEWConst {} -> []
    VEUnOp _ _ a -> exprP a
    VEOp _ a _ b -> exprP a ++ exprP b
    VEVar i -> [ident i]
    VEConcat es -> concatMap exprP es
    VEIndex i a -> ident i : exprP a
    VESelect a h l -> exprP a ++ exprP h ++ exprP l
    VESelect1 a p -> exprP a ++ exprP p
    VERepeat a b -> exprP a ++ exprP b
    VEIf c t f -> exprP c ++ exprP t ++ exprP f
    VEFctCall f es -> ident f : concatMap exprP es
    VEMacro s -> [Macro s]
    _ -> []

eventP :: VEventExpr -> [Printed]
eventP (VEEOr a b) = eventP a ++ eventP b
eventP (VEEposedge e) = exprP e
eventP (VEEnegedge e) = exprP e
eventP (VEE e) = exprP e
eventP (VEEMacro s e) = Macro s : exprP e

lvalueP :: VLValue -> [Printed]
lvalueP (VLId i) = [ident i]
lvalueP (VLConcat ls) = concatMap lvalueP ls
lvalueP (VLSub l e) = lvalueP l ++ exprP e

stmtP :: VStmt -> [Printed]
stmtP s = case s of
    VAt ev t -> eventP ev ++ stmtP t
    Valways t -> stmtP t
    Vinitial t -> stmtP t
    VSeq ts -> concatMap stmtP ts
    Vcasex e arms _ _ -> exprP e ++ concatMap armP arms
    Vcase e arms _ _ -> exprP e ++ concatMap armP arms
    VAssign l e -> lvalueP l ++ exprP e
    VAssignA l e -> lvalueP l ++ exprP e
    Vif e t -> exprP e ++ stmtP t
    Vifelse e t f -> exprP e ++ stmtP t ++ stmtP f
    Vdumpvars _ is -> map ident is
    VTask t es -> ident t : concatMap exprP es
    VAssert ev es -> eventP ev ++ concatMap exprP es
    VZeroDelay -> []
  where
    armP (VCaseArm es t) = concatMap exprP es ++ stmtP t
    armP (VDefault t) = stmtP t

rangeP :: Maybe VRange -> [Printed]
rangeP = maybe [] (\(h,l) -> exprP h ++ exprP l)

varP :: VVar -> [Printed]
varP (VVar i) = [ident i]
varP (VArray (l,h) i) = ident i : exprP l ++ exprP h

declP :: VVDecl -> [Printed]
declP (VVDecl _ r vs) = rangeP r ++ concatMap varP vs
declP (VVDWire r v e) = rangeP r ++ varP v ++ exprP e

connP :: (VId, Maybe VExpr) -> [Printed]
connP (i,e) = ident i : maybe [] exprP e

argP :: VArg -> [Printed]
argP (VAInput i r) = ident i : rangeP r
argP (VAInout i j r) = ident i : maybe [] (\k -> [ident k]) j ++ maybe [] rangeP r
argP (VAOutput i r) = ident i : rangeP r
argP (VAParameter i r e _) = ident i : rangeP r ++ exprP e

itemP :: VMItem -> [Printed]
itemP x = case x of
    VMDecl d -> declP d
    VMInst m i ps cs -> ident m : ident i : concatMap connP ps ++ concatMap connP cs
    VMAssign l e -> lvalueP l ++ exprP e
    VMStmt _ s -> stmtP s
    VMComment _ y -> itemP y
    VMRegGroup i _ _ y -> ident i : itemP y
    VMGroup _ yss -> concatMap (concatMap itemP) yss
    VMFunction (VFunction n r ds s) -> ident n : rangeP r ++ concatMap declP ds ++ stmtP s
    VMDPI d -> dpiP d

-- the C link name, when given, is printed verbatim before "="
dpiP :: VDPI -> [Printed]
dpiP (VDPI n clink _ _ args) = ident n : map Ident (maybe [] (:[]) clink) ++ [ident i | (i,_,_) <- args]

-- ^\$?[A-Za-z_][A-Za-z0-9_$.]*$
validIdentifier :: String -> Bool
validIdentifier ('$':s) = plain s
validIdentifier s = plain s

plain :: String -> Bool
plain (c:cs) = (letter c || c == '_') && all (\d -> letter d || isDigit d || d `elem` "_$.") cs
plain [] = False

letter :: Char -> Bool
letter c = isAsciiUpper c || isAsciiLower c

-- The printer emits every identifier and comment element verbatim: an element
-- containing a line break leaves its comment. Nothing else carries free text
-- (strings are quoted; instance parameters are named, with no comment).
lexicallyEmittable :: VProgram -> Either String ()
lexicallyEmittable (VProgram modules dpis header) = do
    forM_ (concatMap dpiP dpis ++ concatMap moduleP modules) $ \p ->
        unless (validIdentifier (printedText p)) (Left (printedText p))
    forM_ (header ++ concatMap commentP modules) $ \c ->
        when (any (`elem` "\r\n") c) (Left c)
  where
    moduleP m = ident (vm_name m) : concatMap argP (concatMap fst (vm_ports m)) ++
                concatMap itemP (vm_body m)
    commentP m = vm_comments m ++ concatMap snd (vm_ports m) ++ concatMap commentItem (vm_body m)
    commentItem (VMComment c y) = c ++ commentItem y
    commentItem (VMRegGroup _ _ c y) = c ++ commentItem y
    commentItem (VMGroup _ yss) = concatMap (concatMap commentItem) yss
    commentItem (VMStmt _ (VAssert _ es)) = [s | VEString s <- es] -- printed unquoted
    commentItem _ = []

-- Walking each body in print order, every identifier an expression, lvalue,
-- event, instance connection or parameter uses must already be declared by a
-- port, a parameter, an import-DPI function, or an earlier VMDecl/VVDWire,
-- VMFunction name or instance name. System names ($...) are never declared;
-- function bodies are opaque.
declaredBeforeUse :: VProgram -> Either String ()
declaredBeforeUse (VProgram modules dpis _) = mapM_ modul modules
  where
    imported = [getVIdString n | VDPI n _ _ _ _ <- dpis]
    modul m = foldM_ item (S.fromList (imported ++ concatMap portNames (concatMap fst (vm_ports m)))) (vm_body m)
    portNames (VAInout i j _) = getVIdString i : maybe [] ((:[]) . getVIdString) j
    portNames a = [getVIdString (vargName a)]
    name = getVIdString . vvName
    item declared x = case x of
        VMDecl (VVDecl _ r vs) -> used declared (rangeP r) >> return (foldr (S.insert . name) declared vs)
        VMDecl (VVDWire r v e) -> used declared (rangeP r ++ exprP e) >> return (S.insert (name v) declared)
        VMInst _ i ps cs -> used declared (concatMap conn ps ++ concatMap conn cs) >>
                            return (S.insert (getVIdString i) declared)
        VMAssign l e -> used declared (lvalueP l ++ exprP e) >> return declared
        VMStmt _ s -> used declared (stmtP s) >> return declared
        VMComment _ y -> item declared y
        VMRegGroup _ _ _ y -> item declared y
        VMGroup _ yss -> foldM item declared (concat yss)
        VMFunction (VFunction n _ _ _) -> return (S.insert (getVIdString n) declared)
        VMDPI (VDPI n _ _ _ _) -> return (S.insert (getVIdString n) declared)
    conn (_,e) = maybe [] exprP e
    used declared printed = case [s | Ident s <- printed, take 1 s /= "$", not (S.member s declared)] of
        [] -> Right ()
        s:_ -> Left s
