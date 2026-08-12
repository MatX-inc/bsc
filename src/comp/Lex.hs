module Lex(Token(..), LexItem(..), LexError(..), LFlags(..), prLexItem,
           isIdChar, isSym, convLexErrorToErrMsg) where
-- Token definitions shared by the Bluespec Classic lexer and parser.
-- The lexer itself is the Alex-generated LexAlex module (LexAlex.x),
-- which imports these types.
import Data.Char

import Util(itos)
import Position
import Error(ErrMsg(..))
import FStringCompat

-- import Debug.Trace

-- data structure for lexical errors
-- so raw error messages are not in LexItem
data LexError = LexBadCharLit
              | LexBadStringLit
              | LexBadLexChar Char
              | LexUntermComm Position
              | LexMissingNL
              deriving(Eq)

convLexErrorToErrMsg :: LexError -> ErrMsg
convLexErrorToErrMsg (LexBadCharLit) = EBadCharLit
convLexErrorToErrMsg (LexBadStringLit) = EBadStringLit
convLexErrorToErrMsg (LexBadLexChar c) = EBadLexChar c
convLexErrorToErrMsg (LexUntermComm p) = EUntermComm p
convLexErrorToErrMsg (LexMissingNL) = EMissingNL

data LexItem =
          L_varid FString
        | L_conid FString
        | L_varsym FString
        | L_consym FString
        | L_integer (Maybe Integer) Integer Integer                -- bit size (if specified), base, value
        | L_float Rational
        | L_char Char
        | L_string String
        | L_lpar
        | L_rpar
        | L_semi
        | L_uscore
        | L_bquote
        | L_lcurl
        | L_rcurl
        | L_lbra
        | L_rbra
        -- reserved words
        | L_action | L_case | L_class | L_data | L_deriving | L_do | L_else | L_foreign
        | L_if | L_import | L_in
        | L_infix | L_infixl | L_infixr
        | L_interface | L_instance
        | L_let | L_letseq | L_package | L_of
        | L_primitive | L_qualified | L_rules | L_signature | L_struct
        | L_then | L_module | L_type | L_valueOf | L_stringOf | L_verilog | L_via | L_synthesize | L_when | L_where
        | L_coherent | L_incoherent
        -- reserved ops
        | L_dcolon | L_colon | L_eq | L_at | L_lam | L_bar
        | L_rarrow | L_larrow | L_dot | L_comma | L_drarrow | L_irarrow
        -- layout items
        | L_lcurl_o | L_rcurl_o | L_semi_o
        -- pragma
        | L_lpragma | L_rpragma
        -- unbased unsized bit literals ('0 all-zeros, '1 all-ones)
        | L_unbasedUnsized Bool
        -- pseudo items
        | L_eof
        | L_error LexError
        deriving (Eq)

prLexItem :: LexItem -> String
prLexItem (L_varid s) = getFString s
prLexItem (L_conid s) = getFString s
prLexItem (L_varsym s) = getFString s
prLexItem (L_consym s) = getFString s
prLexItem (L_integer _ _ i) = itos i
prLexItem (L_float r) = show r
prLexItem (L_char c) = show c
prLexItem (L_string s) = show s
prLexItem L_lpar = "("
prLexItem L_rpar = ")"
prLexItem L_semi = ";"
prLexItem L_uscore = "_"
prLexItem L_bquote = "`"
prLexItem L_lcurl = "{"
prLexItem L_rcurl = "}"
prLexItem L_lbra = "["
prLexItem L_rbra = "]"
prLexItem L_action = "action"
prLexItem L_case = "case"
prLexItem L_class = "class"
prLexItem L_data = "data"
prLexItem L_deriving = "deriving"
prLexItem L_via = "via"
prLexItem L_do = "do"
prLexItem L_else = "else"
prLexItem L_foreign = "foreign"
prLexItem L_if = "if"
prLexItem L_import = "import"
prLexItem L_in = "in"
prLexItem L_coherent = "coherent"
prLexItem L_incoherent = "incoherent"
prLexItem L_infix = "infix"
prLexItem L_infixl = "infixl"
prLexItem L_infixr = "infixr"
prLexItem L_interface = "interface"
prLexItem L_instance = "instance"
prLexItem L_let = "let"
prLexItem L_letseq = "letseq"
prLexItem L_package = "package"
prLexItem L_of = "of"
prLexItem L_primitive = "primitive"
prLexItem L_qualified = "qualified"
prLexItem L_rules = "rules"
prLexItem L_signature = "signature"
prLexItem L_struct = "struct"
prLexItem L_module = "module"
prLexItem L_then = "then"
prLexItem L_type = "type"
prLexItem L_valueOf = "valueOf"
prLexItem L_stringOf = "stringOf"
prLexItem L_verilog = "verilog"
prLexItem L_synthesize = "synthesize"
prLexItem L_when = "when"
prLexItem L_where = "where"
prLexItem L_dcolon = "::"
prLexItem L_colon = ":"
prLexItem L_eq = "="
prLexItem L_at = "@"
prLexItem L_lam = "\\"
prLexItem L_bar = "|"
prLexItem L_rarrow = "->"
prLexItem L_larrow = "<-"
prLexItem L_dot = "."
prLexItem L_comma = ","
prLexItem L_drarrow = "==>"
prLexItem L_irarrow = "=>"
prLexItem L_lcurl_o = "{ from layout"
prLexItem L_rcurl_o = "} from layout"
prLexItem L_semi_o = "; from layout"
prLexItem L_lpragma = "{-#"
prLexItem L_rpragma = "#-}"
prLexItem (L_unbasedUnsized False) = "'0"
prLexItem (L_unbasedUnsized True)  = "'1"
prLexItem L_eof = "<EOF>"
prLexItem (L_error s) = "Lexical error: " ++ show (convLexErrorToErrMsg s)

data Token = Token Position LexItem deriving (Eq)

instance Show Token where
    showsPrec _ (Token p l) = showString ("(Token " ++ prPosition p ++ " " ++ prLexItem l ++ ")")


data LFlags = LFlags {
    lf_is_stdlib :: Bool,   -- parsing a stdlib file, annotate positions
    lf_allow_sv_kws :: Bool -- allow SV keywords as identifiers
}

isSym :: Char -> Bool
isSym '!' = True; isSym '@' = True; isSym '#' = True; isSym '$' = True
isSym '%' = True; isSym '&' = True; isSym '*' = True; isSym '+' = True
isSym '.' = True; isSym '/' = True; isSym '<' = True; isSym '=' = True
isSym '>' = True; isSym '?' = True; isSym '\\' = True; isSym '^' = True
isSym '|' = True; isSym ':' = True; isSym '-' = True; isSym '~' = True
isSym ',' = True
isSym c | c >= '\x80' = c `elem` ['\162', '\163', '\164', '\165', '\166',
                                  '\167', '\168', '\169', '\170', '\171',
                                  '\172', '\173', '\174', '\175', '\176',
                                  '\177', '\178', '\179', '\180', '\181',
                                  '\183', '\184', '\185', '\186', '\187',
                                  '\188', '\189', '\190', '\191', '\215',
                                  '\247' ] || (isSymbol c && not (isIdChar c))
isSym _ = False

isIdChar :: Char -> Bool
isIdChar '\'' = True
isIdChar '_' = True
isIdChar c = isAlphaNum c
