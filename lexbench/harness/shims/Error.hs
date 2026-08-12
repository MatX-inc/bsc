-- Shim for bsc's Error module: Lex.hs only needs internalError and the
-- five ErrMsg constructors used by convLexErrorToErrMsg.  The real Error.hs
-- drags in Flags/Classic/etc., which we don't want for a lexer benchmark.
module Error(internalError, ErrMsg(..)) where

import ErrorUtil(internalError)
import Position(Position)

data ErrMsg = EBadCharLit
            | EBadStringLit
            | EBadLexChar Char
            | EUntermComm Position
            | EMissingNL
            deriving (Eq, Show)
