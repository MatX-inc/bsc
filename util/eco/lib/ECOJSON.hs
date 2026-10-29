-- The one JSON value type and codec for external reports and evidence
-- metadata. The parser rejects duplicate keys rather than giving different
-- readers different interpretations of the same record.
module ECOJSON ( EvidenceJSON(..), parseEvidenceJSON, renderEvidenceJSON ) where

import Data.Char (chr, digitToInt, isDigit, isHexDigit, ord)
import Data.List (intercalate)
import qualified Data.Set as S
import Numeric (showHex)
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

data EvidenceJSON = JObject [(String,EvidenceJSON)] | JArray [EvidenceJSON]
                  | JString String | JNumber String | JBool Bool | JNull
  deriving (Eq,Show)

parseEvidenceJSON :: String -> Either String EvidenceJSON
parseEvidenceJSON source = case [x | (x,"")<-readP_to_S (spaces *> value <* spaces <* eof) source] of
  [x] -> Right x
  _ -> Left "Malformed or ambiguous native evidence JSON"
  where
    spaces=skipSpaces
    token p=p <* spaces
    value=token (object +++ array +++ (JString <$> string) +++ number +++
                 (stringLit "true" >> return (JBool True)) +++
                 (stringLit "false" >> return (JBool False)) +++
                 (stringLit "null" >> return JNull))
    stringLit=Text.ParserCombinators.ReadP.string
    object=do
      char '{'; spaces
      xs<-sepBy (do k<-token string; token (char ':'); x<-value; return (k,x)) (token (char ','))
      char '}'
      if length xs==S.size (S.fromList (map fst xs)) then return (JObject xs) else pfail
    array=JArray <$> between (char '[' *> spaces) (char ']') (sepBy value (token (char ',')))
    string=between (char '"') (char '"') (many character)
    character=satisfy (\c->c/='"' && c/='\\' && ord c>=32) +++ do
      char '\\'; c<-get
      case c of
        '"'->return '"'; '\\'->return '\\'; '/'->return '/'
        'b'->return '\b'; 'f'->return '\f'; 'n'->return '\n'; 'r'->return '\r'; 't'->return '\t'
        'u'->do ds<-count 4 (satisfy isHexDigit)
                let n=foldl (\a d->16*a+digitToInt d) 0 ds
                if n>=0xD800 && n<=0xDFFF then pfail else return (chr n)
        _->pfail
    number=do
      sign<-option "" (char '-' >> return "-")
      whole<-(char '0' >> return "0") +++ ((:) <$> satisfy (\c->c>='1' && c<='9') <*> munch isDigit)
      frac<-option "" (do char '.'; ds<-munch1 isDigit; return ('.':ds))
      expo<-option "" (do e<-satisfy (`elem` "eE"); s<-option "" ((:[]) <$> satisfy (`elem` "+-")); ds<-munch1 isDigit; return (e:s++ds))
      return (JNumber (sign++whole++frac++expo))

renderEvidenceJSON :: EvidenceJSON -> String
renderEvidenceJSON (JObject xs)="{"++intercalate "," [quote k++":"++renderEvidenceJSON x|(k,x)<-xs]++"}"
renderEvidenceJSON (JArray xs)="["++intercalate "," (map renderEvidenceJSON xs)++"]"
renderEvidenceJSON (JString x)=quote x
renderEvidenceJSON (JNumber x)=x
renderEvidenceJSON (JBool x)=if x then "true" else "false"
renderEvidenceJSON JNull="null"
quote :: String -> String
quote x='"':concatMap esc x++"\""
  where
    esc '"'="\\\""; esc '\\'="\\\\"
    esc c | ord c<32=let h=showHex (ord c) "" in "\\u"++replicate (4-length h) '0'++h
          | otherwise=[c]
