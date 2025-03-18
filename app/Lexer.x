{
    module Lexer where
}

%wrapper "basic"


$white = [\ \t\n\r\v\r]
$digit = [0-9]
$alpha = [_a-zA-Z]
$strings = $printable # \"

tokens :-

$white+ ;


---------------- comments ----------------
"//".*                                  ;
"/*"(\n|.)*"*/"                         ;
\/ \* ( ~\* | \* ~\/ | \** \n )* \* \/  ;


--------- reserved kotlin keywords ---------
val                                     { \_ -> TokenVal }
var                                     { \_ -> TokenVar }
if                                      { \_ -> TokenIf }
else                                    { \_ -> TokenElse }
while                                   { \_ -> TokenWhile }
break                                   { \_ -> TokenBreak }
return                                  { \_ -> TokenReturn }
fun                                     { \_ -> TokenFun }
main                                    { \_ -> TokenMain }
true                                    { \_ -> TokenTrue }
false                                   { \_ -> TokenFalse }
then                                    { \_ -> TokenThen }
when                                    { \_ -> TokenWhen }
do                                      { \_ -> TokenDo }
print                                   { \_ -> TokenPrint }
println                                 { \_ -> TokenPrintLn }
readln                                  { \_ -> TokenReadLn }


---------------- operators ----------------            
"=="                                    { \_ -> TokenEq }
"!="                                    { \_ -> TokenNeq }
"<"                                     { \_ -> TokenLt }
"<="                                    { \_ -> TokenLeq }
">"                                     { \_ -> TokenGt }
">="                                    { \_ -> TokenGeq }
"&&"                                    { \_ -> TokenAnd }
"||"                                    { \_ -> TokenOr }
"!"                                     { \_ -> TokenNot }
"="                                     { \_ -> TokenAssign }
"+"                                     { \_ -> TokenPlus }
"-"                                     { \_ -> TokenMinus }
"*"                                     { \_ -> TokenMult }
"/"                                     { \_ -> TokenDiv }
"%"                                     { \_ -> TokenMod }
"++"                                    { \_ -> TokenInc }
"--"                                    { \_ -> TokenDec }
"+="                                    { \_ -> TokenPlusAssign }
"-="                                    { \_ -> TokenMinusAssign }
"*="                                    { \_ -> TokenMultAssign }
"/="                                    { \_ -> TokenDivAssign }
"%="                                    { \_ -> TokenModAssign }


-------------- punctuation  ---------------         
"("                                     { \_ -> TokenLParen }
")"                                     { \_ -> TokenRParen }
"{"                                     { \_ -> TokenLBrace }
"}"                                     { \_ -> TokenRBrace }
"["                                     { \_ -> TokenLBracket }
"]"                                     { \_ -> TokenRBracket }
";"                                     { \_ -> TokenSemi }
","                                     { \_ -> TokenComma }
"."                                     { \_ -> TokenDot }
":"                                     { \_ -> TokenColon }


--------------- literals ---------------         
$digit+                                 { \s -> TokenInt (read s) }
$digit+"."$digit+                       { \s -> TokenDouble (read s) }
$alpha($alpha | $digit)*                { \s -> TokenId s }
\"$strings*\"                                { \s -> Token_String (read s)}


{
    data Token
     = TokenVal
        | TokenVar
        | TokenIf
        | TokenElse
        | TokenWhile
        | TokenBreak
        | TokenReturn
        | TokenFun
        | TokenMain
        | TokenTrue
        | TokenFalse
        | TokenThen
        | TokenWhen
        | TokenEq
        | TokenNeq
        | TokenLt
        | TokenLeq
        | TokenGt
        | TokenGeq
        | TokenAnd
        | TokenOr
        | TokenNot
        | TokenAssign
        | TokenPlus
        | TokenMinus
        | TokenMult
        | TokenDiv
        | TokenMod
        | TokenInc
        | TokenDec
        | TokenPlusAssign
        | TokenMinusAssign
        | TokenMultAssign
        | TokenDivAssign
        | TokenModAssign
        | TokenLParen
        | TokenRParen
        | TokenLBrace
        | TokenRBrace
        | TokenLBracket
        | TokenRBracket
        | TokenSemi
        | TokenComma
        | TokenDot
        | TokenDo
        | TokenPrint
        | TokenPrintLn
        | TokenReadLn
        | TokenColon
        | TokenInt Int
        | TokenDouble Double
        | TokenId String
        | Token_String String
        | TokenChar Char
        deriving (Show, Eq, Read)
}





