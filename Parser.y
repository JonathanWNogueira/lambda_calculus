{
module Parser where
import Data.Char
import Lambda
}

%name parserlamb
%tokentype { Token }
%error { parseError }

%token
	lam { TokenLam }
	var    { TokenVar $$ }
	'.'    { TokenPoint }
	'('    { TokenOB }
	')'    { TokenCB }

%right '.'
%nonassoc lam var '(' ')'
%left APP
%%

Term : var                   { Var $1 }
	 | lam var  '.' Term     { Abs $2 $4}
	 | Term Term  %prec APP  { App $1 $2 }
	 | '(' Term ')'          { $2 }

{

parseError :: [Token] -> a
parseError b = error "Parse Error"

data Token 
	= TokenVar Char
	| TokenPoint
	| TokenOB
	| TokenCB
	| TokenLam
	deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | c == '.'  = TokenPoint : lexer cs
    | c == '('  = TokenOB : lexer cs
    | c == ')'  = TokenCB : lexer cs
    | isAlpha c =
        let (a, rest) = span isAlpha (c:cs)
        in if a == "lam"
           then TokenLam : lexer rest
           else TokenVar c : lexer rest
		   
}