{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module Parser where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language
import Data.Char

import DataStructures
import Util
import Cpp

{-
 - Top-level 
 -}

maketeaP :: Parser (Config, Grammar, [Include], [Class])
maketeaP =
	do
		whiteSpace
		c <- configP
		g <- grammarP
		is <- many includeP
		cs <- many classP
		eof
		return (c, g, is, cs)

{-
 - Configuration
 -}

configP :: Parser Config
configP = 
	do
		cs <- many settingP
		return (foldr ($) initConfig cs)
	where
		initConfig = Config {
			  outputDir = "."
			, filePrefix = "AST"
			, externalClasses = []
			, listClass = "list"
			, stringClass = "string"
			, namespace = Nothing 
			, rootName = "Node"
			, noneName = "None"
			, noSourceRep = False
			}

settingP :: Parser (Config -> Config)
settingP = 
	do
		reserved "external"
		reserved "class"
		cn <- stringLiteral
		reservedOp ";"
		return (\c -> c { externalClasses = cn : externalClasses c })
	<|>
	do
		reserved "prefix"
		pf <- stringLiteral
		reservedOp ";"
		return (\c -> c { filePrefix = pf })
	<|>
	do
		reserved "output_dir"
		od <- stringLiteral
		reservedOp ";"
		return (\c -> c { outputDir = od })
	<|>
	do
		reserved "no";
		reserved "source_rep";
		reservedOp ";"
		return (\c -> c { noSourceRep = True })
	<|>
	do
		reserved "use"
		(	
			do
				reserved "list"
				id <- stringLiteral
				reservedOp ";"
				return (\c -> c { listClass = id })
			<|>
			do
				reserved "string"
				id <- stringLiteral
				reservedOp ";"
				return (\c -> c { stringClass = id}) 
			<|>
			do
				reserved "namespace"
				id <- stringLiteral
				reservedOp ";"
				return (\c -> c { namespace = Just id})
			<|>
			do
				reserved "root"
				id <- stringLiteral
				reservedOp ";"
				return (\c -> c { rootName = id })
			<|>
			do
				reserved "none"
				id <- stringLiteral
				reservedOp ";"
				return (\c -> c { rootName = id })
			)

{-
 - Classes
 -}

classP :: Parser Class
classP = 
	do
		reserved "class"
		name <- identifier
		inh <- option [] $ do 
			reservedOp ":"
			class_identifier `sepBy1` comma
		reservedOp "{"
		ss <- many sectionP
		reservedOp "}"
		reservedOp ";"
		return $ (emptyClassNoID name) {
			  extends = inh
			, sections = ss
			}

sectionP :: Parser Section
sectionP = 
	do
		cmnt <- commentP
		access <- accessP
		members <- many memberP
		return (Section cmnt access members)

accessP :: Parser Access
accessP = 
	do
		reserved "private"
		reservedOp ":"
		return Private
	<|> do
		reserved "protected"
		reservedOp ":"
		return Protected
	<|> do
		reserved "public"
		reservedOp ":"
		return Public

memberP :: Parser Member
memberP = try $ do
	cmnt <- commentP
	virtual <- virtualP
	static <- staticP
	decl <- declP
	do
		do	
			reservedOp ";"
			return (Attribute cmnt decl)
		<|> do
			args <- parens (declP `sepBy` comma)
			do
				do
					symbol "= 0;"
					return (PureVirtual cmnt decl args)
				<|> do
					body <- lexeme bodyP
					return (Method cmnt virtual static decl args [body])
		
virtualP :: Parser IsVirtual
virtualP = 
	do
		reserved "virtual"
		return Virtual
	<|>
	do
		return NonVirtual

staticP :: Parser IsStatic 
staticP = 
	do
		reserved "static"
		return Static 
	<|>
	do
		return NonStatic

commentP :: Parser Comment
commentP = many $ do
	string "//" 
	cmnt <- many (noneOf ['\n'])
	whiteSpace
	return cmnt

bodyP :: Parser String
bodyP = 	
	do
		pos <- getPosition
		let name = sourceName pos
		let line = sourceLine pos
		b <- body1
		-- TODO: #line directives are useful, but if we don't "restore" the
		-- line number to AST.cpp after some user code, assertion failures
		-- are going to be reported somewhere in the middle of (or even after)
		-- phc.tea rather than in AST.cpp
		-- return $ "#line " ++ show line ++ " \"" ++ name ++ "\"\n" ++ b
		return b

body1 :: Parser String
body1 = 
	do
		char '{'
		bs <- many $ (many1 $ noneOf ['{','}']) <|> bodyP 
		char '}'
		return ("{" ++ concat bs ++ "}")

declP :: Parser (Decl a)
declP = 
	do
		xs <- many1 (lexeme (many1 (alphaNum <|> oneOf ['&','*','<','>','_']) <|> string "::"))
		return (f (init xs), last xs)
	where
		f [] = ""
		f [x] = x
		f (x:x':xs) = x ++ " " ++ x' ++ f xs


{-
 - Includes
 -}

includeP = 
	do
		symbol "#include"
		l <- lexeme (many $ noneOf ['\n'])
		return ("#include " ++ l)

{-
 - EBNF 
 -}

grammarP :: Parser Grammar
grammarP = many ruleP

ruleP :: Parser (Some Rule)
ruleP = 
	do
		r <- disjunctionP
		return (Exists r)
	<|> do
		r <- conjunctionP
		return (Exists r)

-- a disjunction consists of at least two symbols
disjunctionP :: Parser (Rule Disj)
disjunctionP = try $ 
	do
		head <- nonTerminalP
		reservedOp "::="
		s <- symbolP ; reservedOp "|"
		ss <- symbolP `sepBy` reservedOp "|"
		reservedOp ";"
		return (Disj head (s:ss))

-- a conjunction may be empty
conjunctionP :: Parser (Rule Conj)
conjunctionP = 
	do
		head <- nonTerminalP
		reservedOp "::="
		body <- many termP
		reservedOp ";"
		return (Conj head body)

symbolP :: Parser (Some Symbol)
symbolP = 
	do
		s <- nonTerminalP
		return (Exists s)
	<|> do
		s <- terminalP
		return (Exists s)

termP :: Parser (Some Term)
termP = 
	do
		l <- labelP
		do
			do
				s <- symbolP
				m <- multiplicityP
				return (Exists (Term l s m))
			<|> do
				m <- markerP
				reservedOp "?"
				return (Exists (Marker l m))

labelP :: Parser Label
labelP = option Nothing $ 
	try (do
		id <- identifier
		reservedOp ":"
		return (Just id))

multiplicityP :: Parser Multiplicity
multiplicityP = 
	do
		reservedOp "?"
		return Optional 
	<|> do
		reservedOp "*"
		return Vector
	<|> do
		reservedOp "?*"
		return VectorOpt
	<|> do
		reservedOp "*?"
		return OptVector
	<|> do
		reservedOp "+"
		return Vector
	<|> do
		return Single

nonTerminalP :: Parser (Symbol NonTerminal)
nonTerminalP = try $
	do
		id <- identifier
		-- a non-terminal symbol must have at least some lower-case characters
		if not (all (isAlpha `implies` isUpper) id) 
			then return (NonTerminal id)
			else fail "expected non-terminal symbol"

terminalP :: Parser (Symbol Terminal)
terminalP = try $
	do
		id <- identifier
		ctype <- option Nothing $ do
			l <- lexeme (between (char '<') (char '>') (many $ noneOf ['>'])) 
			return (Just l)
		if all (isAlpha `implies` isUpper) id
			then return (Terminal id ctype)
			else fail "expected terminal symbol"

markerP :: Parser (Name Marker)
markerP = try $
	do
		id <- stringLiteral 
		return id

{-
 - Lexical analysis
 -}

lexer = T.makeTokenParser haskellStyle
	{
	 	reservedOpNames = [
			  "|", ";", "?", "*"
			, "*?", "?*", "::=", ":"
			, "+","{","}","(",")"
			]
	}

whiteSpace = T.whiteSpace lexer
identifier = T.identifier lexer
reserved = T.reserved lexer
reservedOp = T.reservedOp lexer
stringLiteral = T.stringLiteral lexer
lexeme = T.lexeme lexer
comma = T.comma lexer
symbol = T.symbol lexer

-- Allow classes to use ::s
class_lexer = T.makeTokenParser haskellStyle
	{
		identLetter = alphaNum <|> oneOf "_':"
	}

class_identifier = T.identifier class_lexer


parens p = 
		try (between (symbol "(") (symbol ")") p)
	<|> do
		symbol "()"
		return []

