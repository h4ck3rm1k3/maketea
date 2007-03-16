{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module PatternMatching where

import DataStructures
import Cpp
import MakeTeaMonad
import Util
import GrammarAnalysis

addPatternMatching :: MakeTeaMonad ()
addPatternMatching = 
	do
		cs <- withClasses $ mapM f
		setClasses cs
	where
		f cls 
			| hasMethod "match" cls = return cls
			| otherwise = case origin cls of
				Nothing -> return cls
				Just (Left r) -> elim addMatchR r cls
				Just (Right t) -> addMatchT t cls	

addMatchR :: Rule a -> Class -> MakeTeaMonad Class
addMatchR (Disj _ _) cls = return cls
addMatchR (Conj _ body) cls = do
	root <- rootSymbol
	rootCn <- toClassName root
	let decl = ("bool", "match")
	let args = [(rootCn ++ "*", "in")]
	matchTerms <- concatMapM matchTerm (nonMarkers body) 
	let match = defMethod decl args $ [
		  name cls ++ "* that = dynamic_cast<" ++ name cls ++ "*>(in);"
		, "if(that == NULL) return false;"
		, ""
		, "__WILDCARD__* joker;"
		, "joker = dynamic_cast<__WILDCARD__*>(that);"
		, "if(joker != NULL)"
		, "{"
		, "\tjoker->set_value(this);"
		, "\treturn true;"
		, "}"
		, ""
		] ++ matchTerms ++ [
		  "return true;"
		]
	return $ cls { 
		  sections = sections cls ++ [Section [] Public [match]]
		}

matchTerm :: Term NonMarker -> MakeTeaMonad Body
matchTerm t@(Term _ _ m) | not (isVector m) = do 
	let vn = toVarName t
	return [
		  "if(this->" ++ vn ++ " == NULL)"
		, "{"
		, "\tif(that->" ++ vn ++ " != NULL && !that->" ++ vn ++ "->match(this->" ++ vn ++ "))"
		, "\t\treturn false;"
		, "}"
		, "else if(!this->" ++ vn ++ "->match(that->" ++ vn ++ "))" 
		, "\treturn false;"
		, ""
		]
matchTerm t@(Term _ _ m) | isVector m = do
	let vn = toVarName t
	cn <- toClassName t
	return [
		  "if(this->" ++ vn ++ " == NULL)"
		, "{"
		, "\tif(that->" ++ vn ++ " != NULL && !that->" ++ vn ++ "->match(this->" ++ vn ++ "))"
		, "\t\treturn false;"
		, "}"
		, "else"
		, "{"
		, "\t" ++ cn ++ "::const_iterator i, j;"
		, "\tfor("
		, "\t\ti = this->" ++ vn ++ "->begin(), j = that->" ++ vn ++ "->begin();"
		, "\t\ti != this->" ++ vn ++ "->end() && j != that->" ++ vn ++ "->end();"
		, "\t\ti++, j++)"
		, "\t{"
		, "\t\tif(*i == NULL)"
		, "\t\t{"
		, "\t\t\tif(*j != NULL && !(*j)->match(*i))"
		, "\t\t\t\treturn false;"
		, "\t\t}"
		, "\t\telse if(!(*i)->match(*j))"
		, "\t\t\treturn false;"
		, "\t}"
		, "\tif(i != this->" ++ vn ++ "->end() || j != that->" ++ vn ++ "->end())"
		, "\t\treturn false;"
		, "}"
		, ""
		]

addMatchT :: Symbol Terminal -> Class -> MakeTeaMonad Class
addMatchT t@(Terminal _ ctype) cls = do
	root <- rootSymbol
	rootCn <- toClassName root
	let decl = ("bool", "match")
	let args = [(rootCn ++ "*", "in")]
	let matchHeader = [
		  name cls ++ "* that = dynamic_cast<" ++ name cls ++ "*>(in);"
		, "if(that == NULL) return false;"
		, ""
		, "__WILDCARD__* joker;"
		, "joker = dynamic_cast<__WILDCARD__*>(that);"
		, "if(joker != NULL)"
		, "{"
		, "\tjoker->set_value(this);"
		, "\treturn true;"
		, "}"
		, ""
		]
	let matchBody var = [ 
		-- We do the check for the joker here rather than relying on String* to
		-- implement match
		  "joker = dynamic_cast<__WILDCARD__*>(this->" ++ var ++ ");"
		, "if(joker != NULL)"
		, "{"
		, "\tjoker->set_value(that->" ++ var ++ ");"
		, "\treturn true;"
		, "}"
		, ""
		, "joker = dynamic_cast<__WILDCARD__*>(that->" ++ var ++ ");"
		, "if(joker != NULL)"
		, "{"
		, "\tjoker->set_value(this->" ++ var ++ ");"
		, "\treturn true;"
		, "}"
		, ""
		, "if(this->" ++ var ++ " == NULL || that->" ++ var ++ " == NULL)"
		, "{"
		, "\tif(this->" ++ var ++ " == NULL && that->" ++ var ++ " == NULL)"
		, "\t\treturn true;"
		, "\telse"
		, "\t\treturn false;"
		, "}"
		, "else"
		, "\treturn (*this->" ++ var ++ " == *that->" ++ var ++ ");"
		]
	let match = case ctype of
		Nothing -> defMethod decl args (matchHeader ++ matchBody "value")
		Just _ -> defMethod decl args (matchHeader ++ matchBody "source_rep")
	return $ cls {
		  sections = sections cls ++ [Section [] Public [match]]
		}

wildcardClass :: MakeTeaMonad Body
wildcardClass = do
	root <- rootSymbol
	rootCn <- toClassName root
	return [
		 "class __WILDCARD__"
		, "{"
		, "public:"
		, "\tvirtual void set_value(" ++ rootCn ++ "* in) = 0;"
		, "};"
		, ""
		, "template<class C>"
		, "class Joker : public virtual C, public __WILDCARD__"
		, "{"
		, "public:"
		, "\tC* value;"
		, ""
		, "\tvirtual void set_value(" ++ rootCn ++ "* in)"
		, "\t{"
		, "\t\tvalue = dynamic_cast<C*>(in);"
		, "\t}"
		, ""
		, "\tvirtual bool match(" ++ rootCn ++"* in)"
		, "\t{"
		, "\t\tC* that = dynamic_cast<C*>(in);"
		, "\t\tif(in == NULL || that != NULL)"
		, "\t\t{"
		, "\t\t\tvalue = that;"
		, "\t\t\treturn true;"
		, "\t\t}"
		, "\t\treturn false;"
		, "\t}"
		, "};"
		]
