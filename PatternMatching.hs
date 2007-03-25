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

addPatternMatching :: MakeTeaMonad ()
addPatternMatching = 
	do
		cs <- withClasses $ mapM f
		setClasses cs
	where
		f cls = case origin cls of
				Nothing -> return cls
				Just (Left r) -> elim addMatchR r cls
				Just (Right t) -> addMatchT t cls	

-- A node of type X should match a wildcard of type Y if Y is more general 
-- than Y. That means that we should check for wildcards before downcasting
-- to the type of X.
addMatchR :: Rule a -> Class -> MakeTeaMonad Class
addMatchR (Disj _ _) cls = do
	root <- rootSymbol
	rootCn <- toClassName root
	let decl = ("bool", "match")
	let args = [(rootCn ++ "*", "in")]
	let match = PureVirtual [] decl args 
	return $ cls { 
		  sections = sections cls ++ [Section [] Public [match]]
		}
addMatchR (Conj _ body) cls = do
	root <- rootSymbol
	rootCn <- toClassName root
	let decl = ("bool", "match")
	let args = [(rootCn ++ "*", "in")]
	matchTerms <- concatMapM matchTerm (nonMarkers body) 
	let match = defMethod decl args $ [
		  "__WILDCARD__* joker;"
		, "joker = dynamic_cast<__WILDCARD__*>(in);"
		, "if(joker != NULL && joker->match(this))"
		, "\treturn true;"
		, ""
		, name cls ++ "* that = dynamic_cast<" ++ name cls ++ "*>(in);"
		, "if(that == NULL) return false;"
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
			-- that->vn could be a joker
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
		  "if(this->" ++ vn ++ " != NULL && that->" ++ vn ++ " != NULL)"
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
		  "__WILDCARD__* joker;"
		, "joker = dynamic_cast<__WILDCARD__*>(in);"
		, "if(joker != NULL && joker->match(this))"
		, "\treturn true;"
		, ""
		, name cls ++ "* that = dynamic_cast<" ++ name cls ++ "*>(in);"
		, "if(that == NULL) return false;"
		, ""
		]
	let matchBody var = [ 
		  "if(this->" ++ var ++ " != NULL && that->" ++ var ++ " != NULL)"
		, "\treturn (*this->" ++ var ++ " == *that->" ++ var ++ ");"
		, "else"
		, "\treturn true;"
		]
	let match = case ctype of
		Nothing -> defMethod decl args (
			   matchHeader 
			++ matchBody "value"
			)
		Just "" -> defMethod decl args (
			   matchHeader 
			++ matchBody "source_rep"
			)
		Just _ -> defMethod decl args (
			   matchHeader
			++ [
			  "if(!match_value(that))"
			, "\treturn false;"
			, ""
			]
			++ matchBody "source_rep"
			)
	let match_value = defMethod ("bool", "match_value") [(name cls ++ "*", "that")] ["return true;"]
	let methods = case ctype of
		Just t@(_:_) -> [match,match_value]
		_ -> [match]
	return $ cls {
		  sections = sections cls ++ [Section [] Public methods]
		}

wildcardClass :: MakeTeaMonad Body
wildcardClass = do
	root <- rootSymbol
	rootCn <- toClassName root
	cid <- getNextClassID
	return [
		  "class __WILDCARD__"
		, "{"
		, "public:"
		, "\tvirtual ~__WILDCARD__() {}"
		, ""
		, "public:"
		, "\tvirtual bool match(" ++ rootCn ++ "* in) = 0;"
		, "};"
		, ""
		, "template<class C>"
		, "class Wildcard : public virtual C, public __WILDCARD__"
		, "{"
		, "public:"
		, "\tWildcard() : value(NULL) {}"
		, "\tWildcard(C* v) : value(v) {}"
		, "\tvirtual ~Wildcard() {}"
		, ""
		, "public:"
		, "\tC* value;"
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
		, ""
		, "\tvirtual Wildcard* clone()"
		, "\t{"
		, "\t\tif(value != NULL)"
		, "\t\t\treturn new Wildcard(value->clone());"
		, "\t\telse"
		, "\t\t\treturn new Wildcard(NULL);"
		, "\t}"
		, ""
		, "\tvirtual bool equals(AST_node* in)"
		, "\t{"
		, "\t\tWildcard* that = dynamic_cast<Wildcard*>(in);"
		, "\t\tif(that == NULL) return false;"
		, ""
		, "\t\tif(this->value == NULL || that->value == NULL)"
		, "\t\t{"
		, "\t\t\tif(this->value != NULL || that->value != NULL)"
		, "\t\t\t\treturn false;"
		, "\t\t}"
		, ""
		, "\t\treturn value->equals(that->value);"
		, "\t}"
		, ""
		, "private:"
		, "\tint classid()"
		, "\t{"
		, "\t\treturn " ++ show cid ++ ";"
		, "\t}"
		, "};"
		]
