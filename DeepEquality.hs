{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module DeepEquality where

import DataStructures
import Cpp
import MakeTeaMonad
import Util

addDeepEquality :: MakeTeaMonad ()
addDeepEquality = 
	do
		cs <- withClasses $ mapM f
		setClasses cs
	where
		f cls = case origin cls of
			Nothing -> return cls
			Just (Left r) -> elim addEqualR r cls
			Just (Right t) -> addEqualT t cls	

addEqualR :: Rule a -> Class -> MakeTeaMonad Class
addEqualR (Disj _ _) cls = do
	root <- rootSymbol
	rootCn <- toClassName root
	let decl = ("bool", "equals")
	let args = [(rootCn ++ "*", "in")]
	let equals = PureVirtual [] decl args 
	return $ cls { 
		  sections = sections cls ++ [Section [] Public [equals]]
		}
addEqualR (Conj _ body) cls = do
	root <- rootSymbol
	rootCn <- toClassName root
	let decl = ("bool", "equals")
	let args = [(rootCn ++ "*", "in")]
	equalTerms <- concatMapM (elim equalTerm) body 
	let equal = defMethod decl args $ [
		  name cls ++ "* that = dynamic_cast<" ++ name cls ++ "*>(in);"
		, "if(that == NULL) return false;"
		, ""
		] ++ equalTerms ++ [
		  "return true;"
		]
	return $ cls { 
		  sections = sections cls ++ [Section [] Public [equal]]
		}

equalTerm :: Term a -> MakeTeaMonad Body
equalTerm m@(Marker _ _) = do
	let vn = toVarName m
	return [
		  "if(this->" ++ vn ++ " != that->" ++ vn ++ ")"
		, "\treturn false;"
		, ""
		]
equalTerm t@(Term _ _ m) | not (isVector m) = do 
	let vn = toVarName t
	return [
		  "if(this->" ++ vn ++ " == NULL || that->" ++ vn ++ " == NULL)"
		, "{"
		, "\tif(this->" ++ vn ++ " != NULL || that->" ++ vn ++ " != NULL)"
		, "\t\treturn false;"
		, "}"
		, "else if(!this->" ++ vn ++ "->equals(that->" ++ vn ++ "))" 
		, "\treturn false;"
		, ""
		]
equalTerm t@(Term _ _ m) | isVector m = do
	let vn = toVarName t
	cn <- toClassName t
	return [
		  "if(this->" ++ vn ++ " == NULL || that->" ++ vn ++ " == NULL)"
		, "{"
		, "\tif(this->" ++ vn ++ " != NULL || that->" ++ vn ++ " != NULL)"
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
		, "\t\tif(*i == NULL || *j == NULL)"
		, "\t\t{"
		, "\t\t\tif(*i != NULL || *j != NULL)"
		, "\t\t\t\treturn false;"
		, "\t\t}"
		, "\t\telse if(!(*i)->equals(*j))"
		, "\t\t\treturn false;"
		, "\t}"
		, "\tif(i != this->" ++ vn ++ "->end() || j != that->" ++ vn ++ "->end())"
		, "\t\treturn false;"
		, "}"
		, ""
		]

addEqualT :: Symbol Terminal -> Class -> MakeTeaMonad Class
addEqualT t@(Terminal _ ctype) cls = do
	root <- rootSymbol
	rootCn <- toClassName root
	let decl = ("bool", "equals")
	let args = [(rootCn ++ "*", "in")]
	let equalHeader = [
		  name cls ++ "* that = dynamic_cast<" ++ name cls ++ "*>(in);"
		, "if(that == NULL) return false;"
		, ""
		]
	let equalBody vn = [ 
		  "if(this->" ++ vn ++ " == NULL || that->" ++ vn ++ " == NULL)"
		, "{"
		, "\tif(this->" ++ vn ++ " != NULL || that->" ++ vn ++ " != NULL)"
		, "\t\treturn false;"
		, "}"
		, "else if(*this->" ++ vn ++ " != *that->" ++ vn ++ ")"
		, "\treturn false;"
		, ""
		]
	let equal = case ctype of
		Nothing -> defMethod decl args (
			   equalHeader 
			++ equalBody "value"
			++ ["return true;"]
			)
		Just "" -> defMethod decl args (
			   equalHeader 
			++ equalBody "source_rep"
			++ ["return true;"]
			)
		Just t -> defMethod decl args (
			   equalHeader 
			++ [
			  "if(!equals_value(that))"
			, "\treturn false;"
			, ""
			]
			++ equalBody "source_rep"
			++ ["return true;"]
			)
	let equals_value_body isP 
		| isP = "return (*this->value == *that->value);"
		| not isP = "return (this->value == that->value);"
	let equals_value isP = defMethod ("bool", "equals_value") [(name cls ++ "*", "that")] [equals_value_body isP]
	let methods = case ctype of
		Just t@(_:_) -> [equal,equals_value (isPointer t)]
		_ -> [equal]
	return $ cls {
		  sections = sections cls ++ [Section [] Public methods]
		}
