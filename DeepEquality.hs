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
import Mixin

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
	ime <- isMixinEqual cls
	let equal = defMethod decl args $ [
		  name cls ++ "* that = dynamic_cast<" ++ name cls ++ "*>(in);"
		, "if(that == NULL) return false;"
		, ""
		] ++ equalTerms ++ ime ++ [
		  "return true;"
		]
	return $ cls { 
		  sections = sections cls ++ [Section [] Public [equal]]
		}

isMixinEqual :: Class -> MakeTeaMonad Body
isMixinEqual cls = do
		ext <- allExtends [name cls]
		concatMapM f ext 
	where
		f cn = do
			hasM <- mixinHasMethod cn "is_mixin_equal" 
			if hasM
				then return ["if(!" ++ cn ++ "::is_mixin_equal(that)) return false;"]
				else return []

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
	ime <- isMixinEqual cls
	noSourceRep <- getNoSourceRep
	let equal = case (ctype, noSourceRep) of
		(Nothing, _) -> defMethod decl args (
			   equalHeader 
			++ equalBody "value"
			++ ime
			++ ["return true;"]
			)
		(Just "", False) -> defMethod decl args (
			   equalHeader 
			++ equalBody "source_rep"
			++ ime
			++ ["return true;"]
			)
		(Just "", True) -> defMethod decl args (
			   equalHeader 
			++ ime
			++ ["return true;"]
			)
		(Just t, False) -> defMethod decl args (
			   equalHeader 
			++ [
			  "if(!equals_value(that))"
			, "\treturn false;"
			, ""
			]
			++ equalBody "source_rep"
			++ ime
			++ ["return true;"]
			)
		(Just t, True) -> defMethod decl args (
			   equalHeader 
			++ [
			  "if(!equals_value(that))"
			, "\treturn false;"
			, ""
			]
			++ ime
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
