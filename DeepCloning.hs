{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module DeepCloning where

import DataStructures
import Cpp
import MakeTeaMonad
import Util
import Mixin

addDeepCloning :: MakeTeaMonad ()
addDeepCloning = 
	do
		cs <- withClasses $ mapM f
		setClasses cs
	where
		f cls = case origin cls of
			Nothing -> return cls
			Just (Left r) -> elim addCloneR r cls
			Just (Right t) -> addCloneT t cls	

addCloneR :: Rule a -> Class -> MakeTeaMonad Class
addCloneR (Disj _ _) cls = do
	root <- rootSymbol
	rootCn <- toClassName root
	let clone = PureVirtual [] (name cls ++ "*", "clone") []
	return $ cls { 
		  sections = sections cls ++ [Section [] Public [clone]]
		}
addCloneR (Conj _ body) cls = do
	root <- rootSymbol
	rootCn <- toClassName root
	let decl = (name cls ++ "*", "clone")
	cloneTerms <- concatMapM (elim cloneTerm) body 
	cmf <- cloneMixinFrom cls
	let clone = defMethod decl [] $ cloneTerms ++ [
	      name cls ++ "* clone = new " ++ name cls ++ "(" ++ flattenComma (map toVarName body) ++ ");"
		] ++ cmf ++ [
		  "return clone;"
		]
	return $ cls { 
		  sections = sections cls ++ [Section [] Public [clone]]
		}

cloneMixinFrom :: Class -> MakeTeaMonad Body
cloneMixinFrom cls = do
		ext <- allExtends [name cls]
		concatMapM f ext 
	where
		f cn = do
			hasM <- mixinHasMethod cn "clone_mixin_from" 
			if hasM
				then return ["clone->" ++ cn ++ "::clone_mixin_from(this);"]
				else return []

cloneTerm :: Term a -> MakeTeaMonad Body
cloneTerm m@(Marker _ _) = do
	let vn = toVarName m
	return [
		  "bool " ++ vn ++ " = this->" ++ vn ++ ";" 
		]
cloneTerm t@(Term _ _ m) | not (isVector m) = do 
	let vn = toVarName t
	cn <- toClassName t
	return [
		  cn ++ "* " ++ vn ++ " = this->" ++ vn ++ " ? this->" ++ vn ++ "->clone()" ++ " : NULL;" 
		]
cloneTerm t@(Term _ _ m) | isVector m = do
	let vn = toVarName t
	cn <- toClassName t
	return [
		  cn ++ "* " ++ vn ++ " = NULL;"
		, "if(this->" ++ vn ++ " != NULL)"
		, "{"
		, "\t" ++ cn ++ "::const_iterator i;"
		, "\t" ++ vn ++ " = new " ++ cn ++ ";"
		, "\tfor(i = this->" ++ vn ++ "->begin(); i != this->" ++ vn ++ "->end(); i++)" 
		, "\t\t" ++ vn ++ "->push_back(*i ? (*i)->clone() : NULL);"
		, "}"
		]

addCloneT :: Symbol Terminal -> Class -> MakeTeaMonad Class
addCloneT t@(Terminal _ ctype) cls = do
	root <- rootSymbol
	rootCn <- toClassName root
	let decl = (name cls ++ "*", "clone")
	string <- getStringClass
	let cloneStr vn = string ++ "* " ++ vn ++ " = new " ++ string ++ "(*this->" ++ vn ++ ");"
	cmf <- cloneMixinFrom cls
	let clone = case (ctype) of
		(Nothing) -> defMethod decl [] $ [
			  cloneStr "value"
			, name cls ++ "* clone = new " ++ name cls ++ "(value);"
			] ++ cmf ++ [
			  "return clone;"
			]
		(Just "") -> defMethod decl [] $ [
			  name cls ++ "* clone = new " ++ name cls ++ "();"
			] ++ cmf ++ [
			  "return clone;"
			]
		(Just t) -> defMethod decl [] $ [
			  "value = clone_value();"
			, name cls ++ "* clone = new " ++ name cls ++ "(value);"
			] ++ cmf ++ [
			  "return clone;"
			]
	let clone_value t = defMethod (t, "clone_value") [] ["return value;"] 
	let methods = case ctype of
		Just t@(_:_) -> [clone,clone_value t]
		_ -> [clone]
	return $ cls {
		  sections = sections cls ++ [Section [] Public methods]
		}
