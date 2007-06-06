{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module ValidityCheck where

import DataStructures
import Cpp
import MakeTeaMonad
import Util
import Mixin

addIsValid :: MakeTeaMonad ()
addIsValid = 
	do
		cs <- withClasses $ mapM f
		setClasses cs
	where
		f cls = case origin cls of
			Nothing -> return cls
			Just (Left r) -> elim addValidR r cls
			Just (Right t) -> addValidT t cls	

addValidR :: Rule a -> Class -> MakeTeaMonad Class
addValidR (Disj _ _) cls = do
	let valid = PureVirtual [] ("void", "assert_valid") []
	return $ cls { 
		  sections = sections cls ++ [Section [] Public [valid]]
		}
addValidR (Conj _ body) cls = do
	let decl = ("void", "assert_valid")
	validTerms <- concatMapM validTerm (nonMarkers body)
	amv <- assertMixinValid cls
	let valid = defMethod decl [] $ validTerms ++ amv 
	return $ cls { 
		  sections = sections cls ++ [Section [] Public [valid]]
		}

assertMixinValid :: Class -> MakeTeaMonad Body
assertMixinValid cls = do
		ext <- allExtends [name cls]
		concatMapM f ext 
	where
		f cn = do
			hasM <- mixinHasMethod cn "assert_mixin_valid" 
			if hasM
				then return ["" ++ cn ++ "::assert_mixin_valid();"]
				else return []

validTerm :: Term NonMarker -> MakeTeaMonad Body
validTerm t@(Term _ _ Single) = do 
	let vn = toVarName t
	return
		[
		  "assert(" ++ vn ++ " != NULL);"
		, "" ++ vn ++ "->assert_valid();"
		]
validTerm t@(Term _ _ Optional) = do 
	let vn = toVarName t
	return
	 	[
		  "if(" ++ vn ++ " != NULL) " ++ vn ++ "->assert_valid();"
		]
validTerm t@(Term _ _ Vector)  = do
	let vn = toVarName t
	cn <- toClassName t
	return 
		[
		  "assert(" ++ vn ++ " != NULL);"
		, "{"
		, "\t" ++ cn ++ "::const_iterator i;"
		, "\tfor(i = this->" ++ vn ++ "->begin(); i != this->" ++ vn ++ "->end(); i++)" 
		, "\t{"
		, "\t\tassert(*i != NULL);"
		, "\t\t(*i)->assert_valid();"
		, "\t}"
		, "}"
		]
validTerm t@(Term _ _ OptVector)  = do
	let vn = toVarName t
	cn <- toClassName t
	return 
		[
		  "if(" ++ vn ++ " != NULL)"
		, "{"
		, "\t" ++ cn ++ "::const_iterator i;"
		, "\tfor(i = this->" ++ vn ++ "->begin(); i != this->" ++ vn ++ "->end(); i++)" 
		, "\t{"
		, "\t\tassert(*i != NULL);"
		, "\t\t(*i)->assert_valid();"
		, "\t}"
		, "}"
		]
validTerm t@(Term _ _ VectorOpt)  = do
	let vn = toVarName t
	cn <- toClassName t
	return 
		[
		  "assert(" ++ vn ++ " != NULL);"
		, "{"
		, "\t" ++ cn ++ "::const_iterator i;"
		, "\tfor(i = this->" ++ vn ++ "->begin(); i != this->" ++ vn ++ "->end(); i++)" 
		, "\t{"
		, "\t\tif(*i != NULL) (*i)->assert_valid();"
		, "\t}"
		, "}"
		]

addValidT :: Symbol Terminal -> Class -> MakeTeaMonad Class
addValidT t@(Terminal _ ctype) cls = do
	let decl = ("void", "assert_valid")
	amv <- assertMixinValid cls
	let valid = case ctype of
		Nothing -> defMethod decl [] $ 
			[
			  "assert(value != NULL);"
			] ++ amv 
		Just "" -> defMethod decl [] $ 
			[
			] ++ amv 
		Just t -> defMethod decl [] $ 
			[
			  "assert_value_valid();"
			] ++ amv 
	let assert_value_valid = defMethod ("void", "assert_value_valid") [] ["// Assume value is valid"] 
	let methods = case ctype of
		Just t@(_:_) -> [valid,assert_value_valid]
		_ -> [valid]
	return $ cls {
		  sections = sections cls ++ [Section [] Public methods]
		}
