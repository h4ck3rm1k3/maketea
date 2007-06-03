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
	let valid = PureVirtual [] ("bool", "is_valid") []
	return $ cls { 
		  sections = sections cls ++ [Section [] Public [valid]]
		}
addValidR (Conj _ body) cls = do
	let decl = ("bool", "is_valid")
	validTerms <- concatMapM validTerm (nonMarkers body)
	imv <- isMixinValid cls
	let valid = defMethod decl [] $ validTerms ++ imv ++ ["return true;"]
	return $ cls { 
		  sections = sections cls ++ [Section [] Public [valid]]
		}

isMixinValid :: Class -> MakeTeaMonad Body
isMixinValid cls = do
		ext <- allExtends [name cls]
		concatMapM f ext 
	where
		f cn = do
			hasM <- mixinHasMethod cn "is_mixin_valid" 
			if hasM
				then return ["if(!" ++ cn ++ "::is_mixin_valid()) return false;"]
				else return []

validTerm :: Term NonMarker -> MakeTeaMonad Body
validTerm t@(Term _ _ Single) = do 
	let vn = toVarName t
	return
		[
		  "if(" ++ vn ++ " == NULL || !" ++ vn ++ "->is_valid()) return false;"
		]
validTerm t@(Term _ _ Optional) = do 
	let vn = toVarName t
	return
	 	[
		  "if(" ++ vn ++ " != NULL && !" ++ vn ++ "->is_valid()) return false;"
		]
validTerm t@(Term _ _ Vector)  = do
	let vn = toVarName t
	cn <- toClassName t
	return 
		[
		  "if(" ++ vn ++ " == NULL) return false;"
		, "{"
		, "\t" ++ cn ++ "::const_iterator i;"
		, "\tfor(i = this->" ++ vn ++ "->begin(); i != this->" ++ vn ++ "->end(); i++)" 
		, "\t{"
		, "\t\tif(*i == NULL || !(*i)->is_valid()) return false;"
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
		, "\t\tif(*i == NULL || !(*i)->is_valid()) return false;"
		, "}"
		]
validTerm t@(Term _ _ VectorOpt)  = do
	let vn = toVarName t
	cn <- toClassName t
	return 
		[
		  "if(" ++ vn ++ " == NULL) return false;"
		, "{"
		, "\t" ++ cn ++ "::const_iterator i;"
		, "\tfor(i = this->" ++ vn ++ "->begin(); i != this->" ++ vn ++ "->end(); i++)" 
		, "\t\tif(*i != NULL && !(*i)->is_valid()) return false;"
		, "}"
		]

addValidT :: Symbol Terminal -> Class -> MakeTeaMonad Class
addValidT t@(Terminal _ ctype) cls = do
	let decl = ("bool", "is_valid")
	imv <- isMixinValid cls
	let valid = case ctype of
		Nothing -> defMethod decl [] $ 
			[
			  "if(value == NULL) return false;"
			] ++ imv ++ ["return true;"]
		Just "" -> defMethod decl [] $ 
			[
			  "if(source_rep == NULL) return false;"
			] ++ imv ++ ["return true;"]
		Just t -> defMethod decl [] $ 
			[
			  "if(source_rep == NULL) return false;"
			, "if(!is_value_valid()) return false;"
			] ++ imv ++ ["return true;"]
	let is_value_valid = defMethod ("bool", "is_value_valid") [] ["return true;"] 
	let methods = case ctype of
		Just t@(_:_) -> [valid,is_value_valid]
		_ -> [valid]
	return $ cls {
		  sections = sections cls ++ [Section [] Public methods]
		}
