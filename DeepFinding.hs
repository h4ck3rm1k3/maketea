{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module DeepFinding where

import DataStructures
import Cpp
import MakeTeaMonad
import Util

addDeepFinding :: MakeTeaMonad ()
addDeepFinding = 
	do
		cs <- withClasses $ mapM f
		setClasses cs
	where
		f cls = case origin cls of
				Nothing -> return cls
				Just (Left r) -> elim addFindR r cls
				Just (Right t) -> addFindT t cls	

addFindR :: Rule a -> Class -> MakeTeaMonad Class
addFindR (Disj _ _) cls = do
	root <- rootSymbol
	rootCn <- toClassName root
	let decl = ((rootCn ++ "*"), "find")
	let args = [(rootCn ++ "*", "in")]
	let find = PureVirtual [] decl args 
	return $ cls { 
		  sections = sections cls ++ [Section [] Public [find]]
		}
addFindR (Conj _ body) cls = do
	root <- rootSymbol
	rootCn <- toClassName root
	let decl = ((rootCn ++ "*"), "find")
	let args = [(rootCn ++ "*", "in")]
	findTerms <- concatMapM (elim findTerm) body 
	let find = defMethod decl args $ [
		  "if (this->match (in))"
		, "	return this;"
		, ""
		] ++ findTerms ++ [
		  "return NULL;"
		]
	return $ cls { 
		  sections = sections cls ++ [Section [] Public [find]]
		}

findTerm :: Term a -> MakeTeaMonad Body
findTerm t@(Marker _ _) = do
	let vn = toVarName t		
	return [] -- nothing to do here
findTerm t@(Term _ _ m) | not (isVector m) = do 
	let vn = toVarName t
	return [
		  "Node* " ++ vn ++ "_res = " ++ vn ++ "->find(in);"
		, "if (" ++ vn ++ "_res) return " ++ vn ++ "_res;"
		, ""
		]
findTerm t@(Term _ _ m) | isVector m = do
	let vn = toVarName t
	cn <- toClassName t
	return [
		  "if(this->" ++ vn ++ " != NULL)"
		, "{"
		, "\t" ++ cn ++ "::const_iterator i;"
		, "\tfor("
		, "\t\ti = this->" ++ vn ++ "->begin();"
		, "\t\ti != this->" ++ vn ++ "->end();"
		, "\t\ti++)"
		, "\t{"
		, "\t\tif(*i != NULL)"
		, "\t\t{"
		, "\t\t\tNode* res = (*i)->find (in);"
		, "\t\t\tif (res) return res;"
		, "\t\t}"
		, "\t}"
		, "}"
		, ""
		]

addFindT :: Symbol Terminal -> Class -> MakeTeaMonad Class
addFindT t@(Terminal _ ctype) cls = do
	root <- rootSymbol
	rootCn <- toClassName root
	let decl = ((rootCn ++ "*"), "find")
	let args = [(rootCn ++ "*", "in")]
	let find = defMethod decl args $ [
		  "if (this->match (in))"
		, "	return this;"
		, ""
		, "return NULL;"
		]
	return $ cls { 
		  sections = sections cls ++ [Section [] Public [find]]
		}
