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
		cs_all <- withClasses $ mapM f_all
		setClasses cs_all
	where
		f cls = case origin cls of
				Nothing -> return cls
				Just (Left r) -> elim addFindR r cls
				Just (Right t) -> addFindT t cls	
		f_all cls = case origin cls of
				Nothing -> return cls
				Just (Left r) -> elim addFindAllR r cls
				Just (Right t) -> addFindAllT t cls	



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
		, "\treturn this;"
		, ""
		] ++ findTerms ++ [
		  "return NULL;"
		]
	return $ cls { 
		  sections = sections cls ++ [Section [] Public [find]]
		}

findTerm :: Term a -> MakeTeaMonad Body
findTerm t@(Marker _ _) = do
	return [] -- nothing to do here
findTerm t@(Term _ _ m) | not (isVector m) = do 
	let vn = toVarName t
	root <- rootSymbol
	rootCn <- toClassName root
	return [
		  "if (this->" ++ vn ++ " != NULL)"
		, "{"
		, "\t" ++ rootCn ++ "* " ++ vn ++ "_res = this->" ++ vn ++ "->find(in);"
		, "\tif (" ++ vn ++ "_res) return " ++ vn ++ "_res;"
		, "}"
		, ""
		]
findTerm t@(Term _ _ m) | isVector m = do
	let vn = toVarName t
	cn <- toClassName t
	root <- rootSymbol
	rootCn <- toClassName root
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
		, "\t\t\t" ++ rootCn ++ "* res = (*i)->find (in);"
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
		, "\treturn this;"
		, ""
		, "return NULL;"
		]
	return $ cls { 
		  sections = sections cls ++ [Section [] Public [find]]
		}


{-
 - FindAll puts the result in a list, and keeps looking.
 -}

addFindAllR :: Rule a -> Class -> MakeTeaMonad Class
addFindAllR (Disj _ _) cls = do
	root <- rootSymbol
	rootCn <- toClassName root
	rootCnList <- toClassName (Term Nothing root Vector)
	let decl = ("void", "find_all")
	let args = [(rootCn ++ "*", "in"), (rootCnList ++ "*", "out")]
	let findAll = PureVirtual [] decl args 
	return $ cls { 
		  sections = sections cls ++ [Section [] Public [findAll]]
		}
addFindAllR (Conj _ body) cls = do
	root <- rootSymbol
	rootCn <- toClassName root
	rootCnList <- toClassName (Term Nothing root Vector)
	let decl = ("void", "find_all")
	let args = [(rootCn ++ "*", "in"), (rootCnList ++ "*", "out")]
	findAllTerms <- concatMapM (elim findAllTerm) body 
	let findAll = defMethod decl args $ [
		  "if (this->match (in))"
		, "\tout->push_back (this);"
		, ""
		] ++ findAllTerms
	return $ cls { 
		  sections = sections cls ++ [Section [] Public [findAll]]
		}

findAllTerm :: Term a -> MakeTeaMonad Body
findAllTerm t@(Marker _ _) = do
	return [] -- nothing to do here
findAllTerm t@(Term _ _ m) | not (isVector m) = do 
	let vn = toVarName t
	return [
		  "if (this->" ++ vn ++ " != NULL)"
		, "\tthis->" ++ vn ++ "->find_all(in, out);"
		, ""
		]
findAllTerm t@(Term _ _ m) | isVector m = do
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
		, "\t\t\t(*i)->find_all (in, out);"
		, "\t\t}"
		, "\t}"
		, "}"
		, ""
		]

addFindAllT :: Symbol Terminal -> Class -> MakeTeaMonad Class
addFindAllT t@(Terminal _ ctype) cls = do
	root <- rootSymbol
	rootCn <- toClassName root
	rootCnList <- toClassName (Term Nothing root Vector)
	let decl = ("void", "find_all")
	let args = [(rootCn ++ "*", "in"), (rootCnList ++ "*", "out")]
	let findAll = defMethod decl args $ [
		  "if (this->match (in))"
		, "\tout->push_back (this);"
		]
	return $ cls { 
		  sections = sections cls ++ [Section [] Public [findAll]]
		}



