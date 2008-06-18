{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module Fold where

import Control.Monad
import Util
import DataStructures
import MakeTeaMonad
import Cpp
import List

param :: ToClassName a => a -> MakeTeaMonad String 
param sym = ("_" ++) `fmap` toClassName sym

foldClass :: MakeTeaMonad String 
foldClass = do
	templateParams <- withSymbols $ mapM param 
	concreteFolds <- withConj $ mapM concreteFold
	tokenFolds <- withTokens $ mapM tokenFold
	recFolds <- withConj $ concatMapM recFold
	dispatchers <- withDisj $ concatMapM dispatcher
	return $ unlines 
		[
		  "template"
		, "<" ++ (flattenWith ",\n " $ map ("class " ++) (sort templateParams)) ++ ">"
		, "class Fold"
		, "{"
		, "// Access this class from subclasses without copying out the template instantiation"
		, "public:"
		, "   typedef Fold<" ++ (flattenWith ", " (sort templateParams)) ++ "> parent;"
		, "// Recursively fold the children before folding the parent"
		, "// This methods form the client API for a fold, but should not be"
		, "// overridden unless you know what you are doing"
		, "public:"
		, unlines $ map ("\t" ++) recFolds
		, ""
		, "// The user-defined folds"
		, "// Override these methods to get specific functionality"
		, "public:"
		, unlines $ map ("\t" ++) concreteFolds
		, unlines $ map ("\t" ++) tokenFolds
		, ""
		, "// Manual dispatching for abstract classes"
		, "// Override only if you know what you are doing!"
		, unlines $ map ("\t" ++) dispatchers 
		, ""
		, "// Virtual destructor to avoid compiler warnings"
		, "\tvirtual ~Fold() {}" 
		, "};"
		, ""
		, "template<class T>"
		, "class Uniform_fold : public Fold<" ++ flattenComma (replicate (length templateParams) "T") ++ "> {};"
		]

concreteFold :: Rule Conj -> MakeTeaMonad String
concreteFold (Conj head body) = do
	returnType <- param head
	cn <- toClassName head
	let fnName = toVarName head
	args <- forM body $ \term -> do
		argType <- elim (termToParam "*") term
		let argName = toVarName term
		return (argType ++ " " ++ argName) 
	return $ "virtual " ++ returnType ++ " fold_impl_" ++ fnName ++ "(" ++ flattenComma ((cn ++ "* orig") : args) ++ ") { assert(0); };"

tokenFold :: Symbol Terminal -> MakeTeaMonad String
tokenFold sym = do
	returnType <- param sym 
	cn <- toClassName sym
	let fnName = toVarName sym 
	return $ "virtual " ++ returnType ++ " fold_" ++ fnName ++ "(" ++ cn ++ "* orig) { assert(0); };"

dispatcher :: Rule Disj -> MakeTeaMonad [String]
dispatcher (Disj head _) = do
	returnType <- param head
	cn <- toClassName head
	let fnName = toVarName head
	conc <- concreteInstances head 
	body <- forM conc $ \term -> do
		cn <- toClassName term
		let vn = toVarName term
		return [
			  "\t\tcase " ++ cn ++ "::ID:"
			, "\t\t\treturn fold_" ++ vn ++ "(dynamic_cast<" ++ cn ++ "*>(in));" 
			]
	return $ 
		[ "virtual " ++ returnType ++ " fold_" ++ fnName ++ "(" ++ cn ++ "* in)"
		, "{"
		, "\tswitch(in->classid())"
		, "\t{"
		] ++ concat body ++ 
		[ "\t}"
		, "\tassert(0);"
		, "}\n"
		]

recFold :: Rule Conj -> MakeTeaMonad [String]
recFold (Conj head body) = do 
	cn <- toClassName head
	returnType <- param head
	let fnName = toVarName head
	    vars = map toVarName body	
	recs <- forM body (elim recCall) 
	return $ 
		[ 
		  "virtual " ++ returnType ++ " fold_" ++ fnName ++ "(" ++ cn ++ "* in)"
		, "{"] ++ concat recs ++ [
		  "\treturn fold_impl_" ++ fnName ++ "(" ++ flattenComma ("in" : vars) ++ ");" 
		, "}\n"
		]

recCall :: Term a -> MakeTeaMonad [String] 
recCall t@(Term lab sym mult) | not (isVector mult) = do 
	let t' = Term Nothing sym Single 
	    vn = toVarName t
	cn <- param t
	return 
		[ 
		  -- TODO: returning "0" for NULL values isn't completely satisfactory
		  "\t" ++ cn ++ " " ++ vn ++ " = 0;"
		, "\tif(in->" ++ vn ++ " != NULL) " ++ vn ++ " = fold_" ++ toVarName t' ++ "(in->" ++ vn ++ ");"
		] 
recCall t@(Term lab sym mult) | isVector mult = do
	let t' = Term Nothing sym Single 
	param <- termToParam "" t
	cn <- toClassName t
	let vn = toVarName t
	return 
		[
		  -- TODO: Again, using 0 for NULL values
		  "\t" ++ param ++ "* " ++ vn ++ " = 0;"
		, if mult == OptVector then "if (in->" ++ vn ++ ")" else ""
		, "\t{"
		, "\t\t" ++ vn ++ " = new " ++ param ++ ";"
		, "\t\t" ++ cn ++ "::const_iterator i;"
		, "\t\tfor(i = in->" ++ vn ++ "->begin(); i != in->" ++ vn ++ "->end(); i++)"
		, "\t\t\tif(*i != NULL) " ++ vn ++ "->push_back(fold_" ++ toVarName t' ++ "(*i));" 
		, "\t\t\telse " ++ vn ++ "->push_back(0);" 
		, "\t}"
		]
recCall m@(Marker _ _) = do
	return ["\tbool " ++ toVarName m ++ " = in->" ++ toVarName m ++ ";"]

termToParam :: String -> Term a -> MakeTeaMonad String
termToParam listSuffix t@(Term lab sym mult) = do
	list <- getListClass
	tp <- param (Term lab sym Single)
	return $ if isVector mult 
		then list ++ "<" ++ tp ++ ">" ++ listSuffix
		else tp
termToParam _ m@(Marker _ _) = return "bool" 
