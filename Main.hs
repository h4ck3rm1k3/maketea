{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module Main where

import Text.ParserCombinators.Parsec
import Control.Monad.State
import System
import System.Directory

import Parser
import DataStructures
import ContextResolution
import MakeTeaMonad
import BasicClasses
import Cpp
import TransformAPI
import VisitorAPI
import PrettyPrinter
import Mixin
import Init
import PatternMatching
import DeepEquality
import DeepCloning
import DeepFinding
import ValidityCheck
import Constructors
import FactoryMethod
import Fold
import Util

main :: IO ()
main = do
	args <- getArgs
	let [filename] = 
		if length args == 1 
			then args 
			else error "usage: maketea filename"
	parseResult <- parseFromFile maketeaP filename
	case parseResult of 
		Left parseError -> do
			print parseError
		Right (config, grammar, includes, mixin) -> 
			runMakeTea config grammar includes mixin

runMakeTea :: Config -> Grammar -> [Include] -> [Class] -> IO ()
runMakeTea config grammar includes mixinCode = do 
	--  
	let 
		maketea = do
			contextResolution
			createBasicClasses
			addConstructors 
			addInit
			addPatternMatching
			addDeepEquality
			addDeepCloning
			addDeepFinding
			addIsValid
			-- The mixin code can override anything we add, so it should
			-- be added last
			addMixin
			-- Order the classes so that C++ won't complain
			orderClasses
			-- Extract relevant components
			outputDir	<- getOutputDir
			prefix		<- getFilePrefix
			namespace	<- getNamespace
			contexts		<- withContexts return
			classes		<- withClasses $ filterM (liftM not . isExternal . name)
			transform	<- transformClass
			visitor		<- visitorClass
			fold			<- foldClass
			wildcard		<- wildcardClass
			factory		<- factoryMethod
			return (outputDir, prefix, namespace, contexts, classes, transform, visitor, fold, wildcard, factory)
		init = initState config grammar mixinCode
		runMaketea = evalState maketea init
		(outputDir, prefix, namespace, contexts, classes, transform, visitor, fold, wildcard, factory) = runMaketea
		commonHeader = unlines $ includes ++ [
			  "#include <list>"
			, "#include <string>"
			, "#include <assert.h>"
			, "using namespace std;"
			, ""
			]
		addNamespace = case namespace of
			Nothing   -> id
			Just name -> \body -> "namespace " ++ name ++ "{\n" ++ body ++ "}\n"
		writeClass name cl = do 
			writeFile (outputDir ++ "/" ++ prefix ++ "_" ++ name ++ ".h") $ unlines [
			      "#ifndef _" ++ prefix ++ "_" ++ strToUpper name ++ "_H_"
				, "#define _" ++ prefix ++ "_" ++ strToUpper name ++ "_H_"
				, ""
				, commonHeader
				, "#include \"" ++ prefix ++ ".h\""
				, ""
				, addNamespace $ showClassHeader cl 
				, ""
				, "#endif"
				]
			writeFile (outputDir ++ "/" ++ prefix ++ "_" ++ name ++ ".cpp") $ unlines [
				  "#include \"" ++ prefix ++ "_" ++ name ++ ".h\""
				, ""
				, addNamespace $ showClassImplementation cl 
				]
	-- And create output
	createDirectoryIfMissing True outputDir
	writeFile (outputDir ++ "/" ++ prefix ++ "-contexts") $ unlines (map show contexts)
	writeFile (outputDir ++ "/" ++ prefix ++ ".h") $ unlines [
		  "#ifndef _" ++ prefix ++ "_H_"
		, "#define _" ++ prefix ++ "_H_"
		, ""
		, commonHeader
		] ++ (addNamespace $ unlines [
		  unlines (map (\c -> "class " ++ c ++ ";") (map name classes))
		, "class Transform;"
		, "class Visitor;"
		, ""
		, unlines (map showClassHeader classes)
		, unlines wildcard
		]) ++ unlines [ 
		  ""
		, "#endif"
		]
	writeFile (outputDir ++ "/" ++ prefix ++ ".cpp") $ unlines [
		  "#include \"" ++ prefix ++ ".h\""
		, "#include \"" ++ prefix ++ "_transform.h\""
		, "#include \"" ++ prefix ++ "_visitor.h\""
		, ""
		, addNamespace $ unlines (map showClassImplementation classes)
		]
	writeClass "transform" transform
	writeClass "visitor" visitor
	writeClass "factory" factory
	writeFile (outputDir ++ "/" ++ prefix ++ "_fold.h") $ unlines [
		  commonHeader
		, "#include \"" ++ prefix ++ ".h\""
		, addNamespace $ fold 
		]
