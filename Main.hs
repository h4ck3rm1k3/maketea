{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module Main where

import Text.ParserCombinators.Parsec
import Control.Monad.State
import System

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
import Constructors
import FactoryMethod

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
			-- The mixin code can override anything we add, so it should
			-- be added last
			addMixin
			-- Order the classes so that C++ won't complain
			orderClasses
			-- Extract relevant components
			prefix <- getPrefix
			contexts <- withContexts return
			classes <- withClasses return
			transform <- transformClass
			visitor <- visitorClass
			wildcard <- wildcardClass
			factory <- factoryMethod
			return (prefix, contexts, classes, transform, visitor, wildcard, factory)
		init = initState config grammar mixinCode
		runMaketea = evalState maketea init
		(prefix, contexts, classes, transform, visitor, wildcard, factory) = runMaketea
		commonHeader = unlines $ includes ++ [
			  "#include <list>"
			, "#include <string>"
			, "#include <assert.h>"
			, "using namespace std;"
			, ""
			]
	-- And create output
	writeFile (prefix ++ "-contexts") $ unlines (map show contexts)
	writeFile (prefix ++ ".h") $ unlines [
		  "#ifndef _" ++ prefix ++ "_H_"
		, "#define _" ++ prefix ++ "_H_"
		, ""
		, commonHeader
		, unlines (map (\c -> "class " ++ c ++ ";") (map name classes))
		, "class " ++ prefix ++ "_transform;"
		, "class " ++ prefix ++ "_visitor;"
		, ""
		, unlines (map showClassHeader classes)
		, unlines wildcard
		, ""
		, "#endif"
		]
	writeFile (prefix ++ ".cpp") $ unlines [
		  "#include \"" ++ prefix ++ ".h\""
		, "#include \"" ++ prefix ++ "_transform.h\""
		, "#include \"" ++ prefix ++ "_visitor.h\""
		, ""
		, unlines (map showClassImplementation classes)
		]
	writeFile (prefix ++ "_transform.h") $ unlines [
		   "#ifndef _" ++ prefix ++ "_TRANSFORM_H_"
		, "#define _" ++ prefix ++ "_TRANSFORM_H_"
		, ""
		, commonHeader
		, "#include \"" ++ prefix ++ ".h\""
		, ""
		, showClassHeader transform
		, ""
		, "#endif"
		]
	writeFile (prefix ++ "_transform.cpp") $ unlines [
		  "#include \"" ++ prefix ++ "_transform.h\""
		, ""
		, showClassImplementation transform
		]
	writeFile (prefix ++ "_visitor.h") $ unlines [
	      "#ifndef _" ++ prefix ++ "_VISITOR_H_"
		, "#define _" ++ prefix ++ "_VISITOR_H_"
		, ""
		, commonHeader
		, "#include \"" ++ prefix ++ ".h\""
		, ""
		, showClassHeader visitor 
		, ""
		, "#endif"
		]
	writeFile (prefix ++ "_visitor.cpp") $ unlines [
		  "#include \"" ++ prefix ++ "_visitor.h\""
		, ""
		, showClassImplementation visitor 
		]
	writeFile (prefix ++ "_factory.h") $ unlines [
	      "#ifndef _" ++ prefix ++ "_FACTORY_H_"
		, "#define _" ++ prefix ++ "_FACTORY_H_"
		, ""
		, commonHeader
		, "#include \"" ++ prefix ++ ".h\""
		, ""
		, showClassHeader factory 
		, ""
		, "#endif"
		]
	writeFile (prefix ++ "_factory.cpp") $ unlines [
		  "#include \"" ++ prefix ++ "_factory.h\""
		, ""
		, showClassImplementation factory
		]
