module FactoryMethod where

import DataStructures
import MakeTeaMonad
import Cpp
import Util

factoryMethod :: MakeTeaMonad Class 
factoryMethod = do
	prefix <- getPrefix
	listClass <- getListClass
	lists <- allLists
	creates <- withConj (concatMapM createNode)
	createLists <- concatMapM createList lists
	let 
		cmnt = []
		decl = ("Object*", "create")
		args = [("char const*", "type_id"),(listClass ++ "<Object*>*", "args")]
		body = 
			[
			  listClass ++ "<Object*>::const_iterator i = args->begin();"
			] ++
			  creates ++ createLists
			  ++
			[
			  "assert(0);"
			, "return NULL;"
			]
		create = Method cmnt NonVirtual Static decl args body 
	return $ (emptyClassNoID (prefix ++ "_factory"))
		{
			sections = [Section [] Public [create]]
		}

createNode :: Rule Conj -> MakeTeaMonad Body
createNode (Conj head body) = do
	cn <- toClassName head
	create <- mapM (elim declChild) body 
	return $ 
		[
		  "if(!strcmp(type_id, \"" ++ cn ++ "\"))"
		, "{"
		] ++
		  map ("\t" ++) create 
		  ++
		[
		  "\tassert(i == args->end());"	
		, "\treturn new " ++ cn ++ "(" ++ flattenComma (map toVarName body) ++ ");"
		, "}"
		]

declChild :: Term a -> MakeTeaMonad String 
declChild t@(Term _ _ _) = do
	cn <- toClassName t
	let var = toVarName t
	return $ cn ++ "* " ++ var ++ " = dynamic_cast<" ++ cn ++ "*>(*i++);"
declChild t@(Marker _ _) = do
	let var = toVarName t
	return $ "bool " ++ var ++ " = dynamic_cast<Boolean*>(*i++)->value();"

createList :: Some Symbol -> MakeTeaMonad Body
createList s = do
	cn <- toClassName s
	return $ 
		[
		  "if(!strcmp(type_id, \"" ++ cn ++ "_list\"))"
		, "{"
		, "\tList<" ++ cn ++ "*>* list = new List<" ++ cn ++ "*>;"
		, "\twhile(i != args->end())"
		, "\t\tlist->push_back(dynamic_cast<" ++ cn ++ "*>(*i++));"
		, "\treturn list;" 
		, "}"
		]

