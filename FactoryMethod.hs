module FactoryMethod where

import DataStructures
import MakeTeaMonad
import Cpp
import Util

factoryMethod :: MakeTeaMonad Class 
factoryMethod = do
	listClass <- getListClass
	lists <- allLists
	creates <- withConj (concatMapM createNode)
	createLists <- concatMapM createList lists
	createTokens <- withTokens (concatMapM createToken)
	str <- getStringClass
	let 
		cmnt = 
			[
			  "If type_id corresponds to an AST node, the elements in args must"
			, "correspond to the children of the node."
			, ""
			, "If type_id corresponds to a list (of the form \"..._list\"),"
			, "the elements of arg must be of the same type as the elements"
			, "in the list, and all elements in args are added to the list."
			, ""
			, "If type_id corresponds to a token (terminal symbol), args must"
			, "contain a single node of type " ++ str ++ ". Terminal symbols"
			, "with non-default values are not supported."
			, ""
			, "If the node type is not recognized, NULL is returned."
			]
		decl = ("Object*", "create")
		args = [("char const*", "type_id"),(listClass ++ "<Object*>*", "args")]
		body = 
			[
			  listClass ++ "<Object*>::const_iterator i = args->begin();"
			] ++
			  creates ++ createTokens ++ createLists 
			  ++
			[
			  "return NULL;"
			]
		create = Method cmnt NonVirtual Static decl args body 
	return $ (emptyClassNoID "Node_factory")
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
	listClass <- getListClass
	return $ 
		[
		  "if(!strcmp(type_id, \"" ++ cn ++ "_list\"))"
		, "{"
		, "\t" ++ listClass ++ "<" ++ cn ++ "*>* list = new " ++ listClass ++ "<" ++ cn ++ "*>;"
		, "\twhile(i != args->end())"
		, "\t\tlist->push_back(dynamic_cast<" ++ cn ++ "*>(*i++));"
		, "\treturn list;" 
		, "}"
		]

createToken :: Symbol Terminal -> MakeTeaMonad Body
createToken t@(Terminal _ Nothing) = do
	cn <- toClassName t
	str <- getStringClass
	return $
		[
		  "if(!strcmp(type_id, \"" ++ cn ++ "\"))"
		, "{"
		, "\t" ++ str ++ "* value = dynamic_cast<" ++ str ++ "*>(*i++);"
		, "\tassert(i == args->end());"
		, "\treturn new " ++ cn ++ "(value);"
		, "}"
		]
createToken _ = return []
