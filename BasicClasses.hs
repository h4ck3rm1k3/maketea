{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module BasicClasses where

import DataStructures
import MakeTeaMonad
import Cpp
import Util

createBasicClasses :: MakeTeaMonad () 
createBasicClasses = do
	ast_classes <- withGrammar (mapM (elim createClass))
	token_classes <- withTokens (mapM createTokenClass)
	setClasses (ast_classes ++ token_classes) 

createClass :: Rule a -> MakeTeaMonad Class
createClass r@(Disj c _) = do
	inh <- directSuperclasses (Exists c)
	cn <- toClassName c
	inhn <- mapM (toClassName . NonTerminal) inh
	let c = emptyAbstractClass cn
	prefix <- getPrefix 
	return $ c { 
		  extends = inhn
		, comment = [show r] 
		, friends = [prefix ++ "_transform", prefix ++ "_visitor"]
		, origin = Just (Left (Exists r))
		}
createClass r@(Conj c body) = do
	inh <- directSuperclasses (Exists c)
	cn <- toClassName c
	inhn <- mapM (toClassName . NonTerminal) inh
	fieldDecls <- mapM (elim createFieldDecl) body
	let fields = map (Attribute []) fieldDecls 
	let fieldSection = Section [] Public fields
	c <- emptyClass cn
	prefix <- getPrefix 
	return $ c { 
		  extends = inhn
		, comment = [show r]
		, sections = [fieldSection] ++ sections c 
		, friends = [prefix ++ "_transform", prefix ++ "_visitor"]
		, origin = Just (Left (Exists r))
		}

createFieldDecl :: Term a -> MakeTeaMonad (Decl Variable) 
createFieldDecl t@(Marker _ _) = do
	let name = termToVarName t
	return ("bool", name)
createFieldDecl t@(Term _ _ _) = do
	typ <- toClassName t
	let name = termToVarName t
	return (typ ++ "*", name)

{-
 - We add a get_value_as_string method only if the ctype is the default
 -}
createTokenClass :: Symbol Terminal -> MakeTeaMonad Class
createTokenClass t@(Terminal n ctype) = do
	inh <- directSuperclasses (Exists t)
	cn <- toClassName t
	inhn <- mapM (toClassName . NonTerminal) inh
	c <- emptyClass cn
	prefix <- getPrefix 
	let val t = Attribute [] (t, "value")
	string <- getStringClass
	let source_rep = Attribute [] (string ++ "*", "source_rep")
	let getv = defMethod (string ++ "*", "get_value_as_string") [] ["return value;"]
	let getsr = defMethod (string ++ "*", "get_source_rep") [] ["return source_rep;"]
	let fields = case ctype of
		Nothing -> [val (string ++ "*"),getv]
		Just "" -> [source_rep, getsr]
		Just t -> [val t, source_rep,getsr]
	let fieldSection = Section [] Public fields 
	return $ c {
		  extends = inhn
		, friends = [prefix ++ "_transform", prefix ++ "_visitor"]
		, sections = sections c ++ [fieldSection]
		, origin = Just (Right t)
		}
		
