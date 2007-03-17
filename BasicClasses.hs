{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module BasicClasses where

import DataStructures
import MakeTeaMonad
import GrammarAnalysis
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
	prefix <- withPrefix return
	return $ c { 
		  extends = inhn 
		, friends = [prefix ++ "transform", prefix ++ "visitor"]
		, origin = Just (Left (Exists r))
		}
createClass r@(Conj c body) = do
	inh <- directSuperclasses (Exists c)
	cn <- toClassName c
	inhn <- mapM (toClassName . NonTerminal) inh
	fieldDecls <- mapM (elim createFieldDecl) body
	let fields = map (Attribute []) fieldDecls 
	let fieldSection = Section [] Public fields
	let constrSection = Section [show r] Public [constructor cn fields]
	c <- emptyClass cn
	prefix <- withPrefix return
	return $ c { 
		  extends = inhn
		, sections = [constrSection, fieldSection] ++ sections c 
		, friends = [prefix ++ "transform", prefix ++ "visitor"]
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

-- Create a constructor for a list of fields
constructor :: Name Class -> [Member] -> Member
constructor cn fields 
		= defConstr ("", cn) args (concatMap copy fields)
	where
		args = [(t,n) | Attribute _ (t,n) <- fields]
		copy (Attribute _  (_,n)) = ["this->" ++ n ++ " = " ++ n ++ ";"]
		copy _ = []

{-
 - We add a get_value_as_string method only if the ctype is the default
 -}
createTokenClass :: Symbol Terminal -> MakeTeaMonad Class
createTokenClass t@(Terminal n ctype) = do
	inh <- directSuperclasses (Exists t)
	cn <- toClassName t
	inhn <- mapM (toClassName . NonTerminal) inh
	c <- emptyClass cn
	prefix <- withPrefix return
	let val t = Attribute [] (t, "value")
	let source_rep = Attribute [] ("string*", "source_rep")
	let getv = defMethod ("string*", "get_value_as_string") [] ["return value;"]
	let fields = case ctype of
		Nothing -> [val "string*",getv]
		Just "" -> [source_rep]
		Just t -> [val t, source_rep]
	let fieldSection = Section [] Public fields 
	let constrSection = Section [] Public [constructor cn fields]
	return $ c {
		  extends = inhn
		, friends = [prefix ++ "transform", prefix ++ "visitor"]
		, sections = sections c ++ [fieldSection, constrSection]
		, origin = Just (Right t)
		}
		
