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
import ContextResolution

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
	let visit = PureVirtual [] ("void", "visit") [("Visitor*", "visitor")]
	let trCh = PureVirtual [] ("void", "transform_children") [("Transform*", "transform")]
	let tvS = Section [] Public [visit, trCh]
	return $ c { 
		  extends = inhn
		, comment = [show r] 
		, origin = Just (Left (Exists r))
		, sections = [tvS] ++ sections c 
		}
createClass r@(Conj c body) = do
	inh <- directSuperclasses (Exists c)
	cn <- toClassName c
	inhn <- mapM (toClassName . NonTerminal) inh
	fieldDecls <- mapM (elim createFieldDecl) body
	let fields = map (Attribute []) fieldDecls 
	let fieldS = Section [] Public fields
	tvS <- visitTransformSection (Exists c) 
	c <- emptyClass cn
	return $ c { 
		  extends = inhn
		, comment = [show r]
		, sections = [fieldS, tvS] ++ sections c 
		, origin = Just (Left (Exists r))
		}

visitTransformSection :: Some Symbol -> MakeTeaMonad Section
visitTransformSection s = do
	-- Any of the original contexts will do
	(_,s',_):_ <- findOrigContexts s 
	let visit = defMethod ("void", "visit") [("Visitor*", "visitor")] ["visitor->visit_" ++ toVarName s' ++ "(this);"]
	let trCh = defMethod ("void", "transform_children") [("Transform*", "transform")] ["transform->children_" ++ toVarName s' ++ "(this);"]
	return $ Section [] Public [visit, trCh]

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
	let val t = Attribute [] (t, "value")
	string <- getStringClass
	let source_rep = Attribute [] (string ++ "*", "source_rep")
	let getv = defMethod (string ++ "*", "get_value_as_string") [] ["return value;"]
	let getsr = defMethod (string ++ "*", "get_source_rep") [] ["return source_rep;"]
	noSourceRep <- getNoSourceRep
	let fields = case (ctype, noSourceRep) of
		(Nothing, _)     -> [val (string ++ "*"),getv]
		(Just "", False) -> [source_rep, getsr]
		(Just "", True)  -> []
		(Just t, False)  -> [val t, source_rep,getsr]
		(Just t, True)   -> [val t]
	let fieldS = Section [] Public fields
	tvS <- visitTransformSection (Exists t) 
	return $ c {
		  extends = inhn
		, sections = [tvS, fieldS] ++ sections c 
		, origin = Just (Right t)
		}
		
