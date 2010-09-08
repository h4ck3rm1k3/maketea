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
	none_class <- createNoneClass
	setClasses (ast_classes ++ token_classes ++ [none_class])

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

createNoneClass :: MakeTeaMonad Class
createNoneClass = do
	name <- getNoneName 
	c <- emptyClass name

	-- Overwrite all expected methods
	stringClass <- getStringClass
	rc <- getRootName
	root <- rootSymbol
	let rootName = rc ++ "*"
	rootCnList <- toClassName (Term Nothing root Vector)

	let rootName = rc ++ "*"
	let visit = defMethod ("void", "visit") [("Visitor*", "visitor")] ["assert (0);"]
	let transform = defMethod ("void", "transform_children") [("Transform*", "transform")] ["assert (0);"]
	let clone = defMethod (name ++ "*", "clone") [] ["assert (0);"]
	let valid = defMethod ("void", "assert_valid") [] ["assert (0);"]
	let get_value_as_string = defMethod (stringClass ++ "*", "get_value_as_string") [] ["assert (0);"]
	let classid = defMethod ("int", "classid") [] ["assert (0);"]
	let match = defMethod ("bool", "match") [(rootName, "in")] ["assert (0);"]
	let equals = defMethod ("bool", "equals") [(rootName, "in")] ["assert (0);"]
	let find = defMethod (rootName, "find") [(rootName, "in")] ["assert (0);"]
	let find_all = defMethod ("void", "find_all") [(rootName, "in"), (rootCnList ++ "*", "out")] ["assert (0);"]

	let methods = Section [] Public [visit, transform, clone, valid, get_value_as_string, classid, match, equals, find, find_all]

	-- Inherit from all classes
	syms <- withSymbols $ mapM toClassName 
	return $ c {
			  extends = syms
			, comment = ["The top of the class hierarchy. If the Fold will not allow you fold to anything else, try this."]
			, sections = [methods]
			, origin = Nothing
	}

visitTransformSection :: Some Symbol -> MakeTeaMonad Section
visitTransformSection s = do
	-- Any of the original contexts will do
	(_,s',_):_ <- findOrigContexts s 
	let visit = defMethod ("void", "visit") [("Visitor*", "visitor")] ["visitor->visit_" ++ toFuncPart s' ++ "(this);"]
	let trCh = defMethod ("void", "transform_children") [("Transform*", "transform")] ["transform->children_" ++ toFuncPart s' ++ "(this);"]
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
	let getv = defMethod (string ++ "*", "get_value_as_string") [] ["return value;"]
	let fields = case (ctype) of
		(Nothing)     -> [val (string ++ "*"),getv]
		(Just "")  -> []
		(Just t)   -> [val t]
	let fieldS = Section [] Public fields
	tvS <- visitTransformSection (Exists t) 
	return $ c {
		  extends = inhn
		, sections = [tvS, fieldS] ++ sections c 
		, origin = Just (Right t)
		}
		
