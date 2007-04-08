{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module VisitorAPI where

import Data.Graph.Inductive
import Data.List

import DataStructures
import MakeTeaMonad
import Cpp
import Util
import ContextResolution

visitorClass :: MakeTeaMonad Class
visitorClass = do
	prefix <- getPrefix 
	{- API -}
	pre <- withSymbols $ mapM (elim (prepost "pre_"))
	post <- withSymbols $ mapM (elim (prepost "post_"))
	conc <- concreteSymbols
	pre_chain <- mapM preChain conc
	post_chain <- mapM postChain conc
	children <- withConj $ mapM chPublic
	children_t <- withTokens $ mapM chToken 
	{- Internal methods -}
	nm <- allNonMarkers
	visits <- (concatMapM visit . nubBy eqTermVisitor) nm
	abs <- usedAbstractSymbols
	a_pre_chain <- mapM (dispatcher "pre_" "_chain") abs
	a_post_chain <- mapM (dispatcher "post_" "_chain") abs
	a_children <- mapM (dispatcher "children_" "") abs
	let destructor = defMethod ("", "~" ++ prefix ++ "_visitor") [] []
	let visit_null = defMethod ("void", "visit_null") [("char const*", "name")] []
	let visit_marker = defMethod ("void", "visit_marker") [("char const*", "name"), ("bool", "value")] [] 
	return $ (emptyClassNoID (prefix ++ "_visitor")) {
			sections = [
		  	  Section [] Public [destructor] 
			, Section [] Public pre
			, Section [] Public post
			, Section [] Public children
			, Section [] Public children_t
			, Section [] Public pre_chain 
			, Section [] Public post_chain 
			, Section [] Public visits
			, Section [] Public [visit_null, visit_marker]
			, Section [] Public a_pre_chain
			, Section [] Public a_post_chain
			, Section [] Public a_children
			]
		}

-- True if both terms have the same visitor 
eqTermVisitor :: Term NonMarker -> Term NonMarker -> Bool 
eqTermVisitor (Term _ s m) (Term _ s' m') 
	= s == s' && isVector m == isVector m'

{-
 - Internal methods
 -}

visit :: Term NonMarker -> MakeTeaMonad [Member]
visit t@(Term _ s m) | isVector m = do
	cn <- toClassName t
	let decl = ("void", termToVisitor t)
	let args = [(cn ++ "*", "in")]
	let t' = Term undefined s Single
	cn' <- toClassName t'
	let decl' = ("void", termToVisitor t')
	let args' = [(cn' ++ "*", "in")]
	let visitM = defMethod decl args [
		  cn ++ "::const_iterator i;"
		, ""
		, "if(in == NULL)"
		, "\tvisit_null(\"" ++ cn ++ "\");"
		, "else for(i = in->begin(); i != in->end(); i++)"
		, "{"
		, "\tvisit_" ++ toVarName s ++ "(*i);"
		, "}"
		]
	let visitS = defMethod decl' args' [
		  "if(in == NULL)"
		, "\tvisit_null(\"" ++ cn' ++ "\");"
		, "else"
		, "{"
		, "\tpre_" ++ toVarName s ++ "_chain(in);"
		, "\tchildren_" ++ toVarName s ++ "(in);"
		, "\tpost_" ++ toVarName s ++ "_chain(in);"
		, "}"
		]
	-- If the context m' is Single, that means that there must be an explicit
	-- Single occurence of the term somewhere else, and we do not need to
	-- generate the visitS here
	(_,_,m') <- findContext s 
	let methods = if isVector m' 
		then [visitM, visitS]
		else [visitM]
	return methods 
visit t@(Term _ s m) | not (isVector m) = do
	cn <- toClassName t
	let decl = ("void", termToVisitor t)
	let args = [(cn ++ "*", "in")]
	let body = [
		  "if(in == NULL)"
		, "\tvisit_null(\"" ++ cn ++ "\");"
		, "else"
		, "{"
		, "\tpre_" ++ toVarName s ++ "_chain(in);"
		, "\tchildren_" ++ toVarName s ++ "(in);"
		, "\tpost_" ++ toVarName s ++ "_chain(in);"
		, "}"
		]
	return $ [defMethod decl args body]

dispatcher :: String -> String -> Name NonTerminal -> MakeTeaMonad Member
dispatcher pre post nt = 
	do 
		cn <- toClassName (NonTerminal nt)
		let decl = ("void", pre ++ nt ++ post)
		let args = [(cn ++ "*", "in")]
		conc <- concreteInstances (NonTerminal nt)
		cases <- concatMapM switchcase conc	
		let body = ["switch(in->classid())", "{"] ++ cases ++ ["}"]
		return (defMethod decl args body)
	where
		switchcase :: Some Symbol -> MakeTeaMonad Body
		switchcase s = do
			cn <- toClassName s
			return [
				  "case " ++ cn ++ "::ID:"
				, "\t" ++ pre ++ toVarName s ++ post ++ "(dynamic_cast<" ++ cn ++ "*>(in));"
				, "\tbreak;"
				]

{-
 - API methods
 -}

prepost :: String -> Symbol a -> MakeTeaMonad Member
prepost pp s = do
	cn <- toClassName s
	let decl = ("void", pp ++ toVarName s)
	let args = [(cn ++ "*", "in")] 
	return $ defMethod decl args [] 

chPublic :: Rule Conj -> MakeTeaMonad Member
chPublic (Conj nt body) = do
	cn <- toClassName nt 
	let decl = ("void", "children_" ++ nameOf nt)
	let args = [(cn ++ "*", "in")]
	let 
		f :: Term a -> String
		f t@(Term _ _ _) = termToVisitor t ++ "(in->" ++ toVarName t ++ ");"
		f m@(Marker _ _) = "visit_marker(\"" ++ toVarName m ++ "\", in->" ++ toVarName m ++ ");"
	return $ defMethod decl args (map (elim f) body)

chToken :: Symbol Terminal -> MakeTeaMonad Member
chToken t@(Terminal n _) = do
	cn <- toClassName t
	let decl = ("void", "children_" ++ toVarName t)
	let args = [(cn ++ "*", "in")]
	return (defMethod decl args [])

termToVisitor :: Term NonMarker -> Name Method
termToVisitor (Term _ s m) 
	| isVector m = "visit_" ++ toVarName s ++ "_list"
	| otherwise = "visit_" ++ toVarName s

ppChain :: String -> Bool -> Some Symbol -> MakeTeaMonad Member
ppChain pp rev s = do
	top <- withTopological return 
	cn <- toClassName s
	sc <- allSuperclasses [s]
	let sc_ordered 
		= (if rev then reverse else id) $ filter (`elem` sc) top
	let decl = ("void", pp ++ toVarName s ++ "_chain")
	let args = [(cn ++ "*", "in")]
	return $ defMethod decl args (map (\s -> pp ++ toVarName s ++ "(in);") sc_ordered)

preChain = ppChain "pre_" False
postChain = ppChain "post_" True


