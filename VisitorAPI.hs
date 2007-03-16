{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 -}

module VisitorAPI where

import Data.Graph.Inductive
import Data.List

import DataStructures
import MakeTeaMonad
import Cpp
import Util
import GrammarAnalysis

visitorClass :: MakeTeaMonad Class
visitorClass = do
	prefix <- withPrefix return
	{- API -}
	pre <- withSymbols $ mapM (elim (prepost "pre_"))
	post <- withSymbols $ mapM (elim (prepost "post_"))
	conc <- concreteSymbols
	pre_chain <- mapM preChain conc
	post_chain <- mapM postChain conc
	children <- withConj $ mapM chPublic
	children_t <- withTokens $ mapM chToken 
	{- Internal methods -}
	visits <- withNonMarkers $ mapM visit . nubBy eqTermVisitor
	abs <- usedAbstractSymbols
	a_pre_chain <- mapM (dispatcher "pre_" "_chain") abs
	a_post_chain <- mapM (dispatcher "post_" "_chain") abs
	a_children <- mapM (dispatcher "children_" "") abs
	return $ (emptyClassNoID (prefix ++ "visitor")) {
			sections = [
			  Section [] Public pre
			, Section [] Public post
			, Section [] Public children
			, Section [] Public children_t
			, Section [] Public pre_chain 
			, Section [] Public post_chain 
			, Section [] Protected visits
			, Section [] Protected a_pre_chain
			, Section [] Protected a_post_chain
			, Section [] Protected a_children
			]
		}

-- True if both terms have the same visitor 
eqTermVisitor :: Term NonMarker -> Term NonMarker -> Bool 
eqTermVisitor (Term _ s m) (Term _ s' m') 
	= s == s' && isVector m == isVector m'

{-
 - Internal methods
 -}

visit :: Term NonMarker -> MakeTeaMonad Member
visit t@(Term _ s m) | isVector m = do
	cn <- toClassName t
	let decl = ("void", termToVisitor t)
	let args = [(cn ++ "*", "in")]
	let body = [
		  cn ++ "::const_iterator i;"
		, "for(i = in->begin(); i != in->end(); i++)"
		, "{"
		, "\tpre_" ++ toVarName s ++ "_chain(*i);"
		, "\tchildren_" ++ toVarName s ++ "(*i);"
		, "\tpost_" ++ toVarName s ++ "_chain(*i);"
		, "}"
		]
	return $ defMethod decl args body
visit t@(Term _ s m) | not (isVector m) = do
	cn <- toClassName t
	let decl = ("void", termToVisitor t)
	let args = [(cn ++ "*", "in")]
	let body = [
		  "pre_" ++ toVarName s ++ "_chain(in);"
		, "children_" ++ toVarName s ++ "(in);"
		, "post_" ++ toVarName s ++ "_chain(in);"
		]
	return $ defMethod decl args body 

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
			cid <- findClassID s
			cn <- toClassName s
			return [
				  "case " ++ show cid ++ ":"
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
		f :: Term NonMarker -> String
		f t = termToVisitor t ++ "(in->" ++ termToVarName t ++ ");"
	return $ defMethod decl args (map f (nonMarkers body))

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

-- TODO: we re-calculate the topological sort here on every invocation
-- If we have efficiency issues, this would be a good place to start :-)
ppChain :: String -> Bool -> Some Symbol -> MakeTeaMonad Member
ppChain pp rev s = do
	ihgraph <- withDisj inheritanceGraph
	let topological = topsort' ihgraph
	cn <- toClassName s
	sc <- allSuperclasses [s]
	let sc_ordered 
		= (if rev then reverse else id) $ filter (`elem` sc) topological
	let decl = ("void", pp ++ toVarName s ++ "_chain")
	let args = [(cn ++ "*", "in")]
	return $ defMethod decl args (map (\s -> pp ++ toVarName s ++ "(in);") sc_ordered)

preChain = ppChain "pre_" False
postChain = ppChain "post_" True


