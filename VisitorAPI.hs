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
	-- unparser support
	let visit_marker = defMethod ("void", "visit_marker") [("char const*", "name"), ("bool", "value")] [] 
	let visit_null = defMethod ("void", "visit_null") [("char const*", "type_id")] []
	let visit_null_list = defMethod ("void", "visit_null_list") [("char const*", "type_id")] []
	let pre_list = defMethod ("void", "pre_list") [("char const*", "type_id"), ("int", "size")] [] 
	let post_list = defMethod ("void", "post_list") [("char const*", "type_id"), ("int", "size")] []
	return $ (emptyClassNoID (prefix ++ "_visitor")) {
			sections = [
		  	  Section [] Public [destructor] 
			, Section ["Invoked before the children are visited"] Public pre
			, Section ["Invoked after the children have been visited"] Public post
			, Section ["Visit the children of a node"] Public children
			, Section ["Tokens don't have children, so these methods do nothing by default"] Public children_t
			, Section ["Unparser support"] Public [visit_marker, visit_null, visit_null_list, pre_list, post_list]
			, Section ["Invoke the chain of pre-visit methods along the inheritance hierachy","Do not override unless you know what you are doing"] Public pre_chain 
			, Section ["Invoke the chain of post-visit methods along the inheritance hierarchy","(invoked in opposite order to the pre-chain)","Do not override unless you know what you are doing"] Public post_chain 
			, Section ["Call the pre-chain, visit children and post-chain in order","Do not override unless you know what you are doing"] Public visits
			, Section ["Invoke the right pre-chain (manual dispatching)","Do not override unless you know what you are doing"] Public a_pre_chain
			, Section ["Invoke the right post-chain (manual dispatching)","Do not override unless you know what you are doing"] Public a_post_chain
			, Section ["Invoke the right visit-children (manual dispatching)","Do not override unless you know what you are doing"] Public a_children
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
		, "\tvisit_null_list(\"" ++ cn' ++ "\");"
		, "else"
		, "{"
		, "\tpre_list(\"" ++ cn' ++ "\", in->size());"
		, ""
		, "\tfor(i = in->begin(); i != in->end(); i++)"
		, "\t{"
		, "\t\tvisit_" ++ toVarName s ++ "(*i);"
		, "\t}"
		, ""
		, "\tpost_list(\"" ++ cn' ++ "\", in->size());"
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


