{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 -}

module GrammarAnalysis where

import Data.List
import Data.Maybe
import Control.Monad
import Data.Graph.Inductive

import DataStructures
import MakeTeaMonad
import Util
import PrettyPrinter

{-
 - isAbstract nt is true if nt is defined by a disjunction
 -}

isAbstract :: Name NonTerminal -> MakeTeaMonad Bool
isAbstract nt = do
	r <- findRuleFor nt
	return (elim isDisj r)

{-
 - Is a rule a disjunction?
 -}

isDisj :: Rule a -> Bool
isDisj (Disj _ _) = True
isDisj (Conj _ _) = False 

{-
 - Is a symbol a non-terminal?
 -}

isNonTerminal :: Some Symbol -> Bool
isNonTerminal = elim f
	where
		f :: Symbol a -> Bool
		f (NonTerminal _) = True
		f _ = False

{-
 - findRuleFor nt finds the rule in the grammar that defines nt
 -}

findRuleFor :: Name NonTerminal -> MakeTeaMonad (Some Rule)
findRuleFor nt = withGrammar $ (fromJustM $ "Unknown non-terminal " ++ nt) . find (elim f)
	where
		f :: Rule a -> Bool
		f r = nameOf (ruleHead r) == nt 

{-
 - The instances of a symbol s always includes s itself. For abstract symbols
 - defined by a disjunction x1 .. xn, the instances of s also include all
 - instances of the xs. The instances will be returned in order from general to
 - more specific (that is, if t is an instance of t', t' will be reported
 - before t).
 -}

allInstances :: Symbol a -> MakeTeaMonad [Some Symbol]
allInstances i@(NonTerminal nt) = 
	do
		r <- findRuleFor nt
		elim f r	
	where
		f :: Rule a -> MakeTeaMonad [Some Symbol]
		f (Disj _ xs) = do
			is <- mapM (elim allInstances) xs
			return (Exists i:concat is)
		f (Conj _ _) = return [Exists i]
allInstances i@(Terminal _ _) = return [Exists i]

{-
 - Like allInstances, but filtered to include the concrete instances only
 -}

concreteInstances :: Symbol a -> MakeTeaMonad [Some Symbol]
concreteInstances i@(NonTerminal nt) = 
	do
		r <- findRuleFor nt
		elim f r	
	where
		f :: Rule a -> MakeTeaMonad [Some Symbol]
		f (Disj _ xs) = do
			is <- mapM (elim concreteInstances) xs
			return (concat is)
		f (Conj _ _) = return [Exists i]
concreteInstances i@(Terminal _ _) = return [Exists i]

{-
 - commonInstance finds the most general common instance of two symbols, if it
 - exists.
 -}

commonInstance :: Symbol a -> Symbol b -> MakeTeaMonad (Maybe (Some Symbol))
commonInstance s1 s2 = do
	is1 <- allInstances s1
	is2 <- allInstances s2
	return (find (`elem` is2) is1)

{-
 - The direct superclasses of a symbol c are all those symbols s which are
 - defined by a rule s ::= ... | c | ...
 -}

directSuperclasses :: Some Symbol -> MakeTeaMonad [Name NonTerminal]
directSuperclasses c = withDisj $ return . catMaybes . (map f)
	where
		f :: Rule Disj -> Maybe (Name NonTerminal)
		f (Disj s cs) = if c `elem` cs then Just (nameOf s) else Nothing

{-
 - Transitive reflexive closure of directSuperclasses
 -}

allSuperclasses :: [Some Symbol] -> MakeTeaMonad [Some Symbol]
allSuperclasses [] = return []
allSuperclasses c = do
	ds <- concatMapM directSuperclasses c
	let ds' = (nub . map (Exists . NonTerminal)) ds
	dds <- allSuperclasses ds' 
	return (nub (c ++ ds' ++ dds))

{-
 - isVector is true for Vector, VectorOpt and OptVector
 -}

isVector :: Multiplicity -> Bool
isVector Single = False
isVector Optional = False
isVector Vector = True
isVector VectorOpt = True
isVector OptVector = True

{-
 - All concrete symbols (that is, non-terminal symbols that are defined as a
 - conjunction, and terminal symbols) in the grammar
 -}

concreteSymbols :: MakeTeaMonad [Some Symbol]
concreteSymbols = do
	nts <- withConj $ return . (map (Exists . ruleHead))
	tokens <- withTokens $ return . map Exists
	return (nts ++ tokens)

{-
 - All abstract symbols (that is, non-terminal sysbols that are defined as a
 - disjunction)
 -}

abstractSymbols :: MakeTeaMonad [Name NonTerminal]
abstractSymbols = withDisj $ return . (map (nameOf . ruleHead))

{-
 - All used symbols; that is, all symbols that appear in the RHS of a
 - conjunction 
 -}

usedSymbols :: MakeTeaMonad [Some Symbol]
usedSymbols = withConj $ return . nub . concatMap f
	where
		f :: Rule Conj -> [Some Symbol]
		f (Conj _ body) = [s | Exists (Term _ s _) <- body]

{-
 - All used abstract symbols
 -}

usedAbstractSymbols :: MakeTeaMonad [Name NonTerminal]
usedAbstractSymbols 
		= usedSymbols >>= filterM isAbstract . catMaybes . map (elim f)
	where
		f :: Symbol a -> Maybe (Name NonTerminal)
		f (NonTerminal n) = Just n
		f _ = Nothing

{-
 - Inheritance graph
 -}

inheritanceGraph :: [Rule Disj] -> MakeTeaMonad (Gr (Some Symbol) ()) 
inheritanceGraph rs = do
	labels <- withSymbols $ \ss -> return [(s,no) | s <- ss | no <- [1..]]
	let 
		labelFor s = case lookup s labels of Just l -> l
		nodes = map (\(a,b) -> (b,a)) labels
		edges = concatMap edgesFor rs
		edgesFor :: Rule Disj -> [LEdge ()]
		edgesFor (Disj nt body) =
			let lr = labelFor (Exists nt) 
			in [(lr,labelFor s,()) | s <- body] 
	return (mkGraph nodes edges)

{-
 - Find the name of stuff
 -}

class NameOf a where
	nameOf :: a -> String

instance NameOf (Some Symbol) where
	nameOf = elim nameOf 

instance NameOf (Symbol a) where
	nameOf (NonTerminal name) = name
	nameOf (Terminal name _) = name

instance NameOf (Some Term) where
	nameOf = elim nameOf

instance NameOf (Term a) where
	nameOf (Term (Just label) _ _) = label
	nameOf (Term Nothing s m) 
		| isVector m = nameOf s ++ "s"
		| otherwise = nameOf s
	nameOf (Marker (Just label) _) = label
	nameOf (Marker Nothing name) = name

instance NameOf Class where
	nameOf = name

instance NameOf Member where
	nameOf (Attribute _ (_, name)) = name
	nameOf (Method _ _ _ (_, name) _ _) = name
	nameOf (PureVirtual _ (_, name) _) = name
