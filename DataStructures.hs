{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module DataStructures where

import Control.Monad.State
import Util
import qualified Data.Graph.Inductive as FGL

{-
 - Definition of EBNF
 -}

type Grammar = [Some Rule]

data Conj
data Disj
data Rule :: * -> * where
	Disj :: Symbol NonTerminal -> [Some Symbol] -> Rule Disj
	Conj :: Symbol NonTerminal -> [Some Term] -> Rule Conj

data NonTerminal
data Terminal
data Symbol :: * -> * where
	NonTerminal :: Name NonTerminal -> Symbol NonTerminal
	Terminal :: Name Terminal -> Maybe CType -> Symbol Terminal

data NonMarker
data Marker
data Term :: * -> * where
	Term :: Label -> Some Symbol -> Multiplicity -> Term NonMarker
	Marker :: Label -> Name Marker -> Term Marker

type Name a = String
type Label = Maybe String
data Multiplicity = Single | Optional | Vector | VectorOpt | OptVector 
	deriving Eq
type Context = (Some Symbol, Some Symbol, Multiplicity)

{-
 - Equality and ordering
 -}

instance Eq (Some Symbol) where
	(==) = elim2 eqSymbol 

instance Eq (Symbol a) where
	(==) = eqSymbol

eqSymbol :: Symbol a -> Symbol b -> Bool
eqSymbol (NonTerminal n) (NonTerminal n') = n == n'
eqSymbol (Terminal n ctype) (Terminal n' ctype') = n == n' && ctype == ctype'
eqSymbol _ _ = False

instance Ord (Some Symbol) where
	compare = elim2 ordSymbol

instance Ord (Symbol a) where
	compare = ordSymbol

ordSymbol :: Symbol a -> Symbol b -> Ordering
ordSymbol (NonTerminal n) (NonTerminal n') = compare n n'
ordSymbol (Terminal n ctype) (Terminal n' ctype') 
	| n == n'   = compare ctype ctype' 
	| otherwise = compare n n'
ordSymbol (NonTerminal _) _ = LT
ordSymbol (Terminal _ _) _ = GT

{-
 - C++ classes
 -}

type Include = String

data Class = Class {
	  name :: Name Class
	, comment :: Comment 
	, extends :: [Name Class]
	, sections :: [Section]
	, classid :: Integer
	, friends :: [Name Class]
	, origin :: Maybe (Either (Some Rule) (Symbol Terminal))
	}
data Section = Section Comment Access [Member]
data Access = Private | Protected | Public

data Variable 
data Method
data Member = 
	  Attribute Comment (Decl Variable) 
	| Method Comment IsVirtual IsStatic (Decl Method) [Decl Variable] Body 
	| PureVirtual Comment (Decl Method) [Decl Variable] 

data IsVirtual = Virtual | NonVirtual
data IsStatic = Static | NonStatic

type Decl a = (CType, Name a)
type Body = [String]
type CType = String
type Comment = [String] 

{-
 - The maketea monad 
 - 
 - Nearly all important functions in maketea will be instances of this monad;
 - all that really means is that they inspect or transform the state (defined
 - below) in some way; for example, they might add classes, inspect the
 - grammar, etc. 
 -}

type MakeTeaMonad a = State MakeTeaState a
data MakeTeaState = MTS {
	  grammar :: Grammar  
	, nextClassID :: Integer
	, contexts :: Maybe [Context]
	, classes :: Maybe [Class]
	, inheritanceGraph :: FGL.Gr (Some Symbol) ()
	, topological :: [Some Symbol]
	, config :: Config
	}

{-
 - Configuration
 -}

data Config = Config {
	  prefix :: String
	, external_classes :: [Name Class]
	, listClass :: String
	, stringClass :: String
	}
