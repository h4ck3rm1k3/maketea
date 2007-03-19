{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module Init where

import Control.Monad.Identity

import DataStructures
import Cpp
import GrammarAnalysis
import MakeTeaMonad

addInit :: MakeTeaMonad ()
addInit = withClasses $ setClasses . map addInitMethod

addInitMethod :: Class -> Class
addInitMethod cls = 
		if hasMethod "_init" cls 
		then runIdentity $ mapMembers addInitMethod' cls 
		else cls
	where
		addInitMethod' :: Member -> Identity Member
		addInitMethod' m@(Method c v s (t, n) a b) | n == name cls 
			= return $ Method c v s (t, n) a (b ++ ["_init();"])
		addInitMethod' m = return m
