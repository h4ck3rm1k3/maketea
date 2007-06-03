{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module Init where

import Control.Monad.Identity

import DataStructures
import Cpp
import MakeTeaMonad
import Mixin

addInit :: MakeTeaMonad ()
addInit = do
	cs <- withClasses $ mapM addInitMethod  
	setClasses cs 

addInitMethod :: Class -> MakeTeaMonad Class
addInitMethod cls = do
		hasM <- mixinHasMethod (name cls) "_init"
		if hasM 
			then return . runIdentity $ mapMembers addInitMethod' cls 
			else return cls
	where
		addInitMethod' :: Member -> Identity Member
		addInitMethod' m@(Method c v s (t, n) a b) | n == name cls 
			= return $ Method c v s (t, n) a (b ++ ["_init();"])
		addInitMethod' m = return m
