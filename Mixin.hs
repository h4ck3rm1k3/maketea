{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module Mixin where

import Data.List

import DataStructures
import MakeTeaMonad

addMixin :: [Class] -> MakeTeaMonad ()
addMixin mixinCode = do
	cs <- withClasses (mixin mixinCode)
	setClasses cs

mixin :: [Class] -> [Class] -> MakeTeaMonad [Class]
mixin mixinCode = mapM (mixinClass mixinCode) 

mixinClass :: [Class] -> Class -> MakeTeaMonad Class
mixinClass mixinCode c = do
	let c' = find (\c' -> name c == name c') mixinCode
	case c' of 
		Nothing -> return c
		Just c' -> return (combineClasses c c')

combineClasses :: Class -> Class -> Class
combineClasses c c' = Class {
		  name = name c
		, comment = comment c ++ comment c'
		, extends = extends c ++ extends c'
		, sections = sections c ++ sections c'
		, classid = classid c
		, friends = friends c ++ friends c'
		, origin = origin c
		}
