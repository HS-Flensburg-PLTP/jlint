module Language.Java.HelperMethods.Ident
  ( name,
  )
where

import Language.Java.Syntax (Ident (Ident))

name :: Ident -> String
name (Ident _ n) = n
