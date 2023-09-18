module Language.Java.Syntax.Annotation.Extra (name) where

import Language.Java.Syntax (Annotation (..), Name)

-- TODO: move to Language.Java.Syntax.Annotation
name :: Annotation p -> Name
name (NormalAnnotation _ name _) = name
name (SingleElementAnnotation _ name _) = name
name (MarkerAnnotation _ name) = name
