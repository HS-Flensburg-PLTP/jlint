module Language.Java.Rules.NoLoopBreak (check) where

import Data.Generics.Uniplate.Data (universeBi)
import Data.List.Extra ((//))
import Language.Java.SourceSpan (sourceSpan)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Stmt as Stmt
import qualified Language.Java.Syntax.Stmt.Extra as Stmt.Extra
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  loop <- filter Stmt.Extra.isLoop (universeBi cUnit)
  checkLoop loop path

checkLoop :: Stmt Parsed -> FilePath -> [RDF.Diagnostic]
checkLoop loop path =
  -- discarding first element of `universeBi loop` (will always be `loop` itself)
  -- there is no function getting ALL children, but excluding itself
  let loopBodyStmts = tail (universeBi loop :: [Stmt Parsed])
      shallowStmts = loopBodyStmts // universeBi (filter Stmt.Extra.isLoop loopBodyStmts)
   in map
        ( \stmt ->
            RDF.rangeDiagnostic
              "Language.Java.Rules.NoLoopBreak"
              ["Eine Schleife sollte nicht mit einem", Markdown.code "return", "beendet werden."]
              (sourceSpan stmt)
              path
        )
        (filter Stmt.isReturn shallowStmts)
        ++ map
          ( \stmt ->
              RDF.rangeDiagnostic
                "Language.Java.Rules.NoLoopBreak"
                ["Eine Schleife sollte nicht mit einem", Markdown.code "break", "beendet werden."]
                (sourceSpan stmt)
                path
          )
          (filter Stmt.isBreak (shallowStmts // universeBi (filter Stmt.isSwitch loopBodyStmts)))
