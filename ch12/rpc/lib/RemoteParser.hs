{-# LANGUAGE RecordWildCards #-}

module RemoteParser where

import Language.Haskell.Exts ( parseDecl, ParseResult(ParseOk), SrcSpanInfo, Decl(TypeSig), Name(Ident) )
import Language.Haskell.Meta.Syntax.Translate (toType)
import Language.Haskell.TH as TH ( Q, Type )
import Data.Char ( isSpace )

data FuncInfo = FuncInfo {
    name :: String
  , ty :: TH.Type
  }

parseRemoteInterface :: String -> Q [FuncInfo]
parseRemoteInterface quote = concat <$> mapM (funcInfo . parseDecl) tysigs
  where
    tysigs = filter (not . null) $ map (dropWhile isSpace) $ lines quote

funcInfo :: ParseResult (Decl SrcSpanInfo) -> Q [FuncInfo]
funcInfo (ParseOk (TypeSig _ ids t)) =
   pure $ [FuncInfo {..} | Ident _ name <- ids,
                       let ty = toType t]
funcInfo err =
  fail $ "Error when parsing remote interface (type signature expected)\n"
         <> show err
