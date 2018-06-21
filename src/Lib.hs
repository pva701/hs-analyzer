module Lib
    ( hsAnalize
    ) where

import Universum

import System.Environment
import Language.Haskell.Exts

hsAnalize :: FilePath -> IO ()
hsAnalize file = parseFile file >>= \case
    ParseOk mod -> case mod of
        Module _ _ _ _ decls -> do
            -- mapM_ (\x -> putText $ show (prettyPrint x) <> "\n\n") decls
            mapM_ (\x -> putText $ show (const () <$> x) <> "\n\n") decls
            mapM_ (putText . show . isJust . detectDataLikeNewtype) decls
        _                  -> putText "Not supported yet"
    ParseFailed srcLoc reason -> putText "Parsing failed"

-- QName

-- Detect "data" which doesn't have bind type variables and contexts,
-- hovewer, with single constructor with single field.
-- If so suggest replacing with "newtype"
detectDataLikeNewtype :: Decl SrcSpanInfo -> Maybe (SrcSpanInfo, String)
detectDataLikeNewtype (DataDecl dataLoc (DataType _) _ (DHead _ (Ident _ nm)) [QualConDecl _ Nothing Nothing constr] _)
    | checkOneField constr = Just (dataLoc, nm)
    | otherwise            = Nothing
  where
    checkOneField :: ConDecl SrcSpanInfo -> Bool
    checkOneField (ConDecl _ _ [_]) = True
    checkOneField (RecDecl _ _ [_]) = True
    checkOneField                 _ = False
detectDataLikeNewtype _ = Nothing

-- newtype A = forall x . A x
-- newtype B x =  Ord x => B x
-- newtype D x =  x ~ Int => D x
-- newtype X = Yep {lol :: Int}

-- data A1 = forall x . A1 x
-- data B1 x = Ord x => B1 x
-- data C1 x = Ord x => C1 x | C2
-- data D x =  x ~ Int => D x

-- r :: C1 A1
-- r = C2 -- C1 (A1 3)
