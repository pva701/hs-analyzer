module Lib
    ( hsAnalize
    ) where

import Universum

import System.Environment
import Language.Haskell.Exts
import Data.Generics.Schemes (everything)
import Data.Generics.Aliases (mkQ)

type Decl' = Decl SrcSpanInfo
type Exp' = Exp SrcSpanInfo

hsAnalize :: FilePath -> IO ()
hsAnalize file = parseFile file >>= \case
    ParseOk mod -> case mod of
        Module _ _ _ _ decls -> do
            -- mapM_ (\x -> putText $ show (prettyPrint x) <> "\n\n") decls
            mapM_ (\x -> putText $ show (const () <$> x) <> "\n\n") decls
            forM_ decls $ \dc -> case detectDataLikeNewtype dc of
                Just (old, nw)    -> putText $ "Old: " <> show (prettyPrint old) <> "\nNew: " <> show (prettyPrint nw) <> "\n\n"
                Nothing -> pure ()
            -- mapM_ (putText . (<> "\n\n") . show . isJust . detectDataLikeNewtype) decls
            -- mapM_ (putText . (<> "\n\n") . show . prettyPrint) (concatMap detectMapFMapG decls)
        _                  -> putText "Not supported yet"
    ParseFailed srcLoc reason -> putText $ "Parsing failed, reason: " <> show reason


-- Detect "data" which doesn't have bind type variables and contexts,
-- hovewer, with single constructor with single field.
-- If so suggest replacing with "newtype".
detectDataLikeNewtype :: Decl' -> Maybe (Decl', Decl')
detectDataLikeNewtype dt@(DataDecl a (DataType l0) b c qc@[QualConDecl _ Nothing Nothing constr] e)
    | checkOneField constr = Just (dt, DataDecl a (NewType l0) b c qc e)
    | otherwise            = Nothing
  where
    checkOneField :: ConDecl SrcSpanInfo -> Bool
    checkOneField (ConDecl _ _ [_]) = True
    checkOneField (RecDecl _ _ [_]) = True
    checkOneField                 _ = False
detectDataLikeNewtype _ = Nothing

-- Detect map f . map g
detectMapFMapG :: Decl' -> [Exp']
detectMapFMapG = everything (++) ([] `mkQ` mapFMapG)
  where
    mapFMapG :: Exp' -> [Exp']
    mapFMapG e@(InfixApp _ e1 (QVarOp _ (UnQual _ (Symbol _ "."))) e2) -- detecting "map f . map g"
        | mapApp (skipParens e1) && mapApp (skipParens e2) = [e]
    mapFMapG e@(App _ e1 e2)                                           -- detecting "map f (map g xs)"
        | mapApp (skipParens e1) && mapApp2Args (skipParens e2) = [e]
    mapFMapG e@(InfixApp _ e1 (QVarOp _ (UnQual _ (Symbol _ "$"))) e2) -- detecting "map f $ map g xs"
        | mapApp (skipParens e1) && mapApp2Args (skipParens e2) = [e]
    mapFMapG _ = []

    mapApp :: Exp' -> Bool
    mapApp (App _ e1 _) = mapOrfmap (skipParens e1)
    mapApp _ = False

    mapApp2Args :: Exp' -> Bool
    mapApp2Args (App _ e1 _) = mapApp (skipParens e1)
    mapApp2Args _ = False

    mapOrfmap :: Exp' -> Bool
    mapOrfmap (Var _ (UnQual _ (Ident _ "map"))) = True
    mapOrfmap (Var _ (UnQual _ (Ident _ "fmap"))) = True
    mapOrfmap _ = False

    skipParens :: Exp' -> Exp'
    skipParens (Paren _ e) = skipParens e
    skipParens e = e
