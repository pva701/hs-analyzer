module Lib
    ( hsAnalize
    ) where

import Universum hiding (Type)

import System.Environment
import Language.Haskell.Exts
import Data.Generics.Schemes (everything)
import Language.Haskell.Exts.Extension
import Data.Generics.Aliases (mkQ)
import Data.String.Utils

import Rendering

type Decl' = Decl SrcSpanInfo
type Exp' = Exp SrcSpanInfo

enabledExtensions :: [Extension] -> [Extension]
enabledExtensions [] = []
enabledExtensions (e@(EnableExtension _) : xs) = e : enabledExtensions xs
enabledExtensions (e : xs) = enabledExtensions xs

hsAnalize :: FilePath -> IO ()
hsAnalize file = do
    -- putStrLn $ "file: " ++ file
    lines <- map toString . lines <$> readFile file
    let parseMode = defaultParseMode { fixities = Just [], extensions = enabledExtensions knownExtensions}
    parseFileWithMode parseMode file >>= \case
      ParseOk mod -> case mod of
          Module _ _ _ _ decls -> do
              let newtypeDetections = map ("newtype" :: String, ) $ catMaybes $ map detectDataLikeNewtype decls
              let mapfMapgDetections = map ("map f . map g" :: String, ) $ concatMap detectMapFMapG decls
              forM_ (newtypeDetections ++ mapfMapgDetections) $ \(insp, Detection span nw) -> do
                  renderInspection insp (srcSpanStartLine $ srcInfoSpan span)
                  renderSourceCode 4 lines (srcInfoSpan span)
                  putStrLn ("  may be replaced with" :: String)
                  putStrLn $ highlightCode False $ "    " <> replace "\n" "\n    " nw <> "\n"
          _ -> putText "Not supported yet"
      ParseFailed srcLoc reason -> renderParserFail lines srcLoc reason

data Detection = Detection
    { oldSpan    :: SrcSpanInfo
    , suggestion :: String
    }

-- Detect "data" which doesn't have bind type variables and contexts,
-- hovewer, with single constructor with single field.
-- If so suggest replacing with "newtype": return span of code and suggestion of new code
detectDataLikeNewtype :: Decl' -> Maybe Detection
detectDataLikeNewtype dt@(DataDecl loc (DataType l0) b c qc@[QualConDecl _ Nothing Nothing constr] e)
    | checkOneFieldNoBang constr =
        Just $ Detection loc $ prettyPrint $ DataDecl loc (NewType l0) b c qc e
    | otherwise            = Nothing
  where
    checkOneFieldNoBang :: ConDecl SrcSpanInfo -> Bool
    checkOneFieldNoBang (ConDecl _ _ [fieldType]) = checkNoBang fieldType
    checkOneFieldNoBang (RecDecl _ _ [FieldDecl _ _ fieldType]) = checkNoBang fieldType
    checkOneFieldNoBang                 _ = False

    checkNoBang :: Type SrcSpanInfo -> Bool
    checkNoBang (TyBang _ _ _ _) = False
    checkNoBang _ = True

detectDataLikeNewtype _ = Nothing

-- Detect map f . map g
-- Return span of code and suggestion of new code
detectMapFMapG :: Decl' -> [Detection]
detectMapFMapG = everything (++) ([] `mkQ` mapFMapG)
  where
    mapFMapG :: Exp' -> [Detection]
    mapFMapG e@(InfixApp _ e1 (QVarOp _ (UnQual _ (Symbol _ "."))) e2) -- detecting "map f . map g"
        | Just (op, fe) <- mapApp (skipParens e1),
          Just (_, ge) <- mapApp (skipParens e2) = [Detection (ann e) (constrMap op fe ge Nothing)]
    mapFMapG e@(App _ e1 e2)                                           -- detecting "map f (map g xs)"
        | Just (op, fe) <- mapApp (skipParens e1),
          Just ((_, ge), arg) <- mapApp2Args (skipParens e2) = [Detection (ann e) (constrMap op fe ge (Just arg))]
    mapFMapG e@(InfixApp _ e1 (QVarOp _ (UnQual _ (Symbol _ "$"))) e2) -- detecting "map f $ map g xs"
        | Just (op, fe) <- mapApp (skipParens e1),
          Just ((_, ge), arg) <- mapApp2Args (skipParens e2) = [Detection (ann e) (constrMap op fe ge (Just arg))]
    mapFMapG _ = []

    pointe = QVarOp () (UnQual () (Symbol () "."))
    mape fname = Var () (UnQual () (Ident () fname))

    constrMap :: String -> Exp' -> Exp' -> Maybe Exp' -> String
    constrMap fname fe fg Nothing = prettyPrint $ App ()
       (Var () (UnQual () (Ident () fname)))
       (InfixApp () (addParens fe) pointe  (addParens fg))
    constrMap fname fe fg (Just arg) = prettyPrint $ App ()
        (App ()
          (Var () (UnQual () (Ident () fname)))
          (InfixApp () (addParens fe) pointe (addParens fg)))
        (addParens arg)

    mapApp :: Exp' -> Maybe (String, Exp')
    mapApp (App _ e1 e2) = (,) <$> mapOrfmap (skipParens e1) <*> Just (skipParens e2)
    mapApp (InfixApp _ e1 (QVarOp _ (UnQual _ (Symbol _ "$"))) e2) = (,) <$> mapOrfmap (skipParens e1) <*> Just (skipParens e2)
    mapApp _ = Nothing

    mapApp2Args :: Exp' -> Maybe ((String, Exp'), Exp')
    mapApp2Args (App _ e1 e2) = (,) <$> mapApp (skipParens e1) <*> Just (skipParens e2)
    mapApp2Args (InfixApp _ e1 (QVarOp _ (UnQual _ (Symbol _ "$"))) e2) = (,) <$> mapApp (skipParens e1) <*> Just (skipParens e2)
    mapApp2Args _ = Nothing

    mapOrfmap :: Exp' -> Maybe String
    mapOrfmap (Var _ (UnQual _ (Ident _ "map"))) = Just "map"
    mapOrfmap (Var _ (UnQual _ (Ident _ "fmap"))) = Just "fmap"
    mapOrfmap _ = Nothing

    addParens :: Exp' -> Exp ()
    addParens v@(Var _ _) = void v
    addParens v = Paren () $ void v

    skipParens :: Exp' -> Exp'
    skipParens (Paren _ e) = skipParens e
    skipParens e = e
