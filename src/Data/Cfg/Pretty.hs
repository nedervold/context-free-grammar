-- | Pretty-printing
{-# LANGUAGE FlexibleContexts #-} -- for Pretty (V t nt) constraints
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-} -- because constraints aren't smaller
module Data.Cfg.Pretty (
    Pretty(..)
    ) where

import Data.Cfg.Cfg
import Data.Cfg.FreeCfg(FreeCfg)
import Data.Cfg.Item
import qualified Data.Set as S
import Text.PrettyPrint as P

-- | Objects that can be pretty-printed
class Pretty p where
    pretty :: p -> Doc  -- ^ pretty-print the object
    prettyList :: [p] -> Doc -- ^ pretty-print a list of the objects
    prettyList = hsep . map pretty

instance Pretty (V t nt) => Pretty (Production t nt) where
    prettyList = vcat . map pretty
    pretty (Production hd tl :: Production t nt)
        = hsep [pretty (NT hd :: V t nt),
                text "::=",
                prettyList tl P.<> text "."]

instance (Pretty (V t nt)) => Pretty (FreeCfg t nt) where
    prettyList = vcat . map pretty      -- I'd rather have vsep...
    pretty (cfg :: FreeCfg t nt) = vcat [ss, ts, nts, prods]
        where
        ss = text "Start symbol:" <+> pretty (NT (startSymbol cfg) :: V t nt)

        ts = text "Terminals:" <+> commaFSep (map (pretty . (T :: t -> V t nt))
                                                  (S.toList $ terminals cfg))
        nts = text "Nonterminals:"
                  <+> commaFSep (map (pretty . (NT :: nt -> V t nt))
                                     (S.toList $ nonterminals cfg))
        commaFSep :: [Doc] -> Doc
        commaFSep = fsep . punctuate comma

        prods = text "Productions:" $$ nest 4 (vcat prods')
            where
            prods' :: [Doc]
            prods' = zipWith f [1..] (productions cfg)
                where
                f :: Int -> Production t nt -> Doc
                f n p = parens (int n) <+> pretty p

instance Pretty (V t nt) => Pretty (Item t nt) where
    prettyList items = brackets $ hsep $ punctuate comma (map pretty items)
    pretty (item :: Item t nt)
        = hsep [pretty (NT hd :: V t nt), text "::=", hsep rhs'] P.<> char '.'
        where
        rhs' = map pretty (beforeMark item)
                       ++ char markChar
                       : map pretty (afterMark item)

        hd = productionHead $ production item
        markChar = '\x2022'
