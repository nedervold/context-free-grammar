-- | Pretty-printing that requires a context
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Data.Cfg.CPretty
  ( CPretty(..)
  ) where

import Control.Monad.Reader
import Text.PrettyPrint

-- | Pretty-printing that requires a context
class CPretty p ctxt where
  cpretty :: (MonadReader ctxt m) => p -> m Doc
    -- ^ pretty-print in a monad providing the context
