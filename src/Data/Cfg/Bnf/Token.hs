-- | Bnf tokens
module Data.Cfg.Bnf.Token(
    Token(..),
    TokenType(..)
    ) where

-- | Token types
data TokenType = ERROR
    | FULL_STOP			-- ^ @.@
    | LOWER_IDENTIFIER

	-- ^ an identifier made of lower-case characters possibly
	-- separated by underscores

    | OR			-- ^ @|@
    | UPPER_IDENTIFIER

	-- ^ an identifier made of upper-case characters possibly
	-- separated by underscores

    | YIELDS			-- ^ @::=@
    deriving (Eq, Show)

-- | Basic Bnf tokens.	They do not contain location information.
data Token = Token TokenType String
  deriving (Eq, Show)
