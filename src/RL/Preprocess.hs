module RL.Preprocess (preprocessProgram) where

import RL.AST
import RL.Valueable
import RL.Values

{- Preprocessing of RL programs so that they can be
 - interpreted by a self-interpreter. -}

-- | A preprocessed program is an RL value
type Preprocessed = Value

-- | Preprocessing a program always has the entry block first.
--
-- TODO: preprocess expressions,
-- and figure out what to do with variable declaratioon
preprocessProgram :: Eq store => Program String store -> Maybe Preprocessed
preprocessProgram (_, blocks) = do
  [entryBlock] <- pure $ filter isEntry blocks
  [exitBlock] <- pure $ filter isExit blocks
  let otherBlocks = filter (\b -> not (isEntry b || isExit b)) blocks
  let blocks' = if entryBlock == exitBlock
                then [entryBlock]
                else [entryBlock] ++ otherBlocks ++ [exitBlock]
  return $ asValue $ mapLabel Atom blocks'
