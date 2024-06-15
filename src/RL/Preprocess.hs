module RL.Preprocess (preprocessProgram) where

import Data.Maybe (fromJust)
import RL.AST
import RL.Valueable
import RL.Values

{- Preprocessing of RL programs so that they can be
 - interpreted by a self-interpreter. -}

-- | A preprocessed program is an RL value
type Preprocessed = Value

-- | Preprocessing a program always has the entry block first.
-- All block labels are converted to indices.
--
-- TODO: preprocess expressions,
-- and figure out what to do with variable declaratioon
preprocessProgram :: Program String store -> Maybe Preprocessed
preprocessProgram (_, blocks) = do
  [entryBlock] <- pure $ filter isEntry blocks
  let nonEntryBlocks = filter (not . isEntry) blocks
  let blocks' = entryBlock : nonEntryBlocks
  let labels = label <$> blocks'
  let blockAssoc = Num . fromJust . flip lookup (zip labels [0 ..])
  return $ asValue $ mapLabel blockAssoc blocks'
