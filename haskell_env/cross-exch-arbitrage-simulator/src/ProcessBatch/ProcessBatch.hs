module ProcessBatch.ProcessBatch where

import           Control.Parallel.Strategies (parMap, rdeepseq)
import           Data.List.Split           (chunksOf)

import           Data_parse.Snapshot (Snapshot(..), Tick(..))
import           Data.Text       (Text)

import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map

import Data.List.Split (chunksOf)

{--
Paralell processing of the batch to get all the arbitrage opportunities similarly follows the MapReduce paradigm

Map: break batch into chunks and those chunks further iterate on each sub-chunk, then from that atomic unit, consolidate all of them
through merging of each atomic chunk's best map
--}

-- map a symbol to a tuple of two pairs, the lowest buy price @ buy_exch and highest sell price @ sell_exch 
-- for one exchange snapshot in a batch chunk, get its BestMap

-- The first pair in the tuple is for buy side, the second pair is for sell side
-- This can be thought of as an init step, the merge will later take care of these duplicate/placeholder values
getSnapshotBestMap :: (Text,Snapshot) -> Map Text ((Double,Text),(Double,Text))
getSnapshotBestMap (exch, snap) = 
    let ochl_map = ochl snap
    in Map.map (\t -> ((close t,exch),(close t,exch))) ochl_map

-- merge two Tuple pairs
mergeBest :: ((Double,Text),(Double,Text)) -> ((Double,Text),(Double,Text)) -> ((Double,Text),(Double,Text))
mergeBest ((bp1,bx1),(sp1,sx1)) ((bp2,bx2),(sp2,sx2)) =
  (if bp2 < bp1 then (bp2,bx2) else (bp1,bx1) , if sp1 < sp2 then (sp2,sx2) else (sp1,sx1))

-- For a chunk, find each BestMap for each element in the list and merge all BestMaps in the list
processChunk :: [(Text, Snapshot)] -> Map Text ((Double,Text), (Double,Text))
processChunk batchChunk =
    -- for each snapshot build its init mini‐map, then union them all
    let exchMaps = map getSnapshotBestMap batchChunk
    in  Map.unionsWith mergeBest exchMaps

parallelBestMap :: Int -> [(Text, Snapshot)] -> Map Text ((Double, Text), (Double, Text))
parallelBestMap numCores batch =
    let n         = length batch
        -- ceil function, chunkSize determines # snapshots per chunk and for division with remainder resultant, you need to round up so snapshots are not discarded
        chunkSize = (n + numCores - 1) `div` numCores 
        chunks    = chunksOf chunkSize batch -- distribute the snapshots to the chunks
        partials  = parMap rdeepseq processChunk chunks -- compute and retrieve the best maps of each chunk in parallel 
    in  Map.unionsWith mergeBest partials -- final merge of each chunks best map to get the singular best map of the batch