module Main where

import Data.Map (Map, filter, toAscList, toDescList, insert, fromList, toList)
import Debug.Trace
import Data.List
import Data.List.Index
import Control.Monad (guard)

data FileId = FileId Int | Empty
  deriving (Eq, Show)

type Loc = Int

type MemMap = Map Loc FileId

type DiskMap = [Int]

data DiskMapIdEntry = FileIdSize Int Int | EmptySize Int

type DiskMapId = [DiskMapIdEntry]

expand :: DiskMapIdEntry -> [FileId]
expand dme = case dme of
  FileIdSize fileId fileSize -> replicate fileSize (FileId fileId)
  EmptySize emptySize -> replicate emptySize Empty

enrichDiskMapWithId :: DiskMap -> DiskMapId
enrichDiskMapWithId dm = map (\(index, size) -> if index `mod` 2 == 0 then FileIdSize (index `div` 2) size else EmptySize size) $ indexed dm

diskMapIdToMemMap :: DiskMapId -> MemMap
diskMapIdToMemMap dmi = fromList $ indexed $ concatMap (expand) dmi


getEmptyLocationsAscOrder :: MemMap -> [Loc]
getEmptyLocationsAscOrder mem = map fst $ toAscList $ Data.Map.filter (\fid -> fid == Empty) mem

getFileLocationsDescOrder :: MemMap -> [(Loc, FileId)]
getFileLocationsDescOrder mem = toDescList $ Data.Map.filter (\fid -> fid /= Empty) mem

findLeftMostEmpty :: MemMap -> Maybe Loc
findLeftMostEmpty mem = do
  ((loc, _), _) <- uncons $ toAscList $ Data.Map.filter (\fid -> fid == Empty) mem
  return loc

findRightMostFile :: MemMap -> Maybe (Loc, FileId)
findRightMostFile mem = do
  (fileLoc, _) <- uncons $ toDescList $ Data.Map.filter (\fid -> fid /= Empty) mem
  return fileLoc

moveRightMostBlockToLeftMostEmpty :: MemMap -> Maybe MemMap
moveRightMostBlockToLeftMostEmpty mem =
  do 
    emptyLoc <- findLeftMostEmpty mem
    (oldLoc, fileId) <- findRightMostFile mem
    guard (emptyLoc <= oldLoc)
    let mem1 = Data.Map.insert emptyLoc fileId mem
        mem2 = Data.Map.insert oldLoc Empty mem1
    return mem2

compactMem :: MemMap -> MemMap
compactMem mem = case moveRightMostBlockToLeftMostEmpty mem of
  Nothing -> mem
  Just mem' -> compactMem mem'

computeCheckSum :: MemMap -> Int
computeCheckSum mem = sum $ map
  (\(loc, fileId) -> 
    case fileId of
          FileId fid -> loc * fid
          Empty -> 0)
  $ toList mem

main :: IO ()
main = do
  input :: [Char] <- readFile "input.txt"
  let diskMap = map (read . pure) input :: DiskMap
      diskMapWithId = enrichDiskMapWithId diskMap
      memMap = diskMapIdToMemMap diskMapWithId
      compactedMemMap = compactMem memMap
  print $ computeCheckSum compactedMemMap -- part 1: takes around 30 sec
