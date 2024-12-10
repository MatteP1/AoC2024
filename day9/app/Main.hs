module Main where

import Data.Map (Map, filter, toAscList, toDescList, insert, fromList, toList)
import Data.List
import Data.List.Index
import Control.Monad (guard)

data FileId = FileId Int | Empty
  deriving (Eq, Show)

type Loc = Int

type MemMap = Map Loc FileId

type DiskMap = [Int]

data DiskMapIdEntry = FileIdSize Int Int | EmptySize Int
  deriving (Eq, Show)

type DiskMapId = [DiskMapIdEntry]

enrichDiskMapWithId :: DiskMap -> DiskMapId
enrichDiskMapWithId dm = map (\(index, size) -> if index `mod` 2 == 0 then FileIdSize (index `div` 2) size else EmptySize size) $ indexed dm

expand :: DiskMapIdEntry -> [FileId]
expand dme = case dme of
  FileIdSize fileId fileSize -> replicate fileSize (FileId fileId)
  EmptySize emptySize -> replicate emptySize Empty

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

replace :: Eq a => a -> a -> [a] -> [a]
replace old new ls = map (\e -> if old == e then new else e) ls

insertIntoFirstEmpty :: DiskMapIdEntry -> DiskMapId -> DiskMapId
insertIntoFirstEmpty (EmptySize _) dmi = dmi
insertIntoFirstEmpty fis@(FileIdSize fid fsize) dmi =
  -- dmiLeftRev is already considered entries listed in reverse (for performance reasons)
  let inner _ [] = -- Should be an impossible case
          dmi
      inner dmiLeftRev (dmie@(FileIdSize fid' _):dmiRight) = 
          if fid == fid' then dmi
                         else inner (dmie:dmiLeftRev) dmiRight
      inner dmiLeftRev (dmie@(EmptySize es):dmiRight) = 
          if es < fsize then inner (dmie:dmiLeftRev) dmiRight
                        else (reverse dmiLeftRev) ++ (fis:(EmptySize (es - fsize)):(replace fis (EmptySize fsize) dmiRight))
    in
      inner [] dmi

compactDiskMapIdNoFragmentation :: DiskMapId -> DiskMapId
compactDiskMapIdNoFragmentation dmi =
  foldr (\dmie dmi' -> insertIntoFirstEmpty dmie dmi') dmi dmi

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

      fileCompactedDiskMap = compactDiskMapIdNoFragmentation diskMapWithId
      fileCompactedMemMap = diskMapIdToMemMap fileCompactedDiskMap

  print $ computeCheckSum compactedMemMap -- part 1: takes around 30 sec
  print $ computeCheckSum fileCompactedMemMap -- part 2: takes around 5 sec
