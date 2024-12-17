module Main where

import Data.Bits
import Data.List ((!?), intersperse, isPrefixOf)
import Debug.Trace

data Registers = Registers { a :: Int, b :: Int, c :: Int }
  deriving (Show, Eq)

type Program = [Int]

type Output = [Int]

type InstrPointer = Int

data Instr = 
    Adv
  | Bxl
  | Bst
  | Jnz
  | Bxc
  | Out
  | Bdv
  | Cdv
  deriving (Show, Eq)

opcodeToInstr :: Int -> Instr
opcodeToInstr opcode = case opcode of
  0 -> Adv
  1 -> Bxl
  2 -> Bst
  3 -> Jnz
  4 -> Bxc
  5 -> Out
  6 -> Bdv
  7 -> Cdv
  _ -> Adv

combo :: Int -> Registers -> Int
combo operand regs =
  case operand of
      0 -> 0
      1 -> 1
      2 -> 2
      3 -> 3
      4 -> a regs
      5 -> b regs
      6 -> c regs
      7 -> 42
      _ -> 69

-- part 1
evaluate :: Registers -> Program -> Output
evaluate registers prog = 
  let eval :: Registers -> InstrPointer -> Output -> Output
      eval regs@(Registers {a, b, c}) ip accOut =
        let next = do
              opcode <- prog !? ip
              operand <- prog !? (ip + 1)
              return (opcodeToInstr opcode, operand)
        in case next of
          Nothing -> accOut
          Just (opcode, operand) ->
            case opcode of
              Adv ->
                let res = a `div` (2 ^ (combo operand regs)) in
                  eval (Registers {a=res, b=b, c=c}) (ip+2) accOut
              Bxl ->
                let res = b `xor` operand in
                  eval (Registers {a=a, b=res, c=c}) (ip+2) accOut
              Bst ->
                let res = (combo operand regs) `mod` 8 in
                  eval (Registers {a=a, b=res, c=c}) (ip+2) accOut
              Jnz ->
                if a == 0 then
                  eval regs (ip+2) accOut
                else
                  eval regs (operand) accOut
              Bxc ->
                let res = b `xor` c in
                  eval (Registers {a=a, b=res, c=c}) (ip+2) accOut
              Out ->
                let res = (combo operand regs) `mod` 8 in
                eval regs (ip+2) (res:accOut)
              Bdv ->
                let res = a `div` (2 ^ (combo operand regs)) in
                  eval (Registers {a=a, b=res, c=c}) (ip+2) accOut
              Cdv ->
                let res = a `div` (2 ^ (combo operand regs)) in
                  eval (Registers {a=a, b=b, c=res}) (ip+2) accOut
  in
    reverse $ eval registers 0 [] 

-- part 2
evaluateQuine :: Registers -> Program -> Int
evaluateQuine registers prog =
  let eval :: Registers -> InstrPointer -> Output -> Bool
      eval regs@(Registers {a, b, c}) ip accOut =
        if not $ (reverse accOut) `isPrefixOf` prog then
          False
        else
        let next = do
              opcode <- prog !? ip
              operand <- prog !? (ip + 1)
              return (opcodeToInstr opcode, operand)
        in case next of
          Nothing -> (reverse accOut == prog)
          Just (opcode, operand) ->
            case opcode of
              Adv ->
                let res = a `div` (2 ^ (combo operand regs)) in
                  eval (Registers {a=res, b=b, c=c}) (ip+2) accOut
              Bxl ->
                let res = b `xor` operand in
                  eval (Registers {a=a, b=res, c=c}) (ip+2) accOut
              Bst ->
                let res = (combo operand regs) `mod` 8 in
                  eval (Registers {a=a, b=res, c=c}) (ip+2) accOut
              Jnz ->
                if a == 0 then
                  eval regs (ip+2) accOut
                else
                  eval regs (operand) accOut
              Bxc ->
                let res = b `xor` c in
                  eval (Registers {a=a, b=res, c=c}) (ip+2) accOut
              Out ->
                let res = (combo operand regs) `mod` 8 in
                eval regs (ip+2) (res:accOut)
              Bdv ->
                let res = a `div` (2 ^ (combo operand regs)) in
                  eval (Registers {a=a, b=res, c=c}) (ip+2) accOut
              Cdv ->
                let res = a `div` (2 ^ (combo operand regs)) in
                  eval (Registers {a=a, b=b, c=res}) (ip+2) accOut
  in
    if eval registers 0 [] then
      a registers
    else
      evaluateQuine (Registers {a=traceShowId $ a registers + 1, b = b registers, c = c registers}) prog

main :: IO ()
main =
  let registers = Registers {a=22817223, b=0, c=0}
      program = [2,4,1,2,7,5,4,5,0,3,1,7,5,5,3,0] :: Program

      -- test1Reg = Registers {a=729, b=0, c=0}
      -- test1Prog = [0,1,5,4,3,0]
      -- test2Prog = [0,3,5,4,3,0]

      quineA = evaluateQuine (Registers {a=0, b=0, c=0}) program

      -- The Program:
      -- B <- A mod 8
      -- B <- B xor 2
      -- C <- A >> B
      -- B <- B xor C
      -- A <- A >> 3
      -- B <- B xor 7
      -- OUT <- B mod 8
      -- IF A != 0 THEN JMP 0

      -- Notes about the program:
      -- Only one 'Out' instruction: Out B
      -- 'Out' followed by jnz 0, which is the only jump instruction
      -- To create the program, we must do this jump 16 times.
      -- I.e. the program ATOB: [2,4,1,2,7,5,4,5,0,3,1,7] must first put 2 in B
      -- Running it again should put 4 in B. Then 1, 2, 7, and so on.

      -- Program must perform 16 loops. Each iteration has a "state", which
      -- is the values of the registers at the first instruction in the loop.
      -- State 16 must be A=0, B=program[15], C=?, indicating that the program ended last iteration with program[15] being the final Output.

      -- State 16 must be A=0, B=program[15], C=?
      -- State 15 must be A=?, B=program[14], C=?
      -- State 14 must be A=?, B=program[13], C=?
      -- State 13 must be A=?, B=program[12], C=?
      -- State 12 must be A=?, B=program[11], C=?
      -- State 11 must be A=?, B=program[10], C=?
      -- State 10 must be A=?, B=program[9],  C=?
      -- State 9  must be A=?, B=program[8],  C=?
      -- State 8  must be A=?, B=program[7],  C=?
      -- State 7  must be A=?, B=program[6],  C=?
      -- State 6  must be A=?, B=program[5],  C=?
      -- State 5  must be A=?, B=program[4],  C=?
      -- State 4  must be A=?, B=program[3],  C=?
      -- State 3  must be A=?, B=program[2],  C=?
      -- State 2  must be A=?, B=program[1],  C=?
      -- State 1  must be A=?, B=program[0],  C=?
      -- State 0  must be A=?, B=0,           C=0
      
      -- Task: Find algorithm that can take us from state i to i-1.

      -- Answer < 10000000000000000

      output = evaluate registers program
      outputString = map (show) output
      commaSepOutput = concat $ intersperse "," outputString
  in do
    print commaSepOutput
    print quineA
