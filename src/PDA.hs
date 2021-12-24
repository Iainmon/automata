module PDA where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (inits,intercalate)
import Data.Maybe 

import Utilities

pair a b = (a,b)
tru True  = const
tru False = flip const
cleanList []            = []
cleanList (Nothing:xs)  = cleanList xs
cleanList ((Just x):xs) = x : cleanList xs

data FlatTree a = FTr (a,[a]) deriving (Eq,Ord,Show)
data Tree a = Tr (a,[Tree a]) deriving (Eq,Ord,Show)

type Stack a = [a]

type Sigma = Maybe Char

data State = Q Int deriving (Eq,Ord,Show)

type PDAState = (State,Stack Char)

-- Encode the data for if the transitioning can happen
type TransitionRule = (Maybe Char,Maybe Char,String)
type Transition = (State,TransitionRule,State)
type TransitionGraph = Map State (Set Transition)

type TransitionFunction = Maybe Char -> PDAState -> [PDAState]

type Σ = Char
type Σ' = Maybe Σ
type Q = State
type Γ = Char
type Γ' = [Γ]

type StackEdit a = Stack a -> Stack a

data Machine = M { q0 :: State, qfs :: Set State, delta :: TransitionFunction }

transGraphFromList :: [Transition] -> TransitionGraph
transGraphFromList trs = Map.fromListWith Set.union [(q,Set.singleton (q,tr,q')) | (q,tr,q') <- trs]

applyTransitionRule :: TransitionRule -> Maybe Char -> Maybe (Stack Char -> Maybe (Stack Char))
applyTransitionRule (Nothing,_,_) (Just _) = Nothing
applyTransitionRule (Nothing,pop,push) _  = Just $ applyTransitionRule' pop push
applyTransitionRule (Just _,_,_) Nothing  = Nothing
applyTransitionRule (Just c,pop,push) (Just c') = if c == c'
                                                  then Just $ applyTransitionRule' pop push
                                                  else Nothing
                        
applyTransitionRule' Nothing pu stack  = Just (pu ++ stack)
applyTransitionRule' (Just c) pu stack = case stack of
                                          (c':cs) -> if c == c' then Just (pu ++ cs) else Nothing
                                          []   -> Nothing

transToTransFunction :: Transition -> Maybe Char -> PDAState -> Maybe PDAState
transToTransFunction (q,tr,q') char (q1,stack) = if q1 == q then applyTransitionRule tr char >>= flip ($) stack >>= return . pair q' else Nothing

possibleTransitions :: TransitionGraph -> State -> [Transition]
possibleTransitions g q = Set.toList $ Map.findWithDefault Set.empty q g

possibleTransFunctions g q = map transToTransFunction $ possibleTransitions g q

transGraphToDeltaTrans :: TransitionGraph -> TransitionFunction
transGraphToDeltaTrans g c (q,stk) = cleanList $ map appRule $ Set.toList $ Map.findWithDefault Set.empty q g
    where appRule tr =  transToTransFunction tr c (q,stk) 

-- deltaOf = transGraphToDeltaTrans

-- deltaStar :: TransitionFunction -> String -> PDAState -> Tree PDAState
-- deltaStar delta [] (q,[]) = Tr ((q,[]),[])
-- deltaStar delta (x:xs) (q,[]) = Tr ((Q (-1),[]),[])
-- deltaStar delta [] s      = Tr (s,map (\q -> Tr (q,[])) $ delta Nothing s)
-- deltaStar delta (c:cs) s  = Tr (s,map (deltaStar delta cs) $ delta (Just c) s ++ delta Nothing s)

-- nodeLabel :: Tree PDAState -> PDAState
-- nodeLabel (Tr (label,_)) = label
-- nodeChildren :: Tree PDAState -> [Tree PDAState]
-- nodeChildren (Tr (_,childs)) = childs
-- computationPaths :: Tree PDAState -> [[PDAState]]
-- computationPaths (Tr (label,[])) = [[label]]
-- computationPaths (Tr (label,childs)) = map ((:) label) $ foldl (++) [] $ map computationPaths childs

-- paths :: [a] -> Tree a -> [[a]]
-- paths hist (Tr (a,[])) = [a:hist]
-- paths hist (Tr (a,as)) = let hist' = (a:hist) in foldl (++) [] $ map (paths hist') as

-- traversals = computationPaths
-- traversals' = paths []

-- acceptingTraversals :: Machine -> Tree PDAState -> [[PDAState]]
-- acceptingTraversals M{qfs=finals} = filter (flip Set.member finals . fst . head . reverse) . computationPaths

-- finalStates :: Tree PDAState -> Set State
-- finalStates = Set.fromList . map (fst . head) . traversals'

computeMachineOnInput :: Machine -> String -> Tree PDAState
computeMachineOnInput = undefined -- M{q0=q0,delta=delta} str = deltaStar delta str (q0,"#") 

-- findDerivation :: Machine -> String -> [[PDAState]]
-- findDerivation m s = filter ((==) (Q(2)) . fst . head) $ traversals' $ computeMachineOnInput m s

-- runPDA :: Machine -> String -> [(String,Bool,[PDAState])]
-- runPDA m s = map (\output -> (s,not . null $ output,output)) $ findDerivation m s

runPDA :: Machine -> String -> [(Bool,String,Set PDAState)]
runPDA m s = map run (inits s)
  where ds = deltaStar' $ delta m
        run s' = let fs = ds s' initialState
                    in ((intersFinSts $ extractStates fs),s',fs)
        intersFinSts sts = Set.isSubsetOf (qfs m) sts
        extractStates = Set.map fst
        initialState = Set.singleton ((q0 m),"#")

composableDelta :: TransitionFunction -> Maybe Char -> Set PDAState -> Set PDAState
composableDelta d c s = squishSetSet $ Set.map (Set.fromList . d c) s

delta' :: TransitionFunction -> Maybe Char -> Set PDAState -> Set PDAState
delta' d c s = let (del,del') = (composableDelta d,runEpsilonTransitions d)
                in case c of
                    Nothing  -> del' s
                    (Just c) -> del (Just c) $ del' s
                    
          
deltaStar' :: TransitionFunction -> String -> Set PDAState -> Set PDAState

deltaStar' d str s = go s str
  where del = composableDelta d
        del' = runEpsilonTransitions d
        go s' []     = del' s'
        go s' (c:cs) = go (delta' d (Just c) s') cs




runEpsilonTransitions :: TransitionFunction -> Set PDAState -> Set PDAState
runEpsilonTransitions d s = fst $ runEpsilonTransitions' d s

runEpsilonTransitions' :: TransitionFunction -> Set PDAState -> (Set PDAState,[Set PDAState])
runEpsilonTransitions' d s = go (s,[]) -- go (del s,[])
  where del = composableDelta d Nothing
        go (prev,prevs) = let new = del prev <&> prev
                          in if new == prev
                             then (new,prev : prevs)
                             else go (new,prev : prevs)
