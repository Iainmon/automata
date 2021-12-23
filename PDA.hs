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

type StackEdit a = Stack a -> Stack a

data Machine = M { q0 :: State, qfs :: Set State, delta :: TransitionFunction }

-- trans :: Transition -> Maybe Char -> PDAState -> Maybe [PDAState]
-- trans (q,(Just c,pop,push),q') Nothing state = Nothing
-- trans tr@(q,(Just c,pop,push),q') (Just c') state = transToTransFunction tr (Just c') state
-- trans tr@(q,(Nothing,pop,push),q') (Just c') state@(q1,stk) = Just [trans q 'q pop push stk, transToTransFunction tr (Just c') state]
-- trans (q,(Nothing,pop,push),q') Nothing (_,stk) = trans' q q' pop push stk

-- loopEpsilon pop push q0 q' (q,stk) | q0 == q = loopEpsilon' pop push q' (q,stk)
--                                    | otherwise          = (q,stk) : []
--   where loopEpsilon' (Just c) push _ (q,(c':cs)) = (q,[]) : []
--         loopEpsilon' (Just c) push q' (q,(c':cs)) = (q,(c':cs)) : if c' == cs then loopEpsilon' (Just c) push q0 q' (q',cs)
--         loopEpsilon Nothing push q' (q,stk) = (q,stk) : loopEpsilon

-- trans' q q' Nothing push stk = Just (q',push ++ stk) >>= loopEpsilon Nothing push q q' (q',push ++ stk)          
-- trans' q q' (Just c) push stk = case stk
--                                 of {(c':cs) -> if c' == c
--                                               then trans' q q' Nothing push cs >>= loopEpsilon (Just c) push q q'
--                                               else Nothing;
--                                    [] -> Nothing}

transGraphFromList :: [Transition] -> TransitionGraph
transGraphFromList trs = Map.fromListWith Set.union [(q,Set.singleton (q,tr,q')) | (q,tr,q') <- trs]

applyTransitionRule :: TransitionRule -> Maybe Char -> Maybe (Stack Char -> Maybe (Stack Char))
applyTransitionRule (Nothing,pop,push) _  = Just $ applyTransitionRule' pop push
applyTransitionRule (Just _,_,_) Nothing  = Nothing
applyTransitionRule (char,pop,push) char' = if char == char'
                                            then Just $ applyTransitionRule' pop push
                                            else Nothing
                        
applyTransitionRule' Nothing pu stack  = Just (pu ++ stack)
applyTransitionRule' (Just c) pu stack = case stack of
                                          (c':cs) -> if c == c' then Just (pu ++ cs) else Nothing
                                          []   -> Nothing

transToTransFunction :: Transition -> Maybe Char -> PDAState -> Maybe PDAState
transToTransFunction (q,tr,q') char (_,stack) = applyTransitionRule tr char >>= flip ($) stack >>= return . pair q'

possibleTransitions :: TransitionGraph -> State -> [Transition]
possibleTransitions g q = Set.toList $ Map.findWithDefault Set.empty q g

possibleTransFunctions g q = map transToTransFunction $ possibleTransitions g q

transGraphToDeltaTrans :: TransitionGraph -> TransitionFunction
transGraphToDeltaTrans g c (q,stk) = cleanList $ foldl (++) [] $ map appRule $ Set.toList $ Map.findWithDefault Set.empty q g
    where appRule tr =  [transToTransFunction tr c (q,stk)] -- : (tailWithDefault 3  $ take 2 $ drop $ map Just $ epsilonLoopTrans tr c (q,stk))
          epsilonLoopTrans tr@(q,trr,q') (Just c) state@(q1,stk) = if q' == q1
                                                              then epsilonLoopTrans' tr (Just c) state
                                                              else []
          epsilonLoopTrans _ _ _ = []
          epsilonLoopTrans' tr@(q,(Nothing,Nothing,push),q') char state@(q1,stk) = let state' = (q',push ++ stk) in state : epsilonLoopTrans tr char state'
        -- epsilonLoopTrans' tr@(q,(Nothing,Just c,push),q') char state@(q1,stk)   = state : case stk of { (c':cs) -> if c == c' then (let state' = (q',push ++ cs) in epsilonLoopTrans tr char state') else [];
        --                                                                                                 []      -> [] } 
          epsilonLoopTrans' _ _ _ = []
          tailWithDefault 0 (_:xs) = xs 
          tailWithDefault n (_:xs) = tailWithDefault (n-1) xs

-- take :: Int -> [a] -> [a]
-- take n xs     = if n >= 1 
--                 then take n xs : take (n-1) xs
--                 else []
-- take n (x:xs) = x : take (n-1) xs 


deltaOf = transGraphToDeltaTrans
-- transGraphToDeltaTrans g c (q,stk) = cleanList $ posTrans
--   where posTrans' = possibleTransFunctions g q
--         posTrans  = map (\tr -> tr c (q,stk)) posTrans'  -- [tr c' | tr <- posTrans', c' <- [c,Nothing]]
-- deltaOf = transGraphToDeltaTrans

-- delta' :: TransitionFunction -> Maybe Char -> PDAState -> FlatTree PDAState
-- delta' delta c (q,stk) = FTr . pair (q,stk) $ delta c (q,stk)

-- compose :: [a -> a] -> a -> a
-- compose fs v = foldl (flip (.)) id fs $ v

deltaStar :: TransitionFunction -> String -> PDAState -> Tree PDAState
deltaStar delta [] (q,[]) = Tr ((q,[]),[])
deltaStar delta (x:xs) (q,[]) = Tr ((Q (-1),[]),[])
deltaStar delta [] s      = Tr (s,map (\q -> Tr (q,[])) $ delta Nothing s)
deltaStar delta (c:cs) s  = Tr (s,map (deltaStar delta cs) $ delta (Just c) s ++ delta Nothing s)

nodeLabel :: Tree PDAState -> PDAState
nodeLabel (Tr (label,_)) = label
nodeChildren :: Tree PDAState -> [Tree PDAState]
nodeChildren (Tr (_,childs)) = childs
computationPaths :: Tree PDAState -> [[PDAState]]
computationPaths (Tr (label,[])) = [[label]]
computationPaths (Tr (label,childs)) = map ((:) label) $ foldl (++) [] $ map computationPaths childs

paths :: [a] -> Tree a -> [[a]]
paths hist (Tr (a,[])) = [a:hist]
paths hist (Tr (a,as)) = let hist' = (a:hist) in foldl (++) [] $ map (paths hist') as

traversals = computationPaths
traversals' = paths []

acceptingTraversals :: Machine -> Tree PDAState -> [[PDAState]]
acceptingTraversals M{qfs=finals} = filter (flip Set.member finals . fst . head . reverse) . computationPaths

finalStates :: Tree PDAState -> Set State
finalStates = Set.fromList . map (fst . head) . traversals'

computeMachineOnInput :: Machine -> String -> Tree PDAState
computeMachineOnInput M{q0=q0,delta=delta} str = deltaStar delta str (q0,"#") 

findDerivation :: Machine -> String -> [[PDAState]]
findDerivation m s = filter ((==) (Q(2)) . fst . head) $ traversals' $ computeMachineOnInput m s

runPDA :: Machine -> String -> [(String,Bool,[PDAState])]
runPDA m s = map (\output -> (s,not . null $ output,output)) $ findDerivation m s

composableDelta :: TransitionFunction -> Maybe Char -> Set PDAState -> Set PDAState
composableDelta d c = foldl Set.union Set.empty . Set.map (Set.fromList . d c)

delta' :: TransitionFunction -> Maybe Char -> Set PDAState -> Set PDAState
-- delta' d c s = let (del,del') = (composableDelta d,runEpsilonTransitions d) 
--                 in let states' = del c $ Set.union (del' s) s
--                     -- in Set.union (del Nothing states') states' 
--                     in states' 
delta' d c s = let (del,del') = (composableDelta d,runEpsilonTransitions d)
                in case c of
                    Nothing  -> Set.union (del' s) s
                    (Just c) -> del (Just c) $ delta' d Nothing s
                    
          
deltaStar' :: TransitionFunction -> String -> Set PDAState -> Set PDAState
-- deltaStar' d ""  s = runEpsilonTransitions d s
-- deltaStar' d str s = foldl (flip ($)) s $ map (del . Just) str
--   where del = delta' d
--         -- go (c:cs) 
deltaStar' d str s = go s str -- foldl (flip ($)) s $ map (del . Just) str
  where del = delta' d
        go s' []     = runEpsilonTransitions d s' -- del Nothing s' -- s'
        go s' (c:cs) = go (del (Just c) s') cs

-- deltaStar'' d (a:x) s = let del = deltaStar' (d a)



runEpsilonTransitions :: TransitionFunction -> Set PDAState -> Set PDAState
runEpsilonTransitions d s = let (h,hs) = runEpsilonTransitions' d s
                              in (squishSetList (h:hs)) <&> s
                              -- in Set.union (foldl Set.union Set.empty hs) s

-- runEpsilonTransitions d s = fst $ runEpsilonTransitions' d s

runEpsilonTransitions' :: TransitionFunction -> Set PDAState -> (Set PDAState,[Set PDAState])
runEpsilonTransitions' d s = go (s,[]) -- go (del s,[])
  where del = composableDelta d Nothing
        go (prev,prevs) = let newS = del prev 
                            in if newS == Set.empty
                               then (prev,reverse prevs)
                               else if newS == prev 
                                          then (newS,reverse $ prev : prevs)
                                          else go (newS,prev : prevs)

