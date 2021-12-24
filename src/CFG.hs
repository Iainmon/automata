module CFG where 
import PDA

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (inits,intercalate)
import Data.Maybe 

data ProdSymType
  = Term Char    -- Must be lowercase
  | NonTerm Char -- Must be capital
  deriving (Eq,Ord,Show)

type ProdRule = (Char,[ProdSymType]) -- First char must be capital

type CFG = [ProdRule]

charify :: ProdSymType -> Char
charify (Term c)    = c
charify (NonTerm c) = c

tuplize :: ProdRule -> (Char,[Char])
tuplize (name,deriv) = (name, map charify deriv)
untuplize :: (Char,[Char]) -> ProdRule
untuplize (l,rhs) = (l,map conSymType rhs)
  where conSymType c | 'A' <= c && c <= 'Z' = NonTerm c
                     | 'a' <= c && c <= 'z' = Term c 

constructGrammar :: [(Char,String)] -> CFG
constructGrammar = map untuplize

initialTransition :: State -> Transition
finalTransition :: State -> Transition
initialTransition s = ((Q 0),(Nothing,Just '#',"S#"),s)
finalTransition s   = (s,(Nothing,Just '#',""),(Q (2)))

prodRuleToTransRule :: ProdRule -> TransitionRule
prodRuleToTransRule  prule = (Nothing,Just name,deriv)
  where (name,deriv) = tuplize prule

consumeTransitions = map (\c -> (Just c,Just c,[])) "abc" -- ['a'..'z']
loopTransRule q transRule = (q,transRule,q)

prodRuleToTransition :: State -> State -> ProdRule -> Transition
prodRuleToTransition q q' prule = (q,prodRuleToTransRule prule,q')

cfgToPda :: CFG -> Machine
cfgToPda rules = M { q0 = start', qfs = Set.singleton final', delta = transGraphToDeltaTrans transitionTable }
  where start' = Q 0
        final' = Q 2
        transitionTable = cfgToTransitionGraph rules
cfgToTransitionGraph :: CFG -> TransitionGraph
cfgToTransitionGraph rules = transGraphFromList (defaultTransitions ++ grammarTransitions)
  where middle = Q 1
        defaultTransitions = initialTransition middle : finalTransition middle : map (loopTransRule middle) consumeTransitions
        grammarTransitions = map (loopTransRule middle . prodRuleToTransRule) rules