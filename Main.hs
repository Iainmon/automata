module Main where 
  
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (inits,intercalate)
import PDA
import CFG


attach' v f = map (v:) $ f v
attach'' f vs = (map (foldl (++) []) $ map (flip attach' f) vs)
attach f vs = (foldr (++) []) $ map (attach'' f) vs



gram = constructGrammar [('S',"aSa"),('S',"bSb"),('S',"a"),('S',"b"),('S',"c")]
mach = cfgToPda gram

main = do
  putStrLn . show $ computeMachineOnInput mach "aaa"

-- cfgGram = constructGrammar g
-- transTable = cfgToTransitionGraph cfgGram


-- machine = cfgToPda cfgGram


-- g = [('S',"aSa")
--     ,('S',"bSb")
--     ,('S',"fSf")
-- --    ,('S',"Sf")
--     ,('S',"a")
--     ,('S',"b")
--     ,('S',"c")]
    
-- cfgGram :: CFG
-- cfgGram = constructGrammar g
-- transTable :: TransitionGraph
-- transTable = cfgToTransitionGraph cfgGram

-- machine :: Machine
-- machine = cfgToPda cfgGram




palinGram = [('S',"aSa")
            ,('S',"bSb")
            ,('S',"a")
            ,('S',"b")]

cfgGram :: CFG
cfgGram = constructGrammar palinGram

transTable :: TransitionGraph
transTable = cfgToTransitionGraph cfgGram

machine :: Machine
machine = cfgToPda cfgGram

