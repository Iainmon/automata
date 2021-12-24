module Main where 
  
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (inits,intercalate)
import PDA
import CFG
import qualified PDATests

attach' v f = map (v:) $ f v
attach'' f vs = (map (foldl (++) []) $ map (flip attach' f) vs)
attach f vs = (foldr (++) []) $ map (attach'' f) vs



gram = constructGrammar [('S',"aSa"),('S',"bSb"),('S',"c")]
mach = cfgToPda gram

transTable :: TransitionGraph
transTable = cfgToTransitionGraph gram

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




d = delta mach 
puts :: Show a => [a] -> IO ()
puts = mapM_ (putStrLn . show)
initS (n,stk) = Set.singleton (Q n,stk)

--puts $ deltaStar' d "" $ init (0,"#")


d' c = delta' d (Just c)  -- d' 'a'
rawDel c = composableDelta d c
del' = delta' d Nothing
ds' = deltaStar' d
--puts $ compose' (map d' "babbab") (init (0,"#"))


-- puts $ deltaStar' d "" (initS (0,"#"))

