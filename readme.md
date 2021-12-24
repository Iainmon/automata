# Haskell PDA


```haskell
gram = constructGrammar [('S',"aSa")
                        ,('S',"bSb")
                        ,('S',"c")]
mach = cfgToPda gram

*Main CFG PDA Utilities> mapM_ (putStrLn . show) $ runPDA mach "abacaba"
(False,"",fromList [(Q 0,"#"),(Q 1,"S#"),(Q 1,"aSa#"),(Q 1,"bSb#"),(Q 1,"c#")])
(False,"a",fromList [(Q 1,"Sa#"),(Q 1,"aSaa#"),(Q 1,"bSba#"),(Q 1,"ca#")])
(False,"ab",fromList [(Q 1,"Sba#"),(Q 1,"aSaba#"),(Q 1,"bSbba#"),(Q 1,"cba#")])
(False,"aba",fromList [(Q 1,"Saba#"),(Q 1,"aSaaba#"),(Q 1,"bSbaba#"),(Q 1,"caba#")])
(False,"abac",fromList [(Q 1,"aba#")])
(False,"abaca",fromList [(Q 1,"ba#")])
(False,"abacab",fromList [(Q 1,"a#")])
(True,"abacaba",fromList [(Q 1,"#"),(Q 2,"")])
```

```
stack repl
```
