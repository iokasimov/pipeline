# Lightweight, continuation-based and minimal streaming library

Implementation of streaming from ["Faster coroutine pipelines"](https://dl.acm.org/citation.cfm?doid=3136534.3110249) paper.

## Example: copy file contents line by line

```haskell
source :: Pipeline i String IO () ()
source = impact (lines <$> readFile "text_src.txt") >>= sequence_ . fmap yield

target :: Pipeline String o IO () ()
target = forever $ await >>= effect . appendFile "text_tgt.txt" . flip (<>) "\n"

main = pipeline (source =*= target)
```
