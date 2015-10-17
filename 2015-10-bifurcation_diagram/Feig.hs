module Feig where

foo x r = r * x * (1 - x)

lazy_list x r = x : lazy_list (foo x r) r

feig rs = map (\ r -> lazy_list 0.5 r ) rs


