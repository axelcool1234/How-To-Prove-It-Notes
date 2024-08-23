module Tuples where
import Prelude hiding (fst, snd)

fst (x, _, _) = x
snd (_, x, _) = x
thrd (_, _, x) = x
