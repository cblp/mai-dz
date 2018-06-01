module Data.STRef.Extra where

import           Control.Monad.ST (ST)
import           Data.STRef (STRef, modifySTRef, newSTRef, readSTRef,
                             writeSTRef)

new :: a -> ST s (STRef s a)
new = newSTRef

read :: STRef s a -> ST s a
read = readSTRef

($=) :: STRef s a -> a -> ST s ()
($=) = writeSTRef

($~) :: STRef s a -> (a -> a) -> ST s ()
($~) = modifySTRef
infix 1 $~
