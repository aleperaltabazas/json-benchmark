module Data.Time.Extra
  ( epoch
  , diff
  , POSIXTime
  )
where

import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)

epoch = realToFrac <$> getPOSIXTime

diff :: IO a -> IO (a, POSIXTime)
diff eff = do
  start <- epoch
  res   <- eff
  end   <- epoch
  return (res, end - start)
