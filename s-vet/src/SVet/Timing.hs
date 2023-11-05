module SVet.Timing (logSemy) where

import Data.Fixed
import Data.Time hiding (formatTime)
import Data.Time.Clock.POSIX
import Katip
import Relude
import Text.Printf

timeInPico :: MonadIO m => m Integer
timeInPico = do
  MkFixed i <- nominalDiffTimeToSeconds <$> liftIO getPOSIXTime
  pure i

formatTime :: Integer -> String
formatTime t
  | t > pow 12 = printf "%.3fs" ((fromIntegral t * 1e-12) :: Double)
  | t > pow 9 = printf "%.3fms" ((fromIntegral t * 1e-9) :: Double)
  | t > pow 6 = printf "%.3fμs" ((fromIntegral t * 1e-6) :: Double)
  | t > pow 3 = printf "%.3fns" ((fromIntegral t * 1e-3) :: Double)
  | otherwise = printf "%.3fps" t
 where
  pow :: Integer -> Integer
  pow x = 10 ^ x

logSemy :: (KatipContext m) => Text -> m a -> m a
logSemy t s = do
  t1 <- timeInPico
  r <- s
  t2 <- timeInPico
  katipAddContext (sl "duration" (formatTime $ t2 - t1) <> sl "request" t) $
    $logTM DebugS "request completed"
  pure r
