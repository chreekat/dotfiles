import Control.Monad
import Data.Function
import Data.Time
-- Why do I have to do this extra import :(
import Data.Time.Calendar.OrdinalDate
import Data.Monoid
import System.IO
import Text.Read

import Debug.Trace

-- Our state is UTCTime, except with potential nulls. Oh, and did we print the week number
-- yet?
data St = St
    { stweek :: Maybe Int
    , stday :: Maybe Day
    , sttime :: Maybe TimeOfDay
    } deriving (Eq, Show)

instance Semigroup St where
    St a m x <> St b n y =
        St
            (getLast ((mappend `on` Last) a b))
            (getLast ((mappend `on` Last) m n))
            (getLast ((mappend `on` Last) x y))

instance Monoid St where
    mempty = St Nothing Nothing Nothing

pDay :: String -> Maybe Day
pDay = parseTimeM False defaultTimeLocale "%Y-%m-%d" . take 10

pTime :: String -> Maybe TimeOfDay
pTime = parseTimeM False defaultTimeLocale "%H:%M:" . take 6

pWeek :: String -> Maybe Int
pWeek s = case words s of
    ("#":"Week":ss:_) -> readMaybe ss
    _ -> Nothing

readLineSt :: String -> St
-- TODO: Short-circuit
readLineSt s = St (pWeek s) (pDay s) (pTime s)

readFileSt :: String -> St
readFileSt = foldMap readLineSt . lines

printAccumSt :: String -> IO St
printAccumSt s = readLineSt s <$ putStrLn s

zeroSt :: IO St
zeroSt = do
    ZonedTime (LocalTime day time) _ <- getZonedTime
    pure $ St (Just 1) (Just day) (Just time)

-- Week is Nothing if it doesn't need to be changed.
-- It doesn't need to be changed when zeroWeekNum == lastWeekNum.
-- If zeroWeekNum /= lastWeekNum, that can be for two reasons.
-- 2. lastWeekNum is Just, but different. Use (stweek last + 1).
-- 1. lastWeekNum is Nothing. Use (stweek zero).
nextWk :: St -> St -> Maybe Int
nextWk zero last
    | zeroWeekNum == lastWeekNum = Nothing
    | Just l <- lastWeekNum = fmap (+ 1) (stweek last)
    | otherwise = stweek zero
    where
        zeroWeekNum = fst . mondayStartWeek <$> stday zero
        lastWeekNum = fst . mondayStartWeek <$> stday last

-- Day is Nothing if it doesn't need to be changed.
-- It doesn't need to be changed when stday zero == stday last
-- Otherwise it should be stday zero.
nextDay :: St -> St -> Maybe Day
nextDay zero last
    | stday zero == stday last = Nothing
    | otherwise = stday zero


nextSt :: St -> St -> St
nextSt zero last =
    let w = nextWk zero last
        d = nextDay zero last
    in St w d (sttime zero)

renderWk Nothing = []
renderWk (Just w) =
    let s = "# Week " <> show w
        border = map (const '#') s
    in ["", border, s, border]

renderDay Nothing = []
renderDay (Just d) =
    let s = show d
        border = map (const '-') s
    in ["", s, border]

renderTime (Just t) = ["", formatTime defaultTimeLocale "%H:%M:" t, "       " ]

renderSt (St w d t) =
    concat
    [ renderWk w
    , renderDay d
    , renderTime t
    ]

main = do
    zero <- zeroSt
    last <- foldMap printAccumSt . lines =<< hGetContents stdin
    putStrLn . unlines . renderSt $ nextSt zero last
