{-# OPTIONS_GHC -Wno-missing-signatures #-}
import Data.Char ()
import Data.Function ( on, fix )
import Data.Time
    ( formatTime,
      defaultTimeLocale,
      parseTimeM,
      getZonedTime,
      Day,
      LocalTime(LocalTime),
      TimeOfDay,
      ZonedTime(ZonedTime) )
-- Why do I have to do this extra import :(
import Data.Time.Calendar.OrdinalDate ( toOrdinalDate, mondayStartWeek )
import Data.Monoid ( First (..) )
import Text.Read ( readMaybe )
import System.Environment ( getArgs )
import Data.Functor ( ($>) )
import Prelude hiding (last)

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
            (getFirst ((mappend `on` First) a b))
            (getFirst ((mappend `on` First) m n))
            (getFirst ((mappend `on` First) x y))

instance Monoid St where
    mempty = St Nothing Nothing Nothing

full :: St -> Maybe St
full st@(St w d t) = w *> d *> t $> st

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

accumSt :: String -> St
accumSt = fix go mempty . reverse . lines where
    go _ acc [] = acc
    go f acc (x:xs) =
        let st = acc <> readLineSt x in
        case full st of
            Just _ -> st
            Nothing -> f st xs

todaySt :: IO St
todaySt = do
    ZonedTime (LocalTime day time) _ <- getZonedTime
    pure $ St Nothing (Just day) (Just time)

-- Week is Nothing if it doesn't need to be changed.
-- It doesn't need to be changed when todayYear = lastYear and todayWeekNum == lastWeekNum.
-- If todayWeekNum /= lastWeekNum, that can be for two reasons.
-- 2. lastWeekNum is Just, but different. Use (stweek last + 1).
-- 1. lastWeekNum is Nothing. Use 1.
nextWk :: St -> St -> Maybe Int
nextWk today last
    | todayWeekNum == lastWeekNum && todayYear == lastYear = Nothing
    | Just _ <- lastWeekNum = fmap (+ 1) (stweek last)
    | otherwise = Just 1
    where
        todayWeekNum = fst . mondayStartWeek <$> stday today
        lastWeekNum = fst . mondayStartWeek <$> stday last
        todayYear = fst . toOrdinalDate <$> stday today
        lastYear = fst . toOrdinalDate <$> stday last

-- Day is Nothing if it doesn't need to be changed.
-- It doesn't need to be changed when stday today == stday last
-- Otherwise it should be stday today.
nextDay :: St -> St -> Maybe Day
nextDay today last
    | stday today == stday last = Nothing
    | otherwise = stday today


nextSt :: St -> St -> St
nextSt today last =
    let w = nextWk today last
        d = nextDay today last
    in St w d (sttime today)

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

renderTime Nothing = []
renderTime (Just t) = ["", formatTime defaultTimeLocale "%H:%M:" t, "       " ]

renderSt (St w d t) =
    unlines
    $ concat
    [ renderWk w
    , renderDay d
    , renderTime t
    ]

main = do
    today <- todaySt
    args <- getArgs
    contents <- case args of
        [f] -> readFile f
        _ -> getContents
    let last = accumSt contents
        next = nextSt today last
    putStr (renderSt next)
