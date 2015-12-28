import Data.Map as Map
import Data.Function (on)

data Player = Player { pid :: Int, playerName :: String, points :: Int } deriving (Show, Eq, Ord)
data Match = Match { team1 :: [Int], score1 :: Int, team2 :: [Int], score2 :: Int } deriving (Show)
data EventData = EventData { eventId :: Int, body :: Event }

instance Show (EventData) where
  show = show . eventId

instance Eq (EventData) where
  (==) = (==) `on` eventId

data Event = PlayerCreated Player | MatchPlayed Match

type UnrankedList = [(Int, Player)]
