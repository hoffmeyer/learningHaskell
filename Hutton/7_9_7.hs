import           Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin ( n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

make9 :: [Bit] -> [Bit]
make9 bits = parity bits : make8 bits

parity :: [Bit] -> Bit
parity bits = if even $ numOnes bits then 0 else 1

numOnes :: [Bit] -> Int
numOnes bits = length $ filter (==1) bits

encode :: String -> [Bit]
encode = concatMap (make9 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 []   = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . validate . chop9

validate :: [[Bit]] -> [[Bit]]
validate = map validateOne

validateOne :: [Bit] -> [Bit]
validateOne (x:xs) =  if parity xs == x then xs else error "Validation failed"

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

faultyChannel :: [Bit] -> [Bit]
faultyChannel = tail
