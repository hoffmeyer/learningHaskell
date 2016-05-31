module Vehicles where

data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PopuAir | CatapultsRUs | TakeYourChanceUnited deriving (Eq, Show)
data PlaneSize = PlaneSize Integer deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline PlaneSize deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PopuAir (PlaneSize 1200)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar 

getManu :: Vehicle -> Manufacturer
getManu (Car x _) = x

data Example = MakeExample deriving Show
data Example2 = MakeExample2 Integer deriving Show
