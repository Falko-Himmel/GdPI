-- GdpI Blatt 4 -----------------------------------------------------------------
-- Hinweis
{-
    Für die Aufgaben auf diesem Blatt sind die Funktionen `concat`, `foldr` und
    `map`, Listenkomprehension, sowie die Funktionskomposition (.) nützliche
    Werkzeuge.
-}

-- Vorgegebene Definitionen -----------------------------------------------------
data Bud
    = Growth Double
    | Flower
    | UnripeFruit Double
    | RipeFruit
    deriving (Show)


data NutrientDemand = NPKDemand Double Double Double
    deriving (Show, Eq)


weeklyBudNutrientDemand :: Bud -> NutrientDemand
weeklyBudNutrientDemand (Growth length)
    | 0 <= length = NPKDemand
        (0.005 * length)
        (0.001 * length)
        (0.001 * length)
    | otherwise = error $
        "Invalid internode length" ++ show length
weeklyBudNutrientDemand Flower = NPKDemand 0.002 0.003 0.003
weeklyBudNutrientDemand (UnripeFruit ripeness)
    | 0 <= ripeness = NPKDemand
        0.002
        (0.003 + 0.004*ripeness)
        (0.003 + 0.003*ripeness)
    | otherwise = error $
        "Invalid ripeness degree: " ++ show ripeness
weeklyBudNutrientDemand RipeFruit = NPKDemand 0.001 0.001 0.001


addNutrientDemand :: NutrientDemand -> NutrientDemand -> NutrientDemand
addNutrientDemand (NPKDemand n1 p1 k1) (NPKDemand n2 p2 k2)
    = NPKDemand (n1+n2) (p1+p2) (k1+k2)


type DailyThroughput = Double


budWaterDemand :: Bud -> DailyThroughput
budWaterDemand (Growth l)      = l * 2/1000
budWaterDemand Flower          = 1/1000
budWaterDemand (UnripeFruit r) = r * 3/1000
budWaterDemand RipeFruit       = 3/1000


-- Aufgabe 1 --------------------------------------------------------------------
-- a)
-- Definition von Plant

-- b)
calculatePlantWaterDemand :: undefined
calculatePlantWaterDemand = undefined

-- c)
calculatePlantNutrientDemand :: undefined
calculatePlantNutrientDemand = undefined


-- Aufgabe 2 --------------------------------------------------------------------
class Growing a where
    grow :: a -> [a]

-- a)
-- Instanzdefinition: Growing Bud

-- b)
-- Instanzdefinition: Growing Plant

-- c)
simulateGrowth :: undefined
simulateGrowth = undefined

-- d)
accumulateOverLifecycle :: undefined
accumulateOverLifecycle = undefined
