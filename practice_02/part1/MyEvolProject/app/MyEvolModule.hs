module MyEvolModule where

data MyEvolution = LUCA 
                 | Cyanobacteria 
                 | Trilobite 
                 | Ichthyostega 
                 | Dimetrodon 
                 | Archaeopteryx 
                 | Morganucodon 
                 | Purgatorius 
                 | Australopithecine 
                 | Humans

instance Show MyEvolution where
    show LUCA                = "Last Universal Common Ancestor"
    show Cyanobacteria       = "Synechococcus"
    show Trilobite           = "Paradoxides"
    show Ichthyostega        = "Ichthyostega"
    show Dimetrodon          = "Dimetrodon"
    show Archaeopteryx       = "Archaeopteryx"
    show Morganucodon        = "Morganucodon"
    show Purgatorius         = "Purgatorius"
    show Australopithecine   = "Australopithecus Afarensis"
    show Humans              = "Homo Sapiens"

instance Read MyEvolution where
    readsPrec _ "Last Universal Common Ancestor" = [(LUCA, "")]
    readsPrec _ "Synechococcus"                  = [(Cyanobacteria, "")]
    readsPrec _ "Paradoxides"                    = [(Trilobite, "")]
    readsPrec _ "Ichthyostega"                   = [(Ichthyostega, "")]
    readsPrec _ "Dimetrodon"                     = [(Dimetrodon, "")]
    readsPrec _ "Archaeopteryx"                  = [(Archaeopteryx, "")]
    readsPrec _ "Morganucodon"                   = [(Morganucodon, "")]
    readsPrec _ "Purgatorius"                    = [(Purgatorius, "")]
    readsPrec _ "Australopithecus Afarensis"     = [(Australopithecine, "")]
    readsPrec _ "Homo Sapiens"                   = [(Humans, "")]
    readsPrec _ _                                = []

instance Eq MyEvolution where
    LUCA == LUCA = True
    Cyanobacteria == Cyanobacteria = True
    Trilobite == Trilobite = True
    Ichthyostega == Ichthyostega = True
    Dimetrodon == Dimetrodon = True
    Archaeopteryx == Archaeopteryx = True
    Morganucodon == Morganucodon = True
    Purgatorius == Purgatorius = True
    Australopithecine == Australopithecine = True
    Humans == Humans = True
    _ == _ = False

instance Enum MyEvolution where
    fromEnum LUCA                = 0
    fromEnum Cyanobacteria       = 1
    fromEnum Trilobite           = 2
    fromEnum Ichthyostega        = 3
    fromEnum Dimetrodon          = 4
    fromEnum Archaeopteryx       = 5
    fromEnum Morganucodon        = 6
    fromEnum Purgatorius         = 7
    fromEnum Australopithecine   = 8
    fromEnum Humans              = 9
    
    toEnum 0 = LUCA
    toEnum 1 = Cyanobacteria
    toEnum 2 = Trilobite
    toEnum 3 = Ichthyostega
    toEnum 4 = Dimetrodon
    toEnum 5 = Archaeopteryx
    toEnum 6 = Morganucodon
    toEnum 7 = Purgatorius
    toEnum 8 = Australopithecine
    toEnum 9 = Humans
    toEnum _ = error "Invalid MyEvolution value"

instance Ord MyEvolution where
    compare x y = compare (fromEnum x) (fromEnum y)

instance Bounded MyEvolution where
    minBound = LUCA
    maxBound = Humans

data MyEvolution' = LUCA' 
                  | Cyanobacteria' 
                  | Trilobite' 
                  | Ichthyostega' 
                  | Dimetrodon' 
                  | Archaeopteryx' 
                  | Morganucodon' 
                  | Purgatorius' 
                  | Australopithecine' 
                  | Humans'
                  deriving (Show, Read, Eq, Ord, Enum, Bounded)