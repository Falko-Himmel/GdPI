-- Wir modellieren nur mit drei Variablen:
-- Max, Falko, Tom stehen jeweils dafür, dass die Person die Wahrheit sagt (True) oder lügt (False).
-- Ziel: A1 && A2 && A3 soll genau EINE Lösung haben und festlegen, wer lügt.
-- Dazu brechen wir die Symmetrie zwischen Max und Falko leicht:
-- 1) Max sagt: "Falko lügt"        
-- 2) Falko sagt: "Tom lügt"        
-- 3) Tom sagt: "Genau einer der beiden (Max,Falko) hat recht"

-- Als Bool-Formeln (nur mit Max, Falko, Tom):
-- A1: (Max && not Falko) || (not Max && Falko)                        
-- A2: (Falko && not Tom)  || (not Falko && Tom)                        
-- A3: (Tom && ((Max && not Falko) || (not Max && Falko))) || (not Tom && not ((Max && not Falko) || (not Max && Falko)))
-- Wahrheitstabelle
-- Max  Falko Tom | A1 | A2 | A3 | A1 && A2 && A3
--  0     0    0    0     0    1          0
--  0     0    1    0     1    0          0
--  0     1    0    1     1    0          0
--  0     1    1    1     0    1          0
--  1     0    0    1     0    0          0
--  1     0    1    1     1    1          1   Lösung
--  1     1    0    0     1    1          0
--  1     1    1    0     0    0          0

-- Optional: kleine Prüffunktion in Haskell
foo :: Bool -> Bool -> Bool -> Bool
foo maxT falkoT tomT = a1 && a2 && a3
  where
    xorMF = (maxT && not falkoT) || (not maxT && falkoT)      
    xorFT = (falkoT && not tomT) || (not falkoT && tomT)      
    a1     = xorMF                                             
    a2     = xorFT                                             
    a3     = (tomT && xorMF) || (not tomT && not xorMF)




