-- A = Objekt ist in A
-- B = Objekt ist in B

-- A B | A && B | A || B |A XOR B | A => B | A <=> B
-- 0 0 |   0    |   0    |   0    |   1    |   1
-- 0 1 |   0    |   1    |   1    |   1    |   0
-- 1 0 |   0    |   1    |   1    |   0    |   0
-- 1 1 |   1    |   1    |   0    |   1    |   1

-- Implikation != Äquivalenz



-- A sagt es regnet nicht
-- B sagt die Straße ist nass
-- C sagt wenn die Straße nass ist, dann regnet es
-- B sagt definitiv die Wahrheit

-- A && (not R)
-- B && N
-- (N => R)

-- => A falsch, B wahr