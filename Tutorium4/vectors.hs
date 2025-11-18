type Vector = (Double, Double, Double)

kreuzprodukt :: Vector -> Vector -> Vector
kreuzprodukt (x1, y1, z1) (x2, y2, z2) =
    ( y1 * z2 - z1 * y2
    , z1 * x2 - x1 * z2
    , x1 * y2 - y1 * x2
    )
