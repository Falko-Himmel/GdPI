-- Kreuzprodukt
type Vector3D = (Double, Double, Double)

crossProduct :: Vector3D -> Vector3D -> Vector3D
crossProduct (x1, y1, z1) (x2, y2, z2) =
    let cx = y1 * z2 - z1 * y2
        cy = z1 * x2 - x1 * z2
        cz = x1 * y2 - y1 * x2
    in (cx, cy, cz)


type Matrix = (Vector3D, Vector3D, Vector3D)

matrixVectorProduct :: Matrix -> Vector3D -> Vector3D
matrixVectorProduct (row1, row2, row3) vec =
    let dotProduct (a1, a2, a3) (b1, b2, b3) = a1 * b1 + a2 * b2 + a3 * b3
        x = dotProduct row1 vec
        y = dotProduct row2 vec
        z = dotProduct row3 vec
    in (x, y, z)