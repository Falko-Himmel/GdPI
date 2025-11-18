-- Kreuzprodukt
type Vector3D = (Double, Double, Double)

crossProduct :: Vector3D -> Vector3D -> Vector3D
crossProduct (x1, y1, z1) (x2, y2, z2) =
    let cx = y1 * z2 - z1 * y2
        cy = z1 * x2 - x1 * z2
        cz = x1 * y2 - y1 * x2
    in (cx, cy, cz)