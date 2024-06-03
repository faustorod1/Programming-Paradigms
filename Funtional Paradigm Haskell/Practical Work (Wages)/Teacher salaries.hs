-- Presupuesto Universitario

{-
NOTAS
1: Para que una propuesta funcione se debe comentar la otra
2: Cuando se trabajan menos de 5hr semanales o mas de 50 el programa devuelve un sueldo de 0 ya que es una caso inexistente segun la conigna
-}

-- PRIMER PROPUESTA (INEFICIENTE)(antes de conocer los Guards)
{-
-- Funcion Principal

sueldo::String->Float->Float->Float
sueldo puesto nivel tiempo = cargo puesto * (antiguedad0 (nivel0 nivel) + antiguedad1 (nivel1 nivel) + antiguedad2 (nivel2 nivel) + antiguedad3 (nivel3 nivel) + antiguedad4 (nivel4 nivel)) * (proporcionalidad0 (horas0 tiempo) + proporcionalidad1 (horas1 tiempo) + proporcionalidad2 (horas2 tiempo) + proporcionalidad3 (horas3 tiempo) + proporcionalidad4 (horas4 tiempo) + proporcionalidad5 (horas5 tiempo))

-- Funciones Auxiliares

cargo::String->Float
cargo "titular" = 149000
cargo "adjunto" = 116000
cargo "ayudante" = 66000
cargo otro = 0

nivel0::Float->Bool
nivel0 x = (x < 3)
nivel1 x = (x >= 3 && x < 5)
nivel2 x = (x >= 5 && x < 10)
nivel3 x = (x >= 10 && x < 24)
nivel4 x = (x >= 24)

antiguedad0::Bool->Float
antiguedad0 True = 1
antiguedad0 False = 0
antiguedad1 True = 1.2
antiguedad1 False = 0
antiguedad2 True = 1.3
antiguedad2 False = 0
antiguedad3 True = 1.5
antiguedad3 False = 0
antiguedad4 True = 2.2
antiguedad4 False = 0

horas0::Float->Bool
horas0 x = (x<5 || x>50)
horas1 x = (x>=5 && x<15)
horas2 x = (x>=15 && x<25)
horas3 x = (x>=25 && x<35)
horas4 x = (x>=35 && x<45)
horas5 x = (x>=45 && x<=50)

proporcionalidad0::Bool->Float
proporcionalidad0 True = 0
proporcionalidad0 False = 0
proporcionalidad1 True = 1
proporcionalidad1 False = 0
proporcionalidad2 True = 2
proporcionalidad2 False = 0
proporcionalidad3 True = 3
proporcionalidad3 False = 0
proporcionalidad4 True = 4
proporcionalidad4 False = 0
proporcionalidad5 True = 5
proporcionalidad5 False = 0
-}
--SEGUNDA PROPUESTA (MAS EFICIENTE) (utilizo Guards)

-- Funcion Principal

sueldo::String->Float->Float->Float
sueldo x y z = cargo x * antiguedad y * horas z

-- Funciones Auxiliares
cargo::String->Float
cargo "titular" = 149000
cargo "adjunto" = 116000
cargo "ayudante" = 66000
cargo otro = 0

antiguedad::Float->Float
antiguedad x
    |(x < 3) = 1
    |(x >= 3 && x < 5) = 1.2
    |(x >= 5 && x < 10) = 1.3
    |(x >= 10 && x < 24) = 1.5
    |(x >= 24) = 2.2

horas::Float->Float
horas x
    |(x<5 || x>50) = 0
    |(x>=5 && x<15) = 1
    |(x>=15 && x<25) = 2
    |(x>=25 && x<35) = 3
    |(x>=35 && x<45) = 4
    |(x>=45 && x<=50) = 5

--Continuacion del TP1: Llegar a fin de mes

distPobreza23::String->Float->Float->Int->Float
distPobreza23 x y z integrantes = sueldo x y z - canastabasica integrantes

distPobreza24::String->Float->Float->Int->Float -- idem a 2023 pero contemplando los aumentos de la canasta y de la los sueldos
distPobreza24 x y z integrantes= (sueldo x y z)*1.22 - (canastabasica integrantes)*1.71

canastabasica::Int->Float
canastabasica 1 = 126000
canastabasica 3 = 310000
canastabasica 4 = 390000
canastabasica 5 = 410000
