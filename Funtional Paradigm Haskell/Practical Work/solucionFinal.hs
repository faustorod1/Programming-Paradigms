--Punto 1
data Ciudad = UnaCiudad{
    nombre::String,
    fundacion::Int,
    atracciones::[String],
    costoDeVida::Int 
}deriving Show

--Ciudades Ejemplo:o
ciudad1::Ciudad
ciudad1 = UnaCiudad {nombre="Baradero", fundacion=1615, atracciones=["Parque del Este", "Museo Alejadro Barbich"], costoDeVida=150}
ciudad2::Ciudad
ciudad2 = UnaCiudad {nombre="Nullish", fundacion=1800, atracciones=[], costoDeVida=140}
ciudad3::Ciudad
ciudad3 = UnaCiudad {nombre="Caleta Olivia", fundacion=1901, atracciones=["El Glorioso", "Faro Costanera"], costoDeVida=120}
ciudad4::Ciudad
ciudad4 = UnaCiudad "Azul" 1832 ["Teatro Espaniol", "Parque Municipal Sarmiento", "Costanera Cacique Catriel" ] 190 --La "ñ" nos traía problemas al imprimirla por consola y "ni" nos traia problemas con funcines como es Sobria

--Valor de una ciudad
definirValorDeCiudad::Ciudad->Int
definirValorDeCiudad ciudad 
    |fundacion ciudad < 1800 = 5 * (1800 - fundacion ciudad)
    |length (atracciones ciudad) == 0 = 2 * costoDeVida ciudad
    |otherwise = 3 * costoDeVida ciudad

--Punto 2
--Alguna Atraccion Copada
tieneAtraccionCopada :: Ciudad -> Bool
tieneAtraccionCopada ciudad = any inicial (atracciones ciudad)
vocales = ['A', 'E', 'I', 'O', 'U', 'a', 'e', 'i', 'o', 'u']

inicial :: String ->Bool
inicial elemento = any (head elemento ==) vocales

--Ciudad Sobria
esSobria :: Int -> Ciudad -> Bool
esSobria x ciudad =  all (>x) (cantidadDeLetras ciudad) && not(null(atracciones ciudad)) --la parte despues del "&&" es por si no tiene atracciones
cantidadDeLetras :: Ciudad -> [Int]
cantidadDeLetras ciudad = map length (atracciones ciudad)

--Ciudad con Nombre Raro
tieneNombreRaro :: Ciudad -> Bool
tieneNombreRaro ciudad = length (nombre ciudad) < 5

--Punto 3
--Sumar una nueva atracción
sumarNuevaAtraccion::String->Ciudad->Ciudad
sumarNuevaAtraccion nuevaAtraccion ciudad = ciudad {atracciones= (atracciones ciudad) ++ [nuevaAtraccion], costoDeVida = round (fromIntegral (costoDeVida ciudad) * 1.2)}

--Crisis
crisis::Ciudad->Ciudad
crisis ciudad = ciudad {atracciones= restaAtraccion(atracciones ciudad) , costoDeVida = bajarCostoDeVida ciudad}

restaAtraccion :: [String] -> [String]
restaAtraccion atracciones
  |null atracciones = []
  |otherwise = init atracciones

bajarCostoDeVida::Ciudad->Int
bajarCostoDeVida ciudad = round (fromIntegral (costoDeVida ciudad) * 0.9) -- Inicialmente habia usado take (length(atracciones ciudad)-1) (atracciones ciudad) en vez de init

--Remodelación 
remodelacion::Int->Ciudad->Ciudad -- El porcentaje de incremento debe pasarse como numero entero comprendido entre 0 y 100
remodelacion incremento ciudad = ciudad{
    nombre=agregarPrefijo(nombre ciudad),
    costoDeVida= incrementarCosto ciudad incremento
    }

agregarPrefijo::String->String
agregarPrefijo nombre = "New " ++ nombre

incrementarCosto::Ciudad->Int->Int
incrementarCosto ciudad incremento = round (fromIntegral(costoDeVida ciudad) * (1 + fromIntegral(incremento) *0.01))

--Reevaluación 
reevaluacion::Int->Ciudad->Ciudad
reevaluacion valor ciudad
    |esSobria valor ciudad = ciudad {
        costoDeVida = round(fromIntegral(costoDeVida ciudad)*1.1)
        }
    |otherwise = ciudad {
        costoDeVida = costoDeVida ciudad - 3
        }

--Parte 4
remoCrisisReeva::Ciudad->Int->Int->Ciudad -- hecho con aplicacion parcial
remoCrisisReeva ciudad incremento valor = reevaluacion valor (crisis (remodelacion incremento ciudad))

--SEGUNDA ENTREGA
--Punto 4
--4.1 Los años pasan...
data Anio = UnAnio{
    numero::Int,
    eventos::[Ciudad->Ciudad]
}

pasoDeAnio::Ciudad->Anio->Ciudad  -- En la consola: Pasar el evento y otro\s parametro\s correspondiente\s dentro de parentesis
pasoDeAnio ciudad anio
    |null (eventos anio) = ciudad
    |otherwise = foldl1 (.) (eventos anio) ciudad -- Podria usar $

--Años Ejemplo:
anio2015::Anio
anio2015 = UnAnio {numero=2015, eventos = []}
anio2021::Anio
anio2021 = UnAnio {numero=2021, eventos = [crisis, sumarNuevaAtraccion "playa"]}
anio2022::Anio
anio2022 = UnAnio {numero=2022, eventos = [crisis, remodelacion 5, reevaluacion 7]}
anio2023::Anio
anio2023 = UnAnio {numero=2023, eventos = [crisis, sumarNuevaAtraccion "parque", remodelacion 10, remodelacion 20]}

--4.2 Algo mejor
algoMejor :: String -> (Ciudad->Ciudad) -> Ciudad -> Bool
algoMejor criterio evento ciudad
 | criterio == "cantidad de atracciones" = length(atracciones  (evento ciudad)) > length (atracciones ciudad) 
 | criterio == "costo de vida" = costoDeVida (evento ciudad) >  costoDeVida ciudad
 | otherwise = error "criterio no existente o no cuantificable" --Agregado extra a la consigna

--4.3 Costo de vida que suba
filtrarporcriterio ::(Ciudad->(Ciudad->Ciudad)->Bool)->Ciudad->Anio->Ciudad
filtrarporcriterio filtro ciudad anio = pasoDeAnio ciudad (UnAnio {numero = numero anio, eventos = filter (filtro ciudad) (eventos anio)})

costoDeVidaSuba::Ciudad->(Ciudad->Ciudad)->Bool
costoDeVidaSuba ciudad evento = costoDeVida (evento ciudad) > costoDeVida ciudad

--4.4 Costo de vida que baje
costoDeVidaBaje::Ciudad->(Ciudad->Ciudad)->Bool
costoDeVidaBaje ciudad evento = costoDeVida (evento ciudad) < costoDeVida ciudad

--4.5 Valor que suba
valorSuba::Ciudad->(Ciudad->Ciudad)->Bool
valorSuba ciudad evento = definirValorDeCiudad (evento ciudad) > definirValorDeCiudad ciudad

--Punto 5
--5.1 Eventos ordenados 
eventosOrdenados::Anio->Ciudad->Bool --Asumo que no debo comparar el costo de vida de la ciudad tras el primer evento con el costo de vida de la ciudad sin ser atravesada por ningun evento.
eventosOrdenados anio ciudad
    | null (eventos anio) = error "No se puede aplicar esta funcion para un año sin eventos."
   -- | tieneUnElem (eventos anio) = True --Asumo que un unico evento esta siempre ordenado
    | null(drop 1 (eventos anio)) = True
    | costoDeVida (((eventos anio) !! 1 ) ciudad) > costoDeVida (head (eventos anio) ciudad) = eventosOrdenados (UnAnio {numero = numero anio, eventos = drop 1 (eventos anio)}) (head (eventos anio) ciudad)
    | otherwise = False
{-
tieneUnElem :: [a] -> Bool
tieneUnElem [x] = True
tieneUnElem y = False
-}

--5.2 Ciudades ordenadas
ciudadesOrdenadas::(Ciudad->Ciudad)->[Ciudad]->Bool -- En la consola: Pasar el evento y el int correspondiente dentro de parentesis
ciudadesOrdenadas evento ciudades
   | length ciudades == 1 = True --Asumo que una unica ciudad esta siempre ordenada
   | costoDeVida (evento (head ciudades)) < costoDeVida (evento (ciudades !! 1)) = ciudadesOrdenadas evento (drop 1 ciudades)
   | null ciudades = error "No se puede aplicar esta funcion para una lista sin ciudades."
   | otherwise = False

listaDeCiudades1::[Ciudad]
listaDeCiudades1 = [ciudad3, ciudad2, ciudad1, ciudad4]

listaDeCiudades2::[Ciudad]
listaDeCiudades2 = [ciudad3, ciudad4, ciudad1]

--5.3 Años ordenados
aniosOrdenados::[Anio]->Ciudad->Bool
aniosOrdenados anios ciudad
    | null anios = error "No se puede aplicar esta funcion para una lista sin años."
    | length anios == 1 = True --Asumo que un unico anio esta siempre ordenado
    | costoDeVida (pasoDeAnio ciudad (head anios)) < costoDeVida (pasoDeAnio ciudad (anios !! 1)) = aniosOrdenados (drop 1 anios) ciudad
    | otherwise = False

listaDeAnios1::[Anio]
listaDeAnios1 = [anio2021, anio2022, anio2022]

listaDeAnios2::[Anio]
listaDeAnios2 = [anio2022, anio2021, anio2023]

--Punto 6
--Una serie de eventos interminables 
{-
Definir el año 2024 con una lista infinita de eventos.
-}
anio2024::Anio
anio2024 = UnAnio {numero=2024, eventos = repeat crisis} --Unicamente repito el evento crisis infinitamete

anio2024HechoAMano::Anio
anio2024HechoAMano = UnAnio {numero=2024, eventos = concatenarInfinitamete [crisis]} --Unicamente repito el evento crisis infinitamete

concatenarInfinitamete::[Ciudad->Ciudad]->[Ciudad->Ciudad]
concatenarInfinitamete evento = evento ++ (concatenarInfinitamete evento)

anio2024Mejorado::Anio
anio2024Mejorado = UnAnio {numero=2024, eventos = cycle [crisis, remodelacion 5, reevaluacion 7, sumarNuevaAtraccion "parque"]}

anio2024Otro::Anio
anio2024Otro = UnAnio {numero=2024, eventos = cycle [crisis, sumarNuevaAtraccion "parque", remodelacion 10, remodelacion 20]}

--Eventos ordenados
{-
Considerando que la funcion devuelve un bool podria ocurrir que constantemente encuentre eventos que cumplan
las condiciones y por ende no salir del bucle ya que para que devuelva "True" todas las condiciones deben
ser verdaderas. En este caso considero que no habria un resultado posible ya que al ser los eventos infinitos,
no puede saberse si todas las condiciones fueron verdaderas.
Por otro lado (en el caso mas probable), si en algun momento de la recursion un evento no cumple las condiciones
para entrar en la recurcion, devolveria "False" y tendriamos ese resultado, ya que con un unico caso falso alcanza
para refutar (contraejemplo).
Para concluir, al llevar a la practica lo planteado y pasarle a la funcion eventosOrdenados un anio con eventos
infinitos y una ciudad, nunca obtuve un resultado. Probablemente esto haya sido por la funcion length y es por
eso que manteno la opinion expuesta en los parrafos previos.
-}

--Ciudades ordenadas
{-
Supongo que la consigna se refiere a una lista infinita de ciudades. Aplico misma logica en en la respuesta a
"Eventos Ordenados" y Tambien tendria la misma problematica con length para llevarlo a la practica.
-}
listaDeCiudadesInfinitas::[Ciudad]
listaDeCiudadesInfinitas = cycle [ciudad1, ciudad2, ciudad3, ciudad4]

--Años ordenados
{-
Obviando la problematica que presenta length, ahora tendriamos la funcion pasoDeAnio que nunca terminaria de
aplicarse. Por este motivo esta funcion no tendria un resultado posible.
-}