
import Text.Show.Functions

--Funciones utiles

between menor mayor medio
  |menor <= medio = medio <= mayor
  |otherwise = False 

type Crecimiento = Int -> Int
type Especie = (String, Crecimiento)

data Arbol = Arbol {
    especie :: Especie,
    altura :: Int,
    ancho :: Int,
    vitalidad :: Float
} deriving (Show)

--Ejemplos de algunas especies
cerezo = ("cerezo", escalonado)
banano = ("banano", progresivo)
jacaranda = ("jacaranda", progresivo)
pino = ("pino", duplica)
eucalipto = ("eucalipto", escalonado)
alga= ("alga", lento)

--Ejemplos dados de arboles
unJacaranda = Arbol jacaranda 6 1 1.4
unPino = Arbol pino 5 3 1.9
unEucaliptu = Arbol eucalipto 5 4 0.7
otroJacaranda = Arbol jacaranda 10 2 1.0
unCerezo = Arbol cerezo 7 11 0.9
unBanano = Arbol banano 8 10 2.1
unAlga= Arbol alga 6 1 1.4

--Tipos de crecimiento

progresivo :: Crecimiento
progresivo = (`div` 2)

escalonado :: Crecimiento
escalonado = (`mod` 2)

duplica :: Crecimiento
duplica = (2*)

torcido :: Crecimiento
torcido = progresivo.escalonado

lento :: Crecimiento
lento = (`div` 4)

--PUNTO 2

frondosoYAltura :: [Arbol] -> Bool
frondosoYAltura arboles = any esFrondosoYVital arboles

esFrondosoYVital:: Arbol -> Bool
esFrondosoYVital unArbol = esFrondoso unArbol &&  esVital unArbol

esFrondoso :: Arbol -> Bool
esFrondoso unArbol = (between 6 15.altura $ unArbol) && (ancho unArbol > altura unArbol)

esVital :: Arbol -> Bool
esVital unArbol = (>1).vitalidad $ unArbol

--Punto 3

type FactorClimatico = Arbol -> Arbol

modificarAltura :: (Int -> Int) -> Arbol -> Arbol
modificarAltura f unArbol = unArbol{altura = f.altura $ unArbol}

modificarAncho :: (Int -> Int) -> Arbol -> Arbol
modificarAncho f unArbol = unArbol{ancho= f.ancho $ unArbol}

modificarVitalidad :: (Float -> Float) -> Arbol -> Arbol
modificarVitalidad f unArbol = unArbol{vitalidad= f.vitalidad $ unArbol}

granizo:: FactorClimatico
granizo = modificarAltura (div 2).modificarAncho (div 2)

lluvia::Float -> FactorClimatico
lluvia mm unArbol = modificarAltura (+ 1).modificarVitalidad (sumarMm mm) $ unArbol

sumarMm:: Float -> Float ->Float
sumarMm mm vitalidad = vitalidad * (1 + mm / 100)

temperatura:: Float -> FactorClimatico
temperatura grados unArbol
    | grados < 0 = modificarVitalidad (/2) unArbol
    | grados > 40 = modificarVitalidad (sumarMm (-40)) unArbol
    | otherwise = unArbol

--PUNTO 4 

alturaEstimada:: Arbol -> Int
alturaEstimada unArbol = (altura unArbol) + (altura.modificarAltura (obtenerTipoCrecimiento unArbol) $ unArbol)

obtenerTipoCrecimiento:: Arbol -> (Int -> Int)
obtenerTipoCrecimiento = (snd.especie)

--PUNTO 5

crecer unArbol = unArbol{altura = alturaEstimada unArbol}

crecimientoDeConjunto :: [Arbol] -> [Arbol]
crecimientoDeConjunto arboles = map crecer arboles

--Punto 6
--Se declaro el arbol unAlga de especie alga, con un tipo de crecimiento lento. Esta declarado a lo largo del parcial.

--Punto 7

restar metros n = n-metros

podar::Int -> Arbol -> Arbol
podar metros= (modificarAncho (restar metros ).modificarVitalidad (sumarMm 10))

--PUNTO 8

temporada:: [FactorClimatico] -> Arbol ->  Arbol
temporada factores unArbol = foldr ($) unArbol factores

--PUNTO 9

nuevo:: FactorClimatico
nuevo = (modificarAltura (+5))

-----Ejemplo de invocacion
-- podar 1.crecer.temporada [lluvia 50,nuevo] $ unBanano
-- Arbol {especie = ("banano",<function>), altura = 21, ancho = 9, vitalidad = 3.465}

-----PUNTO 10
--a

arbolesOrdenadosA:: [Arbol]-> Bool
arbolesOrdenadosA arboles = estaOrdenado (map condicionA arboles)

condicionA unArbol = altura (temporada [granizo,lluvia 20] unArbol)

--b

arbolesOrdenadosB:: [Arbol]-> Bool
arbolesOrdenadosB arboles = estaOrdenado (map condicionB arboles)

condicionB unArbol = vitalidad (temporada [temperatura 45,nuevo] unArbol) --NO DICE QUE VALOR PODAR

estaOrdenado :: (Ord a) => [a] -> Bool
estaOrdenado []       = True
estaOrdenado [x]      = True
estaOrdenado (x:y:xs) = x <= y && estaOrdenado (y:xs)

--Ejemplos de invocacion punto 10

-- arbolesOrdenadosA [unJacaranda,unBanano]
-- True

-- arbolesOrdenadosB [unBanano,unJacaranda]
-- False

--PUNTO 11

--Si se puede, en la funcion que verifica si dentro de una lista de arboles alguno es frondoso y tiene una vitalidad mayor a 1. Ya que al obtener
-- al primero que cumpla con lo requerido parara de correr la funcion

-- frondosoYAltura :: [Arbol] -> Bool
-- frondosoYAltura arboles = any esFrondosoYVital arboles

--Ejemplo
--frondosoYAltura (arbolInfinito unBanano)
-- True
--Funciona ya que el any al recibir el primer true frena gracias a la lazy evaluation

arbolInfinito unArbol = unArbol : arbolInfinito unArbol  