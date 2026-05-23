module Parcial where
import Text.Show.Functions()

data Plato = UnPlato {
    dificultad :: Int,
    componentes :: [Componente]
} deriving (Show, Eq)

data Participante = UnParticipante {
    nombre :: String,
    trucos :: [Truco],
    especialidad :: Plato
}

type Componente = (String, Float)
type Truco = Plato -> Plato

endulzar :: Float -> Truco
endulzar gramos plato = plato { componentes = [("azucar", gramos)] ++ componentes plato }
salar :: Float -> Truco
salar gramos plato = plato { componentes = ("sal", gramos) ++ componentes plato }
darSabor :: Float -> Float -> Truco
darSabor gramosSal gramosAzucar = salar gramosSal . endulzar gramosAzucar
duplicarPorcion :: Truco
duplicarPorcion plato = plato { componentes = map (\(ingrediente, peso) -> (ingrediente, peso * 2)) (componentes plato) }
simplificar :: Truco
simplificar plato 
    | esUnBardo plato = plato { dificultad = 5, componentes = quitarLivianos (componentes plato) }
    | otherwise     = plato

esUnBardo :: Plato -> Bool
esUnBardo plato = tieneMuchosComponentes plato && esDificil plato

tieneMuchosComponentes :: Plato -> Bool
tieneMuchosComponentes plato = length (componentes plato) > 5

esDificil :: Plato -> Bool
esDificil plato = dificultad plato > 7

quitarLivianos :: [Componente] -> [Componente]
quitarLivianos componentes = filter pesaMasDe10Gramos componentes

pesaMasDe10Gramos :: Componente -> Bool
pesaMasDe10Gramos (_, peso) = peso >= 10

esVegano :: Plato -> Bool
esVegano = noTieneEstosIngredientes ["carne", "huevo", "leche", "queso", "crema"] 

esSinTacc :: Plato -> Bool
esSinTacc = noTieneEstosIngredientes ["harina"]

esComplejo :: Plato -> Bool
esComplejo plato = tieneMuchosComponentes plato && esDificil plato

noTieneEstosIngredientes :: [String] -> Plato -> Bool
noTieneEstosIngredientes prohibidos plato = not (any (\(ingrediente, _) -> ingrediente `elem` prohibidos) (componentes plato))

noAptoHipertension :: Plato -> Bool
noAptoHipertension plato = cantidadDeSal plato > 2

cantidadDeSal :: Plato -> Float
cantidadDeSal plato = sum (map snd (filter esSal (componentes plato)))

esSal :: Componente -> Bool
esSal (ingrediente, _) = ingrediente == "sal"

platoDePepe :: Plato
platoDePepe = UnPlato {
    dificultad = 8,
    componentes = [("sal", 3), ("harina", 200), ("huevo", 50), ("agua", 100), ("pimienta", 5), ("aceite", 15)]
}

pepe :: Participante
pepe = UnParticipante {
    nombre = "Pepe Ronccino",
    trucos = [darSabor 2 5, simplificar, duplicarPorcion],
    especialidad = platoDePepe
}

cocinar :: Participante -> Plato
cocinar participante = foldl aplicarTruco (especialidad participante) (trucos participante)

aplicarTruco :: Plato -> Truco -> Plato
aplicarTruco platoActual truco = truco platoActual

esMejorQue :: Plato -> Plato -> Bool
esMejorQue plato1 plato2 = (dificultad plato1 > dificultad plato2) && (pesoTotal plato1 < pesoTotal plato2)

pesoTotal :: Plato -> Float
pesoTotal plato = sum (map snd (componentes plato))


participanteEstrella :: [Participante] -> Participante
participanteEstrella [p] = p
participanteEstrella (p1:p2:ps)
    | esMejorQue (cocinar p1) (cocinar p2) = participanteEstrella (p1:ps)
    | otherwise                            = participanteEstrella (p2:ps)