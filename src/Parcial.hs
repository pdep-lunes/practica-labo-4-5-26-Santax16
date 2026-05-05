module Parcial where
import Text.Show.Functions()


zara :: Perro {raza = dalmata, juguetesfavoritos = ["pelota","mantita"],tiempoEnLaGuarderia = 90, energia = 80}

data Perro = UnPerro {
    raza :: String,
    juguetesfavoritos :: [String],
    tiempoEnLaGuarderia :: Int,
    energia :: Int
}

data Actividad = UnActividad{
    tiempo :: Int,
    ejercicio :: Perro -> Perro
}

data Guarderia = UnaGuarderia {
    nombre :: String,
    rutina :: [Actividad]
}

modificarEnergia :: Int -> Perro -> Perro 
modificarEnergia modificar unPerro = unPerro{ energia = max 0 (energia unPerro + modificar)}

jugar :: Perro -> Perro
jugar = modificarEnergia (-10)

ladrar :: Int -> Perro -> Perro
ladrar ladridos unPerro = modificarEnergia (ladridos / 2 )

regalar :: String -> Perro -> Perro
regalar juguete unPerro = unPerro {juguetesfavoritos = juguetesfavoritos unPerro ++ [juguete]}

razaExtravagante :: Perro -> Bool
razaExtravagante unPerro = raza unPerro == "dalmata" || raza unPerro == "pomerania"

minimoDeTiempoEnGuarderia :: Perro -> Bool
minimoDeTiempoEnGuarderia unPerro = energia unPerro > 50

diaDeSpa :: Perro -> Perro
diaDeSpa unPerro 
  | minimoDeTiempoEnGuarderia unPerro || razaExtravagante unPerro = unPerro{energia = 100, juguetesfavoritos = juguetesfavoritos unPerro ++ ["peine de goma"]}
  |otherwise = unPerro

perderJuguete :: Perro -> Perro
perderJuguete unPerro = unPerro {juguetesfavoritos = drop 1 (juguetesfavoritos unPerro)}  

diaDeCampo :: Perro -> Perro 
diaDeCampo unPerro = (perderJuguete.jugar ) unPerro

sumarTiempoRutina :: [Actividad] -> Int
sumarTiempoRutina rutina = sum(map tiempo rutina)

puedeEstarEnLaGuarderia :: Guarderia -> Perro -> Bool 
puedeEstarEnLaGuarderia guarderia unPerro =  tiempoEnLaGuarderia unPerro > sumarTiempoRutina(rutina guarderia)

perroResponsable :: Perro -> Bool
perroResponsable unPerro = (>3). length . juguetesfavoritos . diaDeCampo




