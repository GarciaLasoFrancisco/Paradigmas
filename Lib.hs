
module Lib where
import Text.Show.Functions

data Jugador = Jugador {
        nombre          :: String,
        dinero          :: Int,
        tactica         :: String,
        propiedades     :: [Propiedad],
        acciones        :: [Accion]
} deriving (Show)

data Propiedad = Propiedad {
        nombrePropiedad       :: String,
        precio                :: Int
} deriving (Show)

type Accion  = Jugador -> Jugador

carolina = Jugador {
        nombre          = "Carolina",
        dinero          = 500,
        tactica         = "Accionista",
        propiedades     = [],
        acciones        = [pasarPorElBanco, pagarAAccionistas]
}

manuel = Jugador {
        nombre          = "Manuel",
        dinero          = 500,
        tactica         = "Oferente singular",
        propiedades     = [],
        acciones        = [pasarPorElBanco, enojarse]
}

mediterraneo = Propiedad{
        nombrePropiedad     = "Avenida Mediterraneo",
        precio              = 50
}

pacifico = Propiedad{
        nombrePropiedad     = "Avenida Pacifico",
        precio              = 200
}

pasarPorElBanco :: Accion
pasarPorElBanco = mapTactica "Comprador Compulsivo" . mapDinero (+40)

mapDinero :: (Int -> Int) -> Jugador -> Jugador
mapDinero f jugador = jugador { dinero = max 0 . f $ dinero jugador }

mapTactica :: String -> Jugador -> Jugador
mapTactica nuevaTactica jugador = jugador {tactica = nuevaTactica}

enojarse :: Accion
enojarse = agregarAccion gritar . mapDinero (+50)

mapAccion :: ([Accion] -> [Accion]) -> Jugador -> Jugador
mapAccion f jugador = jugador { acciones = f $ acciones jugador }

agregarAccion :: Accion -> Jugador -> Jugador
agregarAccion accion = mapAccion (accion :)

gritar :: Accion
gritar jugador = jugador { nombre = "AHHHH"  ++  nombre jugador}

subastar :: Propiedad -> Accion
subastar propiedad jugador
        |  tieneLaTacticaNecesaria jugador = agregarPropiedad propiedad (mapDinero (subtract (precio propiedad))jugador)
        |  otherwise = jugador

mapPropiedades :: ([Propiedad] -> [Propiedad]) -> Jugador -> Jugador
mapPropiedades f jugador = jugador { propiedades = f $ propiedades jugador }

agregarPropiedad :: Propiedad -> Jugador -> Jugador
agregarPropiedad  propiedad = mapPropiedades (propiedad :)

tieneLaTacticaNecesaria :: Jugador -> Bool
tieneLaTacticaNecesaria jugador = esAccionista jugador || esOferenteSingular jugador

esAccionista :: Jugador -> Bool
esAccionista = (=="Accionista") . tactica

esOferenteSingular :: Jugador ->Bool
esOferenteSingular = (=="Oferente  singular") . tactica

cobrarAlquileres :: Accion
cobrarAlquileres jugador = mapDinero (+gananciasPropiedadesBaratas jugador)(mapDinero(+gananciasPropiedadesCaras jugador)jugador)

esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata = (<150) . precio

esPropiedadCara :: Propiedad -> Bool
esPropiedadCara = (>=150) . precio

cantidadDePropiedadesBaratas :: Jugador -> Int
cantidadDePropiedadesBaratas = length. filter esPropiedadBarata . propiedades

cantidadDePropiedadesCaras :: Jugador -> Int
cantidadDePropiedadesCaras = length. filter esPropiedadCara . propiedades

gananciasPropiedadesBaratas :: Jugador -> Int
gananciasPropiedadesBaratas = (*10) . cantidadDePropiedadesBaratas

gananciasPropiedadesCaras :: Jugador -> Int
gananciasPropiedadesCaras = (*20) . cantidadDePropiedadesCaras

pagarAAccionistas :: Accion
pagarAAccionistas jugador
    | esAccionista jugador = mapDinero (+200) jugador 
    | otherwise = mapDinero (subtract 100) jugador 

hacerBerrinchePor :: Propiedad -> Jugador -> Accion
hacerBerrinchePor propiedad jugador
    |   puedeComprarla propiedad jugador = agregarPropiedad propiedad (mapDinero (subtract (precio propiedad))jugador
    |   otherwise = hacerBerrinchePor propiedad (gritar . mapDinero(+10))

puedeComprarla :: Propiedad -> Jugador -> Bool
puedeComprarla propiedad jugador = dinero jugador >= precio propiedad

ultimaRonda :: Accion
ultimaRonda jugador = foldr ($) jugador $ acciones jugador 

juegoFinal :: Jugador -> Jugador -> String
juegoFinal jugador1 jugador2
   | dineroFinal jugador1 > dineroFinal jugador2 = nombre jugador1
   | otherwise = nombre jugador2

dineroFinal :: Jugador -> Int
dineroFinal = dinero . ultimaRonda