module Library where
import PdePreludat

-- Parte 1

type Nombre = String
type Durabilidad = Number
type Escudo = Number
type Ataque = Number
type Poder = Nave -> Nave

data Nave = UnaNave {
    nombre :: Nombre,
    durabilidad :: Durabilidad,
    escudo :: Escudo,
    ataque :: Ataque,
    poder :: Poder
} deriving (Show, Eq)

tieFighter :: Nave
tieFighter = UnaNave  "TIE Fighter" 200 100 50 movimientoTurbo

xWing :: Nave
xWing = UnaNave "X Wing" 300 150 100 reparacionEmergencia

darthVader :: Nave
darthVader = UnaNave "Nave de Darth Vader" 500 300 200 movimientoSuperTurbo

millenniumFalcon :: Nave
millenniumFalcon = UnaNave "Millennium Falcon" 1000 500 50 movimientoFalcon

movimientoTurbo :: Poder
movimientoTurbo = modificarAtaque 25

reparacionEmergencia :: Poder
reparacionEmergencia = modificarAtaque (-30)  . modificarDurabilidad 50

movimientoSuperTurbo :: Poder
movimientoSuperTurbo = modificarDurabilidad (-45) .repetirMovimiento 3 movimientoTurbo

movimientoFalcon :: Poder
movimientoFalcon = modificarEscudo 100 . reparacionEmergencia

movimientoInventado :: Poder
movimientoInventado = modificarNombre "Pro " . repetirMovimiento 2 movimientoSuperTurbo 

repetirMovimiento :: Number -> Poder -> Poder
repetirMovimiento 0  _ nave = nave 
repetirMovimiento n movimiento nave = repetirMovimiento (n-1)movimiento (movimiento nave)

modificarNombre :: Nombre -> Nave -> Nave
modificarNombre nuevoNombre nave = nave {nombre = nuevoNombre ++ nombre nave}

modificarAtaque :: Number -> Poder
modificarAtaque x nave = nave {ataque = ataque nave + x}

modificarEscudo :: Number -> Poder
modificarEscudo x nave = nave {escudo = escudo nave + x}

modificarDurabilidad :: Number -> Poder
modificarDurabilidad x nave = nave {durabilidad = durabilidad nave + x}


-- Parte 2

type Flota = [Nave]

durabilidadTotalFlota :: Flota -> Number
durabilidadTotalFlota  = sum . durabilidadFlota 

durabilidadFlota :: Flota -> [Number]
durabilidadFlota flota = map durabilidad flota


-- Parte 3

ataqueNave :: Nave -> Nave -> Nave
ataqueNave naveAtacada naveAtacante = modificarDurabilidad  (negate (recibirAtaque naveAtacada naveAtacante)) naveAtacada

recibirAtaque :: Nave -> Nave -> Number
recibirAtaque naveAtacada naveAtacante = dañoRecibido (activarPoderEspecial naveAtacada) (activarPoderEspecial naveAtacante)

activarPoderEspecial :: Poder
activarPoderEspecial nave = poder nave nave

dañoRecibido :: Nave -> Nave -> Number
dañoRecibido naveAtacada naveAtacante = max 0 (ataque naveAtacante - escudo naveAtacada)


-- Parte 4

estaFueraDeCombate :: Nave -> Bool
estaFueraDeCombate  = (==0) . durabilidad

-- Parte 5

type Estrategia = Nave -> Bool

misionSorpresa :: Nave -> Flota -> Estrategia -> Flota
misionSorpresa _ [] _ = []
misionSorpresa nave (x : xs) estrategia
    | estrategia x = ataqueNave x nave : misionSorpresa nave xs estrategia
    |otherwise = x : misionSorpresa nave xs estrategia

navesDebiles :: Estrategia
navesDebiles = (< 200) . escudo

navesConPeligrosidad :: Number -> Estrategia
navesConPeligrosidad valor = (> valor) . ataque 

navesQuedarianFueraDeCombate :: Nave -> Estrategia
navesQuedarianFueraDeCombate naveAtacada naveAtacante = estaFueraDeCombate (ataqueNave naveAtacada naveAtacante)

estrategiaInventada :: Nave -> Estrategia 
estrategiaInventada naveAtacante naveAtacada = ataque naveAtacante > escudo naveAtacada

-- Parte 6

llevarAdelanteMision :: Nave -> Flota -> Estrategia -> Estrategia -> Flota
llevarAdelanteMision nave flota estrategia1 estrategia2 = misionSorpresa nave flota (obtenerEstrategiaMinimizaDurabilidad nave flota estrategia1 estrategia2) 


obtenerEstrategiaMinimizaDurabilidad :: Nave -> Flota -> Estrategia -> Estrategia -> Estrategia
obtenerEstrategiaMinimizaDurabilidad nave flota estrategia1 estrategia2
    |buscarFlotaMinimizaDurabilidad nave flota estrategia1 estrategia2 = estrategia2
    |otherwise = estrategia1
    
buscarFlotaMinimizaDurabilidad :: Nave -> Flota -> Estrategia -> Estrategia -> Bool
buscarFlotaMinimizaDurabilidad nave flota estrategia1 estrategia2 = durabilidadTotalFlota (misionSorpresa nave flota estrategia1) > durabilidadTotalFlota (misionSorpresa nave flota estrategia2)

-- Parte 7

flotaInifinita :: Nave -> Flota
flotaInifinita nave = nave : flotaInifinita nave

{-
 En este caso, no podemos aprovechar la caracteristica de que Haskell opera con lazy evaluation, ya que se debe recorrer toda la lista para obtener un resultado. Por ende, no es posible determinar su durabilidad total.

Obtendremos la flota tras el ataque, pero probablemente culmine en un stack overflow ya que la lista es infinita, por lo que nunca terminará de recorrerla.
-}