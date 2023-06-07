-- Ej 1. Declarar Auto y Carrera
data Auto = Auto {
    color:: String,
    velocidad :: Int,
    distancia :: Int
}
autosDeEjemplo :: [Auto]
autosDeEjemplo = [
    Auto "Rojo" 0 0,
    Auto "Azul" 4 5,
    Auto "Violeta" 10 10,
    Auto "Gris" 30 3,
    Auto "Rosa" 20 0]
type Carrera = [Auto]
-- 1.a) Saber si un auto está cerca de otro auto, que se cumple si son autos distintos y la distancia que hay entre ellos 
--(en valor absoluto) es menor a 10.

estaCerca :: Auto -> Auto -> Bool
estaCerca a1 a2 = sonDistintos a1 a2 && (distanciaEntreAutos a1 a2)<10

sonDistintos :: Auto -> Auto -> Bool
sonDistintos a1 a2 = color a1 == color a2

distanciaEntreAutos :: Auto->Auto->Int
distanciaEntreAutos a1 a2 = abs (distancia a1 - distancia a2)
--1.b) Un aauto vaTranquilo en una carrera si no tiene ningun auto cerca y les va ganando a todos
vaTranquilo :: Auto->Carrera->Bool
vaTranquilo auto carrera = (cuantosCerca auto carrera)==0 && (aCuantosGana auto carrera)==0

cuantosCerca :: Auto-> Carrera -> Int
cuantosCerca auto carrera = length (filter (estaCerca auto) carrera)   -- mejorar con composicion, creo q es asi, si no borro el punto y me fijo. 

leGana :: Auto ->Auto->Bool -- retorna True si el auto 1 va ganando
leGana a1 a2 = distancia a1 > distancia a2

aCuantosGana :: Auto->Carrera -> Int
aCuantosGana auto carrera = length (filter (not.leGana auto) carrera)
-- 1.c) El puesto de un auto es 1+n (n es la cantidad de autos que le va ganando)
puesto :: Auto -> Carrera ->Int
puesto auto carrera = 1+aCuantosGana auto carrera

-- 2.a) hacer que un auto corra determinado tiempo. 
--nueva distancia =  distancia que llevaba recorrida + ese tiempo * la velocidad a la que estaba yendo
aumentarDistancia :: Int->Auto->Auto
aumentarDistancia delta auto = auto{distancia = distancia auto +delta}

correr :: Int->Auto -> Auto
correr tiempo auto = aumentarDistancia (tiempo*distancia auto) auto 
--2.b)i A partir de un modificador de tipo Int -> Int, queremos poder alterar la velocidad de un auto 
--para que su velocidad final sea la resultante de usar dicho modificador con su velocidad actual.

type Modificador = Int->Int
alterarVelocidad :: Modificador -> Auto -> Auto
alterarVelocidad modificador auto = auto{velocidad = modificador (velocidad auto)}
--2.b)ii Usar alterarVeloocidad para bajar la velocidad de un auto en una cantidad indicada 
--de modo que se le reste a la velocidad actual la cantidad indicada, 
--y como mínimo quede en 0, ya que no es válido que un auto quede con velocidad negativa.
bajarVelocidad :: Int -> Auto  -> Auto
bajarVelocidad cant auto = alterarVelocidad (cant-) auto

--3 representar los powerus descriptos, haciendo sencillo agregar mas si fuera necesario. 
type Powerup = Auto -> Carrera-> Carrera
afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

--terremoto: los autos que están cerca bajan su velocidad en 50.
terremoto :: Powerup
terremoto auto = afectarALosQueCumplen (estaCerca auto) (bajarVelocidad 50 )

--miguelitos:  indicar en cuánto deberán bajar la velocidad los autos que se vean afectados por su uso. 
--Los autos a afectar son aquellos a los cuales el auto que gatilló el power up les vaya ganando.
miguelito :: Int ->Powerup
miguelito cantidad auto = afectarALosQueCumplen (not.leGana auto) (bajarVelocidad cantidad)
{-jet pack: este poder debe afectar solamente al auto que gatilló el poder. ....................................................
El jet pack tiene un impacto que dura una cantidad limitada de tiempo, el cual se espera poder configurar.......................
El auto afectado duplica su velocidad actual, ..................................................................................
luego corre durante el tiempo indicado  y finalmente su velocidad vuelve al valor que tenía antes de que se active el poder.
Por simplicidad, no se espera que los demás autos que participan de la carrera también avancen en ese tiempo.-}


autoJP :: Int -> Auto  -> Auto
autoJP tiempo auto = alterarVelocidad (div 2)(correr tiempo (alterarVelocidad (2*) auto)) 

jetpack :: Int -> Powerup -- en los tipos de afectar, a es AUTO...
jetpack tiempo auto = afectarALosQueCumplen (not.sonDistintos auto) (autoJP tiempo)


--4 
type Evento = Carrera -> Carrera 

type Color = String
type Posicion = (Int, Color)
posicionDeUnAuto :: Carrera -> Auto -> Posicion
posicionDeUnAuto carrera auto = (puesto auto carrera , color auto)

obtenerPosiciones :: Carrera -> [Posicion]
obtenerPosiciones carrera = map (posicionDeUnAuto carrera) carrera

simularCarrera :: Carrera -> [Evento] -> [Posicion]
simularCarrera carrera = obtenerPosiciones . foldr ($) carrera


--b) correnTodos que hace que todos los autos que están participando de la carrera corran durante un tiempo indicado.
--usaPowerUp que a partir de un power up y del color del auto que gatilló el poder en cuestión, 
--encuentre el auto correspondiente dentro del estado actual de la carrera para usarlo y produzca los efectos esperados para ese power up.

correnTodos :: Int -> Evento
correnTodos tiempo = map (correr tiempo)

usaPowerup :: Powerup -> Color -> Evento
usaPowerup powerup colorAuto carrera = powerup (buscarPorColor colorAuto carrera) carrera

buscarPorColor :: Color -> Carrera -> Auto
--buscarPorColor colorAuto = head.filter((==colorAuto).color)
buscarPorColor = undefined
