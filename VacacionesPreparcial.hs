--Ej 1
data Turista = Turista {
    cansancio :: Int,
    estres :: Int, 
    viajaSolo :: Bool, 
    idiomas :: [String]
} deriving (Show, Eq)
type Excursion = Turista -> Turista
ana :: Turista
ana = Turista 0 21 False ["español"] 
beto :: Turista
beto = Turista 15 15 True ["aleman"]
cathi :: Turista
cathi = Turista 15 15 True ["aleman","catalan"]
-- funciones que sirven para no repetir logica:
cambiarCansancio :: Int -> Turista -> Turista
cambiarCansancio cant tur = tur{cansancio = cansancio tur + cant}
cambiarEstres :: Int -> Turista -> Turista
cambiarEstres cant tur = tur{estres = estres tur + cant}

--Ej 2
irALaPlaya :: Excursion-- baja cansancio en 5 si estasolo o estres en 1 si esta acompañado
irALaPlaya tur
 | viajaSolo tur = cambiarCansancio (-5) tur 
 | otherwise = cambiarEstres (-1) tur

apreciarPaisaje:: String -> Excursion-- reduce estres en el largo del nombre de la cosa a apreciar. 
apreciarPaisaje nombreCosaAApreciar tur = cambiarEstres (-length nombreCosaAApreciar) tur  

agregarAListaSiNoEsta :: String -> [String] -> [String]
agregarAListaSiNoEsta palabra lista
 | elem palabra lista = lista -- si esa palabra ya esta en la lista no la agrega
 | otherwise = lista ++ [palabra]   -- si la palabra no esta en la lista la concatena 

salirAHablarIdioma :: String -> Excursion -- aprende el idioma y vuelve acompañado. 
salirAHablarIdioma idiom tur = Turista {cansancio=cansancio tur, estres= estres tur, viajaSolo= False, idiomas= agregarAListaSiNoEsta idiom (idiomas tur)} 
intensidadCaminata :: Int -> Int
intensidadCaminata tiempo = div tiempo 4
caminar :: Int -> Excursion -- aumenta cansancio y reduce estres segun la intensidad de la caminata. 
caminar tiempo = cambiarEstres (-intensidadCaminata tiempo) .cambiarCansancio (intensidadCaminata tiempo)
--caminar tiempo tur = cambiarEstres (-intensidadCaminata tiempo) (cambiarCansancio (intensidadCaminata tiempo) tur ) 

paseoEnBarco :: String -> Excursion   --resolver con pattern matching. el string es como esta la marea
paseoEnBarco marea tur 
 | marea == "Fuerte" = cambiarCansancio(10) (cambiarEstres(6) tur) -- si lo hago con composicion y point free no tipa. 
 | marea == "Moderado" = tur
 | marea == "Tranquila" = caminar 10 (apreciarPaisaje "mar" (salirAHablarIdioma "aleman" tur)  ) -- ver el temita de composicion
 |otherwise = undefined -- en e enunciado no dice nada, pero si cambiaran las condiciones podria haber otro caso
 -- (ademas asi no salta el warning xd)

bajarEstresPorcentual :: Int -> Turista -> Turista
bajarEstresPorcentual porciento turista = cambiarEstres (div (porciento * estres turista) 100) turista

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion exc tur =   exc (bajarEstresPorcentual 10 tur)
    
deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2 -- cuanto varió un indice dada una excursion

-- definir deltaExcursionSegun que a partir de un indice, un turista y una exurson determine cuanto vario ese indice
-- luego de que ese turista hiciera esa excursion. indice:: Turista -> Int Excursion :: Turista -> Turista
deltaExcursionSegun :: (Turista->Int)-> Turista -> Excursion -> Int
deltaExcursionSegun indice tur exc = deltaSegun indice (hacerExcursion exc tur) tur

-- una excursion es educativa si el tursita aprende unidioma
cantidadDeIdiomas :: Turista -> Int
cantidadDeIdiomas = length.idiomas 

esEducativa :: Turista -> Excursion  -> Bool
esEducativa tur = (>0).deltaExcursionSegun cantidadDeIdiomas tur

-- una excursion es desestresante si su nivel de estres baja en 3 o mas unidades. 
esDesestresante :: Turista -> Excursion-> Bool
esDesestresante tur = (<=3).deltaExcursionSegun estres tur

{-EJ3: TOURS
Modelar los tours. 
 -}
type Tour = [Excursion]

completo :: Tour 
completo = [caminar 20, apreciarPaisaje "cascada", caminar 40, irALaPlaya, salirAHablarIdioma "melmacquiano"]

ladoB :: Excursion -> Tour
ladoB exc = [paseoEnBarco "Tranquila", exc,caminar 120 ]

excIslaVecina :: String -> Excursion
excIslaVecina clima
 | clima == "Fuerte" = apreciarPaisaje "lago"
 | otherwise = irALaPlaya

islaVecina :: String -> Tour
islaVecina clima = [paseoEnBarco clima, excIslaVecina clima, paseoEnBarco clima]
--a. hacer que el turista haga un tour: aumenta el estres segun la cantidad de excursiones y luego las hace en orden
cantidadExcursiones :: Tour -> Int
cantidadExcursiones tour = length tour
hacerTour :: Turista -> Tour -> Turista
hacerTour turista tour= foldl (flip hacerExcursion) (cambiarEstres (cantidadExcursiones tour)turista) tour -- de turista en turista


-- b. Dado n conjunto de tours, saber si alguno convence al turista.(hay alguna excursion desestresante que dea al turista acompañado
propuestaConvincente :: Turista -> [Tour] -> Bool
propuestaConvincente turista  =  any (tourConvincente turista) 

--listaExcursionesDesestresantes :: Turista -> Tour -> Tour
--listaExcursionesDesestresantes tur = filter (esDesestresante tur) 

tourConvincente :: Turista -> Tour -> Bool -- exc desestresante que dea al turista acompañado. 
tourConvincente turista tour=   any (esDesestresante turista) (filter (dejaAcompaniado turista) tour)

dejaAcompaniado :: Turista->Excursion->Bool 
dejaAcompaniado tur exc =  viajaSolo (hacerExcursion exc tur)==False

--c. Saber la efectividad de un tour para un conunto de turistas, sumatoria de la espiritualidad recibida de cada turista 
--a quienes les resultó convincente el tour. La espiritualidad de un turista es la suma de las pérdidas de stress y cansancio tras el tour.
sumaEstresCansancio :: Turista ->Int
sumaEstresCansancio tur = estres tur + cansancio tur

espiritualidadTurista :: Turista->Tour->Int
espiritualidadTurista turista tour= deltaSegun sumaEstresCansancio (hacerTour turista tour) turista 

-- sumatoria de espiritualidades delos turistas a los que les resulto convincente el tour
efectividad :: Tour->[Turista]->  Int
efectividad tour grupo = sum (map (sumaEstresCansancio) (convencidos tour grupo))

convencidos ::Tour-> [Turista] -> [Turista]
convencidos tour = filter  ((flip tourConvincente)tour) -- retorna la lista de los convencidos del tour
