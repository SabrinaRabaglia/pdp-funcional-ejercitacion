---------------------parte 1------------------------------

data Alfajor = Alfajor {
    relleno :: [String],
    peso:: Int,
    dulzor :: Int,
    nombre :: String
} deriving (Show, Eq)

jorgito :: Alfajor
jorgito = Alfajor ["Dulce de leche"] 80 8 "Jorgito"
havanna :: Alfajor
havanna = Alfajor ["Mousse", "Mousse"] 60 12 "Havanna"
capitanDelEspacio :: Alfajor
capitanDelEspacio = Alfajor ["Dulce de leche"] 40 12 "Capitan del espacio"

coeficienteDeDulzor :: Alfajor -> Int
coeficienteDeDulzor alfajor = div (dulzor alfajor) (peso alfajor)

precio :: Alfajor -> Int
precio alfajor = (peso alfajor )*2+sum(map precioCapa (relleno alfajor) )

precioCapa :: String -> Int
precioCapa nombre 
 | nombre == "Dulce de leche" = 12 
 | nombre == "Mousse" = 15
 | nombre == "Fruta" =  10
 | otherwise = 0 

esPotable :: Alfajor -> Bool
esPotable alfajor = (tieneCapas alfajor) && (capasDelMismoSabor alfajor) && (muyDulce alfajor)


tieneCapas :: Alfajor -> Bool
tieneCapas = not.null.relleno

capasDelMismoSabor :: Alfajor -> Bool
capasDelMismoSabor alfajor = all (\x -> x == head (relleno alfajor)) (relleno alfajor)
 
muyDulce :: Alfajor -> Bool
muyDulce alfajor = dulzor alfajor > 1

---------------------parte 2------------------------------
abaratar :: Alfajor -> Alfajor
abaratar alfajor = Alfajor {relleno = relleno alfajor , peso = max (peso alfajor -10) 5, dulzor = dulzor alfajor -7,nombre=nombre alfajor }

renombrar :: String -> Alfajor -> Alfajor
renombrar nuevoNombre alfajor= alfajor {nombre = nuevoNombre}

agregarCapa :: String -> Alfajor -> Alfajor
agregarCapa capa alfajor = alfajor {relleno = relleno alfajor ++ [capa]}

hacerPremium :: Alfajor -> Alfajor
hacerPremium alfajor 
 | esPotable alfajor = renombrar ((nombre (agregarCapa (head (relleno alfajor)) (alfajor))) ++ " premium") alfajor
 | otherwise = alfajor

hacerPremiumGrado :: Int  -> Alfajor -> Alfajor
hacerPremiumGrado 1 alfajor = hacerPremium alfajor
hacerPremiumGrado n alfajor = hacerPremiumGrado (n-1) (hacerPremium alfajor)

jorgitito :: Alfajor 
jorgitito = renombrar "Jorgitito" (abaratar (jorgito))

jorgelin :: Alfajor
jorgelin = renombrar "Jorgelin" (agregarCapa "Dulce de leche" jorgito)

capitanCostaACosta :: Alfajor
capitanCostaACosta = renombrar "Capitan del Espacio de costa a costa" (hacerPremiumGrado 4 (abaratar capitanDelEspacio))

---------------------parte 3------------------------------
type Criterio = Alfajor ->Bool
data Cliente = Cliente {
    nombreCliente :: String,
    dinero :: Int,
    gustos :: Criterio, --Alfajor -> Bool
    alfajoresComprados :: [Alfajor]
}
emi :: Cliente
emi = Cliente "Emi" 120 criterioEmi []
tomi :: Cliente
tomi = Cliente "Tomi" 100 criterioTomi []
dante :: Cliente
dante = Cliente "Dante" 200 criterioDante []
juan :: Cliente
juan = Cliente "Juan" 500 criterioJuan []
------------- criterios
isInfixOf :: String -> String -> Bool-- me dice si la 1er lista esta contenida la segunda. 
isInfixOf = undefined 
criterioEmi :: Criterio
criterioEmi = buscaMarca "Capitan del espacio"
criterioTomi :: Criterio
criterioTomi  a =  ((&& esDulcero a).esPretencioso)a
criterioDante :: Criterio
criterioDante alfajor = (not (esPotable alfajor)) && (sinTalGusto "Dulce de leche"  alfajor)
criterioJuan :: Criterio
criterioJuan a = (buscaMarca "Jorgito" a) && (esPretencioso a) && (sinTalGusto "Mousse" a)
-- juan : solo jorgito , es pretencioso(solo le gustan los premium) y anti mousse (no come mouse)
esPretencioso :: Criterio 
esPretencioso alfajor = isInfixOf "premium" (nombre alfajor)
buscaMarca :: String -> Criterio
buscaMarca marca alfajor = isInfixOf marca (nombre alfajor)
esDulcero :: Criterio
esDulcero = (>2).coeficienteDeDulzor
sinTalGusto :: String -> Criterio
sinTalGusto tipoCapa a = not (elem tipoCapa (relleno a))

leGustaUnAlfajor :: Cliente -> Alfajor  -> Bool
leGustaUnAlfajor  c a  = gustos c a
alfajoresQueLeGustan :: Cliente -> [Alfajor] -> [Alfajor] 
alfajoresQueLeGustan cliente lista= filter (leGustaUnAlfajor cliente) lista

-- aca compra cualquier alfajor, no solo los que cumplen su criterio.  
comprarAlfajor :: Cliente -> Alfajor -> Cliente
comprarAlfajor cliente alfajor 
 | (precio alfajor) <= (dinero cliente) = agregarAlfajor alfajor (restarDinero alfajor cliente)
 | otherwise = cliente

agregarAlfajor :: Alfajor -> Cliente-> Cliente
agregarAlfajor alfajor cliente = cliente {alfajoresComprados = (alfajor : (alfajoresComprados cliente)) }

restarDinero :: Alfajor -> Cliente -> Cliente
restarDinero alfajor cliente = cliente {dinero = dinero cliente - (precio alfajor)}
-- que un cliente compre todos los alfajores que cumplen con s criterio de una lista de alajores. 
compraTodos :: Cliente -> [Alfajor] -> Cliente -- filtro los que cumplen y hago un foldeo para comprarlos todos. 
compraTodos cliente lista = foldl  comprarAlfajor cliente (alfajoresQueLeGustan cliente lista) -- esto retorna la lista de alfajores que cumplen on el criterio.
