module Library where
import PdePreludat

-- Punto 1
-- ingrediente, tiene calorias
-- 1) type Ingrediente = Number (o Caloria)
-- 2) data
data Ingrediente = Ingrediente {
  nombreIngrediente :: String,
  calorias :: Number
} deriving (Show)

-- 3) una tupla
-- type Ingrediente = (String, Number)

----------------------------------------------------------------
-- "Esto a mí así no me sirve" (Usuario contemporáneo)
----------------------------------------------------------------

type Porcentaje = Number
type Gramos = Number

-- chocolate
data Chocolate = Chocolate {
  nombre :: String,
  porcentajeCacao :: Porcentaje,
  porcentajeAzucar :: Porcentaje,
  gramaje :: Gramos,
  ingredientes :: [Ingrediente]
} deriving (Show)




-- Punto 1
precio :: Chocolate -> Number
precio chocolate
-- los chocolates se calculan como el gramaje del chocolate multiplicado por el precio premium. 
 | chocolateAmargo chocolate               = precioPremium chocolate * gramaje chocolate
-- Por el contrario, si tiene más de 4 ingredientes, el precio es de $8 por la cantidad de ingredientes que tiene. 
 | ((>4).cantidadDeIngredientes) chocolate = 8 * cantidadDeIngredientes chocolate
-- Caso contrario, el costo es de $1,5 por gramo
 | otherwise                               = 1.5 * gramaje chocolate

cantidadDeIngredientes :: Chocolate -> Number
cantidadDeIngredientes = length.ingredientes

-- los chocolates amargos (que tienen más de un 60% de cacao) 
chocolateAmargo :: Chocolate -> Bool
chocolateAmargo = ( > 60). porcentajeCacao

-- El precio premium varía si es apto para diabéticos
-- en cuyo caso es de $8 por gramo o bien es de $5 por gramo.
precioPremium :: Chocolate -> Number
precioPremium chocolate
  | aptoParaDiabeticos chocolate = 8
  | otherwise                    = 5

-- apto para diabéticos (es decir que el chocolate tiene un porcentaje cero de azúcar)
aptoParaDiabeticos :: Chocolate -> Bool
aptoParaDiabeticos  = (== 0) . porcentajeAzucar

-- Punto 2
-- Cuándo un chocolate esBombonAsesino que ocurre cuando tiene algún ingrediente de
-- más de 200 calorias.
esBombonAsesino :: Chocolate -> Bool
esBombonAsesino = any ((>=200).calorias).ingredientes

-- También queremos saber el totalCalorias para un chocolate que es la sumatoria de
-- los aportes de cada ingrediente.
totalCalorias :: Chocolate -> Number
-- Alternativa 1
-- totalCalorias chocolate = (foldr ((+).calorias) 0.ingredientes) chocolate 
-- totalCalorias chocolate = (foldr1 (+).map calorias.ingredientes) chocolate 
totalCalorias chocolate = (sum .map calorias.ingredientes) chocolate

-- Y por último, dada una caja de chocolates, queremos tomar los chocolates aptoParaNinios
-- donde separamos 3 chocolates que no sean bombones asesinos, sin importar cuáles.
type Caja = [Chocolate]

aptoParaNinios :: Caja -> Caja
aptoParaNinios = take 3.filter (not.esBombonAsesino)

-- Punto 3
-- Por ejemplo, el frutalizado permite agregarle como ingrediente una cierta cantidad de unidades 
-- de una fruta. Toda fruta tiene dos calorías por cada unidad.
type Transformacion = Chocolate -> Chocolate
type UnidadesDeFruta = Number 

frutalizado :: String -> UnidadesDeFruta -> Transformacion
frutalizado nombre unidades chocolate = agregarIngrediente nombre (unidades * 2) chocolate

agregarIngrediente :: String -> Number -> Transformacion
agregarIngrediente nombre calorias chocolate = chocolate {
  ingredientes = ingredientes chocolate ++ [Ingrediente nombre calorias]
}

-- Un clásico es el dulceDeLeche que agrega dicho ingrediente el cual siempre aporta 220 calorías. 
-- Además al nombre del chocolate le agrega al final la palabra "tentación": Por ejemplo 
-- el "Chocolate con almendras" pasa a ser "Chocolate con almendras tentación".
dulceDeLeche :: Transformacion
dulceDeLeche chocolate = agregarIngrediente "Dulce de leche" 220 chocolate {
  nombre = nombre chocolate ++ " tentación"
}

-- Otro famoso proceso es la celiaCrucera que dado un cierto porcentaje de azúcar, 
-- aumenta el nivel del mismo en el chocolate.
celiaCrucera :: Number -> Transformacion
celiaCrucera = agregarAzucar 

agregarAzucar :: Number -> Transformacion
agregarAzucar porcentajeAzucarAAgregar chocolate = chocolate {
  porcentajeAzucar = porcentajeAzucar chocolate + porcentajeAzucarAAgregar
}


-- Por último contamos con la embriagadora que para un determinado grado de alcohol aporta
-- como ingrediente Licor con una caloría por cada grado de alcohol, hasta un máximo de 30 calorías.
-- Es decir que si agregamos una bebida con 40 grados, son 30 calorías de licor. 
-- En cambio si ponemos una bebida con 20 grados, son 20 calorías aportadas. 
-- Además agrega un porcentaje de 20 en azúcar.

embriagadora :: Number -> Transformacion
embriagadora gradosDeAlcohol = agregarIngrediente "Licor" (min 30 gradosDeAlcohol) . agregarAzucar 20

-- Punto 4
-- Dar un ejemplo de una receta que conste de los siguientes procesos: 
-- agregar 10 unidades de naranja, dulce de leche y un licor de 32 grados.

type Receta = [Transformacion]
receta :: Receta
receta = [frutalizado "Naranja" 10 , dulceDeLeche , embriagadora 32]

-- Punto 5
-- Implementar la preparación de un chocolate que a partir de un determinado chocolate
-- tomado como base y una serie de procesos nos permite obtener el chocolate resultante. 
-- En este punto NO se puede utilizar recursividad.
preparacionChocolate :: Chocolate -> Receta -> Chocolate
preparacionChocolate = foldr ($)


-- Punto 6
-- 
-- Resolver la función hastaAcaLlegue que dada una persona y una caja de chocolates, devuelve los chocolates que puede comer. 
-- La persona comerá todos los chocolates que pueda hasta que llegue al nivel de saturación de calorías. 
-- Al mismo tiempo debe descartar los chocolates que tengan un ingrediente que rechace por el criterio de la persona.
-- Es decir, si tenemos 4 chocolates de 300, 400, 150 y 50 calorías respectivamente, y Juan tiene un límite de 800 calorías, la función debe devolver 
-- los tres primeros chocolates (por más que a Juan le hubiera encantado comer el último chocolate). 
-- Esto es porque cuando come el tercer chocolate suma 850 calorías que es más que el tope de 800 calorías.
-- Si tenemos 4 chocolates, todos de 100 calorías, en el segundo chocolate hay naranja y en los otros tres no. 
-- Recordemos que a Juan no le gusta la naranja y soporta 800 calorías. Si invocamos la función con 
-- Juan y estos 4 chocolates, nos debe devolver una lista con los chocolates que originalmente están en la 
-- posición 1, 3 y 4, descartando el chocolate de naranja y teniendo en cuenta que los tres bombones suman 300 calorías < 800 de tope.
-- Tener en cuenta que el criterio de aceptación depende de cada persona.

type CriterioDeAceptacionDeIngrediente = Ingrediente -> Bool

data Persona = Persona {
  nivelDeSaturacion :: Number,
  criterio :: CriterioDeAceptacionDeIngrediente 
}

hastaAcaLlegue :: Persona -> Caja -> Caja
hastaAcaLlegue _ [] = []
hastaAcaLlegue persona (chocolate:chocolates)
 | noQuiereComer persona chocolate = hastaAcaLlegue persona chocolates
 | (<=0).nivelDeSaturacion $ persona = []
 | otherwise = chocolate:hastaAcaLlegue (comer persona chocolate) chocolates  

comer :: Persona -> Chocolate -> Persona 
comer persona chocolate = persona {
  nivelDeSaturacion = nivelDeSaturacion persona - totalCalorias chocolate 
}

noQuiereComer :: Persona -> Chocolate -> Bool
noQuiereComer persona = any (not . criterio persona) . ingredientes

-- Punto 7
totalCalorias' :: Caja -> Number
totalCalorias' = foldr ((+) . totalCalorias) 0

chocoArroz :: Chocolate
chocoArroz = Chocolate "chocoArroz" 0.5 300 10 [Ingrediente "arroz" 1, Ingrediente "Benzoato de potasio" 2]

cajaInfinita :: Caja
cajaInfinita = chocoArroz : cajaInfinita






















