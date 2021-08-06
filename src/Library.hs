module Library where
import PdePreludat

data Gimnasta = Gimnasta {
    nombre :: String 
    ,edad  ::  Number 
    ,peso ::  Number  
    ,coefTonificion ::  Number }deriving(Show, Eq)

pancho = Gimnasta "Francisco" 40.0 120.0 1.0
andres = Gimnasta "Andy" 22.0 80.0 6.0



relax :: Number->Ejercicio
relax mins = id

--Punto 1 

estaSaludable :: Gimnasta->Bool 
estaSaludable aGimnasta = (not.estaObeso) aGimnasta && coefTonificion aGimnasta >5

estaObeso :: Gimnasta->Bool
estaObeso = (>100).peso
 
-- 2 Hacer que gimnaste queme una cant de calorias , q hace q baje peso
--type Calorias = Number

quemarCalorias :: Number->Gimnasta->Gimnasta
quemarCalorias calorias aGimnasta 
    | estaObeso aGimnasta = bajarPeso  (calorias /150) aGimnasta
    | edad aGimnasta >30 && calorias > 200 = bajarPeso  1 aGimnasta
    | otherwise = bajarPeso  (calorias / (peso aGimnasta * edad aGimnasta)) aGimnasta

bajarPeso :: Number->Gimnasta->Gimnasta
bajarPeso n aGimnasta  = aGimnasta{ peso = peso aGimnasta - n}

-- 3
type Ejercicio = Gimnasta->Gimnasta
--a
caminataEnCinta :: Number->Ejercicio
caminataEnCinta mins =  quemarCalorias (5* mins)
--a2
entrenamientoEnCinta :: Number->Ejercicio
entrenamientoEnCinta mins =  quemarCalorias (((6 + velocidadMax mins) /2) * mins)

velocidadMax:: Number->Number
velocidadMax mins =   truncate (mins / 5) + 6
--b
pesas :: Number->Number->Ejercicio
pesas kilos mins aGimnasta 
    |mins >10 = tonifica (kilos / 10)  aGimnasta
    |otherwise = aGimnasta

tonifica :: Number->Gimnasta->Gimnasta
tonifica n aGimnasta= aGimnasta { coefTonificion = coefTonificion aGimnasta + n}

--c
colina :: Number->Number->Ejercicio
colina inclinacion mins  = quemarCalorias (2 * inclinacion * mins) 

--d 
montania :: Number->Number->Ejercicio
montania inclinacion mins = tonifica 1  .colina (inclinacion + 3) (mins/2) . colina inclinacion (mins/2)


-- Punto 4
data Rutina = UnaRutina {
    nombreR :: String
    ,duracion :: Number
    ,ejercicios :: [Ejercicio]
    }deriving (Show,Eq)

    {-
Dada una Rutina (es un Data con un nombre, duración total y lista de ejercicios específicos) y un gimnasta, obtener al gimnasta luego de realizar la rutina. La cantidad de minutos dedicada a cada ejercicio es la misma. 
    -Mostrar un ejemplo de uso usando todos los ejercicios del punto anterior. 
    -Resolverlo usando recursividad.
    -Hacer otra solución usando fold.
-}
--rutinaEjemplo :: [Ejercicio]
rutinaJueves = UnaRutina{
    nombreR = "rutina rompehuesos"
    ,duracion = 60
    ,ejercicios = [caminataEnCinta 10, entrenamientoEnCinta 10 , pesas 20 10,colina 5 10 , montania 5 10]
}

--CON FOLD 
hacerRutina :: Gimnasta->Rutina->Gimnasta
hacerRutina aGimnasta aRutina =  foldl hacerEjercicio aGimnasta (ejercicios aRutina)

hacerEjercicio :: Gimnasta->Ejercicio->Gimnasta
hacerEjercicio aGimnasta ejercicio = ejercicio aGimnasta

--rutinaEjemplo = [caminataEnCinta 10, entrenamientoEnCinta 10 , pesas 20 10,colina 5 10 , montania 5 10]

-- CON RECURSION 
hacerRutina' :: Rutina->Gimnasta->Gimnasta
hacerRutina' rutina aGimnasta =   completarEjerc'  (ejercicios rutina) aGimnasta

completarEjerc' :: [Ejercicio]->Gimnasta->Gimnasta
completarEjerc' [] aGimnasta = aGimnasta --Caso en el que no tiene ejercicios XD o los termino todos
completarEjerc' (ejrprimero:ejercicios) aGimnasta =  completarEjerc' ejercicios (hacerEjercicio  aGimnasta (ejrprimero)) 


--b
{-Dada una rutina y un gimnasta, obtener el resumen de rutina que es una tupla con el nombre de la misma,
 los kilos perdidos y la tonificación ganada por dicho gimnasta al realizarla.
-}
type Resumen = (String, Number , Number)

resultados :: Rutina->Gimnasta->Resumen
resultados rutina gimnasta = (nombreR rutina ,kilosPerdidos rutina ,) 