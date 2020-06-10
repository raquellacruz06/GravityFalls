module Lib where
import Text.Show.Functions

laVerdad = True



-------Esta primera opcion sólo funciona si los fantasmas sólo reciben items como condicion para superarse
{-Modelar a las personas, de las cuales nos interesa la edad, cuáles son los ítems que tiene y la cantidad de 
experiencia que tiene; 
y a las criaturas teniendo en cuenta lo descrito anteriormente, y lo que queremos hacer en el punto siguiente.-}

{--data Criatura = UnaCriatura 
{-El siempredetras: la peligrosidad de esta criatura legendaria es 0, ya que no le hace nada a la persona que está 
acechando, es tan inofensivo que nunca nadie pudo afirmar que estaba siendo acechado. Sin embargo, no hay nada que 
se pueda hacer para que te deje en paz.
Los gnomos: individualmente son inofensivos, pero se especializan en atacar en grupo. La peligrosidad es 2 elevado a 
la cantidad de gnomos agrupados. Una persona puede deshacerse de un grupo de gnomos si tiene un soplador de hojas entre
 sus ítems.
Los fantasmas: se categorizan del 1 al 10 dependiendo de qué tan poderosos sean, y el nivel de peligrosidad es 
esa categoría multiplicada por 20. Cada fantasma tiene un asunto pendiente distinto, con lo cual se debe indicar
 para cada uno qué tiene que cumplir la persona para resolver su conflicto.-}

-----Opcion sólo considerando los Items como condicion
data Criatura = UnaCriatura {peligrosidad :: Int,
                            seLograSuperar :: String-> Bool } deriving Show
                        
siempredetras :: Criatura
siempredetras = UnaCriatura {peligrosidad = 0, seLograSuperar = superarSiempredetras} 

superarSiempredetras :: String -> Bool
superarSiempredetras _ = False

gnomos :: Int ->  Criatura
gnomos 1 = UnaCriatura {peligrosidad= 0, seLograSuperar = (\x-> False)}
gnomos cantidadGnomos = UnaCriatura {peligrosidad= (2^ cantidadGnomos), seLograSuperar = superarGnomos}

superarGnomos :: String-> Bool
superarGnomos "soplador de hojas" = True

fantasmas :: Int ->  Criatura
fantasmas categoriaPoder = UnaCriatura {peligrosidad = categoriaPoder * 20, seLograSuperar = superarFantasmas }

superarFantasmas :: 

--fantasmas :: Int -> [(Persona -> Bool)] -> Criatura
--fantasmas categoriaPoder condiciones = UnaCriatura {peligrosidad = categoriaPoder * 20, seLograSuperar = superarFantasmas condiciones}

superarFantasmas :: String-> String -> Bool
superarFantasmas itemNecesario itemPoseido = itemNecesario == itemPoseido

{-Hacer que una persona se enfrente a una criatura, que implica que si esa persona puede deshacerse de ella gane 
tanta experiencia como la peligrosidad de la criatura, o que se escape
 (que le suma en 1 la experiencia, porque de lo visto se aprende) en caso de que no pueda deshacerse de ella.-}

enfrentarCriatura :: Persona -> Criatura -> Persona
enfrentarCriatura persona criatura 
    |(seLograSuperar criatura.item) persona = cambiarExperiencia (\x-> peligrosidad criatura) persona
    | otherwise = cambiarExperiencia (+1) persona

cambiarExperiencia :: (Int -> Int) -> Persona -> Persona
cambiarExperiencia funcion persona = persona {experiencia = (funcion.experiencia) persona}


 --Determinar cuánta experiencia es capaz de ganar 
 --una persona luego de enfrentar sucesivamente a un grupo de criaturas.

experienciaGanada :: Persona -> [Criatura] -> Int
experienciaGanada persona criaturas = experiencia  (foldl enfrentarCriatura persona criaturas)-}

 ----Otra opcion
data Persona = UnaPersona { edad :: Int,
                            item :: String,
                            experiencia :: Int } deriving (Show, Eq)

data Criatura = UnaCriatura {peligrosidad :: Int,
                            seLograSuperar :: Persona-> Bool } deriving Show
                        
siempredetras :: Criatura
siempredetras = UnaCriatura {peligrosidad = 0, seLograSuperar = superarSiempredetras} 

superarSiempredetras :: Persona -> Bool
superarSiempredetras _ = False

gnomos :: Int ->  Criatura
gnomos 1 = UnaCriatura {peligrosidad= 0, seLograSuperar = (\x-> False)}
gnomos cantidadGnomos = UnaCriatura {peligrosidad= (2^ cantidadGnomos), seLograSuperar = superarGnomos}

superarGnomos :: Persona-> Bool
superarGnomos persona = item persona == "soplador de hojas" 

fantasmas :: Int ->  [Persona -> Bool]-> Criatura
fantasmas categoriaPoder condiciones = UnaCriatura {peligrosidad = categoriaPoder * 20, seLograSuperar = superarFantasmas condiciones }

superarFantasmas :: [Persona -> Bool] -> Persona-> Bool
superarFantasmas condiciones persona = all (aplicarCondicion persona) condiciones 

aplicarCondicion :: Persona-> (Persona -> Bool) -> Bool
aplicarCondicion persona condicion = condicion persona

--fantasmas :: Int -> [(Persona -> Bool)] -> Criatura
--fantasmas categoriaPoder condiciones = UnaCriatura {peligrosidad = categoriaPoder * 20, seLograSuperar = superarFantasmas condiciones}

--superarFantasmas :: String-> String -> Bool
--superarFantasmas itemNecesario itemPoseido = itemNecesario == itemPoseido

{-Hacer que una persona se enfrente a una criatura, que implica que si esa persona puede deshacerse de ella gane 
tanta experiencia como la peligrosidad de la criatura, o que se escape
 (que le suma en 1 la experiencia, porque de lo visto se aprende) en caso de que no pueda deshacerse de ella.-}

enfrentarCriatura :: Persona -> Criatura -> Persona
enfrentarCriatura persona criatura 
    |seLograSuperar criatura persona = cambiarExperiencia (\x-> peligrosidad criatura) persona
    | otherwise = cambiarExperiencia (+1) persona

cambiarExperiencia :: (Int -> Int) -> Persona -> Persona
cambiarExperiencia funcion persona = persona {experiencia = (funcion.experiencia) persona}


 --Determinar cuánta experiencia es capaz de ganar 
 --una persona luego de enfrentar sucesivamente a un grupo de criaturas.

experienciaGanada :: Persona -> [Criatura] -> Int
experienciaGanada persona criaturas = experiencia  (foldl enfrentarCriatura persona criaturas)

{-Mostrar un ejemplo de consulta para el punto anterior incluyendo las siguientes criaturas: al siempredetras, 
a un grupo de 10 gnomos, un fantasma categoría 3 que requiere que la persona tenga menos de 13 años y un disfraz de oveja
 entre sus ítems para que se vaya y un fantasma categoría 1 que requiere que la persona tenga más de 10 de experiencia.-}

criaturasDeEjemplo :: [Criatura] 
criaturasDeEjemplo = [siempredetras, (gnomos 10), (fantasmas 3 [(<13).edad, (== "disfraz de oveja").item]),(fantasmas 1 [(>10).experiencia]) ]

personajeDeEjemplo :: Persona
personajeDeEjemplo = UnaPersona 2 "Espadita" 1

---Parte 2 Mensajes Ocultos
{-Hacer una función abecedarioDesde :: Char -> [Char] que retorne las letras del abecedario empezando por 
la letra indicada. O sea, abecedarioDesde 'y' debería retornar 'y':'z':['a' .. 'x'].
Hacer una función desencriptarLetra :: Char -> Char -> Char que a partir una 
letra clave (la que reemplazaría a la a) y la letra que queremos desencriptar, retorna 
la letra que se corresponde con esta última en el abecedario que 
empieza con la letra clave. Por ejemplo: desencriptarLetra 'x' 'b' retornaría 'e'.-}

--abecedarioDesde letra = ordenarSegun (>) (letra:abecedario)
--ordenarSegun criterio (x:xs) = 
--(ordenarSegun criterio . filter (not . criterio x)) xs ++ [x] ++ (ordenarSegun criterio . filter (criterio x)) xs

abecedario :: [Char]
abecedario = ['a'..'z']

abecedarioDesde :: Char -> [Char]
abecedarioDesde letra = (filter (>=letra) abecedario) ++ (filter (<letra)abecedario)

desencriptarLetra :: Char -> Char -> Char
desencriptarLetra letraInicial letraFinal = (fst.head . filter ((== letraFinal).snd) . zip abecedario . abecedarioDesde) letraInicial
---Mejorar Abstracciones

{-Definir la función cesar :: Char -> String -> String que recibe la letra clave y un texto encriptado y retorna todo el texto desencriptado, 
teniendo en cuenta que cualquier caracter del mensaje
 encriptado que no sea una letra (por ejemplo '!') se mantiene igual. Usar zipWithIf para resolver este problema.-}


--between n m x = elem x [n .. m]

--desencriptarLetra :: Char -> Char -> Char
--desencriptarLetra letraInicial  letraFinal =  abecedario !! (restar1.length.abecedarioDesdeHasta letraInicial) letraFinal
--abecedarioDesdeHasta :: Char -> Char -> [Char]
--abecedarioDesdeHasta letraInicial letraFinal = filter (between letraInicial letraFinal ) (abecedarioDesde letraInicial)
--restar1 numero = numero +(-1) 
