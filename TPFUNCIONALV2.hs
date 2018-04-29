import Text.Show.Functions

--PUNTO 1
type AcumuladorA = Int
type AcumuladorB = Int
 
data Microprocesador=UnMicroprocesador {
     memoria :: [Int],
     acumuladorA :: AcumuladorA,
     acumuladorB :: AcumuladorB,
     programCounter :: Int,
     etiqueta ::String
     } deriving (Show)

{-Se decidió usar data porque tengo varios elementos (de distintos tipos) q corresponden a un microprocesador
  y es mucho mas reconocible q por ejemplo una tupla,por su expresividad -}

xt8088 :: Microprocesador	
xt8088= UnMicroprocesador {memoria=[],acumuladorA=0,acumuladorB=0,programCounter=0,etiqueta=""}

fp20 :: Microprocesador
fp20 =UnMicroprocesador{memoria=[],acumuladorA=7,acumuladorB=24,programCounter=0,etiqueta=""}

--PUNTO 2  
{-Interviene  el concepto de composicion dado que el programCounter aumentara en 1 
  compongo nop tres veces-}
nop:: Microprocesador->Microprocesador
nop = sumarUno 

sumarUno:: Microprocesador->Microprocesador
sumarUno micro=micro{programCounter=(programCounter micro) +1}

--PUNTO 3
{-Se decicidio continuar  con el nombre sumarUno en vez de NOP por expresividad-}
lodv ::Int->Microprocesador->Microprocesador
lodv a micro=sumarUno(micro{acumuladorA=a})

---esta funcion hace lo de la instrucción SWAP
swap:: Microprocesador->Microprocesador
swap micro=sumarUno(micro{acumuladorA =acumuladorB micro,acumuladorB=acumuladorA micro})

add :: Microprocesador->Microprocesador
add micro=sumarUno(micro{acumuladorA=(acumuladorA micro+acumuladorB micro),acumuladorB=0})

-----Punto 4----

at8086 :: Microprocesador
at8086= UnMicroprocesador {memoria=[1..20],acumuladorA=0,acumuladorB=0,programCounter=0,etiqueta=""}

insertarEnPos a b micro=sumarUno (micro{memoria=(take (a-1) (memoria micro)) ++ [b] ++ (drop a (memoria micro))})

--agregar pos lista valor=take pos lista ++(valor:drop pos lista)
str :: Int->Int->Microprocesador->Microprocesador
str  a b  =(insertarEnPos a b)

--str1 a b micro=micro{memoria=agregar a (memoria micro) b}
 
lod  a micro
	   |	esListaVacia (memoria micro) = sumarUno (micro{acumuladorA = 0})
	   | 	otherwise=sumarUno (micro{acumuladorA=(memoria micro) !! (a-1)})
 --otherwise=(ponerElementosDelaPos a).(str 2 0).(str 1 2) micro
--ponerElemDelaPos a micro={acumuladorA=(memoria micro) !! (a-1)}
esListaVacia lista=(length lista) == 0

divide micro
  |   (==0) (acumuladorB micro)=sumarUno(micro{etiqueta="DIVISION BY ZERO"})
  |	otherwise=sumarUno(micro{acumuladorA=div (acumuladorA micro) (acumuladorB micro)})








 





