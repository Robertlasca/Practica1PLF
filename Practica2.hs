--1. Ejercicio: firstToEnd
--Definir la función firstToEnd tal que (firstToEnd xs) es una lista donde el primer
--elemento de xs, pasa a ser el último elemento de la nueva lista
firstToEnd :: (Ord a)=>[a]->[a]
firstToEnd []=error "lista vacia"
firstToEnd [x]=[x]
firstToEnd (x:xs)=xs++[x]

--2. Ejercicio : minAndMax
--Definir la función minAndMax tal que (minAndMax xs) es una lista con únicamente 2
--elementos (elemento mínimo de xs y elemento máximo de xs), donde xs es una lista
minAndMax :: (Ord a)=>[a]->[a]
minAndMax []=error "lista vacia"
minAndMax [x]=error "lista de un elemento"
minAndMax (xs)=[minimum xs]++[maximum xs] 

--3. Ejercicio: minorsFirstElement
--Definir la función minorsFirstElement tal que (minorsFirstElement xs) es una lista con
--los elementos menores al primer elemento de xs, donde xs es una lista.(El primer
--elemento se ignora)
minorsFirstElement:: (Ord a)=>[a]->[a]
minorsFirstElement[]=error "lista vacia"
minorsFirstElement[x]=error "lista de un elemento"
minorsFirstElement(xs)=filter (<head xs) xs

--4. Ejercicio: greaterOrEqualFirstElement
--Definir la función greaterOrEqualFirstElement tal que (greaterOrEqualFirstElement
--xs) es una lista con los elementos mayores o iguales al primer elemento de xs,
--donde xs es una lista.(El primer elemento se ignora)
greaterOrEqualFirstElement ::(Ord a)=>[a]->[a]
greaterOrEqualFirstElement[]=error "lista vacia"
greaterOrEqualFirstElement[x]=error "lista de un elemento"
greaterOrEqualFirstElement(x:xs)=filter (>= x) xs

--5. Ejercicio: minorsToSumFirstAndSecondElem
--Definir la función minorsToSumFirstAndSecondElem tal que
--(minorsToSumFirstAndSecondElem xs) es una lista con los elementos menores a la
--suma del primer y segundo elemento de xs (sin tomar en cuenta los primeros 2
--elementos), donde xs es una lista.
minorsToSumFirstAndSecondElem ::(Integral a)=>[a]->[a]
minorsToSumFirstAndSecondElem []=error "lista vacia"
minorsToSumFirstAndSecondElem [x]=error "lista de un elemento"
minorsToSumFirstAndSecondElem [x,y]=error "lista de un elemento"
minorsToSumFirstAndSecondElem (x:y:xs)=filter (< numero) xs
    where numero = x+y

--6. Ejercicio: listSumDuplaToList
--Definir la función listSumDuplaToList tal que (listSumDuplaToList xs) es una lista en
--la que cada elemento es la suma de los elementos de cada dupla, donde xs es una
--lista de duplas
listSumDuplaToList  ::(Integral a)=>[(a,a)]->[a]
listSumDuplaToList []=[]
listSumDuplaToList ((x,y):xs)=  [x+y] ++ listSumDuplaToList xs

--7. Ejercicio: listMultTripletaToList
--Definir la función listMultTripletaToList tal que (listMultTripletaToList xs) es una lista
--en la que cada elemento es la multiplicación de los elementos de cada tripleta,
--donde xs es una lista de tripletas.
listMultTripletaToList ::(Integral a)=>[(a,a,a)]->[a]
listMultTripletaToList []=[]
listMultTripletaToList ((x,y,z):xs)=  [x*y*z] ++ listMultTripletaToList xs

--8. Ejercicio: changeFstToSnd
--Definir la función changeFstToSnd tal que (changeFstToSnd xs) es una lista en
--donde los elementos de una dupla cambian de posición, donde xs es una lista de
--duplas.
changeFstToSnd ::(Integral a)=>[(a,a)]->[(a,a)]
changeFstToSnd []=[]
changeFstToSnd ((x,y):xs)=  [(y,x)] ++ changeFstToSnd xs

--9. Ejercicio: sumVectors
--Definir la función sumVectors tal que (sumVectors xs) es un vector resultante de la
--suma de los diferentes vectores de xs, donde xs es una lista de duplas.
sumPrimerElement :: (Integral a ) => [(a,a)] -> a
sumPrimerElement []= 0
sumPrimerElement  ((x,y):xs) =x + sumPrimerElement xs 

sumSegundoElement :: (Integral a ) => [(a,a)] -> a
sumSegundoElement []=0
sumSegundoElement ((x,y):xs) = y + sumSegundoElement xs 

sumVectors :: (Integral a)=>[(a,a)]->(a,a)
sumVectors []= error "Debes ingresar tuplas"
sumVectors x = (sumPrimerElement x , sumSegundoElement x)
--10. Ejercicio: dividers
--Definir la función dividers tal que (dividers n) es una lista de los divisores de n, donde
--n es un número.
dividers :: Int -> [Int]
dividers n= [x | x <-[1..n],mod n x ==0]

--11. Ejercicio: primeNumbers
--Definir la función primeNumbers tal que (primeNumbers n) es una lista con los
--números primos existentes de 1 a n, donde n es un número.
isPrimo::Int ->Bool
isPrimo n = length (dividers n) <= 2

primeNumbers:: Int -> [Int]
primeNumbers n= [x |x <-[2..n],isPrimo x]

--12. Ejercicio:infinitePrimeNumbers
--Definir la función infinitePrimeNumbers tal que (infinitePrimeNumbers) es una lista
--infinita de los números primos.
primeNumbers1:: [Int] -> [Int]
primeNumbers1 (p:xs)=p : primeNumbers1 [x |x <-xs,isPrimo x]

infinitePrimeNumbers :: [Int]
infinitePrimeNumbers = primeNumbers1 [2..] 