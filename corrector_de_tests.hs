
--PRACTICA DE PROGRAMACION DECLARATIVA 2015/2016

-----------------------------------------------------------------------------------------------------
-------------------------------DECLARACION DE LOS TIPOS NECESARIOS-----------------------------------
-----------------------------------------------------------------------------------------------------

--declaracion del tipo Test
--[Pregunta]: lista que representa el conjunto de preguntas que forman el test
--[Modelo]: lista que representa el conjunto de modelos que hay del test
data Test = Test [Pregunta] [Modelo] deriving Show

--declaracion del alias de tipo Modelo: cada modelo es una lista de enteros los cuales cada uno es el
--indice de una pregunta (cada modelo uno tiene un orden de estos indices)
--EL orden referencia de las preguntas, es decir, el modelo base del test es el que se sigue en la lista de preguntas del Test
type Modelo = [Int]

--declaracion del tipo Pregunta. Constructoras de tipo:
--Valor: float que contiene el valor de una pregunta
--Int: entero que contiene el numero de respuestas posibles
--Int: entero que contiene el indice de la alternativa correcta
data Pregunta = Pregunta Valor Int Int deriving Show

--declaracion del alias de tipo Valor: es un float que contiene el valor de una pregunta
type Valor = Float

--declaracion del tipo RespuestaTest
--IDAlumno: string que contiene el identificador del alumno
--Int: entero que representa el modelo del test que se ha respondido, es un indice de la lista [Modelo] de un Test
--[Int] lista de enteros que contiene el indice de la alternativa marcada en cada respuesta del test (0 <-> respuesta en blanco)
data RespuestaTest = RespuestaTest IDAlumno Int [Int] deriving Show

--declaracion del alias de tipo IDAlumno que es un string ([Char])
type IDAlumno = [Char]

--declaracion del tipo Correccion
--IDAlumno: id del alumno al que pertenece la correccion
--Valor: nota del alumno sobre la nota maxima del test
--Valor: nota del alumno sobre 10
--[InfoPregunta]: para cada pregunta i del Test, si es correcta, erronea o en blanco, lista ordenada segun el modelo base del test
data Correccion = Correccion IDAlumno Valor Valor [InfoPregunta] deriving Show

--tipo que representa si una pregunta es correcta, erronea o en blanco
data EstadoPregunta = Correcta | Erronea | Blanco deriving (Eq, Show)

--alias de tipo que expresa que la pregunta i-esima del modelo base del test (representada por un Int) tiene una respuesta EstadoPregunta
type InfoPregunta = (Int, EstadoPregunta)


--declaracion del tipo Estadisticas
--Valor: Nota media
--Valor: numero medio de respuestas respondidas
--Calificaciones: numero de suspensos, aprobados, notables y sobresalientes
--Frecuencias: frecuencias absolutas y relativas de respuestas correctas, erroneas y en blanco para cada pregunta 
--Int: pregunta con mejores resultados
--Int: pregunta con peores resultados
--Int: pregunta mas veces dejada en blanco.
--Int: pregunta menos veces dejada en blanco
data Estadisticas = Estadisticas Valor Valor Calificaciones Frecuencias Int Int Int Int deriving Show

--declaracion del tipo Calificaciones que contiene el numero de suspensos, aprobados, notables y sobresalientes en este orden:
--Int: suspensos
--Int: aprobados
--Int: notables
--Int: Sobresalientes
data Calificaciones = Calificaciones Int Int Int Int deriving Show

--declaracion del tipo Frecuencias que contiene las frecuencias absolutas y relativas de respuestas correctas, erroneas
--y en blanco para cada pregunta, en este orden:
--[Int]: F.Absoluta de aciertos
--[Float]: F.Relativa de aciertos
--[Int]: F.Absoluta de fallos
--[Float]: F.Relativa de fallos
--[Int]: F.Absoluta de blancos
--[Float]: F.Relativa de blancos
data Frecuencias = Frecuencias [Int] [Float] [Int] [Float] [Int] [Float] deriving Show


-----------------------------------------------------------------------------------------------------
-----------------------DECLARACION DE FUNCIONES OBLIGATORIAS PARA LA PRACTICA------------------------
-----------------------------------------------------------------------------------------------------

--funcion corrige en la que ados un test y una respuesta, llama a corrigeAux buscando el modelo del test(orden de preguntas) que hizo el alumno
--e inicializa los parametros de esta funcion
corrige:: Test -> RespuestaTest -> Correccion
corrige (Test preguntas mods) (RespuestaTest id nmod respuestas) = corrigeAux preguntas (devuelveModelo mods nmod) respuestas id 0 0 []


--funcion estadisticas que dados un test y una lista de respuestas, realiza un analisis de los datos llamando
--a la funcion extraeResultados con una lista de las correcciones del test y la inicializacion de sus parametros
estadisticas:: Test -> [RespuestaTest] -> Estadisticas
estadisticas (Test preguntas mods) respuestas =
    let correcciones = multiCorrige (Test preguntas mods) respuestas in extraeResultados correcciones 0 0 0 0 0 0 0 [] (length preguntas)



-----------------------------------------------------------------------------------------------------
---------------------------------------FUNCIONES AUXILIARES------------------------------------------
-----------------------------------------------------------------------------------------------------

--              ---------------------------------------------------------------------
--              -----------------FUNCIONES AUXILIARES A "corrige"--------------------
--              ---------------------------------------------------------------------

--funcion auxuliar a corrige en la que dado un test, un modelo, un id de alumno y sus respuestas, usa parametros auxuliares acumulativos para
--calcular la nota final sobre 10 y sobre la maxima del test.
--Los resultados se devuelven en un elemento de tipo Correccion
--preguntas ([Pregunta]): lista de preguntas del test
--Modelo (pm:pms): una lista de enteros, es decir, indices de las preguntas en un orden dado
--[Int] (r:rs): lista de las respuestas del alumno al test, cada entero es el indice de la alternativa de cada pregunta
--id (IDAlumno): id del alumno de la correccion
--notaAcu (Valor): un acumuldar que contiene la suma de los valores de las preguntas vistas hasta el momento, se usa al final para calcular la nota sobre 10
--notaTest (Valor): un acumulador de la nota que lleva el alumno, puede crecer, disminuir o mantenerse constante a lo largo de la ejecucion de la funcion
--estadoPreguntas ([InfoPregunta]): lista que va creciendo y contiene un valor Correcta, Erronea o Blanco junto con el indice para la i-esima pregunta 
--                                  respondida por un alumno. Esta lista al final se ordena segun el orden del modelo base del test.
corrigeAux:: [Pregunta] -> Modelo -> [Int] -> IDAlumno -> Valor -> Valor -> [InfoPregunta] -> Correccion
corrigeAux preguntas [] [] id notaAcu notaTest estadoPreguntas = Correccion id notaTest (notaSobre10 notaAcu notaTest) (miQsort estadoPreguntas)
corrigeAux preguntas (pm:psm) (r:rs) id notaAcu notaTest estadoPreguntas = 
    let {v = (valorPregunta (preguntas!!pm)) ;
        op = opcionCorrecta (preguntas!!pm) ;
        resta = 1 / ((fromIntegral (numOpciones (preguntas!!pm))) - 1)}  in
            if (op == r) then --si la respuestas es correcta
                corrigeAux preguntas psm rs id (notaAcu + v) (notaTest + v) (estadoPreguntas++[(pm, Correcta)])
            else  if (r /= 0) then --si la respuesta es incorrecta
                corrigeAux preguntas psm rs id (notaAcu + v) (notaTest - resta) (estadoPreguntas++[(pm, Erronea)])
            else --si la respuesta esta en blanco
                corrigeAux preguntas psm rs id (notaAcu + v) notaTest (estadoPreguntas++[(pm, Blanco)])


--funcion que dada una lista de modelos de tests y un indice i nos devuelve el i-esimo modelo
devuelveModelo:: [Modelo] -> Int -> Modelo
devuelveModelo x i = x!!i


--funcion que dada una nota y la puntuacion maxima de un test nos devuelve la puntuacion sobre 10 correspondiente
notaSobre10:: Valor -> Valor -> Valor
notaSobre10 x n = n * (10 / x)


--funcion que devuelve el valor de una pregunta dada
valorPregunta:: Pregunta -> Valor
valorPregunta (Pregunta v _ _) = v


--funcion que devuelve la opcion correcta de una Pregunta dada
opcionCorrecta:: Pregunta -> Int
opcionCorrecta (Pregunta _ _ x) = x


--funcion que devuelve el numero de opciones que tiene una pregunta dada
numOpciones:: Pregunta -> Int
numOpciones (Pregunta _ n _) = n


--funcion que dada una lista [InfoPregunta] = (Int, EstadoPregunta) devuelve la misma lista ordenada segun el primer elemento de la tupla.
--Es una ordenacion por quicksort que solo tiene en cuenta el primer elemnento de la tupla a la hora de ordenar.
miQsort:: [InfoPregunta] -> [InfoPregunta]
miQsort [] = []
miQsort ((i, res):xs) = miQsort [(a, b) | (a, b) <- xs , a < i]
                        ++[(i, res)]++
                        miQsort [(a, b) | (a, b) <- xs , a > i]


--              ---------------------------------------------------------------------
--              ---------------FUNCIONES AUXILIARES A "estadisticas"-----------------
--              ---------------------------------------------------------------------

--funcion auxiliar a estadisticas que devuelve un Estadisticas, y para ello lleva:
--(c:cs) ([Correccion]): las lista de las correcciones de cada alumno
--notaAcu (Valor): va acumulando la suma de las notas de los alumnos
--n (Int): el numero de alumnos para dividir al hacer las medias
--contestadas (Int): numero de respuestas contestadas por los alumnos hasta el momento
--sp (Int): numero de aprobados
--ap (Int): numero de suspensos
--nt (Int): numero de notables
--sb (Int): numero de sobresalientes
--estadoPreguntas ([[InfoPregunta]]): contiene la info sobre cada pregunta contestada por cada alumno
--                                    para poder calcular las frecuencias absolutas y relativas.
--                                    Contiene para cada alumno si la pregunta i-esima es correcta, incorrecta o en blanco.
--numPreguntas (Int): numero de preguntas que forman el test, nos hace falta para calcular las frecuencias
extraeResultados:: [Correccion] -> Valor -> Int -> Int -> Int -> Int -> Int -> Int -> [[InfoPregunta]]-> Int -> Estadisticas
extraeResultados [] notaAcu n contestadas sp ap nt sb estadoPreguntas numPreguntas =
    let frecs = (calculoFrecuencias estadoPreguntas numPreguntas) in
        Estadisticas (notaAcu / (fromIntegral n))
                     ((fromIntegral contestadas) / (fromIntegral n))
                     (Calificaciones sp ap nt sb)
                     frecs
                     (mayorFrecAbs (frecAciertos frecs))
                     (mayorFrecAbs (frecFallos frecs))
                     (mayorFrecAbs (frecBlancos frecs))
                     (menorFrecAbs (frecBlancos frecs))
extraeResultados (c:cs) notaAcu n contestadas sp ap nt sb estadoPreguntas numPreguntas= 
    let {nota = notaS10Correccion c ;
        infoPreguntas = estPregCorreccion c } in
        if (nota < 5) then 
            extraeResultados cs (notaAcu + nota) (n + 1)
                             (contestadas + (preguntasContestadas infoPreguntas))
                             (sp + 1) ap nt sb (estadoPreguntas++[infoPreguntas]) numPreguntas
        else if (nota >= 5 && nota < 7) then
            extraeResultados cs (notaAcu + nota) (n + 1)
                             (contestadas + (preguntasContestadas infoPreguntas))
                             sp (ap + 1) nt sb (estadoPreguntas++[infoPreguntas]) numPreguntas
        else if (nota >= 7 && nota < 9) then
            extraeResultados cs (notaAcu + nota) (n + 1)
                             (contestadas + (preguntasContestadas infoPreguntas))
                             sp ap (nt + 1) sb (estadoPreguntas++[infoPreguntas]) numPreguntas
        else
            extraeResultados cs (notaAcu + nota) (n + 1)
                             (contestadas + (preguntasContestadas infoPreguntas))
                             sp ap nt (sb + 1) (estadoPreguntas++[infoPreguntas]) numPreguntas


--funcion que dado un test y un conjunto de respuestas devuelve una lista con todas las correcciones
multiCorrige:: Test -> [RespuestaTest] -> [Correccion]
multiCorrige t [] = []
multiCorrige t (r:rs) = (corrige t r):(multiCorrige t rs)


--funcion que devuelve la nota sobre 10 de una Correccion dada
notaS10Correccion:: Correccion -> Valor
notaS10Correccion (Correccion _ _ n _) = n


--funcion que dada una correccion nos devuelve la lista [InfoPregunta] que contiene el estado de cada pregunta (acierto, fallo o blanco)
estPregCorreccion:: Correccion -> [InfoPregunta]
estPregCorreccion (Correccion _ _ _ xs) = xs


--funcion que dada una lista [InfoPregunta], nos devuelve el numero de respuestas respondidas, es decir la suma de Correctos y erroneos
preguntasContestadas:: [InfoPregunta] -> Int
preguntasContestadas [] = 0
preguntasContestadas ((_, res):xs) = if (res /= Blanco) then 1 + preguntasContestadas xs
                                     else preguntasContestadas xs


--funcion que devuelve las frecuencias dada una lista de informacion acerca de las respuestas de los alumnos
--[[InfoPregunta]]: lista que contiene las respuetas en modo de lista (correcto, fallo o blanco) de cada alumno
--Int: entero que contiene el numero de preguntas
calculoFrecuencias:: [[InfoPregunta]] -> Int -> Frecuencias
calculoFrecuencias info numPreguntas =
    let { frecAbsAciertos = (frecAbsoluta info 0 numPreguntas Correcta []) ;
          frecAbsFallos = (frecAbsoluta info 0 numPreguntas Erronea []) ;
          frecAbsBlancos = (frecAbsoluta info 0 numPreguntas Blanco [])} in
        Frecuencias frecAbsAciertos (frecRelativa frecAbsAciertos (suma frecAbsAciertos))
                    frecAbsFallos (frecRelativa frecAbsFallos (suma frecAbsFallos))
                    frecAbsBlancos (frecRelativa frecAbsBlancos (suma frecAbsBlancos))


--funcion que calcula la frecuencia absoluta de las respuestas a cada pregunta.
--Se calucla la frecuencia de  los aciertos, fallos o blancos en base al parametro estadpPregunta
--[[InfoPregunta]]: lista con las listas de respuestas de cada alumno. Cada lista contiene si la pregunta i es Correcta, Erronea o Blanco
--i (Int): i-esima pregunta por la que vamos calculando su frecuencia absoluta
--max (Int): numero de preguntas del test, para parar la ejecucion de la funcion
--estadoPregunta (EstadoPregunta): puede valer Correcta, Erronea o Blanco para parametizar la funcion si queremos calcular
--                                 la frecuencia de aciertos, fallos o blancos segun nos convenga
--ret ([Int]): lista en la que vamos concatenando la frecuencia absoluta para la pregunta i-esima, y sera la lista que devolvamos
frecAbsoluta:: [[InfoPregunta]] -> Int -> Int-> EstadoPregunta -> [Int] -> [Int]
frecAbsoluta [] _ _ _ ret = ret
frecAbsoluta infos i max estadoPregunta ret = --con esta lista intensionable reunimos en una sola lista el conjunto de respuestas a una misma pregunta
    let preguntaActual = [(k, e) | x <- infos, (k, e) <- x, k == i] in
        if (i < max) then frecAbsoluta infos (i + 1) max estadoPregunta (ret++[frecAbsolutaAux preguntaActual estadoPregunta])
        else ret


--funcion auxiliar a frecAbsoluta que calcula el numero total de aciertos, fallos o blancos de un conjunto de respuestas.
--((_, x):xs) ([InfoPregunta]): contiene un conjunto de tuplas (i, EstadoPregunta) donde i es igual en todas las tuplas y representa a la i-esima
--                pregunta del test, y EstadoPregunta puede valer Correcta, Erronea o Blanco
--estadoPregunta (EstadoPregunta): este parametro nos indica si queremos contar cuantos aciertos, fallos o blancos hay en la pregunta i.
frecAbsolutaAux:: [InfoPregunta] -> EstadoPregunta -> Int
frecAbsolutaAux [] _ = 0
frecAbsolutaAux ((_, x):xs) estadoPregunta = if (x == estadoPregunta) then 1 + frecAbsolutaAux xs estadoPregunta
                                             else frecAbsolutaAux xs estadoPregunta


--funcion que calcula la frecuencia relativa de las repuestas correctas, erroneas o en blanco
--en funcion de que lista de frecuencias absolutas reciba como parametro
--[Int]: una lista de frecuencias absolutas
--Int: suma de las frecuencias absolutas
frecRelativa:: [Int] -> Int -> [Float]
frecRelativa [] n = []
frecRelativa (x:xs) n = ((fromIntegral x) / (fromIntegral n)):frecRelativa xs n


--funcion suma que devuelve la suma de una lista de enteros
suma:: [Int] -> Int
suma [] = 0
suma (x:xs) = x + (suma xs)


--funcion que devuelve el indice de la pregunta con mas frecuencia absoluta, el indice sigue el orden del modelo base del test
--recibe como parametro una lista de frecuencias absolutas
mayorFrecAbs:: [Int] -> Int
mayorFrecAbs [] = error "Lista vacia, se esperaba una lista con al menos un elemento"
mayorFrecAbs (x:xs) = mayorFrecAbsAux xs x 0 0


--funcion auxiliar a mayorFrecAbs
--(y:ys) [Int]: lista con las frecuencias absolutas de cada pregunta
--x (Int): mayor frecuencia hasta el momento
--i (Int): indice de la frecuencia que estamos mirando en cada iteracion
--ret (Int): indice de la mayor frecuencia calculada hasta ek momento
mayorFrecAbsAux:: [Int] -> Int -> Int -> Int -> Int
mayorFrecAbsAux [] _ _ ret = ret + 1 --devuelve el indice en el intervalo (1..n), no en (0..n - 1), por eso sumamos un 1
mayorFrecAbsAux (y:ys) x i ret = if (y > x) then mayorFrecAbsAux ys y (i + 1) (i + 1)
                                  else mayorFrecAbsAux ys x (i + 1) ret


--funcion que devuelve el indice de la pregunta con menos frecuencia absoluta, el indice sigue el orden del modelo base del test
--recibe como parametro la lista de frecuencias absolutas
menorFrecAbs:: [Int] -> Int
menorFrecAbs [] = error "Lista vacia, se esperaba una lista con al menos un elemento"
menorFrecAbs (x:xs) = menorFrecAbsAux xs x 0 0


--funcion auxiliar a menorFrecAbs
--[Int]: lista con las frecuencias absolutas de cada pregunta
--Int: menor frecuencia hasta el momento
--Int: indice de la frecuencia que estamos mirando en cada iteracion
--Int: indice de la menor frecuencia calculada
menorFrecAbsAux:: [Int] -> Int -> Int -> Int -> Int
menorFrecAbsAux [] _ _ ret = ret + 1 --devuelve el indice en el intervalo (1..n), no en (0..n - 1), por eso sumamos un 1
menorFrecAbsAux (y:ys) x i ret = if (y < x) then menorFrecAbsAux ys y (i + 1) (i + 1)
                                  else menorFrecAbsAux ys x (i + 1) ret


--funcion que devuelve la frecuencia absoluta de aciertos de un Frecuencias
frecAciertos:: Frecuencias -> [Int]
frecAciertos (Frecuencias f _ _ _ _ _) = f


--funcion que devuelve la frecuencia absoluta de fallos de un Frecuencias
frecFallos:: Frecuencias -> [Int]
frecFallos (Frecuencias _ _ f _ _ _) = f


--funcion que devuelve la frecuencia absoluta de respuestas en blanco de un Frecuencias
frecBlancos:: Frecuencias -> [Int]
frecBlancos (Frecuencias _ _ _ _ f _) = f


-----------------------------------------------------------------------------------------------------
-----------------------------------DECLARACION DE FUNCIONES DE IO------------------------------------
-----------------------------------------------------------------------------------------------------

--funcion que dada una Correccion muestra los datos que nos importan con un formato especifico
muestraCorreccion:: Correccion -> IO ()
muestraCorreccion (Correccion id n n10 _) = do putStr ("Correccion:"++"\nAlumno: "++(show id))
                                               putStr ("\nNota test: "++(show n))
                                               putStr ("\nNota sobre 10: "++(show n10)++"\n")

--funcion que dada una Estadisticas muestra sus datos con un formato especifico
muestraEstadisticas:: Estadisticas -> IO ()
muestraEstadisticas (Estadisticas nM mediaContestadas calif frecs mp pp masb menosb) =
        do putStr ("Estadisticas: \n")
           putStr ("Nota media: "++(show nM)++"\n")
           putStr ("Media de respuestas respondidas: "++(show mediaContestadas)++"\n")
           muestraCalificaciones calif
           muestraFrecuencias frecs
           putStr ("Pregunta con mejores resultados: "++(show mp))
           putStr ("\nPregunta con peores resultados: "++(show pp))
           putStr ("\nPregunta más veces dejada en blanco: "++(show masb))
           putStr ("\nPregunta menos veces dejada en blanco: "++(show menosb)++"\n")

--funcion que dada una Calificaciones muestra sus datos con un formato especifico
muestraCalificaciones:: Calificaciones -> IO ()
muestraCalificaciones (Calificaciones sp ap nt sb) = do putStr ("Numero de suspensos (nota < 5): "++(show sp)++"\n")
                                                        putStr ("Numero de aprobados (5 <= nota < 7): "++ (show ap)++"\n")
                                                        putStr ("Numero de notables (7 <= nota < 9): "++(show nt)++"\n")
                                                        putStr ("Numero de sobresalientes (9 <= nota): "++(show sb)++"\n")


--funcionn que dada una Frecuencias muestra sus datos con un formato especifico
muestraFrecuencias:: Frecuencias -> IO ()
muestraFrecuencias (Frecuencias faAciertos frAciertos faFallos frFallos faBlancos frBlancos) =
    do putStr "Frecuencia absolutas y relativas de aciertos, fallos y blancos  para cada pregunta: \n"
       putStr "recuencia asboluta de aciertos:\n"
       imprimeLista faAciertos
       putStr "\nFrecuencia relativa de aciertos: \n"
       imprimeLista frAciertos
       putStr "\nFrecuencia absoluta de fallos:\n"
       imprimeLista faFallos
       putStr "\nFrecuencia relativa de fallos:\n"
       imprimeLista frFallos
       putStr "\nFrecuencia absoluta de respuestas en blanco:\n"
       imprimeLista faBlancos
       putStr "\nFrecuencia relativa de respuestas en blanco:\n"
       imprimeLista frBlancos
       putStr "\n"


--funcion que muestra los elementos de una lista dada
imprimeLista:: Show a => [a] -> IO ()
imprimeLista [] = return ()
imprimeLista (x:xs) = do putStr ((show x)++"   ")
                         imprimeLista xs