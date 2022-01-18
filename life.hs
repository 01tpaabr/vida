import Data.List
import System.IO
-- morto: 0 / vivo: 1 / zumbi: 2

--Testar se celula faz parte de uma lista de celulas
predFazParte :: [Int] -> [[Int]] -> Bool
predFazParte celulaAtual [] = False
predFazParte celulaAtual (h : t) =
    (celulaAtual == h) || predFazParte celulaAtual t

-- Dado uma cordenada, transformar celula em viva ou zumbi (dependendo do modo) se cordenada fizer parte da lista 
decidirValor :: Int -> [Int] -> Int -> [[Int]] -> Int
decidirValor modo celulaAtual valorAntigoDaCelula listaCelulasVivas
    | (predFazParte celulaAtual listaCelulasVivas) = modo
    | otherwise = valorAntigoDaCelula

-- Percorrer Grid Vazia e mudar o valor de celulas para vivas se a celula pertencer a lista de celulas vivas
lidarComLinha :: Int -> Int -> Int -> [Int] -> [[Int]] -> [Int]
lidarComLinha valorLinhaAtual modo valorColunaAtual [] coordenadasCelulasVivas = []
lidarComLinha valorLinhaAtual modo valorColunaAtual (h : t) coordenadasCelulasVivas =
    [(decidirValor modo [valorLinhaAtual, valorColunaAtual] h coordenadasCelulasVivas)] ++ lidarComLinha valorLinhaAtual modo (valorColunaAtual + 1) t coordenadasCelulasVivas

popularInicio :: Int -> Int -> [[Int]] -> [[Int]] -> [[Int]]
-- linhaAtual: Valor auxiliar para guardar coordenada da linhaAtual
popularInicio linhaAtual modo [] coordenadasCelulasVivas = []
popularInicio linhaAtual modo (h : t) coordenadasCelulasVivas =
    [(lidarComLinha linhaAtual modo 0 h coordenadasCelulasVivas)] ++ popularInicio (linhaAtual + 1) modo t coordenadasCelulasVivas

-- Criar grid inicial
criarMundo :: Int -> [[Int]] -> [[Int]] -> [[Int]]
criarMundo n celulasVivas celulasZumbis =
    popularInicio 0 2 (popularInicio 0 1 (replicate n (replicate n 0)) celulasVivas) celulasZumbis

-- Lista de coordenadas existentes no mundo
coordenadasMundo :: Int -> [[Int]]
coordenadasMundo n = [[x, y] | x <- [0 .. n-1], y <- [0 .. n-1]]
    
-- gerarVizinhos 
possiveisVizinhos :: [Int] -> [[Int]]
possiveisVizinhos (y:x:xs) =
    [[y - 1, x - 1], [y - 1, x], [y - 1 , x + 1], [y, x - 1], [y, x + 1], [y + 1, x - 1], [y + 1, x], [y + 1, x + 1]]

apararBordas :: [[Int]] -> [[Int]] -> [[Int]]
apararBordas [] mundo = []
apararBordas vizinhos mundo =
    filter (\x -> predFazParte x mundo) vizinhos


-- Funções vizinhos
vizinhos :: [Int] -> Int -> [[Int]]
vizinhos celulaAtual tamanhoMundo =
    apararBordas (possiveisVizinhos celulaAtual) (coordenadasMundo tamanhoMundo)

valoresVizinhos :: [[Int]] -> [[Int]] -> [Int]
valoresVizinhos [] mundo = []
valoresVizinhos (h : t) mundo =
    (mundo !! (h !! 0) !! (h !! 1)) : valoresVizinhos t mundo

--Contar quantos vizinhos vivos e zumbis
qntVivas :: [Int] -> Int
qntVivas vizinhos =
    length (filter (\x -> x == 1) vizinhos)

qntZumbis :: [Int] -> Int
qntZumbis vizinhos =
    length (filter (\x -> x == 2) vizinhos)

-- Predicados regras destino
-- Para celulas mortas
reprod :: [Int] -> [[Int]] -> Bool
reprod celulaAtual mundo 
    | (qntVivas (valoresVizinhos (vizinhos celulaAtual (length mundo)) mundo) == 3) = True
    | otherwise = False

-- Para celulas vivas
infec :: [Int] -> [[Int]] -> Bool
infec celulaAtual mundo
    | (qntZumbis (valoresVizinhos (vizinhos celulaAtual (length mundo)) mundo) >= 2) = True
    | otherwise = False

subpop :: [Int] -> [[Int]] -> Bool
subpop celulaAtual mundo
    | (qntVivas (valoresVizinhos (vizinhos celulaAtual (length mundo)) mundo) < 2 &&  qntZumbis (valoresVizinhos (vizinhos celulaAtual (length mundo)) mundo) < 2) = True
    | otherwise = False

superpop :: [Int] -> [[Int]] -> Bool
superpop celulaAtual mundo
    | (qntVivas (valoresVizinhos (vizinhos celulaAtual (length mundo)) mundo) > 3 &&  qntZumbis (valoresVizinhos (vizinhos celulaAtual (length mundo)) mundo) == 0) = True
    | otherwise = False

-- Para celulas Zumbis
inani :: [Int] -> [[Int]] -> Bool
inani celulaAtual mundo
    | (qntVivas (valoresVizinhos (vizinhos celulaAtual (length mundo)) mundo) == 0 ) = True
    | otherwise = False


-- Função Destino
destino :: Int -> [Int] -> [[Int]] -> Int
destino valorInicial celulaAtual mundo
    | valorInicial == 0 =
        case () of 
            ()  | reprod celulaAtual mundo -> 1
                | otherwise -> valorInicial
        
    | valorInicial == 1 =
        case () of
            ()  | infec celulaAtual mundo -> 2
                | subpop celulaAtual mundo -> 0
                | superpop celulaAtual mundo -> 0
                | otherwise -> valorInicial
    | valorInicial == 2 =
        case () of
            ()  | inani celulaAtual mundo -> 0
                | otherwise -> valorInicial




-- Aplicar função destino em cada celula por cada iteração
avancarLinha :: Int -> Int -> [Int] -> [[Int]] -> [Int]
avancarLinha valorColunaAtual valorLinhaAtual [] mundo = []
avancarLinha valorColunaAtual valorLinhaAtual (h : t) mundo = 
    [(destino h ([valorLinhaAtual, valorColunaAtual]) mundo)] ++ avancarLinha (valorColunaAtual + 1) valorLinhaAtual t mundo

avancarDestino :: Int -> [[Int]] -> [[Int]]
avancarDestino valorLinhaAtual [] = []
avancarDestino valorLinhaAtual (h : t) =
    [(avancarLinha 0 valorLinhaAtual h (h : t))] ++ avancarDestino (valorLinhaAtual + 1) t

vida :: Int -> Int -> [[Int]] -> [[Int]] -> [[Int]]
-- vida 0 cont mundoAnterior mundo = mundo ++ [[cont - 1]]
vida 0 cont mundoAnterior mundo = mundo
vida n cont mundoAnterior mundo 
    -- | mundoAnterior == mundo = mundo ++ [[cont - 1]]
    | mundoAnterior == mundo = mundo
    | otherwise = (vida (n-1) (cont + 1) mundo (avancarDestino 0 mundo))

historico :: Int -> [[Int]] -> [[[Int]]]
historico 0 mundo = [mundo]
historico n mundo = 
    [vida n 0 [] mundo] ++ (historico (n-1) mundo) 

main = do
    putStrLn "Numero de iterações desejada: "
    input <- getLine
    let n = read input :: Int

    let tamanho = 6

    let celulasVivasInicio = [[1,1],[1,2],[3,4],[3,3],[0,1]]
    let celulasZumbiInicio = [[1,3], [0,2]]

    let m = criarMundo tamanho celulasVivasInicio celulasZumbiInicio

    let final = historico n m

    writeFile "saida.txt" ((show tamanho) ++"\n" ++ (show final))


--     putStrLn input
