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
    | (predFazParte celulaAtual listaCelulasVivas) == True = modo
    | (predFazParte celulaAtual listaCelulasVivas) == False = valorAntigoDaCelula

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


-- Função vizinhos
vizinhos :: Int -> [Int] -> [[Int]] -> [[Int]]
vizinhos linhaAtual celulaAtual mundo =
    mundo

-- Função Destino
-- Aplicar função destino em cada celula por cada iteração

main = do
    putStrLn "Numero de iterações desejada: "
    input <- getLine
    let n = read input :: Integer
    putStrLn input
