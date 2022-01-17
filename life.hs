import Data.List
import System.IO
-- morto: 0 / vivo: 1 / zumbi: 2

--Testar se celula faz parte de uma lista de celulas
celulaFazParte :: [Int] -> [[Int]] -> Bool
celulaFazParte celulaAtual [] = False
celulaFazParte celulaAtual (h : t) =
    (celulaAtual == h) || celulaFazParte celulaAtual t


-- Dado uma cordenada, transformar celula em viva ou zumbi (dependendo do modo) se cordenada fizer parte da lista 
decidirValor :: Int -> [Int] -> Int -> [[Int]] -> Int
decidirValor modo celulaAtual valorAntigoDaCelula listaCelulasVivas
    | (celulaFazParte celulaAtual listaCelulasVivas) == True = modo
    | (celulaFazParte celulaAtual listaCelulasVivas) == False = valorAntigoDaCelula

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

--Função Destino
--Aplicar função destino em cada celula por cada iteração

main = do
    putStrLn "Numero de iterações desejada: "
    input <- getLine
    let n = read input :: Integer
    putStrLn input
