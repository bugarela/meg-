import Data.List
import Data.Maybe

data Jogada = O|X|B deriving Eq

instance Show Jogada where
    show O = "O"
    show X = "X"
    show B = " "

--type Tabuleiro [[Jogada]]

iniciaTabuleiro = replicate 3 (replicate 3 B)

mostraLinha [x] [n] = " " ++ (if x==B then n else show x)
mostraLinha (x:xs) (n:ns) = " " ++ (if x==B then n else show x) ++ " |" ++ mostraLinha xs ns

divisoria = "\n ----------\n"

mostraTabuleiro xs = "\n" ++ mostraLinha (xs!!0) (fmap show [1..3]) ++ divisoria ++ mostraLinha (xs!!1) (fmap show [4..6]) ++ divisoria ++ mostraLinha (xs!!2) (fmap show [7..9]) ++ "\n\n"

vitoria y ts = olha y ts || olha y (transpose ts) || olha y (diagonais ts)

olha y [] = False
olha y (x:xs) = (all (==y) x) || olha y xs

diagonais [a,b,c] = ([[a!!0] ++ [b!!1] ++ [c!!2],[a!!2] ++ [b!!1] ++ [c!!0]])

vazio l c ts = ts!!l!!c==B

insereJogada n l c ts = if (vazio l c ts) then Just [insereJogada' n l c ts 0 0, insereJogada' n l c ts 1 0, insereJogada' n l c ts 2 0] else Nothing
insereJogada' _ _ _ _ _ 3 = []
insereJogada' n l c ts i j = if (i==l && j==c) then (n:insereJogada' n l c ts i (j+1)) else ts!!i!!j:insereJogada' n l c ts i (j+1)

jogoPvP = jogar1 iniciaTabuleiro

jogar1 ts = do
    putStr(mostraTabuleiro ts)
    putStr("Jogador 1, insira sua jogada: ")
    entrada <- getLine
    let jogada = insereJogada O (div ((read entrada)-1) 3) (mod ((read entrada)-1) 3) ts
    let sucesso = isJust jogada
    if sucesso then do
        let novoTabuleiro = fromJust jogada
        if vitoria O novoTabuleiro then do {putStrLn ("\nVitoria do jogador 1!\n");putStrLn (mostraTabuleiro novoTabuleiro);return()} else jogar2 novoTabuleiro
    else do
        putStrLn ("Jogada invalida, tente novamente.\n")
        jogar1 ts

jogar2 ts = do
    putStr(mostraTabuleiro ts)
    putStr("Jogador 2, insira sua jogada: ")
    entrada <- getLine
    let jogada = insereJogada X (div ((read entrada)-1) 3) (mod ((read entrada)-1) 3) ts
    let sucesso = isJust jogada
    if sucesso then do
        let novoTabuleiro = fromJust jogada
        if vitoria X novoTabuleiro then do {putStrLn ("\nVitoria do jogador 2!\n");putStrLn (mostraTabuleiro novoTabuleiro);return()} else jogar1 novoTabuleiro
    else do
        putStrLn ("Jogada invalida, tente novamente.\n")
        jogar2 ts

jogoVsBot = jogarP iniciaTabuleiro

jogarP ts = do
    putStr(mostraTabuleiro ts)
    putStr("Jogador 1, insira sua jogada: ")
    entrada <- getLine
    let jogada = insereJogada X (div ((read entrada)-1) 3) (mod ((read entrada)-1) 3) ts
    let sucesso = isJust jogada
    if sucesso then do
        let novoTabuleiro = fromJust jogada
        if vitoria X novoTabuleiro then do {putStrLn ("\nVitoria do jogador 1!\n");putStrLn (mostraTabuleiro novoTabuleiro);return()} else jogarIA novoTabuleiro
    else do
        putStrLn ("Jogada invalida, tente novamente.\n")
        jogarP ts


--linhaCritica ls = lenght (filter (==X) ls) == 2 && lenght (filter (==B) ls) == 1 

qtdBrancos ts = length (brancos ts)

jogadaVazia ts e = ts!!(div e 3)!!(mod e 3)==B

brancos ts = filter(jogadaVazia ts) [0..8]

fat 1 = 1
fat n = n * fat(n-1)

jogarIA ts = do
    --let b = snd (melhorJogada ts O)
    --let jogada = insereJogada O (div b 3) (mod b 3) ts
    --let novoTabuleiro = fromJust jogada
    let novoTabuleiro = tabuleiroResultante ts O
    if vitoria O novoTabuleiro then do 
        putStrLn ("\nVitoria do Computador!\n")
        putStrLn (mostraTabuleiro novoTabuleiro)
        return()
    else do
        jogarP novoTabuleiro

geraJogadas j ts = geraJogadas' j ts (brancos ts) 
geraJogadas' _ _ [] = []
geraJogadas' j ts (b:bs) = fromJust (insereJogada j (div b 3) (mod b 3) ts):geraJogadas' j ts bs

alterna X = O
alterna O = X

tabuleiroResultante ts j = (geraJogadas j ts)!!(snd (melhorJogada ts j))

melhorJogada [] _ = (-1,0)
melhorJogada ts j = maior (map (jogadas j) (geraJogadas j ts))

jogadas _ [] = -1
jogadas j ts = if vitoria X ts then 0 else (if vitoria O ts then (fat(qtdBrancos ts)) else fst (melhorJogada ts (alterna j)))

maior xs = maior' xs 0
maior' [] e = (-1,0)
maior' [x] e = (x,e)
maior' (x:xs) e = if x > fst(m) || x == 0 then (x,e) else m
    where m = maior' xs (e+1)


t0 = (fromJust (insereJogada X (div 0 3) (mod 0 3) iniciaTabuleiro))
t1 = (fromJust (insereJogada O (div 1 3) (mod 1 3) t0))
t2 = geraJogadas O (fromJust (insereJogada X (div 6 3) (mod 6 3) t1))
 



    
