-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}

-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Sstr String                 -- Une chaîne de caractères
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3)  ==  (((() . +) . 2) . 3)
--          ==>  Scons (Scons (Scons Snil (Ssym "+"))
--                            (Snum 2))
--                     (Snum 3)
--                   
-- (/ (* (- 68 32) 5) 9)
--     ==  (((() . /) . (((() . *) . (((() . -) . 68) . 32)) . 5)) . 9)
--     ==>
-- Scons (Scons (Scons Snil (Ssym "/"))
--              (Scons (Scons (Scons Snil (Ssym "*"))
--                            (Scons (Scons (Scons Snil (Ssym "-"))
--                                          (Snum 68))
--                                   (Snum 32)))
--                     (Snum 5)))
--       (Snum 9)

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                pChar '\n'; return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> not (isAscii c)
                                          || c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

pString :: Parser Sexp
pString = do pChar '"'
             s <- many (satisfy (\c -> not (c == '"')))
             pChar '"'
             return (Sstr s)

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(shorthand-quote E)"
-- La notation "`E" est équivalente à "(shorthand-backquote E)"
-- La notation ",E" est équivalente à "(shorthand-comma E)"
pQuote :: Parser Sexp
pQuote = do { c <- satisfy (\c -> c `elem` "'`,"); pSpaces; e <- pSexp;
              return (Scons
                      (Scons Snil
                             (Ssym (case c of
                                     ',' -> "shorthand-comma"
                                     '`' -> "shorthand-backquote"
                                     _   -> "shorthand-quote")))
                      e) }

-- Une liste (Tsil) est de la forme ( [e .] {e} )
pTsil :: Parser Sexp
pTsil = do _ <- char '('
           pSpaces
           (do { _ <- char ')'; return Snil }
            <|> do hd <- (do e <- pSexp
                             pSpaces
                             (do _ <- char '.'
                                 pSpaces
                                 return e
                              <|> return (Scons Snil e)))
                   pLiat hd)
    where pLiat :: Sexp -> Parser Sexp
          pLiat hd = do _ <- char ')'
                        return hd
                 <|> do e <- pSexp
                        pSpaces
                        pLiat (Scons hd e)

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pTsil <|> pQuote <|> pSymbol <|> pString
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec :: Int -> ReadS Sexp
    readsPrec _ s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Sstr s) = \tl -> "\"" ++ s ++ "\"" ++ tl
showSexp' (Scons e1 e2) = showHead (Scons e1 e2) . showString ")"
    where showHead (Scons Snil e') = showString "(" . showSexp' e'
          showHead (Scons e1' e2')
            = showHead e1' . showString " " . showSexp' e2'
          showHead e = showString "(" . showSexp' e . showString " ."

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire                                          --
---------------------------------------------------------------------------

type Var = String
type Label = Var

data Lexp = Lint Int                    -- Litéral entier.
          | Lstr String                 -- Chaîne de caractères litérale.
          | Lref Var                    -- Référence à une variable.
          | Lobjection [(Label, Lexp)] Var Lexp -- Object&fonction à la fois.
          | Linvoke Lexp Lexp           -- Appel d'objection, avec un argument.
          | Levidence Lexp Label        -- Renvoie l'attribut d'une objection.
          | Lif Lexp Lexp Lexp          -- Execution conditionnelle.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lbind [(Var, Lexp)] Lexp
          deriving (Show, Eq)


---------------------------------------------------------------------------
-- Conversion de Sexp à Lexp                                             --
---------------------------------------------------------------------------

-- Première passe simple qui analyse un Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lint n
s2l (Ssym s) = Lref s
s2l (Scons (Scons (Scons (Scons Snil (Ssym "objection")) attrs)
                  (Scons Snil (Ssym arg)))
           body)
    = Lobjection (s2slots attrs) arg (s2l body)
s2l (Sstr t) = Lstr t
s2l se = error ("Expression Psil inconnue: " ++ (showSexp se))

s2slots :: Sexp -> [(Label, Lexp)]
s2slots Snil = []
-- ¡¡COMPLÉTER ICI!!
s2slots se = error ("Syntaxe inconnue pour attributs d'objection: "
                    ++ (showSexp se))

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

-- Type des valeurs manipulées par l'évaluateur.
data Value = Vint Int
           | Vstr String
           | Vobjection [(Label, Value)] (Value -> Value)

instance Show Value where
    showsPrec :: Int -> Value -> ShowS
    showsPrec p (Vint n) = showsPrec p n
    showsPrec p (Vstr s) = showsPrec p ("\"" ++ s ++ "\"")
    showsPrec _p (Vobjection attrs _body) =
        let o2str [] Nothing acc = "#Obj[" ++ acc
            o2str [] (Just "") acc = "[" ++ acc
            o2str [] (Just name) acc = "#" ++ name ++ "[" ++ acc
            o2str (("obj-name", Vstr name):as) _ acc = o2str as (Just name) acc
            o2str ((aname, aval):as) n acc
                = o2str as n (aname ++ "=" ++ show aval ++ ", " ++ acc)
        in \s -> o2str attrs Nothing ("]" ++ s)

type Env = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: Env
env0 = [("true", valbool True),
        ("false", valbool False),
        ("not", prim "not" (\v -> case v of
                                   Vint 0 -> valbool True
                                   Vint _ -> valbool False
                                   _ -> error ("Pas un booléen: " ++ show v))),
        ("+", iprim "+" (+)),
        ("-", iprim "-" (-)),
        ("*", iprim "*" (*)),
        ("/", iprim "/" div),
        ("<" , iprimb "<" (<)),
        ("<=" , iprimb "≤" (<=)),
        (">" , iprimb ">" (>)),
        ("==" , iprimb "≡" (==)),
        (">=" , iprimb "≥" (>=))
       ]
    where prim name f = Vobjection [("obj-name", Vstr ("prim:" ++ name))] f
          valbool x = Vint (if x then 1 else 0)
          iprim name op
              = prim name
                     (\v1
                      -> case v1 of
                          Vint x
                            -> prim (name ++ "₂")
                                   (\v2 -> case v2 of
                                            Vint y -> Vint (x `op` y)
                                            _ -> error ("Pas un entier: "
                                                       ++ show v2))
                          _ -> error ("Pas un entier: " ++ show v1))
          iprimb name op
              = prim name
                     (\v1
                      -> case v1 of
                          Vint x
                            -> prim (name ++ "₂")
                                   (\v2 -> case v2 of
                                            Vint y -> valbool (x `op` y)
                                            _ -> error ("Pas un entier: "
                                                       ++ show v2))
                          _ -> error ("Pas un entier: " ++ show v1))

-- La fonction d'évaluation principale.
eval :: Env -> Lexp -> Value
eval _ (Lint n) = Vint n
-- ¡¡¡ COMPLETER ICI !!! --

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = eval env0 . s2l

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do inputHandle <- openFile filename ReadMode 
       hSetEncoding inputHandle utf8
       s <- hGetContents inputHandle
       (hPutStr stdout . foldr (\ v out -> "↝* " ++ show v ++ "\n" ++ out) "")
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map evalSexp (sexps s))
       hClose inputHandle
