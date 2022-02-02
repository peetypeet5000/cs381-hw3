-- Homework 3 template

module Sentence where

-- Grammar for the animal sentence language:
--
--   <sentence> ->  <noun> <verb> [<noun>]  
--               	|  <sentence> `and` <sentence>
--				 	
--   <noun> 	-> <adj> <noun> | <noun> `and` <noun>
--					| `cats` | `dogs` | `ducks` | `bunnies`

--   <verb>		->  `chase` | `cuddle` | `hug` | `scare`
--   <adj>		->	`silly` | `small` | `old` | `happy`

data Sentence = NVN Noun Verb Noun
  | NV Noun Verb
  | And Sentence Sentence 
  | End
  deriving (Eq,Show)

data Adj = Silly | Small | Old | Happy
  deriving (Eq,Show)

data Noun = NP Adj Noun
  | NAnd Noun Noun 
  | Cats | Dogs | Ducks | Bunnies
  deriving (Eq,Show)

data Verb = Chase |  Cuddle | Hug | Scare
  deriving (Eq,Show)


-- | The sentence: cats cuddle ducks and dogs cuddle ducks
ex1 :: Sentence
ex1 = NVN Cats Hug Dogs

ex2 :: Sentence
ex2 = NVN (NP Silly Cats) Hug Dogs

ex3 :: Sentence
ex3 = NVN (NAnd Dogs Cats) Chase Ducks

ex4 :: Sentence
ex4 = NVN (NAnd (NP Silly Dogs) Cats) Chase Ducks


-- | Build a sentence from a noun verb noun.
-- | buildS2 Cats Hug Cats
-- | NVN Cats Hug Cats

buildS2 :: Noun -> Verb -> Noun -> Sentence
buildS2 n1 v n2= NVN n1 v n2

-- | Build a sentence from a noun verb 
-- | buildS1 Cats Hug 
-- | NV Cats Hug 

buildS1 :: Noun -> Verb ->Sentence
buildS1 n v = NV n v


-- | Build a noun phrase from an adjective and noun
-- | buildNP Silly Dogs
-- | NP Silly Dogs

buildNP :: Adj -> Noun -> Noun
buildNP a n = (NP a n)

-- | Build a noun conjunction from two nouns
-- | buildNAnd Dogs Cats
-- | NAnd Dogs Cats

buildNAnd :: Noun -> Noun -> Noun
buildNAnd n1 n2 = NAnd n1 n2

-- | Build a sentence that is a conjunction of a list of other sentences.
-- | conjunction [ex1, ex2]
-- | And (NVN Cats Hug Dogs) (NVN (NP Silly Cats) Hug Dogs)
conjunction :: [Sentence] -> Sentence
conjunction (x:xs) 
  | (xs== []) = x
  | otherwise = And x (conjunction(xs))

-- | Pretty print a sentence.
pretty :: Sentence -> String
pretty (NVN s v o) = prettyNoun s ++ " " ++ prettyVerb v ++ " " ++ prettyNoun o
pretty (And l r)   = pretty l ++ " and " ++ pretty r
pretty (NV s v)     = prettyNoun s ++ " " ++ prettyVerb v
pretty (End) = "."

-- | Pretty print a noun.
prettyNoun :: Noun -> String
prettyNoun Cats  = "cats"
prettyNoun Dogs = "dogs"
prettyNoun Ducks = "ducks"
prettyNoun Bunnies = "bunnies"
prettyNoun (NP a n) = prettyAdj a ++ " " ++ prettyNoun n
prettyNoun (NAnd m n) = prettyNoun m ++ " and " ++prettyNoun n

-- | Pretty print a verb.
prettyVerb :: Verb -> String
prettyVerb Chase  = "chase"
prettyVerb Cuddle = "cuddle"
prettyVerb Hug = "hug"
prettyVerb Scare = "scare"

-- | Pretty print an adjective.
prettyAdj :: Adj -> String
prettyAdj Silly  = "silly"
prettyAdj Small  = "small"
prettyAdj Old  = "old"
prettyAdj Happy  = "happy"


-- | Does the sentence contain only cuddling and hugs?
-- | isNice ex2
-- |   True
isNice :: Sentence -> Bool
isNice (NVN _ Chase _)  = False
isNice (NVN _ Cuddle _) = True
isNice (NVN _ Hug _) = True
isNice (NVN _ Scare _) = False
isNice (NV _ Scare) = False
isNice (NV _ Chase) = False
isNice (NV _ Cuddle) = True
isNice (NV _ Hug) = True
isNice (And s1 s2) = isNice s1 && isNice s2
isNice (End) = True
-- finish

-- |Count the number of words in a sentence
-- | wordCount ex4
--    6
wordCount :: Sentence -> Int
wordCount (And l r) = wordCount l + wordCount r + 1
wordCount (NV n _) = nounCount n + 1
wordCount (NVN n1 _ n2) = nounCount n1 + 1 + nounCount n2 
wordCount (End) = 0

-- Helper Function to count the different Noun cases
nounCount :: Noun -> Int
nounCount (NP _ n) = 1 + nounCount n
nounCount (NAnd n1 n2) = nounCount n1 + 1 + nounCount n2
nounCount (_) = 1

main = do
	let s1 = buildS2 Cats Hug Bunnies
	let s2 = buildS1 Cats Cuddle
	let s3 = buildNP Silly Ducks
	let s4 = buildNAnd Dogs Cats
	let s5 = conjunction [s2, s1]
	let s6 = buildS2 s3 Chase s4	
	let s7 = buildS1 Dogs Scare
	let s8 = conjunction [s1, s2, s7]
	let s9 = buildNP Old s3
	let s10 = buildS2 s9 Cuddle Cats

	putStrLn " s1 "
	print (s1)
	print (pretty(s1))
	putStrLn" Is nice s1 "
	print (isNice s1)
	putStrLn" Word count s1 "
	print (wordCount s1)

	putStrLn " s2 "
	print (s2)
	print (pretty(s2))
	putStrLn" Is nice s2 "
	print (isNice s2)
	putStrLn" Word count s2 "
	print (wordCount s2)

	putStrLn " s5 "
	print (s5)
	print (pretty(s5))
	putStrLn" Is nice s5 "
	print (isNice s5)
	putStrLn" Word count s5 "
	print (wordCount s5)  

	putStrLn " s6 "
	print (s6)
	print (pretty(s6))
	putStrLn" Is nice s6 "
	print (isNice s6)
	putStrLn" Word count s6 "
	print (wordCount s6)
	
	putStrLn " s7 "
	print (s7)
	print (pretty(s7))
	putStrLn" Is nice s7 "
	print (isNice s7)
	putStrLn" Word count s7 "
	print (wordCount s7)
	
	putStrLn " s8 "
	print (s8)
	print (pretty(s8))
	putStrLn" Is nice s8 "
	print (isNice s8)
	putStrLn" Word count s8 "
	print (wordCount s8)
	
	putStrLn " s10 "
	print (s10)
	print (pretty(s10))
	putStrLn" Is nice s10 "
	print (isNice s10)
	putStrLn" Word count s10 "
	print (wordCount s10)