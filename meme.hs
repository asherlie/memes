import System.IO  
import System.Environment
import Data.Char

data MWord = Adj String | Noun String | Verb String | PNoun String | Place String | Unknown String deriving (Show)

splitBy cha = foldr f [[]] 
            where f c l@(x:xs) | c == cha = []:l
                             | otherwise = (c:x):xs

{- just checks if string exists in file -- returns inconvenient IO Bool. NOT BOOL -} 
is_in :: (String, FilePath)-> IO Bool
is_in (wrd, fname) =
	do
		f <- readFile fname
		let words = lines f
		{- let wOut = ( (init (init wrd)), ((last (init wrd)):(last wrd):[]) ) -} 
		{- return $ ( ((elem (wrd ++ "\r") words), "") || ((elem (wrd) words), "") || ((elem ((\(x, y) -> x) wOut) words), (\(x, y) -> y)wOut) ) -} 
		return $ ( (elem (wrd ++ "\r") words) || (elem (wrd) words))

	{- to fix: -} 
		{- is_in should operate on exact words -} 
		{- wOut should be defined in word_to_MWord -} 
	
{-my hacky solution to having an IO Bool i can use in wtMW when the word is too short to 'init'-}
iof = do
		{-print("")-}
		putStr("")
		return $ False
{- maps word to its part of speech -} 
word_to_MWord :: String -> IO MWord
word_to_MWord wrd =
	do
		{-let wOut = if ( (init (init wrd)), ((last (init wrd)):(last wrd):[]) ) -}
		is_place   <- is_in(wrd, "places")
		is_place_w <- is_in( (\(x:xs) -> Data.Char.toUpper(x):xs) wrd, "places")
		if is_place || is_place_w then return $ Place(wrd) else
			do
				is_adj <- is_in(wrd, "adj")
				if is_adj then return $ Adj(wrd) else
					do
						is_noun   <- is_in(wrd, "nouns")
						is_noun_w <- if length wrd > 1 then is_in(init wrd, "nouns") else iof
						if is_noun then return $ Noun(wrd) else 
							do
								is_verb    <- is_in(wrd, "verbs")
								{-is_verb_w  <- is_in((\(x, y) -> x)wOut, "verbs")-}
								is_verb_w  <- if length wrd > 2 then is_in((init (init wrd)), "verbs") else iof
								is_verb_ww <- if length wrd > 1 then is_in(init(wrd), "verbs") else iof
								if is_verb || is_verb_w || is_verb_ww then return $ Verb(wrd) else
									do
										return $ Unknown(wrd)


sentence_to_mapped :: String -> [IO MWord]
sentence_to_mapped str = map word_to_MWord(splitBy ' ' str)

			{- (positive option, negative option), top_text, bottom_text -} 
to_meme :: [MWord] -> [([String], (String, String))]
to_meme mw =
	case mw of
		[] -> [(["bad luck brian"], ("tried to make a meme from this article", "failed"))]
		PNoun(x):Verb(y):Verb(z):xs -> [(["blb"], (x, y))]
		Adj(x):Noun(y):Verb(z):xs -> [(["blb"], ((x ++ " " ++ y), (z)))] {- idk -} 
	{- TODO: pattern match on things w/ no constructors in this situation - i -} 
		{- need to be able to fit any word somewhere -} 
	{- one of the meme type options will be chosen based on sent analysis on z ggg/blb -} 
		Place(x):Noun(y):Verb(z):Noun(q):xs -> [(["success kid", "scumbag steve"], ((y ++ " goes to " ++ x), (z ++ " " ++ q))), (["success kid", "bad luck brian"], (q ++ " in " ++ x, "gets " ++ init z ++ "ed"))]
						{- TODO: remove | from definition above. make it a tuple (sk, st), to be pretty printed as sk | st, like above -} 
		{- Noun(x):xs -> [("", ("", ""))] -} 
		{- _ did _ -} 
		Place(x):[] -> [(["bad luck brian"], (x, x))]
		x:xs -> to_meme(xs)

pp_with_delim :: [([String], (String, String))] -> [IO ()]
pp_with_delim m_lst =
	let
		p_one :: ([String], (String, String)) -> IO ()
		p_one (x, (y, z)) =
			do
				{- these delimeters are used when parsing on the python side -} 
				let xp = if length x == 2 then (\(x:y:xs) -> x ++ "|" ++ y) x else head x
				putStr(xp ++ "^^") {- separates type from top from bottom -} 
				putStr(y ++ "^^")
				putStr(z ++ "&&") {- separates two equally valid options to be chosen at random -} 
	in
		map p_one m_lst
		
{- need to print out type with "( before it , ',' , ("top", "bottom") -} 
main =
	do
		a <- getArgs
		let senny = sentence_to_mapped(head a)
		noIO <- (sequence senny)
		with_delims <- sequence(pp_with_delim(to_meme(noIO)))
		print(with_delims)
		{- print(to_meme(noIO)) -} 
		{- print(noIO) -} 
