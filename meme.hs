import System.IO  
import System.Environment
import Data.Char

data MWord = Adj String | Noun String | Verb String | PNoun String | Place String | Unknown String | VUnknown String deriving (Show)
{-VUnknown is verified unknown, meaning that it is not part of a multi-word POS-}

splitBy cha = foldr f [[]] 
            where f c l@(x:xs) | c == cha = []:l
                             | otherwise = (c:x):xs

is_in :: (String, FilePath)-> IO Bool
is_in (wrd, fname) =
	do
		f <- readFile fname
		let words = lines f
		return $ ( elem (wrd) words )
	
{-my hacky solution to having an IO Bool i can use in wtMW when the word is too short to 'init'-}
iof = do
		putStr("")
		return $ False

{- maps word to its part of speech -} 
word_to_MWord :: String -> IO MWord
word_to_MWord wrd =
	do
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
		{-ok. this is dumb, i know-}
to_ioMW :: MWord -> IO MWord
to_ioMW mw =
	do
		putStr ""
		return $ mw

map_unknowns :: [MWord] -> [IO MWord]
map_unknowns lst =
	let
		has_unknown :: [MWord] -> Bool
		has_unknown lst = elem True ((map (\x -> case x of Unknown(i) -> True; _ -> False)) lst)
		consolidate_unknowns :: [MWord] -> [MWord]
		consolidate_unknowns lst =
			if has_unknown lst then
				case lst of
					Unknown(x):Unknown(y):xs -> consolidate_unknowns(VUnknown(x ++ " " ++ y):xs)
					VUnknown(x):Unknown(y):xs -> consolidate_unknowns(VUnknown(x ++ " " ++ y):xs)
					x:y -> x:consolidate_unknowns(y)
			else lst
		map_VU :: [MWord] -> [IO MWord]
		map_VU lst =
			case lst of
				[] -> []
				VUnknown(x):xs -> word_to_MWord(x):map_VU(xs)
				x:xs -> to_ioMW(x):map_VU(xs)
	in
		map_VU(consolidate_unknowns(lst))

sentence_to_mapped :: String -> [IO MWord]
sentence_to_mapped str = (map word_to_MWord(splitBy ' ' str))

			{- (positive option, negative option), top_text, bottom_text -} 
to_meme :: [MWord] -> [([String], (String, String))]
to_meme mw =
	case mw of
		[] -> [(["bad luck brian"], ("tried to make a meme from this article", "failed"))]
		PNoun(x):Verb(y):Verb(z):xs -> [(["blb"], (x, y))]
		Adj(x):Noun(y):Verb(z):xs -> [(["success kid", "bad luck brian"], ((x ++ " " ++ y), (z)))] {- idk -} 
	{- TODO: pattern match on things w/ no constructors in this situation - i -} 
		{- need to be able to fit any word somewhere -} 
			{-maybe make 'Any' type-}

	{- one of the meme type options will be chosen based on sent analysis on z ggg/blb -} 
		Place(x):Noun(y):Verb(z):Noun(q):xs -> [(["success kid", "scumbag steve"], ((y ++ " goes to " ++ x), (z ++ " " ++ q))), (["success kid", "bad luck brian"], (q ++ " in " ++ x, "gets " ++ init z ++ "ed"))]
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
		
main =
	do
		{-TODO: incorporate map_unknown into sentence_to_mapped to make this less gross-}
		a <- getArgs
		let senny = sentence_to_mapped(head a)
		noIO <- (sequence senny)
		let unk = map_unknowns noIO
		nOIO <- (sequence unk)
		{-with_delims <- sequence(pp_with_delim(to_meme(noIO)))-}
		with_delims <- sequence(pp_with_delim(to_meme(nOIO)))
		print(with_delims)
