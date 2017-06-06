import System.IO  
import System.Environment
import Data.Char

data MWord = Adj String | Noun String | Verb String | Place String | Num String | Unknown String | VUnknown String deriving (Show)
{- VUnknown is verified unknown, meaning that it is not part of a multi-word POS - just realized it's only used within map_unknowns and is reclassified using word_to_MWord after - whatever. -}

splitBy cha = foldr f [[]] 
            where f c l@(x:xs) | c == cha = []:l
                             | otherwise = (c:x):xs

is_in :: (String, FilePath)-> IO Bool
is_in (wrd, fname) =
      do
            {- TODO: stop reading from file to check for each individual word -}
            f <- readFile fname
            let words = lines f
            return $ ( elem (wrd) words )
      
{-my hacky solution to having an IO Bool i can use in wtMW when the word is too short to 'init'-}
iof = do
            putStr("")
            return $ False

is_dig str =
      case str of
            []  -> True
            x:y -> if (not ((Data.Char.isDigit x) || (x == '.') )) then False else is_dig(y)

{- maps word to its part of speech -} 
word_to_MWord :: String -> IO MWord
word_to_MWord wrd =
      if wrd == "" then to_ioMW(Unknown("")) else 
      if is_dig wrd then to_ioMW(Num(wrd)) else
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
                                                            if Data.Char.isUpper (head wrd) then
                                                            {- makes things a lot slower but NYTimes uses camelcase for their article titles for some reason... -}
                                                                  word_to_MWord (Data.Char.toLower(head wrd):(tail wrd))
                                                            else
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
to_meme :: [MWord] -> [([String], [(String, String)])]
to_meme mw =
      case mw of
{-change form to this: based on sentiment, a top bottom pair will be chosen - lets me use sent analysis with things that don't fit the same format-}
      {-  [ (["positive", "negative"], [("top", "bottom"), ("top", "bottom")] ) ]  -}
            {-  maybe change the [pos, neg] to be a (pos, neg)  -}
                  {-they need to be lists in case there's one option-}
                        {- also opens the option of a neutral choice -}

            [] -> [(["bad luck brian"], [("tried to make a meme from this article", "failed")] )]
      {- noun, verb, noun { -}
            Adj(x):Noun(y):Verb(z):Noun(q):xs -> [(["The Most Interesting Man In The World"], [( ("i don't always " ++ z ++ " " ++ q), ("but when i do, i'm " ++ x))]), (["Am I The Only One Around Here"], [(("am i the only one around here"), ("who " ++ z ++ " " ++ q))]), (["success kid", "badluck brian"], [((x ++ " " ++ y), (z ++ " " ++ q))])]
      {- one of the meme type options will be chosen based on sent analysis on z ggg/blb -} 
            Noun(x):Verb("loses"):Noun(y):xs -> [(["This Is Where I'd Put My Trophy If I Had One"], [(("this is where I would put my " ++ y), ("if I had one"))])]            
            Place(x):Noun(y):Verb(z):Noun(q):xs -> [(["The Most Interesting Man In The World"], [( ("i don't always " ++ z), ("but when i do, i'm in " ++ x) )]), (["success kid", "scumbag steve"], [((y ++ " goes to " ++ x), (z ++ " " ++ q))]), (["success kid", "bad luck brian"], [(q ++ " in "++ x, "gets " ++ init z ++ "ed")])]
            Noun(x):Verb(y):Noun(z):xs -> [(["The Most Interesting Man In The World"], [( ("i don't always " ++ y), ("but when i do, it's " ++ z))])]
            {-  noun, verb, noun } -}
            {-unknown stuff - deals with chunk unknowns-}
            {-Noun(x):Unknown(y):     -}
            Place(x):[] -> [(["bad luck brian"], [(x, x)])]
            x:xs -> to_meme(xs)

pp_with_delim :: [([String], [(String, String)])] -> [IO ()]
pp_with_delim m_lst =
      let
            {-
             -delim guide:
             -      |  :: separates positive option from negative
             -      ^^ :: separates meme type from content
             -      #% :: separates top text from bottom text
             -      @@ :: separates positive (top, bottom) tuple from negative
             -      && :: separates two equally plausible meme options to be chosen at random
             -}
            p_one :: ([String], [(String, String)]) -> IO ()
            p_one (x, y) =
                  let
                        xp = if length x == 2 then (\(x:y:xs) -> x ++ "|" ++ y) x else head x {- separates pos from neg option in meme type -}
                        top_bot y =
                              case y of
                                    [(i, j), (q, z)] -> i ++ "#%" ++ j ++ "@@" ++ q ++ "#%" ++ z ++ "&&"
                                    [(i, j)]         -> i ++ "#%" ++ j ++ "&&"
                  in
                        putStr(xp ++ "^^" ++ (top_bot y))
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
            {-print(nOIO)-}
            with_delims <- sequence(pp_with_delim(to_meme(nOIO)))
            print(with_delims)
