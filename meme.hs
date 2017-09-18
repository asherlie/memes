import System.IO.Unsafe

import System.IO  
import System.IO.Strict

import System.Environment
import Data.Char
import Data.List
{-import Data.Text-}

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import NLP.POS
{-import NLP.Types.Tree-}
import NLP.Types

data MWord = Adj String | Noun String | Verb String | Place String | Num String | Unknown String | PNoun String | VUnknown String deriving (Show)

{- below is for use with chatter lib -}
data CH_mw = PRP  String | IN String | CD String | RB String | VBP String | VBN String | VBG String | VB String | JJ String | NNS String | NN String | NNP String | VBZ String | CH_unk String deriving (Show)

{- VUnknown is verified unknown, meaning that it is not part of a multi-word POS - just realized it's only used within map_unknowns and is reclassified using word_to_MWord after - whatever. -}

splitBy cha = foldr f [[]] 
            where f c l@(x:xs) | c == cha = []:l
                             | otherwise = (c:x):xs

is_in :: (String, FilePath)-> IO Bool
is_in (wrd, fname) =
      do
            {- TODO: stop reading from file to check for each individual word -}
            f <- System.IO.Strict.readFile fname
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

{-using this for now to increase speed significantly-}
tagger = unsafePerformIO defaultTagger

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
                                                                  {-pass it thru the -}
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
            has_unknown lst = case lst of [] -> False; Unknown(x):xs -> True; x:y -> has_unknown(y);
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

{-TODO: deal with camelcase. things are tagged as proper nouns bc of camelcase-}
{-this function is mean to to be used in conjunction with the python pattern finder-}

{-
 - {-the point of this is to expose the constructors behind tag to maek pattern matching easier once this all compiles -}
 -tag_str :: String -> taggedSentence
 -tag_str str =
 -      let
 -            sent_to_lst :: Sentence -> [String]
 -            {-sent_to_lst :: (Sentence, [NLP.Corpora.Conll.Tag]) -> -}
 -            sent_to_lst sent = Prelude.map (\(Token(x)) -> x) ((\(Sent(i)) -> i)((\(x,y)->x)unz))
 -      in
 -            do
 -                  tagger <- defaultTagger
 -                  let tagged = unzipTags (tag tagger (pack str))
 -
 -            {-applyTags tagged ((\(x,y) -> y)tagged) ((\(x,y) -> x)tagged) -}
 -            {-lol this just retags. v unproductive-}
 -
 -}

{-tags articles from file, writes tagged articles to another file in a format compatible with find_pat-}
write_pats_from_art :: (FilePath, FilePath) -> IO [()]
write_pats_from_art(f_write, f_art) =
      let
            load_json :: FilePath -> IO [[String]]
            load_json str =
                  do
                        {-:set -XOverloadedStrings-}
                        let get_it = B.readFile str
                        jj <- get_it
                        let decoded = decode jj :: Maybe [[String]]
                        return $ ((\(Just(x)) -> x)decoded)

            write_pats :: ([String], FilePath) -> IO [()]
            write_pats(x, y) =
                  let
                        write_pat :: String -> IO ()
                        write_pat str =
                              do
                                    {-tagger <- defaultTagger-}
                                    let tagged = tagStr tagger str
                                    appendFile y (tagged ++ "\n")
                  in
                        do
                              sequence (map write_pat x)
            {-i want to replace all words with lower case unless they're a place-}
            upper str = map Data.Char.toLower str
            {- this is not used with anything but NYTimes -}
            prep_lst :: [String] -> [IO String]
            prep_lst lst = 
                  let
                        prep_str :: String -> IO String
                        prep_str str = 
                              do
                                    is_place <- is_in(str, "places")
                                    {-is_place <- iof-}
                                    return $ if Data.Char.isUpper ((\(x:xs) -> x) str) && is_place then str else map Data.Char.toLower str
                  in
                        map prep_str lst
      in
            do
                  arts <- load_json f_art
                  {-prepped_lst <- sequence (prep_lst (map (\(x:y:xs) -> x) arts))-}
                  {-write_pats(prepped_lst, f_write)-}
                  write_pats((map (\(x:y:xs) -> x)arts), f_write) {- don't need to convert to lowercase if not using NYTimes -}
                  {-write_pats((map (\(x:y:xs) -> upper x) arts), f_write)-}

{-stm_e :: String -> IO [MWord]-}
{-stm_e :: (NLP.Types.Tag, String) -> IO [CH_mw]-}
stm_l :: FilePath -> IO [IO [CH_mw]]
stm_l f_art =
      let
            stm_e :: String -> IO [CH_mw]
            stm_e str =
                  let
                        contains :: (String, String) -> Bool
                        contains(subs, s) =
                              let
                                    f :: (String, Int) -> String
                                    f(str, x) =
                                         case str of
                                                [] -> ""
                                                o:r -> if x == 0 then "" else ([o] ++ (f(r, x-1)))
                              in
                                    case s of
                                          x:y -> if (length (x:y)) < (length subs) then False else if f(s, (length subs)) == subs then True else contains(subs, y)
                                          _   -> False

                        {-to_MW :: [[String]] -> [MWord]-}
                        {- the [[String]] in the beginning is basically just [(String, String)] representing [POS, word] -}
                        to_MW :: [[String]] -> [CH_mw]
                        to_MW(sll) =
                              case sll of
                                    x:xs -> 
                                          case x of
                                                      {- other POS should be included in this one block of if then else's -}
                                                {-f:s:[] -> if s == "NNP" then NNP(f):to_MW(xs) else if contains("NN", s) then Noun(f):to_MW(xs) else if contains("VB", s) then Verb(f):to_MW(xs) else Unknown(f):to_MW(xs) {- should be the only case -}-}
                                                f:s:[] -> if s == "NNP" then NNP(f):to_MW(xs) else if s == "PRP" then PRP(f):to_MW(xs) else if s == "IN" then IN(f):to_MW(xs) else if s == "CD" then CD(f):to_MW(xs) else if s == "RB" then RB(f):to_MW(xs) else if s == "VBP" then VBP(f):to_MW(xs) else if s == "VBN" then VBN(f):to_MW(xs) else if s == "VBG" then VBG(f):to_MW(xs) else if s == "VB" then VB(f):to_MW(xs) else if s == "JJ" then JJ(f):to_MW(xs) else if s == "NNS" then NNS(f):to_MW(xs) else if s == "NN" then NN(f):to_MW(xs) else if s == "VBZ" then  VBZ(f):to_MW(xs) else CH_unk(f):to_MW(xs)
                                                _ -> []
                                    []   -> []
                              
                  in
                        do
                              {-tagger <- defaultTagger-}
                              return $ (to_MW((map (\x -> splitBy '/' x)(splitBy ' ' (tagStr tagger str)))))
                        {-shitty way to to do this - i can definitely figure out a way to use tag to expose original constructors-}
                              {-return $ ((map (\x -> splitBy '/' x)(splitBy ' ' (tagStr tagger str)))))-}
            load_json :: FilePath -> IO [[String]]
            load_json str =
                  do
                        let get_it = B.readFile str
                        jj <- get_it
                        let decoded = decode jj :: Maybe [[String]]
                        return $ ((\(Just(x)) -> x)decoded)
      in
            do
                  arts <- load_json f_art
                  {-return $ map stm_e ((\(x:y:xs) -> x)arts)-}
                  return $ map stm_e (map (\(x:y) -> x) arts)
                  {-return $ map stm_e arts-}
sentence_to_mapped :: String -> [IO MWord]
sentence_to_mapped str =
      let
            cap_to_space str =
                  case str of
                        []  -> [] 
                        x:y -> if Data.Char.isUpper x then ' ':(Data.Char.toLower x):cap_to_space(y) else x:cap_to_space(y)
      in
            (map word_to_MWord(splitBy ' ' (cap_to_space(str))))
            

                  {- (positive option, negative option), top_text, bottom_text -} 
to_meme :: [MWord] -> [([String], [(String, String)])]
to_meme mw =
      case mw of
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

to_meme_CH :: [CH_mw] -> [([String], [(String, String)])]
to_meme_CH mw =
      case mw of
            [] -> [(["bad luck brian"], [("tried to make a meme from this article", "failed")] )]
            VBZ(x):xs -> [([""], [("","")]), ([""], [("", "")])]
            x:y -> to_meme_CH(y)
add_delims :: [([String], [(String, String)])] -> String
add_delims m_lst =
      let
            {-
             -delim guide:
             -      |  :: separates positive option from negative
             -      ^^ :: separates meme type from content
             -      #% :: separates top text from bottom text
             -      @@ :: separates positive (top, bottom) tuple from negative
             -      && :: separates two equally plausible meme options to be chosen at random
             -      [] :: separates 
             -}

            delim_one :: ([String], [(String, String)]) -> String
            delim_one (x, y) =
                  let
                        xp = if length x == 2 then (\(x:y:xs) -> x ++ "|" ++ y) x else head x {- separates pos from neg option in meme type -}
                        top_bot y =
                              case y of
                                    [(i, j), (q, z)] -> i ++ "#%" ++ j ++ "@@" ++ q ++ "#%" ++ z ++ "&&"
                                    [(i, j)]         -> i ++ "#%" ++ j ++ "&&"
                  in
                        xp ++ "^^" ++ (top_bot y)
            add_brackets :: [String] -> String            
            add_brackets sl =
                  case sl of
                        x:y:xs -> "[\"" ++ x ++ "\",\"" ++ y ++ "\"]"++add_brackets(xs)
                        _ -> ""
      in
            add_brackets(map delim_one m_lst)
            
{-write_delim_memes_to_file :: (FilePath, FilePath) -> IO ()-}
write_delim_memes_to_file(f_art, f_write) = 
      let
            write_pats :: ([String], FilePath) -> IO [()]
            write_pats(x, y) =
                  let
                        write_pat :: String -> IO ()
                        write_pat str =
                              do
                                    appendFile y (str++ "\n")
                  in
                        do
                              sequence (map write_pat x)
      in
            do
                  artIOIO <- stm_l f_art
                  let artIO = sequence artIOIO
                  art <- artIO
                  let memes = map add_delims (map to_meme_CH art)
                  return memes

{-main =-}
      {-do-}
            {-{-TODO: incorporate map_unknown into sentence_to_mapped to make this less gross-}-}
            {-a <- getArgs-}
            {-{-let senny = sentence_to_mapped(head a)-}-}
            {-let senny = stm_e(head a)-}
            {-{-noIO <- (sequence senny)-}-}
            {-noIO <- (senny)-}

            {-{- won't be many unknowns with chatter -}-}
            {-let unk = map_unknowns noIO-}
            {-nOIO <- (sequence unk)-}
            {-{-with_delims <- sequence(pp_with_delim(to_meme(noIO)))-}-}

            {-{-print(nOIO)-}-}

            {-{-with_delims <- sequence(pp_with_delim(to_meme(nOIO)))-}-}
            {-{-with_delims <- sequence(pp_with_delim(to_meme(noIO)))-}-}
            {-with_delims <- sequence(pp_with_delim(to_meme_CH(noIO)))-}
            {-print(with_delims)-}
