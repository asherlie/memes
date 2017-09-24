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

data CH_mw = PRP  String | IN String | CD String | RB String | VBP String | VBN String | VBG String | VB String | JJ String | NNS String | NN String | NNP String | VBZ String | CH_unk String deriving (Show)
{- used for any type taken, if usign this stm_l should make everything an A() -}
data ANY = A CH_mw

splitBy cha = foldr f [[]] 
            where f c l@(x:xs) | c == cha = []:l
                             | otherwise = (c:x):xs


{-using this for now to increase speed significantly-}
tagger = unsafePerformIO defaultTagger


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
load_json :: FilePath -> IO [[String]]
load_json str =
      do
            {-:set -XOverloadedStrings-}
            let get_it = B.readFile str
            jj <- get_it
            let decoded = decode jj :: Maybe [[String]]
            return $ ((\(Just(x)) -> x)decoded)

{-tags articles from file, writes tagged articles to another file in a format compatible with find_pat-}
{-this function is mean to to be used in conjunction with the python pattern finder-}
write_pats_from_art :: (FilePath, FilePath) -> IO [()]
write_pats_from_art(f_write, f_art) =
      let
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
      in
            do
                  arts <- load_json f_art
                  write_pats((map (\(x:y:xs) -> x)arts), f_write)

stm_l :: FilePath -> IO [IO [CH_mw]]
stm_l f_art =
      let
            stm_e :: String -> IO [CH_mw]
            stm_e str =
                  let
                        {- the [[String]] in the beginning is basically just [(String, String)] representing [POS, word] -}
                        {-shitty way to to do this - i can definitely figure out a way to use tag to expose original constructors-}
                        to_MW :: [[String]] -> [CH_mw]
                        to_MW(sll) =
                              case sll of
                                    x:xs -> 
                                          case x of
                                                f:s:[] -> if s == "NNP" then NNP(f):to_MW(xs) else if s == "PRP" then PRP(f):to_MW(xs) else if s == "IN" then IN(f):to_MW(xs) else if s == "CD" then CD(f):to_MW(xs) else if s == "RB" then RB(f):to_MW(xs) else if s == "VBP" then VBP(f):to_MW(xs) else if s == "VBN" then VBN(f):to_MW(xs) else if s == "VBG" then VBG(f):to_MW(xs) else if s == "VB" then VB(f):to_MW(xs) else if s == "JJ" then JJ(f):to_MW(xs) else if s == "NNS" then NNS(f):to_MW(xs) else if s == "NN" then NN(f):to_MW(xs) else if s == "VBZ" then  VBZ(f):to_MW(xs) else CH_unk(f):to_MW(xs)
                                                _      -> []
                                    []   -> []
                              
                  in
                        do
                              return $ (to_MW((map (\x -> splitBy '/' x)(splitBy ' ' (tagStr tagger str)))))
      in
            do
                  arts <- load_json f_art
                  return $ map stm_e (map (\(x:y) -> x) arts)
                  {- (positive option, negative option), top_text, bottom_text -} 
{-
 -                the vestiges of the non chatter - keeping around to possibly adapt to chatter:
 -
 -            [] -> [(["bad luck brian"], [("tried to make a meme from this article", "failed")] )]
 -            Adj(x):Noun(y):Verb(z):Noun(q):xs -> [(["The Most Interesting Man In The World"], [( ("i don't always " ++ z ++ " " ++ q), ("but when i do, i'm " ++ x))]), (["Am I The Only One Around Here"], [(("am i the only one around here"), ("who " ++ z ++ " " ++ q))]), (["success kid", "badluck brian"], [((x ++ " " ++ y), (z ++ " " ++ q))])]
 -      {- one of the meme type options will be chosen based on sent analysis on z ggg/blb -} 
 -            Noun(x):Verb("loses"):Noun(y):xs -> [(["This Is Where I'd Put My Trophy If I Had One"], [(("this is where I would put my " ++ y), ("if I had one"))])]            
 -            Place(x):Noun(y):Verb(z):Noun(q):xs -> [(["The Most Interesting Man In The World"], [( ("i don't always " ++ z), ("but when i do, i'm in " ++ x) )]), (["success kid", "scumbag steve"], [((y ++ " goes to " ++ x), (z ++ " " ++ q))]), (["success kid", "bad luck brian"], [(q ++ " in "++ x, "gets " ++ init z ++ "ed")])]
 -            Noun(x):Verb(y):Noun(z):xs -> [(["The Most Interesting Man In The World"], [( ("i don't always " ++ y), ("but when i do, it's " ++ z))])]
 -}

to_s :: CH_mw -> String
to_s inp = 
      case inp of
            NNP(x) -> x
            _      -> ""
to_meme_CH :: [CH_mw] -> [([String], [(String, String)])]
to_meme_CH mw =
      case mw of
            NNP(x):VBZ(y):NNP(z):xs -> [(["success kid", "bad luck brian"], [(x ++ " " ++ y, z)]),(["success kid", "bad luck brian"], [(x ++ " " ++ y, z)])]
            [] -> [(["bad luck brian"], [("tried to make a meme from this article", "failed")])]
            {-VBZ(x):xs -> [([""], [("","")]), ([""], [("", "")])]-}
            x:xs -> to_meme_CH(xs)
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
             -}

            delim_one :: ([String], [(String, String)]) -> String
            delim_one (x, y) =
                  let
                        xp = if length x == 2 then (\(x:y:xs) -> x ++ "|" ++ y) x else head x {- separates pos from neg option in meme type -}
                        top_bot y =
                              case y of
                                    [(i, j), (q, z)] -> i ++ "#%" ++ j ++ "@@" ++ q ++ "#%" ++ z {- ++ "&&" -}
                                    [(i, j)]         -> i ++ "#%" ++ j {- ++ "&&" -}
                  in
                        xp ++ "^^" ++ (top_bot y)
            add_brackets :: [String] -> String            
            add_brackets sl =
                  case sl of
                        {- assume for now that there should only ever be two outermost ops -}
                        x:y:[] -> "[\"" ++ x ++ "\",\"" ++ y ++ "\"]"
                        x:[]   -> "[\"" ++ x ++ "\"]"
                        _ -> ""
      in
            case (map delim_one m_lst) of
                  x:y:[] -> x ++ "&&" ++ y
                  x:[]   -> x
            
write_delim_memes_to_file :: (FilePath, FilePath) -> IO [IO ()]
write_delim_memes_to_file(f_art, f_write) = 
      let
            write_to_file(x, y) =
                  map (appendFile y) (map (++"\n")x)
                  {-map (appendFile y)x-}
      in
            do
                  artIOIO <- stm_l f_art
                  let artIO = sequence artIOIO
                  art <- artIO
                  let memes = map add_delims (map to_meme_CH art)
                  {-return memes-}
                  return $ write_to_file(memes, f_write)

main =
      do
            a <- getArgs
            putStr (head (tail a))
            writeIO <- write_delim_memes_to_file(head a, head(tail a))
            sequence writeIO
