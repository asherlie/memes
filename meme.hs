import System.IO  

import System.Environment
import Data.List
import Data.Text

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import NLP.POS
import NLP.Corpora.Conll
import NLP.Types


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
            write_pats :: NLP.Types.Tag t => (POSTagger t, [String], FilePath) -> IO [()]
            write_pats(tagger, x, y) =
                  let
                        write_pat :: String -> IO ()
                        write_pat str =
                              do
                                    let tagged = tagStr tagger str
                                    appendFile y (tagged ++ "\n")
                  in
                        do
                              sequence (Data.List.map write_pat x)
      in
            do
                  tagger <- defaultTagger
                  arts <- load_json f_art
                  write_pats(tagger, (Data.List.map (\(x:y:xs) -> x)arts), f_write)


stm_ch :: FilePath -> IO [TaggedSentence NLP.Corpora.Conll.Tag]
stm_ch f_art =
      do
            tagger <- defaultTagger
            arts <- load_json f_art
            return $ Data.List.map Data.List.head (Data.List.map (tag tagger) (Data.List.map pack (Data.List.map Data.List.head arts)))

to_meme_ch_cons :: TaggedSentence NLP.Corpora.Conll.Tag -> [([String], [(String, String)])]
to_meme_ch_cons ts =
      let
            tagged tagged_sent = (\(TaggedSent(x)) -> x)tagged_sent{- (Data.List.head tagged_sent) -}
            to_meme :: [POS NLP.Corpora.Conll.Tag] -> [([String], [(String, String)])]
            to_meme pos_l =
                  let 
                        tag_tok_tuple = Data.List.map (\x -> (posTag x, posToken x)) pos_l

                        get_str :: Token -> String
                        get_str pos_t = unpack ((\(Token(x)) -> x)pos_t)

                        parse_pos_l :: [(NLP.Corpora.Conll.Tag, Token)] -> [([String], [(String, String)])]
                        parse_pos_l inp =
                              case inp of
                                     [] -> [(["bad luck brian"], [("tried to make a meme from this article", "failed")])]
                                     (JJ, a):(NNP, b):(VBZ, c):xs -> [(["the most interesting man in the world"], [("i don't always " ++ get_str c, "but when i do, i'm " ++ get_str a)])]
                                     (NNP, a):(VBZ, b):(NNP, c):xs -> [(["success kid", "bad luck brian"], [(get_str a ++ " " ++ get_str b, get_str c)]),(["success kid", "bad luck brian"], [(get_str a ++ " " ++ get_str b, get_str c)])]
                                     (NNP, a):(NNP, b):(NNP, c):xs ->[(["good guy greg"], [(get_str a, get_str b)])]
                                     x:xs -> parse_pos_l xs
                  in
                        parse_pos_l tag_tok_tuple 

      in
            to_meme (tagged ts)
                             
                            
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
                        xp = if Data.List.length x == 2 then (\(x:y:xs) -> x ++ "|" ++ y) x else Data.List.head x {- separates pos from neg option in meme type -}
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
            case (Data.List.map delim_one m_lst) of
                  x:y:[] -> x ++ "&&" ++ y
                  x:[]   -> x
            
write_delim_memes_to_file :: (FilePath, FilePath) -> IO [IO ()]
write_delim_memes_to_file(f_art, f_write) = 
      let
            write_to_file(x, y) =
                  Data.List.map (appendFile y) (Data.List.map (++"\n")x)
      in
            do
                  arts <- stm_ch f_art
                  let memes = Data.List.map add_delims (Data.List.map to_meme_ch_cons arts)
                  return $ write_to_file(memes, f_write)

main =
      do
            a <- getArgs
            case a of
                  [a, b, "-w"] -> do
                                    write_pats_from_art(b, a)
                                    putStr ""
                  a:b:xs       -> do
                                    writeIO <- write_delim_memes_to_file(a, b)
                                    sequence writeIO
                                    putStr ""
                  _            -> putStrLn "<article file> <output file> [-w]\n     -w : write POS tagged articles to file for pattern finding"
