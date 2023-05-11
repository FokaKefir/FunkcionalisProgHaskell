import Data.Char
import Data.List

type Hash = Integer

hash :: String -> Hash
hash str = auxHash str 1
    where
        auxHash [] i = 0
        auxHash (c : str) i = i * 2 ^ (ord c) + auxHash str (i + 1)

type Salt = String
type KDF = Salt -> String -> String
kdf1 :: KDF
kdf1 salt str = str ++ salt ++ reverse str

kdf2 :: Int -> KDF
kdf2 num salt str = take num auxStr
    where
        auxStr
            | null salt = cycle str
            | otherwise = str ++ cycle salt

data Password = P Hash Salt
    deriving (Show)


mkPassword :: KDF -> Salt -> String -> Password
mkPassword funcKDF salt str = P hashV salt
    where
        hashV = hash $ funcKDF salt str 

-- mkPassword (kdf2 32) "Sa77t" "Hello"
passwd1 = P 8322950956743630369429806361109069824 "Sa77t"

-- mkPassword (kdf2 128) "LongSalt" "short"
passwd2 = P 91068695580102028587879960541663330304 "LongSalt"

-- checkPassword passwd1 [kdf1, kdf2 16, kdf2 20, kdf2 32] "Hello"
checkPassword :: Password -> [KDF] -> String -> Bool
checkPassword (P hashV salt) lsKDF str = elem hashV (map func lsKDF)
    where
        func kdf = hash $ kdf salt str

type Alphabet  = [Char]
alphabet :: Alphabet
alphabet = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

numToWord :: Alphabet -> Integer -> String
numToWord alph num = auxNumToWord alph num b
    where
        b = fromIntegral $ length alph
        auxNumToWord alph num b
            | num < b = [alph !! fromIntegral num]
            | otherwise = alph !! tmp : auxNumToWord alph (num `div` b) b
                where 
                    tmp = fromIntegral $ mod num b

type Separator = Char
separator :: Char
separator = '$'

renderPassword :: Alphabet -> Separator -> Password -> String
renderPassword alph separator (P hashV salt) = numToWord alph hashV ++ [separator] ++ salt

wordToNum :: Alphabet -> String -> Maybe Integer
wordToNum alph str = auxWordToNum alph str b 
    where
        b = fromIntegral $ length alph
        auxWordToNum alph "" b = Just 0
        auxWordToNum alph (c : str) b 
            | notElem c alph = Nothing
            | otherwise = res
                where
                    resc = auxWordToNum alph str b 
                    Just k = elemIndex c alph
                    res = case resc of
                        Nothing -> Nothing
                        Just res1 -> Just (fromIntegral k + b * res1)
            
                
