import Test.QuickCheck (Gen)
import Test.QuickCheck (vectorOf)
import Test.QuickCheck.Gen (choose)
import Control.Monad (replicateM)
import Data.List (sort)
data Character = Character
  { strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)
modifier :: Int -> Int
modifier constitution = round (fromIntegral (div (constitution - 10) 2))
diceRoll :: Gen Int
diceRoll = choose (1,6)
ability :: Gen Int
ability = fmap (sum . take 3 . reverse . sort) (vectorOf 4 diceRoll)
character :: Gen Character
character = do
  strength <- ability
  dexterity <- ability
  constitution <- ability
  intelligence <- ability
  wisdom <- ability
  charisma <- ability
  let hitpoints = 10 + (modifier constitution)
  return $ Character strength dexterity constitution intelligence wisdom charisma hitpoints

