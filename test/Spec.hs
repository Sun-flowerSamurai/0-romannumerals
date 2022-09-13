import Test.Hspec
import Control.Exception (evaluate)
import Test.QuickCheck
import RomanNumbers (r2i, i2r)

-- A QuickCheck generator for natural number between 1 and 10000.
naturals :: Gen Int
naturals = choose (1, 10000)

main :: IO ()
main = hspec $ do
  describe "RomanNumbers" $ do
    describe "r2i" $ do

      it "should convert \"I\" to 1" $ do
        r2i "I" `shouldBe` (1::Int)
      it "should convert \"V\" to 5" $ do
        r2i "V" `shouldBe` (5::Int)
      it "should convert \"LXX\" to 70" $ do
        r2i "LXX" `shouldBe` (70::Int)
      it "should convert \"MCMIX\" to 2111" $ do
        r2i "MCMIX" `shouldBe` (2111::Int)
      it "should convert \"MDCLXVI\" to 1666" $ do
        r2i "MDCLXVI" `shouldBe` (1666 ::Int)
      it "should return an error when not a valid Roman number" $ do
        evaluate (r2i "Javascript") `shouldThrow` anyErrorCall

      -- Here go your own tests for r2i.
  
    describe "i2r" $ do

      it "should convert 1 to \"I\"" $ do
        i2r (1::Int) `shouldBe` "I"
      it "should convert 5 to \"V\"" $ do
        i2r (5::Int) `shouldBe` "V"
      it "should convert 70 to \"LXX\"" $ do
        i2r (70::Int) `shouldBe` "LXX"
      it "should convert 1234 to \"MCCXXXIIII\"" $ do
        i2r (1234::Int) `shouldBe` "MCCXXXIIII"
      it "should convert 1666 to \"MDCLXVI\"" $ do
        i2r (1666::Int) `shouldBe` "MDCLXVI"
      it "should convert 0 to \"\"" $ do
        i2r (0::Int) `shouldBe` ""
      it "should return an error if given a negative number" $ do
        evaluate (i2r (-1 ::Int)) `shouldThrow` anyErrorCall 
      
      -- Here go your own tests for i2r.


-- Do not change anything below this line
--
    describe "r2i . i2r" $ do

      it "(forall n : n in N : (r2i . i2r) n >= 1)" $ property $
        forAll naturals (\n -> (r2i . i2r) n >= (1::Int))

      -- Here go your own tests for r2i . i2r.
