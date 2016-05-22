import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import GameOfLife

main :: IO ()
main = hspec $ do
    describe "GameOfLife" $ do
        describe "NextGeneration" $ do
            it "Create valid next glider" $ do
                nextGeneration (glider 5 5) `shouldBe` [
                        [False,False,False,False,False],
                        [True,False,True,False,False],
                        [False,True,True,False,False],
                        [False,True,False,False,False],
                        [False,False,False,False,False]]

            it "Should kill all" $ do
                nextGeneration (gameGrid (2, 2) [(1,1),(2,1)]) `shouldBe` [[False, False],[False, False]]

            it "Should born new cell" $ do
                nextGeneration (gameGrid (3, 3) [(1,1),(2,1), (1,2)]) `shouldBe` [[True,True,False],[True,True,False],[False,False,False]]

        describe "Show grid" $ do
            it "Should render right" $ do
                showGrid (glider 5 5) `shouldBe` "-@---\n--@--\n@@@--\n-----\n-----\n"
            it "Should not fail on empty" $ do
                showGrid ([]) `shouldBe` ""
