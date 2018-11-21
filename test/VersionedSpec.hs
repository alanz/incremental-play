module VersionedSpec (main, spec) where

import           Test.Hspec
import           Language.Incremental.Versioned

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  hspec spec

-- ---------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Versioned data" $ do
    it "get / set Versioned data" $ do
      let
        id1 = firstId
        vd = new id1 "a"
      get vd `shouldBe` "a"
      let vd2 = set id1 vd "b"
      get vd2 `shouldBe` "b"

    -- ---------------------------------

    it "simple change notification Versioned data" $ do
      let
        vd = new firstId "a"
      changed vd `shouldBe` False
      let vd2 = set firstId vd "b"
      changed vd2 `shouldBe` True

    -- ---------------------------------

    it "simple change notification Versioned data" $ do
      let vd = new firstId "a"
      changed vd `shouldBe` False
      let vd2 = set firstId vd "b"
      changed vd2 `shouldBe` True

    -- ---------------------------------

    it "complex change notification Versioned data" $ do
      let
        id1 = firstId
        id2 = nextId id1
        id3 = nextId id2
        id4 = nextId id3
      let vd = new id1 "a"
      changed vd `shouldBe` False
      let
        vd2 = set id4 vd "b"
      changed vd2 `shouldBe` True
      changed_versions vd2 id2 id3 `shouldBe` False
      changed_versions vd2 id2 id4 `shouldBe` True
