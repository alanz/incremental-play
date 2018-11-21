import Test.Hspec.Runner
import qualified Spec

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  hspec Spec.spec

