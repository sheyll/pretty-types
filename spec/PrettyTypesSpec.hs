{-# LANGUAGE UndecidableInstances #-}
module Main (main) where

import           Data.Type.Pretty
import           Test.Hspec
import GHC.TypeLits
import Data.Kind

main :: IO ()
main = hspec spec

-- shorter Proxy
data PX (k :: a) = PX

spec :: Spec
spec = describe "rendering" $ do

  describe "PrettyNat" $ do
    it "renders (PutNat 123) as 123" $
      ptShow (PX :: PX (PutNat 123)) `shouldBe` "123"
    it "renders ('PrettyNat ('PrettyPadded 10) ('PrettyPrecision 5) 'PrettyDec 123) as \"     00123\"" $
      ptShow (PX :: PX ('PrettyNat ('PrettyPadded 10) ('PrettyPrecision 5) 'PrettyDec 123))
        `shouldBe` "     00123"
    it "renders (PutHex8 123) as 7b" $
      ptShow (PX :: PX (PutHex8 123)) `shouldBe` "7b"
    it "renders (PutHeX8 123) as 7B" $
      ptShow (PX :: PX (PutHeX8 123)) `shouldBe` "7B"
    it "renders (PutHeX64 123) as 000000000000007B" $
      ptShow (PX :: PX (PutHeX64 123)) `shouldBe` "000000000000007B"
    it "renders (PutBits16 123) as 0000000001111011" $
      ptShow (PX :: PX (PutBits16 123)) `shouldBe` "0000000001111011"
    it "renders (PutBits32 123) as 00000000000000000000000001111011" $
      ptShow (PX :: PX (PutBits32 123)) `shouldBe` "00000000000000000000000001111011"

  describe "PrettySymbol" $ do
    it "renders (PutStr \"hello\") as \"hello\"" $
      ptShow (PX :: PX (PutStr "hello")) `shouldBe` "hello"
    it "renders ('PrettySymbol ('PrettyPadded 10) ('PrettyPrecision 2) \"hello\") as \"        he\"" $
      ptShow (PX :: PX ('PrettySymbol ('PrettyPadded 10) ('PrettyPrecision 2) "hello"))
        `shouldBe` "        he"

  describe "PrettySeperated" $ do
    it "renders (PutNat 0 <++> PutNat 1) as \"01\"" $
      ptShow (PX :: PX (PutNat 0 <++> PutNat 1)) `shouldBe` "01"
    it "renders (PutNat 0 <+> PutNat 1) as \"0 1\"" $
      ptShow (PX :: PX (PutNat 0 <+> PutNat 1)) `shouldBe` "0 1"
    it "renders (PutNat 0 <$$> PutNat 1) as \"0\\n1\"" $
      ptShow (PX :: PX (PutNat 0 <$$> PutNat 1)) `shouldBe` "0\n1"
    it "renders (PutNat 0 <++> PutNat 1 <+> PutNat 2 <$$> PutNat 3 <+> PutNat 4) as \"01 2\\n3 4\"" $
      ptShow (PX :: PX (PutNat 0 <++> PutNat 1 <+> PutNat 2 <$$> PutNat 3 <+> PutNat 4))
      `shouldBe` "01 2\n3 4"
    it "renders (PutNat 0 <++> 'PrettySpace) as \"0 \"" $
      ptShow (PX :: PX (PutNat 0 <++> 'PrettySpace)) `shouldBe` "0 "
    it "renders (PutNat 0 <++> 'PrettyEmpty) as \"0\"" $
      ptShow (PX :: PX (PutNat 0 <++> 'PrettyEmpty)) `shouldBe` "0"
    it "renders ('PrettyEmpty <++> PutNat 0) as \"0\"" $
      ptShow (PX :: PX ('PrettyEmpty <++> PutNat 0)) `shouldBe` "0"
    it "renders (PutNat 0 <++> 'PrettyEmpty <++> PutNat 1) as \"01\"" $
      ptShow (PX :: PX (PutNat 0 <++> 'PrettyEmpty <++> PutNat 1)) `shouldBe` "01"
    it "renders (PrettyOften 7 (PutStr \".\") as \".......\"" $
      ptShow (PX :: PX (PrettyOften 7 (PutStr "."))) `shouldBe` "......."
    it "renders (PrettyMany (PutNat 777) '[]) as \"\"" $
      ptShow (PX :: PX (PrettyMany (PutNat 777) '[])) `shouldBe` ""
    it "renders (PrettyMany (PutNat 777) '[PutStr \".\"]) as \".\"" $
      ptShow (PX :: PX (PrettyMany (PutNat 777) '[PutStr "."])) `shouldBe` "."
    it "renders (PrettyMany (PutNat 777) '[PutStr \".\", PutNat 3, PutNat 4]) as \".77737774\"" $
      ptShow (PX :: PX (PrettyMany (PutNat 777) '[PutStr ".", PutNat 3, PutNat 4])) `shouldBe` ".77737774"

  describe "ToPretty" $ do
    it "renders a custom type" $ do
      putStrLn $ showPretty (PX :: PX TestTable)
      showPretty (PX :: PX TestTable) `shouldBe` "+-------+-----+------------+\n|  col 1|col 2|       col 3|\n+-------+-----+------------+\n|   2423|  451|       21234|\n| 242322|   42|         n/a|\n|      0| 4351|      623562|\n|   4351|  n/a|         n/a|\n|      0| 4351|      623562|\n+-------+-----+------------+"


type TestTable =
  'MyTable         '[MyCol "col 1" 7, MyCol "col 2" 5, MyCol "col 3" 12]
          '[ MyRow '[2423           ,451             ,21234]
           , MyRow '[242322         ,42]
           , MyRow '[0              ,4351            ,623562]
           , MyRow '[4351]
           , MyRow '[0              ,4351            ,623562]
           ]

data MyTable = MyTable [Type] [Type]
data MyCol :: Symbol -> Nat -> Type
data MyRow :: [Nat] -> Type


type instance ToPretty ('MyTable cols rows) =
           PrettyManyIn (PutStr "+") (RowSepLine cols)
      <$$> PrettyManyIn (PutStr "|") (TableHeading cols)
      <$$> PrettyManyIn (PutStr "+") (RowSepLine cols)
      <$$> PrettyHigh   (TableRows cols rows)
      <$$> PrettyManyIn (PutStr "+") (RowSepLine cols)

type family TableHeading (cols :: [Type]) :: [PrettyType]
type instance TableHeading '[] = '[]
type instance TableHeading (MyCol title width ': r) =
  PutStrW width title  ': TableHeading  r

type family
   RowSepLine (cols :: [Type]) :: [PrettyType] where
   RowSepLine '[] = '[]
   RowSepLine (MyCol title width ': r) =
     PrettyOften width (PutStr "-") ': RowSepLine  r

type family
  TableRows (cols :: [Type]) (rows :: [Type]) :: [PrettyType] where
  TableRows cols '[] = '[]
  TableRows cols (MyRow cells ': rest ) =
    PrettyManyIn (PutStr "|") (TableCells cols cells) ': TableRows cols rest

type family
  TableCells (cols :: [Type]) (cells :: [Nat]) :: [PrettyType] where
  TableCells '[] cells = '[]
  TableCells (MyCol title width ': cols) (value ': cells) =
    PutNatW width value ':  TableCells cols cells
  TableCells (MyCol title width ': cols) '[] =
    PutStrW width "n/a" ':  TableCells cols '[]
