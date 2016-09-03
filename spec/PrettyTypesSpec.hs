{-# LANGUAGE UndecidableInstances #-}
module Main (main) where

import           Data.Type.Pretty
import           Test.Hspec
import GHC.TypeLits
import Data.Kind
import Data.Proxy
import Data.Word
import Data.Int
import Data.Tagged

main :: IO ()
main = hspec spec

-- shorter Proxy
data PX (k :: a) = PX

spec :: Spec
spec = describe "rendering" $ do

  describe "PrettyNat" $ do
    it "renders (PutNat 123) as 123" $
      showPretty (PX :: PX (PutNat 123)) `shouldBe` "123"
    it "renders ('PrettyNat ('PrettyPadded 10) ('PrettyPrecision 5) 'PrettyDec 123) as \"     00123\"" $
      showPretty (PX :: PX ('PrettyNat ('PrettyPadded 10) ('PrettyPrecision 5) 'PrettyDec 123))
        `shouldBe` "     00123"
    it "renders (PutHex8 123) as 7b" $
      showPretty (PX :: PX (PutHex8 123)) `shouldBe` "7b"
    it "renders (PutHeX8 123) as 7B" $
      showPretty (PX :: PX (PutHeX8 123)) `shouldBe` "7B"
    it "renders (PutHeX64 123) as 000000000000007B" $
      showPretty (PX :: PX (PutHeX64 123)) `shouldBe` "000000000000007B"
    it "renders (PutBits16 123) as 0000000001111011" $
      showPretty (PX :: PX (PutBits16 123)) `shouldBe` "0000000001111011"
    it "renders (PutBits32 123) as 00000000000000000000000001111011" $
      showPretty (PX :: PX (PutBits32 123)) `shouldBe` "00000000000000000000000001111011"

  describe "PrettySymbol" $ do
    it "renders (PutStr \"hello\") as \"hello\"" $
      showPretty (PX :: PX (PutStr "hello")) `shouldBe` "hello"
    it "renders ('PrettySymbol ('PrettyPadded 10) ('PrettyPrecision 2) \"hello\") as \"        he\"" $
      showPretty (PX :: PX ('PrettySymbol ('PrettyPadded 10) ('PrettyPrecision 2) "hello"))
        `shouldBe` "        he"

  describe "PrettyInfix" $ do
    it "renders (PutNat 0 <++> PutNat 1) as \"01\"" $
      showPretty (PX :: PX (PutNat 0 <++> PutNat 1)) `shouldBe` "01"
    it "renders (PutNat 0 <+> PutNat 1) as \"0 1\"" $
      showPretty (PX :: PX (PutNat 0 <+> PutNat 1)) `shouldBe` "0 1"
    it "renders (PutNat 0 <$$> PutNat 1) as \"0\\n1\"" $
      showPretty (PX :: PX (PutNat 0 <$$> PutNat 1)) `shouldBe` "0\n1"
    it "renders (PutNat 0 <++> PutNat 1 <+> PutNat 2 <$$> PutNat 3 <+> PutNat 4) as \"01 2\\n3 4\"" $
      showPretty (PX :: PX (PutNat 0 <++> PutNat 1 <+> PutNat 2 <$$> PutNat 3 <+> PutNat 4))
      `shouldBe` "01 2\n3 4"
    it "renders (PutNat 0 <++> 'PrettySpace) as \"0 \"" $
      showPretty (PX :: PX (PutNat 0 <++> 'PrettySpace)) `shouldBe` "0 "
    it "renders (PutNat 0 <++> 'PrettyEmpty) as \"0\"" $
      showPretty (PX :: PX (PutNat 0 <++> 'PrettyEmpty)) `shouldBe` "0"
    it "renders ('PrettyEmpty <++> PutNat 0) as \"0\"" $
      showPretty (PX :: PX ('PrettyEmpty <++> PutNat 0)) `shouldBe` "0"
    it "renders (PutNat 0 <++> 'PrettyEmpty <++> PutNat 1) as \"01\"" $
      showPretty (PX :: PX (PutNat 0 <++> 'PrettyEmpty <++> PutNat 1)) `shouldBe` "01"
    it "renders (PrettyOften 7 (PutStr \".\") as \".......\"" $
      showPretty (PX :: PX (PrettyOften 7 (PutStr "."))) `shouldBe` "......."
    it "renders (PrettyMany (PutNat 777) '[]) as \"\"" $
      showPretty (PX :: PX (PrettyMany (PutNat 777) '[])) `shouldBe` ""
    it "renders (PrettyMany (PutNat 777) '[PutStr \".\"]) as \".\"" $
      showPretty (PX :: PX (PrettyMany (PutNat 777) '[PutStr "."])) `shouldBe` "."
    it "renders (PrettyMany (PutNat 777) '[PutStr \".\", PutNat 3, PutNat 4]) as \".77737774\"" $
      showPretty (PX :: PX (PrettyMany (PutNat 777) '[PutStr ".", PutNat 3, PutNat 4])) `shouldBe` ".77737774"

  describe "PrettyInfix" $ do
    it "renders the seperator if nested docs contain text" $
      showPretty (PX :: PX ('PrettyInfix 'PrettySpace (PutStr "foo") (PutStr "bar"))) `shouldBe` "foo bar"
    it "renders only the the first document of the second is empty, and no seperator" $
      showPretty (PX :: PX ('PrettyInfix 'PrettySpace (PutStr "foo") (PutStr ""))) `shouldBe` "foo"
    it "renders only the the second document of the first is empty, and no seperator" $
      showPretty (PX :: PX ('PrettyInfix 'PrettySpace (PutStr "") (PutStr "bar"))) `shouldBe` "bar"

  describe "PrettyAlternative" $ do
    it "renders the first document if is not empty" $
      showPretty (PX :: PX ('PrettyAlternative (PutStr "foo") (PutStr "bar"))) `shouldBe` "foo"
    it "renders the second document if the first is empty" $
      showPretty (PX :: PX ('PrettyAlternative (PutStr "") (PutStr "bar"))) `shouldBe` "bar"

  describe "PrettyPrefix" $ do
    it "renders the first and second document if the second is not empty" $
      showPretty (PX :: PX ('PrettyPrefix (PutStr "foo") (PutStr "bar"))) `shouldBe` "foobar"
    it "renders nothing if the second document is empty" $
      showPretty (PX :: PX ('PrettyPrefix (PutStr "foo") (PutStr ""))) `shouldBe` ""

  describe "PrettySuffix" $ do
    it "renders the second and first document if the second is not empty" $
      showPretty (PX :: PX ('PrettySuffix (PutStr "foo") (PutStr "bar"))) `shouldBe` "barfoo"
    it "renders nothing if the second document is empty" $
      showPretty (PX :: PX ('PrettySuffix (PutStr "foo") (PutStr ""))) `shouldBe` ""

  describe "<||>" $ do
    it "is the type alias for 'PrettyAlternative " $
      showPretty (PX :: PX (PutStr "foo" <||> PutStr "bar")) `shouldBe` "foo"
    it "has a lower precedence than <++>" $
      showPretty (PX :: PX (PutStr "foo" <++> PutStr "" <||> PutStr "baz")) `shouldBe` "foo"
    it "has a higher precedence than <$$>" $
      showPretty (PX :: PX (PutStr "foo" <$$> PutStr "" <||> PutStr "baz")) `shouldBe` "foo\nbaz"

  describe "<:>" $ do
    it "renders no label if the label is empty" $
      showPretty (PX :: PX ("" <:> PutStr "bar")) `shouldBe` "bar"
    it "renders a label, a colon followed by a space and the body" $
      showPretty (PX :: PX ("foo" <:> PutStr "bar")) `shouldBe` "foo: bar"
    it "renders a label followed by a colon if the body is empty" $
      showPretty (PX :: PX ("foo" <:> 'PrettyEmpty)) `shouldBe` "foo:"

  describe "<:$$>" $ do
    it "renders no label if the label is empty" $
      showPretty (PX :: PX ("" <:$$> PutStr "bar")) `shouldBe` "bar"
    it "renders a label, a colon followed by the body on the next line" $
      showPretty (PX :: PX ("foo" <:$$> PutStr "bar")) `shouldBe` "foo:\nbar"
    it "renders a label followed by a colon if the body is empty" $
      showPretty (PX :: PX ("foo" <:$$> 'PrettyEmpty)) `shouldBe` "foo:"

  describe "<:$$-->" $ do
    it "renders no label if the label is empty" $
      showPretty (PX :: PX ("" <:$$--> PutStr "bar")) `shouldBe` "  bar"
    it "renders a label, a colon followed by indented the body" $
      showPretty (PX :: PX ("foo" <:$$--> PutStr "bar")) `shouldBe` "foo:\n  bar"
    it "renders a label followed by a colon if the body is empty" $
      showPretty (PX :: PX ("foo" <:$$--> 'PrettyEmpty)) `shouldBe` "foo:"

  describe "PrettyIndent" $ do
    it "renders the indentation" $
      showPretty (PX :: PX (PutStr "foo" <$$--> PutStr "bar")) `shouldBe` "foo\n  bar"
    it "renders the indentation only once per line" $
      showPretty (PX :: PX (PutStr "foo" <$$--> PutStr "bar1" <+> PutStr "bar2" )) `shouldBe` "foo\n  bar1 bar2"
    it "renders the indentation of multi lines and the operator precedence is such that no parens are needed." $
      showPretty (PX :: PX (PutStr "foo" <$$--> PutStr "bar1" <$$> PutStr "bar2")) `shouldBe` "foo\n  bar1\n  bar2"
    it "renders the nested indentation." $
      showPretty (PX :: PX (PutStr "foo" <$$-->
                           (PutStr "bar1" <$$>
                            PutStr "bar2" <$$-->
                            PutStr "bar3" <$$>
                            PutStr "bar4")))
      `shouldBe` "foo\n  bar1\n  bar2\n    bar3\n    bar4"

  describe "PrettifyWith" $
    it "applies a 'Prettifier' and creates the desired 'PrettyType'" $
       showPretty (PX :: PX (PrettifyWith (PrettyTitled (PutStr "The Title") 10) "body"))
       `shouldBe`
       "The Title\n          body"
  describe "ToPretty" $ do
    it "renders a 'Tagged'" $
      showPretty (PX :: PX (ToPretty (Tagged "foo" Word8))) `shouldBe` "Word8 (foo)"
    it "renders a type level symbol like PutStr" $
      showPretty (PX :: PX (ToPretty "foo")) `shouldBe` showPretty (PX :: PX (PutStr "foo"))
    it "renders a type level natural like PutNat" $
      showPretty (PX :: PX (ToPretty 123)) `shouldBe` showPretty (PX :: PX (PutNat 123))
    it "renders a type level symbol like PutStr" $
      showPretty (PX :: PX (ToPretty "foo")) `shouldBe` showPretty (PX :: PX (PutStr "foo"))
    it "renders 'Nothing to empty document" $
      showPretty (PX :: PX (ToPretty 'Nothing)) `shouldBe` ""
    it "renders 'Just x to ToPretty x" $
      showPretty (PX :: PX (ToPretty ('Just "test"))) `shouldBe` "test"
    it "renders Word8 as PutStr \"Word8\"" $
      showPretty (PX :: PX (ToPretty Word8)) `shouldBe` "Word8"
    it "renders Word16 as PutStr \"Word16\"" $
      showPretty (PX :: PX (ToPretty Word16)) `shouldBe` "Word16"
    it "renders Word32 as PutStr \"Word32\"" $
      showPretty (PX :: PX (ToPretty Word32)) `shouldBe` "Word32"
    it "renders Word64 as PutStr \"Word64\"" $
      showPretty (PX :: PX (ToPretty Word64)) `shouldBe` "Word64"
    it "renders Int8 as PutStr \"Int8\"" $
      showPretty (PX :: PX (ToPretty Int8)) `shouldBe` "Int8"
    it "renders Int16 as PutStr \"Int16\"" $
      showPretty (PX :: PX (ToPretty Int16)) `shouldBe` "Int16"
    it "renders Int32 as PutStr \"Int32\"" $
      showPretty (PX :: PX (ToPretty Int32)) `shouldBe` "Int32"
    it "renders Int64 as PutStr \"Int64\"" $
      showPretty (PX :: PX (ToPretty Int64)) `shouldBe` "Int64"
    it "renders Int as PutStr \"Int\"" $
      showPretty (PX :: PX (ToPretty Int)) `shouldBe` "Int"
    it "renders Integer as PutStr \"Integer\"" $
      showPretty (PX :: PX (ToPretty Integer)) `shouldBe` "Integer"
    it "renders Bool as PutStr \"Bool\"" $
      showPretty (PX :: PX (ToPretty Bool)) `shouldBe` "Bool"
    it "renders Float as PutStr \"Float\"" $
      showPretty (PX :: PX (ToPretty Float)) `shouldBe` "Float"
    it "renders Double as PutStr \"Double\"" $
      showPretty (PX :: PX (ToPretty Double)) `shouldBe` "Double"
    it "renders 'True as PutStr \"'True\"" $
      showPretty (PX :: PX (ToPretty 'True)) `shouldBe` "'True"
    it "renders 'False as PutStr \"'False\"" $
      showPretty (PX :: PX (ToPretty 'False)) `shouldBe` "'False"
    it "renders a custom type" $ do
      putStrLn prettyTestTable
      showPretty (PX :: PX TestTable) `shouldBe` "+-------+-----+------------+\n|  col 1|col 2|       col 3|\n+-------+-----+------------+\n|   2423|  451|       21234|\n| 242322|   42|         n/a|\n|      0| 4351|      623562|\n|   4351|  n/a|         n/a|\n|      0| 4351|      623562|\n+-------+-----+------------+"

prettyTestTable :: String
prettyTestTable = showPretty (Proxy :: Proxy TestTable)

type TestTable =
  'MyTable         '[MyCol "col 1" 7, MyCol "col 2" 5, MyCol "col 3" 12]
          '[ MyRow '[2423           ,451             ,21234]
           , MyRow '[242322         ,42]
           , MyRow '[0              ,4351            ,623562]
           , MyRow '[4351]
           , MyRow '[0              ,4351            ,623562]
           ]

-- | A type with a list of columns and rows.
data MyTable = MyTable [Type] [Type]

-- | A row of a table, with a list of values, one each for every column.
data MyRow :: [Nat] -> Type

-- | The column of a table. It has a width and a column title.
data MyCol :: Symbol -> Nat -> Type

type instance ToPretty ('MyTable cols rows) =
           PrettyManyIn (PutStr "+") (RowSepLine cols)
      <$$> PrettyManyIn (PutStr "|") (TableHeading cols)
      <$$> PrettyManyIn (PutStr "+") (RowSepLine cols)
      <$$> PrettyHigh   (TableRows cols rows)
      <$$> PrettyManyIn (PutStr "+") (RowSepLine cols)

type family
  TableHeading (cols :: [Type]) :: [PrettyType] where
  TableHeading '[]                      = '[]
  TableHeading (MyCol title width ': r) = PutStrW width title  ': TableHeading  r

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
