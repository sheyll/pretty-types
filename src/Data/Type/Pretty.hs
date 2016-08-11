{-# LANGUAGE UndecidableInstances #-}
-- |
-- = Type Pretty Printing
--
-- == Printing Custom Types
--
-- The main usecase of this library is rendering one's own complex types to
-- pretty 'String' values, e.g. for debugging and tracing purposes.
--
-- One way to create 'PrettyType' /documents/ is to define 'ToPretty' instances
-- for your types by combining the promoted constructors of 'PrettyType'.
--
-- If `UndecidableInstances` isn't holding you back, use the type aliases like
-- 'PutStr', 'PutNat', 'PrettySeperated', etc in these instance definitions.
--
-- 'ToPretty' is an open type family, that converts a custom type to a
-- `PrettyType`.
--
-- 'showPretty' eventually crafts a 'String' value from a proxy to the custom
-- type.
--
-- == Example
--
-- This example shows howto render this table:
--
-- > +-------+-----+------------+
-- > |  col 1|col 2|       col 3|
-- > +-------+-----+------------+
-- > |   2423|  451|       21234|
-- > | 242322|   42|         n/a|
-- > |      0| 4351|      623562|
-- > |   4351|  n/a|         n/a|
-- > |      0| 4351|      623562|
-- > +-------+-----+------------+
--
-- Assume a custom type for tables calles @MyTable@.
--
-- Rendering this table:
--
-- >type TestTable =
-- >  'MyTable         '[MyCol "col 1" 7, MyCol "col 2" 5, MyCol "col 3" 12]
-- >          '[ MyRow '[2423           ,451             ,21234]
-- >           , MyRow '[242322         ,42]
-- >           , MyRow '[0              ,4351            ,623562]
-- >           , MyRow '[4351]
-- >           , MyRow '[0              ,4351            ,623562]
-- >           ]
-- >
--
-- Using this function:
--
-- @
-- prettyTestTable :: String
-- prettyTestTable = 'showPretty' (Proxy :: Proxy TestTable)
-- @
--
-- These are the data types. Note that only numbers can be stored in MyTable.
--
-- > -- | A type with a list of columns and rows.
-- > data MyTable = MyTable [Type] [Type]
-- >
-- > -- | A row of a table, with a list of values, one each for every column.
-- > data MyRow :: [Nat] -> Type
-- >
-- > -- | The column of a table. It has a width and a column title.
-- > data MyCol :: Symbol -> Nat -> Type
--
-- Here's the top-level 'ToPretty' instance:
--
-- @
-- type instance 'ToPretty' ('MyTable cols rows) =
--            'PrettyManyIn' ('PutStr' "+") (RowSepLine cols)
--       '<$$>' 'PrettyManyIn' ('PutStr' "|") (TableHeading cols)
--       '<$$>' 'PrettyManyIn' ('PutStr' "+") (RowSepLine cols)
--       '<$$>' 'PrettyHigh'   (TableRows cols rows)
--       '<$$>' 'PrettyManyIn' ('PutStr' "+") (RowSepLine cols)
-- @
--
-- It delegates to these:
--
-- @
-- type family
--   TableHeading (cols :: [Type]) :: ['PrettyType'] where
--   TableHeading '[]                      = '[]
--   TableHeading (MyCol title width ': r) = 'PutStrW' width title  ': TableHeading  r
--
-- type family
--    RowSepLine (cols :: [Type]) :: ['PrettyType'] where
--    RowSepLine '[] = '[]
--    RowSepLine (MyCol title width ': r) =
--      'PrettyOften' width (PutStr "-") ': RowSepLine  r
--
-- type family
--   TableRows (cols :: [Type]) (rows :: [Type]) :: ['PrettyType'] where
--   TableRows cols '[] = '[]
--   TableRows cols (MyRow cells ': rest ) =
--     'PrettyManyIn' ('PutStr' "|") (TableCells cols cells) ': TableRows cols rest
--
-- type family
--   TableCells (cols :: [Type]) (cells :: [Nat]) :: ['PrettyType'] where
--   TableCells '[] cells = '[]
--   TableCells (MyCol title width ': cols) (value ': cells) =
--     'PutNatW' width value ':  TableCells cols cells
--   TableCells (MyCol title width ': cols) '[] =
--     'PutStrW' width "n/a" ':  TableCells cols '[]
-- @
module Data.Type.Pretty where

import GHC.TypeLits
import Data.Proxy
import Text.Printf

-- * Printing Custom Types

-- | Pretty print a type for which a 'ToPretty' instance was defined, that
-- converts the type to a 'PrettyType'. If you want to roll your own converter
-- for your type to 'PrettyType', just do that and call 'ptShow' directly.
showPretty
  :: forall proxy t . PrettyTypeShow (ToPretty t)
  => proxy t  -- ^ A proxy to the type to print. A 'ToPretty' instance for t must exists.
  -> String
showPretty _ = ptShow (Proxy :: Proxy (ToPretty t))

-- | Write an instance of this for converting your type (preferrable of your
-- kind also) to a promoted 'PrettyType'. NOTE: It might be helpful to turn on
-- UndecidableInstances and use the type like 'PutStr' aliases below.
type family ToPretty (a :: k) :: PrettyType

-- * Building Pretty Type Documents

-- | A 'PrettyType' for a string.
type PutStr str = 'PrettySymbol 'PrettyUnpadded 'PrettyPrecise str

-- | A 'PrettyType' for a string with the exact given width.
type PutStrW width str =
  'PrettySymbol ('PrettyPadded width) ('PrettyPrecision width) str

-- | A 'PrettyType' for a string with a newline character at the end.
type PutStrLn str = PutStr str <++> PutStr "\n"

-- | A 'PrettyType' for a number.
type PutNat x = 'PrettyNat 'PrettyUnpadded 'PrettyPrecise 'PrettyDec x

-- | A 'PrettyType' for a number with a width.
type PutNatW width x = 'PrettyNat ('PrettyPadded width) 'PrettyPrecise 'PrettyDec x

-- | Concatenate two 'PrettyType'.
type (<++>) l r = 'PrettySeperated 'PrettyEmpty l r
infixl 6 <++>

-- | Concatenate two 'PrettyType' using a 'PrettySpace'.
type (<+>) l r = 'PrettySeperated 'PrettySpace l r
infixl 5 <+>

-- | Concatenate two 'PrettyType' using a 'PrettyNewline'.
type (<$$>) l r = 'PrettySeperated 'PrettyNewline l r
infixl 4 <$$>

-- | Surround a pretty with parens
type PrettyParens doc = PrettySurrounded (PutStr "(") (PutStr ")") doc

-- | Surround a pretty with some pretties
type PrettySurrounded open close doc  =
  open <++> doc <++> close

-- | Combine a (type level) list of 'PrettyType's next to each other using
-- 'PrettySpace'
type PrettyWide docs = PrettyMany 'PrettySpace docs

-- | Combine a (type level) list of 'PrettyType's below each other using
-- 'PrettyNewline'
type PrettyHigh docs = PrettyMany 'PrettyNewline docs

-- | A combination of 'PrettySpace' and 'PrettyMany', e.g.:
--
-- >>> ptShow (Proxy :: Proxy (PrettyManyIn (PutStr "|") '[PutStr "a", PutStr "b"]))
-- "|a|b|"
type PrettyManyIn sep docs =
  PrettySurrounded sep sep (PrettyMany sep docs)

-- | Combine a (type level) list of 'PrettyType's seperated by a seperation
-- element.
type family
  PrettyMany (sep :: PrettyType)(docs :: [PrettyType]) :: PrettyType where
  PrettyMany sep '[]            = 'PrettyEmpty
  PrettyMany sep '[singleOne]   = singleOne
  PrettyMany sep (next ': rest) = next <++> sep <++> PrettyMany sep rest

-- | Repeat a 'PrettyType' @n@-times and append the copies.
type family PrettyOften (n :: Nat) (doc :: PrettyType) :: PrettyType where
  PrettyOften 0 doc = 'PrettyEmpty
  PrettyOften n doc = doc <++> PrettyOften (n-1) doc

-- | Create 'PrettyType' from a 'Nat' formatted as hex number using
-- lower-case letters for the hex digits.
type PutHex x = 'PrettyNat 'PrettyUnpadded 'PrettyPrecise 'PrettyHex x
-- | Create 'PrettyType' from a 'Nat' formatted as 8 bit hex number using
-- lower-case letters for the hex digits.
type PutHex8 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 2) 'PrettyHex x
-- | Create 'PrettyType' from a 'Nat' formatted as 16 bit hex number using
-- lower-case letters for the hex digits.
type PutHex16 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 4) 'PrettyHex x
-- | Create 'PrettyType' from a 'Nat' formatted as 32 bit hex number using
-- lower-case letters for the hex digits.
type PutHex32 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 8) 'PrettyHex x
-- | Create 'PrettyType' from a 'Nat' formatted as 64 bit hex number using
-- lower-case letters for the hex digits.
type PutHex64 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 16) 'PrettyHex x
-- | Create 'PrettyType' from a 'Nat' formatted as hex number using
-- lower-case letters for the hex digits.
type PutHeX x = 'PrettyNat 'PrettyUnpadded 'PrettyPrecise 'PrettyHexU x
-- | Create 'PrettyType' from a 'Nat' formatted as 8 bit hex number using
-- uppercase letters for the hex digits.
type PutHeX8 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 2) 'PrettyHexU x
-- | Create 'PrettyType' from a 'Nat' formatted as 16 bit hex number using
-- uppercase letters for the hex digits.
type PutHeX16 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 4) 'PrettyHexU x
-- | Create 'PrettyType' from a 'Nat' formatted as 32 bit hex number using
-- uppercase letters for the hex digits.
type PutHeX32 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 8) 'PrettyHexU x
-- | Create 'PrettyType' from a 'Nat' formatted as 64 bit hex number using
-- uppercase letters for the hex digits.
type PutHeX64 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 16) 'PrettyHexU x

-- | Create 'PrettyType' from a 'Nat' formatted as bit representation,
--
-- >>> ptShow (Proxy :: Proxy (PutBits 5))
-- "101"
type PutBits x = 'PrettyNat 'PrettyUnpadded 'PrettyPrecise 'PrettyBit x
-- | Create 'PrettyType' from a 'Nat' formatted as 8-bit bit representation,
--
-- >>> ptShow (Proxy :: Proxy (PutBits8 5))
-- "00000101"
type PutBits8 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 8) 'PrettyBit x
-- | Create 'PrettyType' from a 'Nat' formatted as 16-bit bit representation,
--
-- >>> ptShow (Proxy :: Proxy (PutBits16 5))
-- "00000000000000101"
type PutBits16 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 16) 'PrettyBit x
-- | Create 'PrettyType' from a 'Nat' formatted as 32-bit bit representation,
--
-- >>> ptShow (Proxy :: Proxy (PutBits32 5))
-- "00000000000000000000000000000000101"
type PutBits32 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 32) 'PrettyBit x
-- | Create 'PrettyType' from a 'Nat' formatted as 64-bit bit representation,
--
-- >>> ptShow (Proxy :: Proxy (PutBits64 5))
-- "00000000000000000000000000000000000000000000000000000000000000000000101"
type PutBits64 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 64) 'PrettyBit x

-- | Combinators for /type documents/.
--
-- The basis for pretty printing is this eDSL. It is rendered via the
-- 'PrettyTypeShow' instances for its promoted constructors.
--
-- Only the promoted constructors are used, only they have instances for that
-- class.
data PrettyType where
  PrettyEmpty :: PrettyType
  PrettySpace :: PrettyType
  PrettyNewline :: PrettyType
  PrettySymbol :: PrettyPadded -> PrettyPrecision -> Symbol -> PrettyType
  PrettyNat :: PrettyPadded -> PrettyPrecision -> PrettyNatFormat -> Nat -> PrettyType
  PrettySeperated :: PrettyType -> PrettyType -> PrettyType -> PrettyType

-- | Padding for 'PrettyType's 'PrettySymbol' and 'PrettyNat'.
data PrettyPadded where
  -- | No minimum or fixed width
  PrettyUnpadded :: PrettyPadded
  -- | Pad a 'PrettySymbol' or 'PrettyNat' with spaces or zeros.
  -- __NOTE__ `PrettyNat`s will never be  shorter than the minimum
  -- number of digits, regardless of this padding.
  PrettyPadded :: Nat -> PrettyPadded

-- | The precision for 'PrettySymbol' and 'PrettyNat'.
data PrettyPrecision where
  -- | No minimum precision.
  PrettyPrecise :: PrettyPrecision
  -- | Precision, for 'Symbol's the maximum width, for 'Nat's the minimum
  -- digits.
  -- __NOTE__`PrettyNat`s will never be  shorter than the minimum
  -- number of digits, wheres `PrettySymbol`s will be  truncated if they are
  -- longer than the precision.
  PrettyPrecision :: Nat-> PrettyPrecision

-- | 'PrettyNat' formatting options.
data PrettyNatFormat =
    -- | Hexa decimal rendering:
    --
    -- >>> ptShow (Proxy::Proxy (PrettyNat PrettyUnpadded PrettyPrecise PrettyHex 51966))
    -- "cafe"
    PrettyHex
    -- | Hexa decimal rendering (upper case):
    --
    -- >>> ptShow (Proxy::Proxy (PrettyNat PrettyUnpadded PrettyPrecise PrettyHexU 51966))
    -- "CAFE"
  | PrettyHexU
    -- | Decimal rendering:
    --
    -- >>> ptShow (Proxy::Proxy (PrettyNat PrettyUnpadded PrettyPrecise PrettyHexU 51966))
    -- "51966"
  | PrettyDec
    -- | Binary rendering:
    --
    -- >>> ptShow (Proxy::Proxy (PrettyNat PrettyUnpadded PrettyPrecise PrettyHexU 51966))
    -- "1100101011111110"
  | PrettyBit

-- * Low-Level Rendering

-- | In order to actually print anything from the promoted constructors of
-- 'PrettyTypeShow', a type class as common interface is required.
class PrettyTypeShow (p :: PrettyType) where
  ptShow :: proxy p -> String

-- | Print nothing.
instance PrettyTypeShow 'PrettyEmpty where ptShow _ = ""
-- | Print a single space character.
instance PrettyTypeShow 'PrettySpace where ptShow _ = " "
-- | Print a single newline character.
instance PrettyTypeShow 'PrettyNewline where ptShow _ = "\n"

-- | Print a 'Symbol' using the 'printf' and the given format parameters.
instance forall t pad prec.
    (KnownSymbol t, PrintfArgModifier pad, PrintfArgModifier prec)
  => PrettyTypeShow ('PrettySymbol pad prec t) where
  ptShow _ = printf ("%" ++ toPrintfArgModifier (Proxy :: Proxy pad)
                         ++ toPrintfArgModifier (Proxy :: Proxy prec)
                         ++ "s")
                    (symbolVal (Proxy :: Proxy t))

-- | Print a 'Nat' using the 'printf' and the given format parameters.
instance forall fmt x pad prec.
  (KnownNat x, PrintfArgModifier fmt, PrintfArgModifier pad, PrintfArgModifier prec)
  => PrettyTypeShow ('PrettyNat pad prec fmt x) where
  ptShow _ = printf ("%" ++ toPrintfArgModifier (Proxy :: Proxy pad)
                         ++ toPrintfArgModifier (Proxy :: Proxy prec)
                         ++ toPrintfArgModifier (Proxy :: Proxy fmt))
                    (natVal (Proxy :: Proxy x))

-- | Concatenate two 'PrettyType's. If one of them is empty print the other
-- without any seperation character.
instance forall l r sep .
  (PrettyTypeShow sep, PrettyTypeShow l, PrettyTypeShow r)
  => PrettyTypeShow ('PrettySeperated sep l r) where
  ptShow _ =
    let rstr = ptShow (Proxy :: Proxy r)
        lstr = ptShow (Proxy :: Proxy l)
        sepStr = ptShow (Proxy :: Proxy sep)
    in if lstr == "" then rstr
        else if rstr == "" then lstr
          else lstr ++ sepStr ++ rstr

-- | Internal 'printf' format generation. Used internally by 'PrettyTypeShow'
-- instances to generate the format string piece by piece with the values for
-- the instances of e.g. 'PrettyPrecise', 'PrettyNatFormat', or 'PrettyEmpty'.
class PrintfArgModifier a where
  -- | Generate a piece of a 'printf' format string from a proxy for a type.
  toPrintfArgModifier :: p a -> String

-- | Translation of 'PrettyHex' to 'printf' format character: @"x"@
instance PrintfArgModifier 'PrettyHex where toPrintfArgModifier _  = "x"
-- | Translation of 'PrettyHexU' to 'printf' format character: @"X"@
instance PrintfArgModifier 'PrettyHexU where toPrintfArgModifier _ = "X"
-- | Translation of 'PrettyDec' to 'printf' format character: @"d"@
instance PrintfArgModifier 'PrettyDec where toPrintfArgModifier _  = "d"
-- | Translation of 'PrettyBit' to 'printf' format character: @"b"@
instance PrintfArgModifier 'PrettyBit where toPrintfArgModifier _  = "b"
-- | Translation of 'PrettyUnpadded' to an empty modifier string
instance PrintfArgModifier 'PrettyUnpadded where toPrintfArgModifier _ = ""
-- | Translation of 'PrettyPadded' to a string with the numeric padding value.
instance forall p. KnownNat p => PrintfArgModifier ('PrettyPadded p) where
    toPrintfArgModifier _ = show (natVal (Proxy :: Proxy p))
-- | Translation of 'PrettyPrecise' to an empty modifier string
instance PrintfArgModifier 'PrettyPrecise where toPrintfArgModifier _ = ""
-- | Translation of 'PrettyPadded' to a string with the numeric precision value,
-- prependen by a  dot @"."@.
instance forall p. KnownNat p => PrintfArgModifier ('PrettyPrecision p) where
    toPrintfArgModifier _ = "." ++ show (natVal (Proxy :: Proxy p))
