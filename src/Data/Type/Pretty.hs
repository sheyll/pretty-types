{-# LANGUAGE UndecidableInstances #-}
-- | Type Level Pretty Printing
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

-- * Pretty Printing

-- | Mini Type level eDSL for pretty printing via 'PrettyTypeShow'.
--
-- To simplify things further, 'ToPretty' is provided, together with
-- 'showPretty' which takes a proxy to the type to print, applies 'ToPretty'
-- to it and then prints it using 'ptShow'.
--
-- Use the type aliases to get rid of unticked or even ticked promoteds.
data PrettyType where
  PrettyEmpty :: PrettyType
  PrettySpace :: PrettyType
  PrettyNewline :: PrettyType
  PrettySymbol :: PrettyPadded -> PrettyPrecision -> Symbol -> PrettyType
  PrettyNat :: PrettyPadded -> PrettyPrecision -> PrettyNatFormat -> Nat -> PrettyType
  PrettySeperated :: PrettyType -> PrettyType -> PrettyType -> PrettyType

-- | Padding for 'PrettyType's 'PrettySymbol' and 'PrettyNat'.
data PrettyPadded where
  PrettyUnpadded :: PrettyPadded
  PrettyPadded
    :: Nat -- ^ Padding
    -> PrettyPadded

-- | The precision for 'PrettySymbol' and 'PrettyNat'.
data PrettyPrecision where
  PrettyPrecise :: PrettyPrecision
  PrettyPrecision
    :: Nat -- ^ Precision, for 'Symbol's the maximum width,
           -- for 'Nat's the minimum digits.
    -> PrettyPrecision

-- | 'Nat' formatting, e.g. "cafe", "CAFE", "51966" or "1100101011111110"
data PrettyNatFormat =
  PrettyHex | PrettyHexU | PrettyDec | PrettyBit

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

-- | A combination of 'PrettySpace' and 'PrettyMany',
-- e.g. @PrettyManyIn (PutStr "|") '[PutStr "a", PutStr "b"] => "|a|b|"@
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

-- | Create 'PrettyType' from a 'Nat' formatted as 8-bit bit representation,
-- i.e. @5 => "00000101"@
type PutBits8 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 8) 'PrettyBit x
-- | Create 'PrettyType' from a 'Nat' formatted as 16-bit bit representation,
-- i.e. @5 => "00000000000000101"@
type PutBits16 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 16) 'PrettyBit x
-- | Create 'PrettyType' from a 'Nat' formatted as 32-bit bit representation,
-- i.e. @5 => "00000000000000000000000000000000101"@
type PutBits32 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 32) 'PrettyBit x
-- | Create 'PrettyType' from a 'Nat' formatted as 64-bit bit representation,
-- i.e. @5 => "00000000000000000000000000000000000000000000000000000000000000000000101"@
type PutBits64 x = 'PrettyNat 'PrettyUnpadded ('PrettyPrecision 64) 'PrettyBit x

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
