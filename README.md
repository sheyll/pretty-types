[![Hackage](https://img.shields.io/badge/hackage-prettytypes-green.svg?style=flat)](http://hackage.haskell.org/package/pretty-types)

# Type Pretty Printing

A tiny eDSL, not unlike `ErrorMessage` for `TypeError`s to pretty print types,
especially uninhabited types.

This Example will render this table:

    +-------+-----+------------+
    |  col 1|col 2|       col 3|
    +-------+-----+------------+
    |   2423|  451|       21234|
    | 242322|   42|         n/a|
    |      0| 4351|      623562|
    |   4351|  n/a|         n/a|
    |      0| 4351|      623562|
    +-------+-----+------------+

From this code:

    module Main (main) where

    import Data.Type.Pretty
    import GHC.TypeLits
    import Data.Kind
    import Data.Proxy

    main :: IO ()
    main = putStrLn $ showPretty (Proxy :: Proxy TestTable)

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
