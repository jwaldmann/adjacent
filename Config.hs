module Config where

import Options.Applicative
import Options.Applicative.Types

data Sym = Anti | Diag
         | Hor | Vert
         | Rot2 | Rot4
   deriving (Eq, Ord, Show, Read)

data Neigh = Congo
           | Frog
           | King
           | Knight
           | Phoenix  
           | Zebra
   deriving ( Eq, Ord, Read, Show )

data Config =
  Config { global :: Bool
         , search :: Bool
         , symmetries :: [Sym]
         , neigh :: Neigh
         , degrees :: [ Int ]
         }
  deriving ( Show )

config :: Parser Config
config = Config
  <$> ( flag True True ( long "global" )
        <|> flag True False ( long "local" ) )
  <*> ( flag True True ( long "search" )
        <|> flag True False ( long "nosearch" ) )
  <*> ( ( \ s -> read $ "[" ++ s ++ "]" )
        <$> strOption ( long "symmetries" <> value "" )
      )
  <*> ( read <$> strArgument idm )
  <*> ( many ( read <$> strArgument idm ) )

config0 = Config
  { global = True, search = True
  , symmetries = [ Rot4 ]
  , neigh = King, degrees = [ 2,5 ]
  }
            

parse :: IO Config
parse  = execParser $ info ( helper <*> config ) fullDesc


