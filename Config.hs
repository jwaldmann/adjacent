module Config where

import Options.Applicative
import Options.Applicative.Types

data Sym = Anti | Diag
         | Hor | Vert
         | Rot2 | Rot4
   deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Neigh = Congo
           | Frog
           | King
           | Knight
           | Phoenix  
           | Zebra
   deriving ( Eq, Ord, Read, Show, Enum, Bounded )

data Config =
  Config { global :: Bool
         , search :: Bool
         , symmetries :: [Sym]
         , minimal :: Bool           
         , neigh :: Neigh
         , degrees :: [ Int ]
         , width :: Int  
         }
  deriving ( Show )

config :: Parser Config
config = Config
  <$> ( option auto ( long "global" <> value True ) )
  <*> ( option auto ( long "search" <> value True ) )
  <*> ( ( \ s -> read $ "[" ++ s ++ "]" )
        <$> strOption
        ( long "symmetries" <> value ""
          <> help (show [minBound .. maxBound :: Sym])
        )
      )
  <*> ( option auto  ( long "minimal" <> value True ) )
  <*> ( read <$> strArgument
        ( metavar "NEIGH"
          <> help (show [minBound .. maxBound::Neigh] 
        ) ) )
  <*> ( many ( read <$> strArgument idm ) ) 
  <*> ( option auto ( long "width" <> value 1 ) )

config0 = Config
  { global = True, search = True
  , symmetries = [ Rot4 ], minimal = True
  , neigh = King, degrees = [ 2,5 ], width = 7
  }
            
parse :: IO Config
parse  = execParser $ info ( helper <*> config )
  $ fullDesc
  <> progDesc "see http://www2.stetson.edu/~efriedma/mathmagic/0315.html"
  <> progDesc "use -h to get more information"
  


