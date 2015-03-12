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

data Counter = Direct | Unary | Binary

   deriving ( Eq, Ord, Read, Show, Enum, Bounded )

data Config =
  Config { global :: Bool
         , search :: Bool
         , symmetries :: [Sym]
         , counter :: Counter
         , minimal :: Bool           
         , neigh :: Neigh
         , degrees :: [ Int ]
         , width :: Int  
         }
  deriving ( Show )

config :: Parser Config
config = Config
  <$> ( option auto ( long "global" <> short 'g' <> value True )
        <|> flag True False ( long "local" <> short 'l' )
      )
  <*> ( option auto ( long "search" <> short 'r' <> value True ) )
  <*> ( ( \ s -> read $ "[" ++ s ++ "]" )
        <$> strOption
        ( long "symmetries" <> short 's' <> value ""
          <> help (show [minBound .. maxBound :: Sym])
        )
      )
  <*> ( option auto (long "counter" <> short 'c' <> value Binary 
          <> help (show [minBound .. maxBound::Counter] )
                    ) ) 
  <*> ( option auto  ( long "minimal" <> short 'm' <> value True ) )
  <*> ( read <$> strArgument
        ( metavar "NEIGH"
          <> help (show [minBound .. maxBound::Neigh] 
        ) ) )
  <*> ( many ( read <$> strArgument idm ) ) 
  <*> ( option auto ( long "width" <> short 'w' <> value 1 ) )

config0 = Config
  { global = True, search = True
  , symmetries = [ Rot4 ], minimal = True, counter = Binary
  , neigh = King, degrees = [ 2,5 ], width = 7
  }
            
parse :: IO Config
parse  = execParser $ info ( helper <*> config )
  $ fullDesc
  <> progDesc "see http://www2.stetson.edu/~efriedma/mathmagic/0315.html"
  <> progDesc "use -h to get more information"
  


