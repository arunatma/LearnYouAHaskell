{-
This piece of Haskell code contains R code.

This needs Rserve to be initiated.

More details at: http://www.r-bloggers.com/dream-team-combining-tableau-and-r/
Just like Tableau, other bindings like Python, Haskell or Java can be done. So,
the client is in one of these programming languages and communicates with the R
server

(In progress: as of now, not worked!)

-}

import qualified Network.Rserve.Client as R
import qualified Data.ByteString.Char8 as B
import Data.Binary

main :: IO ()
main = do 
  rc <- R.connect "localhost" 6311
  print ("Rserve version:"++R.rcRserveVersion rc)
  m <- R.eval rc "rnorm(10)" 
  print (R.unpack m :: Maybe [Double])
  m1 <- R.assign rc "a" (R.RArrayDouble [1.0,201])
  m2 <- R.eval rc "a"
  print m2
  fileStuff rc

fileStuff :: R.RConn -> IO()
fileStuff rconn = do 
  r <- R.createFile rconn fileName 
  r2 <- R.writeFile rconn (B.pack "a,b\n1,2\n8,4")
  r4 <- R.closeFile rconn fileName
  r5 <- R.openFile rconn fileName
  r6 <- R.eval rconn ("x <- read.csv(\""++fileName++"\", header=TRUE)")
  r7 <- R.eval rconn "mean(x$a)"
  r8 <- R.removeFile rconn fileName
  print (R.unpackRArrayDouble r7)
    where fileName = "rserveTemp.csv"
    
    