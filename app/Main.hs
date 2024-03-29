module Main where

import System.Environment
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS

import Lib

getTLSConfig :: IO TLSSettings
getTLSConfig = do
    fullChain <- getEnv "FULLCHAIN"
    privKey <- getEnv "PRIVKEY"
    return (tlsSettings fullChain privKey)

main :: IO ()
main = do
    tlsConfig <- getTLSConfig
    let config = setPort 443 defaultSettings

    runTLS tlsConfig config app
    -- run 8443 app
