module Main where

import Lib
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings, TLSSettings)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import System.Environment

getTLSConfig :: IO TLSSettings
getTLSConfig = do
    fullChain <- getEnv "FULLCHAIN"
    privKey <- getEnv "PRIVKEY"
    return (tlsSettings fullChain privKey)

main :: IO ()
main = do
    tlsConfig <- getTLSConfig
    let config = setPort 443 defaultSettings

    waiApp <- server
    runTLS tlsConfig config waiApp
