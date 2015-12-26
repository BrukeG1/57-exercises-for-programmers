module P31KarvonenHR where
import Control.Exception (catch, IOException)

main :: IO ()
main = undefined

targetHR :: Double -> Double -> Double -> Int
targetHR age rhr intensity =
    round (((220 - age - rhr) * intensity) + rhr)
