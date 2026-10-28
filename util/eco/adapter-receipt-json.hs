-- Explicit rendering of an existing typed receipt; does not replay producers.
module Main where
import ECODiagnostics (writeAdapterReceiptJSON)
import ECO.Replay (fiPath)
import System.Environment (getArgs)
import System.Exit (die)

main = do
    args <- getArgs
    case args of
      [receipt,directory] -> writeAdapterReceiptJSON receipt directory >>= putStrLn . fiPath
      _ -> die "usage: bluehs adapter-receipt-json.hs ADAPTER_RECEIPT FRESH_OUTPUT_DIRECTORY"
