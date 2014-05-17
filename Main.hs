-- TODO make the transcode jobs run concurrently?
import System.Environment
import System.Process
import System.Exit
import Text.Regex

-- Take file paths from the argument list, but filter ones that do not end
-- in .flac
getFilePaths :: IO [String]
getFilePaths = do
  args <- getArgs
  return $ filter isSaneFilePath args

-- Extract the root and extension of a string.
-- First component is the root, second is the extension. If the string does
-- not contain a . character, then the second component will be [] and the first
-- component will be the input string.
removeExtension :: String -> (String, String)
removeExtension inputString =
  let (root, ext, bool) = foldr step ([], [], True) inputString
  in if bool then (ext, root) else (root, ext)
  where step char (root, ext, True) = if char == '.' then (root, ext, False) else (root, char : ext, True)
        step char (root, ext, False) = (char : root, ext, False)

-- The program transcodeOnce assumes that the call to flac will rename its
-- argument to <root file name>.wav (from <root file name>.flac) and also that
-- every input argument ends in .flac. This program converts a .flac to a .wav,
nextFilePath :: String -> String
nextFilePath flacString = ((fst . removeExtension) flacString) ++ ".wav"

isSaneFilePath :: String -> Bool
isSaneFilePath = ((==) "flac") . snd . removeExtension

transcode :: [String] -> IO ()
transcode = mapM_ transcodeOnce

transcodeOnce :: String -> IO ()
transcodeOnce filePath = do
  putStrLn $ "Decoding: " ++ filePath
  (_, _, _, decode) <- createProcess (proc "flac" ["-d", "-f", filePath])
  code <- waitForProcess decode
  case code of
    ExitSuccess -> do
      -- TODO parameterize this call to lame?
      (_, _, _, encode) <- createProcess (proc "lame" ["-V2", nextFilePath filePath])
      code <- waitForProcess encode
      case code of
        ExitSuccess -> do 
          (_, _, _, remove) <- createProcess (proc "rm" [nextFilePath filePath])
          waitForProcess remove
          return ()
        ExitFailure _ -> error "Encoding failed!"
    ExitFailure _ -> error "Decoding failed!"

main = do
  files <- getFilePaths
  putStrLn $ "Will transcode " ++ (show $ length files) ++ " file(s)."
  transcode files
