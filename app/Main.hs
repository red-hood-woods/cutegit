module Main (main) where

import Control.Monad (unless, when)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    setCurrentDirectory
  )
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (BufferMode (NoBuffering), hFlush, hSetBuffering, hSetEcho, stdin, stdout)
import System.Process (readCreateProcessWithExitCode, shell)

data View = View
  { repoPath :: FilePath,
    branchName :: String,
    aheadBehind :: String,
    changedFiles :: [String],
    stagedFiles :: [String],
    recentCommits :: [String]
  }

data Action
  = ShowStatus
  | ShowLog
  | ShowBranches
  | StageEverything
  | CommitChanges
  | PullChanges
  | PushChanges
  | RemoteAdd
  | ChangeRepo
  | InitRepo
  | Refresh
  | Quit

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  cwd <- getCurrentDirectory
  loop cwd

loop :: FilePath -> IO ()
loop cwd = do
  clearScreen
  isRepo <- looksLikeGitRepo cwd
  if isRepo
    then do
      view <- loadView cwd
      drawHome view
      action <- promptAction
      keepGoing <- runAction cwd action
      when keepGoing (loop cwd)
    else do
      drawWelcome cwd
      action <- promptSetupAction
      keepGoing <- runAction cwd action
      when keepGoing (loop cwd)

drawWelcome :: FilePath -> IO ()
drawWelcome cwd = do
  putStrLn " /) /)"
  putStrLn "( . .)   CuteGit"
  putStrLn "( づ♡   A tiny git front end in Haskell"
  putStrLn ""
  putStrLn $ "Current folder: " ++ cwd
  putStrLn ""
  putStrLn "This folder is not a git repository yet."
  putStrLn ""
  putStrLn "1. Initialize git here"
  putStrLn "2. Switch to another folder"
  putStrLn "3. Refresh"
  putStrLn "q. Quit"
  putStrLn ""

drawHome :: View -> IO ()
drawHome view = do
  putStrLn " /) /)"
  putStrLn "( ^.^)   CuteGit"
  putStrLn "( づ♡   Git, but friendlier"
  putStrLn ""
  putStrLn $ "Repo:    " ++ repoPath view
  putStrLn $ "Branch:  " ++ branchName view ++ statusSuffix (aheadBehind view)
  putStrLn $ "Changed: " ++ show (length (changedFiles view)) ++ " file(s)"
  putStrLn $ "Staged:  " ++ show (length (stagedFiles view)) ++ " file(s)"
  putStrLn ""
  putStrLn "Recent commits:"
  if null (recentCommits view)
    then putStrLn "  no commits yet"
    else mapM_ (\line -> putStrLn ("  " ++ line)) (take 5 (recentCommits view))
  putStrLn ""
  putStrLn "1. Status"
  putStrLn "2. Log"
  putStrLn "3. Branches"
  putStrLn "4. Stage all"
  putStrLn "5. Commit"
  putStrLn "6. Pull"
  putStrLn "7. Push"
  putStrLn "8. Remote add"
  putStrLn "9. Change repo"
  putStrLn "0. Refresh"
  putStrLn "q. Quit"
  putStrLn ""

statusSuffix :: String -> String
statusSuffix summary
  | null summary = ""
  | otherwise = "  [" ++ summary ++ "]"

promptAction :: IO Action
promptAction = do
  putStr "Choose an action: "
  hFlush stdout
  c <- readKey
  putStrLn ""
  pure $ case c of
    '1' -> ShowStatus
    '2' -> ShowLog
    '3' -> ShowBranches
    '4' -> StageEverything
    '5' -> CommitChanges
    '6' -> PullChanges
    '7' -> PushChanges
    '8' -> RemoteAdd
    '9' -> ChangeRepo
    '0' -> Refresh
    'q' -> Quit
    'Q' -> Quit
    _ -> Refresh

promptSetupAction :: IO Action
promptSetupAction = do
  putStr "Choose an action: "
  hFlush stdout
  c <- readKey
  putStrLn ""
  pure $ case c of
    '1' -> InitRepo
    '2' -> ChangeRepo
    '3' -> Refresh
    'q' -> Quit
    'Q' -> Quit
    _ -> Refresh

runAction :: FilePath -> Action -> IO Bool
runAction cwd action =
  case action of
    ShowStatus -> do
      showCommandScreen cwd "Status" "git status --short --branch"
      pure True
    ShowLog -> do
      showCommandScreen cwd "Log" "git --no-pager log --oneline --decorate -n 12"
      pure True
    ShowBranches -> do
      showCommandScreen cwd "Branches" "git branch --all --verbose"
      pure True
    StageEverything -> do
      showCommandResult cwd "Stage all" "git add --all"
      pure True
    CommitChanges -> do
      putStr "Commit message: "
      hFlush stdout
      message <- promptLine
      if all isSpace message
        then do
          showMessage "Commit skipped because the message was empty."
          pure True
        else do
          showCommandResult cwd "Commit" ("git commit -m " ++ shellQuote message)
          pure True
    PullChanges -> do
      showCommandResult cwd "Pull" "git pull --stat"
      pure True
    PushChanges -> do
      showCommandResult cwd "Push" "git push"
      pure True
    RemoteAdd -> do
      putStr "Remote name: "
      hFlush stdout
      remoteName <- promptLine
      putStr "Remote URL: "
      hFlush stdout
      remoteUrl <- promptLine
      if all isSpace remoteName || all isSpace remoteUrl
        then do
          showMessage "Remote add skipped because name or URL was empty."
          pure True
        else do
          showCommandResult cwd "Remote add" ("git remote add " ++ shellQuote remoteName ++ " " ++ shellQuote remoteUrl)
          pure True
    ChangeRepo -> do
      putStr "Path to repo folder: "
      hFlush stdout
      target <- promptLine
      next <- resolveTarget cwd target
      exists <- doesDirectoryExist next
      if exists
        then do
          setCurrentDirectory next
          pure True
        else do
          showMessage "That folder does not exist."
          pure True
    InitRepo -> do
      createDirectoryIfMissing True cwd
      showCommandResult cwd "Initialize repository" "git init"
      pure True
    Refresh -> pure True
    Quit -> pure False

showCommandScreen :: FilePath -> String -> String -> IO ()
showCommandScreen cwd title command = do
  (exitCode, out, err) <- runShell cwd command
  clearScreen
  putStrLn $ "== " ++ title ++ " =="
  putStrLn ""
  putStr (if null out then "(no output)\n" else out)
  unless (null err) $ do
    putStrLn ""
    putStrLn "stderr:"
    putStr err
  putStrLn ""
  putStrLn $ "Result: " ++ summarizeExit exitCode
  waitForEnter

showCommandResult :: FilePath -> String -> String -> IO ()
showCommandResult cwd title command = do
  (exitCode, out, err) <- runShell cwd command
  clearScreen
  putStrLn $ "== " ++ title ++ " =="
  putStrLn ""
  putStrLn $ bunnyFor exitCode ++ "  " ++ summarizeExit exitCode
  unless (null out) $ do
    putStrLn ""
    putStr out
  unless (null err) $ do
    putStrLn ""
    putStrLn "stderr:"
    putStr err
  waitForEnter

bunnyFor :: ExitCode -> String
bunnyFor ExitSuccess = "(=^.^=)"
bunnyFor _ = "(;_;)"

summarizeExit :: ExitCode -> String
summarizeExit ExitSuccess = "ok"
summarizeExit (ExitFailure code) = "failed with exit code " ++ show code

loadView :: FilePath -> IO View
loadView cwd = do
  statusText <- fst3 <$> runGit cwd "status --short --branch"
  branchText <- fst3 <$> runGit cwd "branch --show-current"
  commitsText <- fst3 <$> runGit cwd "--no-pager log --oneline --decorate -n 5"
  let branch = currentBranch branchText statusText
  ahead <- aheadBehindSummary statusText
  let entries = parseStatusLines (drop 1 (lines statusText))
  pure
    View
      { repoPath = cwd,
        branchName = branch,
        aheadBehind = ahead,
        changedFiles = [path | (state, path) <- entries, isChangedState state],
        stagedFiles = [path | (state, path) <- entries, isStagedState state],
        recentCommits = filter (not . null) (lines commitsText)
      }

fst3 :: (String, String, ExitCode) -> String
fst3 (a, _, _) = a

currentBranch :: String -> String -> String
currentBranch branchText statusText
  | not (null trimmedBranch) = trimmedBranch
  | otherwise =
      case lines statusText of
        [] -> "unknown"
        (header : _) ->
          let cleaned = dropWhile isSpace (dropWhile (== '#') header)
           in case words cleaned of
                ("No" : "commits" : "yet" : "on" : name : _) -> name
                ("HEAD" : "detached" : _ ) -> "detached"
                (name : _) -> takeWhile (\c -> not (isSpace c) && c /= '.') name
                [] -> "unknown"
  where
    trimmedBranch = trim branchText

aheadBehindSummary :: String -> IO String
aheadBehindSummary text =
  pure $
    case lines text of
      [] -> ""
      (header : _) ->
        case break (== '[') header of
          (_, []) -> ""
          (_, rest) -> takeWhile (/= ']') (drop 1 rest)

parseStatusLines :: [String] -> [(String, String)]
parseStatusLines = mapMaybeStatus

mapMaybeStatus :: [String] -> [(String, String)]
mapMaybeStatus [] = []
mapMaybeStatus (line : rest)
  | length line < 4 = mapMaybeStatus rest
  | otherwise =
      let state = take 2 line
          rawPath = drop 3 line
       in (state, normalizePath rawPath) : mapMaybeStatus rest

normalizePath :: String -> String
normalizePath raw =
  let trimmed = dropWhile isSpace raw
   in case reverse (splitArrow trimmed) of
        [] -> trimmed
        latest : _ -> latest

splitArrow :: String -> [String]
splitArrow = splitOn " -> "

splitOn :: String -> String -> [String]
splitOn needle haystack
  | null needle = [haystack]
  | otherwise = go haystack
  where
    go text =
      case breakOn needle text of
        Nothing -> [text]
        Just (before, after) -> before : go after

breakOn :: String -> String -> Maybe (String, String)
breakOn needle haystack = search [] haystack
  where
    search _ [] = Nothing
    search acc rest
      | needle `isPrefixOf` rest = Just (reverse acc, drop (length needle) rest)
      | otherwise =
          let c : cs = rest
           in search (c : acc) cs

isChangedState :: String -> Bool
isChangedState [x, y] = x /= ' ' || y /= ' '
isChangedState _ = False

isStagedState :: String -> Bool
isStagedState (x : _) = x /= ' ' && x /= '?'
isStagedState _ = False

runGit :: FilePath -> String -> IO (String, String, ExitCode)
runGit cwd args = do
  (exitCode, out, err) <- runShell cwd ("git " ++ args)
  pure (trim out, trim err, exitCode)

runShell :: FilePath -> String -> IO (ExitCode, String, String)
runShell cwd command = readCreateProcessWithExitCode (shell ("cd " ++ shellQuote cwd ++ " && " ++ command)) ""

looksLikeGitRepo :: FilePath -> IO Bool
looksLikeGitRepo cwd = do
  gitDir <- doesDirectoryExist (cwd </> ".git")
  gitFile <- doesFileExist (cwd </> ".git")
  pure (gitDir || gitFile)

resolveTarget :: FilePath -> FilePath -> IO FilePath
resolveTarget cwd target
  | all isSpace target = pure cwd
  | "/" `isPrefixOf` target = pure target
  | otherwise = pure (cwd </> target)

shellQuote :: String -> String
shellQuote text = "'" ++ concatMap escape text ++ "'"
  where
    escape '\'' = "'\\''"
    escape c = [c]

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

showMessage :: String -> IO ()
showMessage message = do
  clearScreen
  putStrLn message
  putStrLn ""
  waitForEnter

waitForEnter :: IO ()
waitForEnter = do
  putStrLn ""
  putStr "Press Enter to continue..."
  hFlush stdout
  _ <- promptLine
  pure ()

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

readKey :: IO Char
readKey = do
  c <- getChar
  case c of
    '\ESC' -> swallowEscape >> readKey
    '\n' -> readKey
    '\r' -> readKey
    _ -> pure c

swallowEscape :: IO ()
swallowEscape = do
  next <- getChar
  if next == '['
    then swallowBracketed
    else pure ()

swallowBracketed :: IO ()
swallowBracketed = do
  c <- getChar
  when (c >= '0' && c <= '9') swallowBracketed

promptLine :: IO String
promptLine = go []
  where
    go acc = do
      c <- getChar
      case c of
        '\n' -> pure (reverse acc)
        '\r' -> pure (reverse acc)
        '\DEL' -> erase acc
        '\BS' -> erase acc
        '\ESC' -> swallowEscape >> go acc
        _ -> do
          putChar c
          hFlush stdout
          go (c : acc)

    erase [] = go []
    erase (_ : rest) = do
      putStr "\b \b"
      hFlush stdout
      go rest
