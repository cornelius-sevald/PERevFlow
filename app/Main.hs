module Main (main) where

import Utils.Error
import Utils.Maps
import Utils.PrettyPrint

import RL.AST
import RL.Wellformed
import RL.Values
import RL.Variables
import RL.Preprocess

import Parsing.Parser

import PE.AST2
import PE.SpecValues
import PE.Preprocessing.BTA
import PE.Preprocessing.Division
import PE.Preprocessing.Normalize
import PE.Preprocessing.Explicicator
import PE.Preprocessing.Annotate
import PE.Preprocessing.Wellformed2
import PE.Specialization.Specialize
import PE.Specialization.PostProcessing

import Inversion.Inverter

import Interpretation.Interpret

import Assertions.AssertionRemoval

import Options.Applicative
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import System.Exit (die)
import Control.Monad (when, unless)

data Options = Specialize SpecOptions
             | Invert InvertOptions
             | Preprocess PreprocessOptions
             | Interpret InterpretOptions
             | Bench BenchOptions
             | Optimize OptimizeOptions

data SpecOptions = SpecOptions
  { specInpFile   :: String
  , specOutFile   :: String
  , specFile      :: String
  , uniformBTA    :: Bool
  , skipSpecPhase :: Bool
  , skipPost      :: Bool
  , specAssertRem :: Bool
  , specVerbose   :: Bool
  , specTrace     :: Bool
  , specIgnore    :: [Name]
  }

data InvertOptions = InvertOptions
  { invInpFile :: String
  , invOutFile :: String
  , invVerbose :: Bool
  }

data PreprocessOptions = PreprocessOptions
  { prepFile :: String
  , prepInputFile :: String
  , prepOutFile :: String
  , prepVerbose :: Bool
  , prepEncode :: Bool
  }

data InterpretOptions = InterpretOptions
  { intFile      :: String
  , intInputFile :: String
  , intVerbose   :: Bool
  }

data BenchOptions = BenchOptions
  { benchFile :: String
  , benchSpecFile :: String
  , dynamicVars :: [Name]
  , benchVerbose :: Bool
  }

-- flags for all steps etc
data OptimizeOptions = OptimizeOptions
  { optimInput :: String
  , optimOutput :: String
  , optimBidirectional :: Bool
  , optimVerbose :: Bool
  }

specParser :: Parser Options
specParser = Specialize <$> (SpecOptions
          <$> argument str (metavar "<Input RL file>")
          <*> argument str (metavar "<Output path>")
          <*> argument str (metavar "<Spec file>")
          <*> switch (long "uniformBTA"
                           <> short 'u'
                           <> help "Perform uniform BTA rather than pointwise")
          <*> switch (long "skipSpec"
                           <> short 's'
                           <> help "Stop after BTA, printing the RL2 program")
          <*> switch (long "skipPost"
                           <> short 'p'
                           <> help "Stop before post-processing, printing the initial residual program")
          <*> flag False True (long "removeAssertions"
                           <> short 'a'
                           <> help "Remove assertions via abstract interpretation")
          <*> flag True False (long "verbose"
                           <> short 'v'
                           <> help "Show messages and info for each phase")
          <*> switch (long "trace"
                           <> short 't'
                           <> help "Show trace of specialization (Not recommended)")
          <*> option strp (long "ignore"
                            <> short 'i'
                            <> help "Treat variables as dynamic"
                            <> value [])
          )
  where
    strp = eitherReader $ return . words

inverterParser :: Parser Options
inverterParser = Invert <$> (InvertOptions
              <$> argument str (metavar "<Input RL file>")
              <*> argument str (metavar "<Output path>")
              <*> flag True False (long "verbose"
                           <> short 'v'
                           <> help "Show messages and info for each phase")
              )

preprocessParser :: Parser Options
preprocessParser = Preprocess <$> (PreprocessOptions
              <$> argument str (metavar "<Input RL file>")
              <*> argument str (metavar "<Spec file> (not needed if not encoding)")
              <*> argument str (metavar "<Output path>")
              <*> flag True False (long "verbose"
                           <> short 'v'
                           <> help "Show messages and info for each phase")
              <*> flag True False (long "encode"
                           <> short 'e'
                           <> help "After preprocessing, encoding program to RL value")
              )

interpretParser :: Parser Options
interpretParser = Interpret <$> (InterpretOptions
               <$> argument str (metavar "<Input RL file>")
               <*> argument str (metavar "<Spec file>")
               <*> flag True False (long "verbose"
                           <> short 'v'
                           <> help "Show messages and info for each phase")
              )

benchParser :: Parser Options
benchParser = Bench <$> (BenchOptions
           <$> argument str (metavar "<Input RL file>")
           <*> argument str (metavar "<Spec file>")
           <*> argument strp (metavar "<List of dynamic input vars>")
           <*> flag False True (long "verbose"
                           <> short 'v'
                           <> help "Show messages and info for each phase")
           )
  where
    strp = eitherReader $ return . words

optimParser :: Parser Options
optimParser = Optimize <$> (OptimizeOptions
           <$> argument str (metavar "<Input RL file>")
           <*> argument str (metavar "<Output RL file>")
           <*> flag False True (long "Bidirectional"
                           <> short 'b'
                           <> help "Use bidirectional analysis instead")
           <*> flag True False (long "verbose"
                           <> short 'v'
                           <> help "Show messages and info for each phase")
           )

optParser :: Parser Options
optParser = hsubparser
              ( command "spec" (info specParser
                (progDesc "Specialize an RL program"))
             <> command "invert" (info inverterParser
                (progDesc "Invert an RL program"))
             <> command "preprocess" (info preprocessParser
                (progDesc "Preprocess an RL program for self-interpretation"))
             <> command "interpret" (info interpretParser
                (progDesc "Interpret an RL program"))
             <> command "bench" (info benchParser
                (progDesc "Run various benchmarks on an RL program"))
             <> command "optimize" (info optimParser
                (progDesc "Optimize an RL program"))
              )

optsParser :: ParserInfo Options
optsParser = info (optParser <**> helper)
  ( fullDesc
  <> progDesc "Various operations on RL programs\n (spec/invert/interpret/bench/optimize)"
  )

trace :: Bool -> String -> IO ()
trace b s = when b $ putStrLn s

fromEM :: String -> EM a -> IO a
fromEM _ (Right a) = return a
fromEM s (Left e) = die $ "Error while " ++ s ++ ": " ++ e

fromLEM :: String -> LEM a -> IO (a, [String])
fromLEM _ (LEM (Right a, l)) = return (a, l)
fromLEM s (LEM (Left e, l)) =
  do putStrLn "LOG:"
     mapM_ putStrLn l
     die $ "Error while " ++ s ++ ": " ++ e

parseFile :: String -> Bool -> (String -> EM a) -> String -> IO a
parseFile goal verbose parse file =
  do trace verbose $ "- Reading " ++ goal ++ " from file: " ++ show file
     content <- readFile file
     trace verbose "- Parsing input"
     fromEM "parsing" $ parse content

writeOutput :: Bool -> String -> String -> IO ()
writeOutput verbose path content =
  do trace verbose $ "Writing to " ++ path
     writeFile path content

main :: IO ()
main = do
  options <- execParser optsParser
  case options of
    Specialize opts -> specMain  opts
    Invert     opts -> invMain   opts
    Preprocess opts -> prepMain  opts
    Interpret  opts -> intMain   opts
    Bench      opts -> benchMain opts
    Optimize   opts -> optimMain opts

invMain :: InvertOptions -> IO ()
invMain InvertOptions { invInpFile = inputPath
                      , invOutFile = outputPath
                      , invVerbose = v} =
  do prog <- parseFile "program" v parseProg inputPath
     _ <- fromEM "performing wellformedness check of input prog"
              $ wellformedProg prog
     trace v "- Inverting program."
     let invProg = invertProg prog
     _ <- fromEM "performing wellformedness check of reverse prog"
              $ wellformedProg invProg
     let out = prettyProg id invProg
     writeOutput v outputPath out

prepMain :: PreprocessOptions -> IO ()
prepMain PreprocessOptions { prepFile = filePath
                          , prepInputFile = inputPath
                          , prepOutFile = outputPath
                          , prepVerbose = v
                          , prepEncode = enc } =
  do prog <- parseFile "program" v parseProg filePath
     _ <- fromEM "performing wellformedness check of input prog"
              $ wellformedProg prog
     spec <- parseFile "division and specilization data" v parseSpec inputPath
     trace v "- Preprocessing program."
     prepProg@(decl, _) <-
       maybe (die "Error preprocessing program")
       pure
       (preprocessProgram prog)
     trace (v && enc) "- Encoding program."
     -- Need to add temporary values from preprocessing to store
     let varNames = input decl ++ output decl ++ temp decl
     let nilStore = fromList $ map (\x -> (x, Nil)) varNames
     let store = combine spec nilStore
     let encProg = encodeProgram prepProg
     let encStore = encodeStore store
     let out = if enc then "prog = '"  <> prettyVal encProg <> "\n" <>
                           "input = '" <> prettyVal encStore <> "\n"
                      else prettyProg id prepProg
     writeOutput v outputPath out

optimMain :: OptimizeOptions -> IO ()
optimMain OptimizeOptions { optimInput = inputPath
                          , optimOutput = outputPath
                          , optimBidirectional = useBidir
                          , optimVerbose = v} =
  do prog <- parseFile "program" v parseProg inputPath
     let mode = if useBidir then " (BIDIRECTIONAL)" else ""
     trace v $ "- Removing assertions" ++ mode
     let removeAsserts = if useBidir then removeAssertionsBi else removeAssertionsUni
     let optimProg = removeAsserts prog
     let out = prettyProg id optimProg
     trace v $ "Assertions removed: " ++ show (assCount prog - assCount optimProg)
     trace v $ "Assertions remaining: " ++ show (assCount optimProg)
     writeOutput v outputPath out
  where
    assCount = sum . map (length . filter isAssertion . body) . snd
    isAssertion (Assert _) = True
    isAssertion _ = False

intMain :: InterpretOptions -> IO ()
intMain InterpretOptions { intFile = filePath
                         , intInputFile = inputPath
                         , intVerbose = v} =
  do prog <- parseFile "program" v parseProg filePath
     _ <- fromEM "performing wellformedness check of input prog"
              $ wellformedProg prog
     initStore <- parseFile "division and specilization data" v parseSpec inputPath
     (out, _) <- fromLEM "execution" $ runProgram prog initStore
     let (outStore, stats) = out
     trace v "Output store: "
     let outvals = filter (\(n, _) -> n `elem` output (fst prog))
                    $ toList outStore
     let outstr = map (\(n, val) -> n ++ ": " ++ prettyVal val) outvals
     putStrLn (unlines outstr)
     trace v "Execution statistics: "
     putStrLn $ prettyStats stats

specMain :: SpecOptions -> IO ()
specMain specOpts@SpecOptions { specInpFile = inputPath
                              , specOutFile = outputPath
                              , uniformBTA  = uniform
                              , specFile    = specPath
                              , specVerbose = v
                              , specIgnore  = ignores} =
  do prog <- parseFile "program" v parseProg inputPath
     let decl = fst prog
     _ <- fromEM "performing wellformedness check of input prog"
              $ wellformedProg prog
     initStore' <- parseFile "division and specilization data"
                    v parseSpec specPath
     let initStore = initStore' `withouts` ignores
     trace v "- Normalizing input program."
     let nprog = normalize "init" "stop" prog (\l i -> l ++ "_" ++ show i)
     trace v "- Creating initial binding."
     d <- fromEM "binding" $ makeDiv initStore decl
     trace v $ "- Performing BTA " ++ if uniform then "(uniform)" else "(pointwise)"
     let btaFunc = if uniform then btaUniform else btaPW
     let congruentDiv = btaFunc nprog d
     let store = makeSpecStore decl (startingDiv nprog congruentDiv) initStore
     trace v "- Annotating program"
     let prog2 = annotateProg congruentDiv nprog
     _ <- fromEM "wellformedness of 2 level lang. This should never happen. Please report."
                 (wellformedProg' congruentDiv prog2)
     let explicated = explicate congruentDiv prog2 (\l i -> l ++ "_" ++ show i)
     out <- if skipSpecPhase specOpts
              then do trace v "- Skip specialization";
                      return $ prettyProg' (serializeExpl id) explicated
              else specMain2 specOpts decl explicated store
     writeOutput v outputPath out

btaUniform, btaPW :: NormProgram Label -> Division -> PWDivision Label
btaUniform = congruentUniformDiv
btaPW p d =
  let initd = initPWDiv p d
  in makeCongruentPW p initd

specMain2 :: SpecOptions -> VariableDecl -> Program' (Explicated Label) -> SpecStore -> IO String
specMain2 specOpts decl prog2 store =
  let v = specVerbose specOpts
  in
  do trace v "- Specializing"
     (res, l) <- fromLEM "specializing" $ specialize decl prog2 store (Regular "entry")
     trace (specTrace specOpts) $ "Trace: (label: State)\n" ++ unlines l
     let (resdecl, resbody) = res
     if skipPost specOpts
     then do
        trace v "- Skip post processing"
        let clean = mapCombine (serializeAnn (serializeExpl id)) resbody
        return $ prettyProg id (resdecl, clean)
     else do
        trace v "- POST PROCESSING"
        (prog', staticVals) <- specPostProcess v decl res
        let prog = if specAssertRem specOpts
                   then removeAllAssertions prog'
                   else prog'
        printStaticOutput decl staticVals
        return $ prettyProg id prog

specPostProcess :: Bool -> VariableDecl -> Program (Explicated Label) (Maybe SpecStore)
                -> IO (Program Label (), [(Name, SpecValue)])
specPostProcess v origdecl (decl, prog) =
  do let showLength p = trace v $ "Nr. of blocks: " ++ show (length p)
     showLength prog
     trace v "- Folding constants"
     (folded, out) <- fromLEM "Folding" $ constFold prog
     trace v $ "Expressions reduced: " ++ show (length out)
     trace v "- Merging explicitors"
     let cleanStores = mapProgStore (fromMaybe emptyMap) folded
     let merged' = mergeExplicators (\l i1 i2 -> l ++ "_e" ++ show i1 ++ "_" ++ show i2) cleanStores
     let merged = mapLabel (serializeExpl id) merged'
     showLength merged
     trace v "- Merging exits"
     let ((decl', singleExit), staticVals) =
            mergeExits origdecl (\l i1 i2 -> l ++ "_x" ++ show i1 ++ "_" ++ show i2) (decl, merged)
     showLength singleExit
     trace v "- Removing dead blocks"
     let liveProg = removeDeadBlocks singleExit
     showLength liveProg
     trace v "- Adding assertions / Making blocks wellformed"
     let withAssertions = changeConditionals liveProg
     fromEM "Wellformedness check 1" $ wellformedProg (decl', withAssertions)
     trace v "- Cleaning names"
     let numeratedStore = enumerateAnn withAssertions
     let clean = mapCombine (\l s -> l ++ "_" ++ show s) numeratedStore
     fromEM "Wellformedness check 2" $ wellformedProg (decl', clean)
     trace v "- Compressing paths"
     let compressed = compressPaths clean
     fromEM "Wellformedness check 3" $ wellformedProg (decl', compressed)
     showLength compressed
     return ((decl', compressed), staticVals)

printStaticOutput :: VariableDecl -> [(Name, SpecValue)] -> IO ()
printStaticOutput decl tpls =
  do trace True "-- FINAL STATIC OUTPUT --"
     let outvals = filter (\(n, v) -> n `elem` output decl && v /= Dynamic) tpls
     let strings = map (\(n,v) -> n ++ " = " ++ showSpecVal v) outvals
     putStrLn $ intercalate "\n" strings
     trace True "-------------------------"
  where
    showSpecVal (Static v) = prettyVal v
    showSpecVal Dynamic = undefined

benchMain :: BenchOptions -> IO ()
benchMain BenchOptions { benchFile     = inputPath
                       , benchSpecFile = specPath
                       , dynamicVars   = dyn
                       , benchVerbose  = v } =
  do prog <- parseFile "program" False parseProg inputPath
     fromEM "wellformedness of input prog" $ wellformedProg prog
     trace v "Initial interpretation"
     completeStore <- parseFile "input" False parseSpec specPath
     ((outInt, statsInt), _) <- fromLEM "execution" $ runProgram prog completeStore
     let decl = fst prog
     let initStore = completeStore `withouts` dyn
     d <- fromEM "binding" $ makeDiv initStore decl
     let nprog = normalize' prog
     let pipeline' = pipeline nprog d decl completeStore initStore outInt
     (resUni, statsUni) <- pipeline' "UNI" btaUniform
     (resPW, statsPW) <- pipeline' "PW" btaPW
     putStrLn $ "-- Source program\n" ++ prettyStats statsInt
     putStrLn $ prettySize (prog, prog)
     prettySpeed' "Uniform PE" statsInt statsUni
     putStrLn $ prettySize resUni
     prettySpeed' "Pointwise PE" statsInt statsPW
     putStrLn $ prettySize resPW
  where

    pipeline nprog d decl runstore initSpec origOut mode bta =
      do (prog2, startDiv) <- preprocess mode bta nprog d
         let specStore = makeSpecStore decl startDiv initSpec
         res <- specialize' mode decl prog2 specStore
         (prog, outStatic) <- postprocess mode decl res
         (outRes, stats) <- run mode prog runstore
         let progOptim = removeAllAssertions prog
         (_, statsOptim) <- run mode progOptim runstore
         let combinedOut = combine (toStore $ fromList outStatic) outRes
         verifyOutput mode origOut combinedOut
         fromEM ("wellformedness of residual prog " ++ mode) $
            wellformedProg prog
         return ((prog, progOptim), (stats, statsOptim))

    preprocess mode bta nprog d =
      do trace v $ "Preprocessing " ++ mode
         let cdiv = bta nprog d
         let p2 = annotateProg cdiv nprog
         fromEM "wellformedness of RL2 prog" (wellformedProg' cdiv p2)
         let expl = explicate cdiv p2 (\l i -> l ++ "_" ++ show i)
         return (expl, startingDiv nprog cdiv)

    run mode p s =
      do trace v $ "Interpreting " ++ mode
         (res, _) <- fromLEM "execution" $ runProgram' p s
         return res

    normalize' p = normalize "init" "stop" p (\l i -> l ++ "_" ++ show i)

    specialize' mode d p s =
      do trace v $ "Specializing " ++ mode
         (prog, _) <- fromLEM "specializing" $ specialize d p s (Regular "entry")
         return prog

    postprocess mode origdecl prog =
      do trace v $ "Postprocessing " ++ mode
         specPostProcess False origdecl prog

    verifyOutput mode regularOut specOut =
      do trace v $ "Checking output " ++ mode
         unless (all (\(n, val) -> val == get n specOut) (toList regularOut))
            $ die "The regular and specialized output do not match"

    prettySpeed' mode origStats (newStats, newStatsO) =
      let origSpeed = totalSteps origStats
          newSpeed = totalSteps newStats
          newSpeedO = totalSteps newStatsO
          speedup, speedupO :: Float
          speedup = fromIntegral origSpeed / fromIntegral newSpeed
          speedupO = fromIntegral origSpeed / fromIntegral newSpeedO
      in do putStrLn $ "-- " ++ mode
            putStrLn $ prettyStats2 newStats newStatsO
            putStrLn $ "Speedup: " ++ take 5 (show speedup) ++ "x" ++ brace (take 5 (show speedupO) ++ "x")

    brace s = " (" ++ s ++ ")"
    blocks = show . length
    lineCount p = show $ sum (map (length . body) p) + 2 * length p
    prettySize ((_, prog), (_, progOptim)) = "Total Blocks: " ++ blocks prog ++ brace (blocks progOptim)
                        ++ ", Total Lines: " ++ lineCount prog ++ brace (lineCount progOptim)
                        ++ ", Asserts removed: " ++ show (asserts prog - asserts progOptim ) ++ "/" ++ show (asserts prog)
    asserts = sum . map (length . filter (\s -> case s of Assert _ -> True; _ -> False) . body)

makeSpecStore :: VariableDecl -> Division -> Store -> SpecStore
makeSpecStore decl d s =
  let dynStore  = fromList . map (\n -> (n, Dynamic)) $ allVars decl
      stats     = allWhere (\_ e -> e == BTStatic) d
      statStore = fromList $ map (\n -> (n, Static $ lookup' Nil n s)) stats
      store     = statStore `combine` dynStore
  in store
