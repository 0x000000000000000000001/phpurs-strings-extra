module Data.String.Extra
  ( camelCase
  , kebabCase
  , pascalCase
  , snakeCase
  , upperCaseFirst
  , words
  , levenshtein
  , sorensenDiceCoefficient
  ) where

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.String.Unicode as Unicode
import Data.CodePoint.Unicode as UCP
import Data.Foldable (foldMap)
import Data.String as String
import Data.String.CodePoints as SCP
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Regex.Flags as Flags
import Prelude ((>>>), (<>), ($), map)

-- | Converts a `String` to camel case
-- |
-- | ```purs
-- | camelCase "Hello world" == "helloWorld"
-- | ```
camelCase :: String -> String
camelCase =
  words >>> Array.uncons >>> foldMap \{ head, tail } ->
    Unicode.toLower head <> foldMap pascalCase tail

-- | Converts a `String` to kebab case
-- |
-- | ```purs
-- | kebabCase "Hello world" == "hello-world"
-- | ```
kebabCase :: String -> String
kebabCase =
  words >>> map Unicode.toLower >>> String.joinWith "-"

-- | Converts a `String` to Pascal case
-- |
-- | ```purs
-- | pascalCase "Hello world" == "HelloWorld"
-- | ```
pascalCase :: String -> String
pascalCase =
  words >>> foldMap upperCaseFirst

-- | Converts a `String` to snake case
-- |
-- | ```purs
-- | snakeCase "Hello world" == "hello_world"
-- | ```
snakeCase :: String -> String
snakeCase =
  words >>> map Unicode.toLower >>> String.joinWith "_"

-- | Converts the first character in a `String` to upper case, lower-casing
-- | the rest of the string.
-- |
-- | ```purs
-- | upperCaseFirst "hello World" == "Hello world"
-- | ```
upperCaseFirst :: String -> String
upperCaseFirst =
  SCP.uncons >>> foldMap \{ head, tail } ->
    SCP.fromCodePointArray (UCP.toTitle head) <> Unicode.toLower tail

-- | Separates a `String` into words based on Unicode separators, capital
-- | letters, dashes, underscores, etc.
-- |
-- | ```purs
-- | words "Hello_world --from TheAliens" == [ "Hello", "world", "from", "The", "Aliens" ]
-- | ```
words :: String -> Array String
words string =
  if hasUnicodeWords string then
    unicodeWords string
  else
    asciiWords string

-- | Calculates the Levenshtein distance between two strings.
-- |
-- | ```purs
-- | levenshtein "book" "back" -- 2
-- | ```
foreign import levenshtein :: String -> String -> Int

-- | Calculates the Sørensen-Dice coefficient between two strings.
-- |
-- | ```purs
-- | sorensenDiceCoefficient "WHIRLED" "WORLD" -- 0.2000
-- | ```
foreign import sorensenDiceCoefficient :: String -> String -> Number

------------------------------------------------------------------------------

regexGlobal :: String -> Regex
regexGlobal regexStr =
  unsafeRegex regexStr (Flags.global <> Flags.unicode)

regexHasASCIIWords :: Regex
regexHasASCIIWords =
  regexGlobal "[^\x00-\x2f\x3a-\x40\x5b-\x60\x7b-\x7f]+"

asciiWords :: String -> Array String
asciiWords =
  Regex.match regexHasASCIIWords >>> foldMap NonEmptyArray.catMaybes

regexHasUnicodeWords :: Regex
regexHasUnicodeWords =
  regexGlobal "[a-z][A-Z]|[A-Z]{2,}[a-z]|[0-9][a-zA-Z]|[a-zA-Z][0-9]|[^a-zA-Z0-9]"

hasUnicodeWords :: String -> Boolean
hasUnicodeWords =
  Regex.test regexHasUnicodeWords

regexUnicodeWords :: Regex
regexUnicodeWords =
  regexGlobal
    $ String.joinWith "|"
        [ rsUpper <> "?" <> rsLower <> "+" <> rsOptContrLower <> "(?=" <> rsBreak <> "|" <> rsUpper <> "|$)"
        , rsMiscUpper <> "+" <> rsOptContrUpper <> "(?=" <> rsBreak <> "|" <> rsUpper <> rsMiscLower <> "|$)"
        , rsUpper <> "?" <> rsMiscLower <> "+" <> rsOptContrLower
        , rsUpper <> "+" <> rsOptContrUpper
        , rsOrdUpper
        , rsOrdLower
        , rsDigit <> "+"
        , rsEmoji
        ]
  where
  -- https://github.com/lodash/lodash/blob/master/.internal/unicodeWords.js
  -- Adapted for PCRE2 (PHP) by using \x{...} and actual unicode code points instead of surrogate pairs.
  rsAstralRange = "\\x{10000}-\\x{10ffff}"
  rsComboMarksRange = "\\x{0300}-\\x{036f}"
  reComboHalfMarksRange = "\\x{fe20}-\\x{fe2f}"
  rsComboSymbolsRange = "\\x{20d0}-\\x{20ff}"
  rsComboMarksExtendedRange = "\\x{1ab0}-\\x{1aff}"
  rsComboMarksSupplementRange = "\\x{1dc0}-\\x{1dff}"
  rsComboRange = rsComboMarksRange <> reComboHalfMarksRange <> rsComboSymbolsRange <> rsComboMarksExtendedRange <> rsComboMarksSupplementRange
  rsDingbatRange = "\\x{2700}-\\x{27bf}"
  rsLowerRange = "a-z\\x{00df}-\\x{00f6}\\x{00f8}-\\x{00ff}"
  rsMathOpRange = "\\x{00ac}\\x{00b1}\\x{00d7}\\x{00f7}"
  rsNonCharRange = "\\x{0000}-\\x{002f}\\x{003a}-\\x{0040}\\x{005b}-\\x{0060}\\x{007b}-\\x{00bf}"
  rsPunctuationRange = "\\x{2000}-\\x{206f}"
  rsSpaceRange = " \\t\\x{000b}\\f\\x{00a0}\\x{feff}\\n\\r\\x{2028}\\x{2029}\\x{1680}\\x{180e}\\x{2000}\\x{2001}\\x{2002}\\x{2003}\\x{2004}\\x{2005}\\x{2006}\\x{2007}\\x{2008}\\x{2009}\\x{200a}\\x{202f}\\x{205f}\\x{3000}"
  rsUpperRange = "A-Z\\x{00c0}-\\x{00d6}\\x{00d8}-\\x{00de}"
  rsVarRange = "\\x{fe0e}\\x{fe0f}"
  rsBreakRange = rsMathOpRange <> rsNonCharRange <> rsPunctuationRange <> rsSpaceRange

  -- Used to compose unicode capture groups.
  rsApos = "['\\x{2019}]"
  rsBreak = "[" <> rsBreakRange <> "]"
  rsCombo = "[" <> rsComboRange <> "]"
  rsDigit = "\\d"
  rsDingbat = "[" <> rsDingbatRange <> "]"
  rsLower = "[" <> rsLowerRange <> "]"
  rsMisc = "[^" <> rsAstralRange <> rsBreakRange <> rsDigit <> rsDingbatRange <> rsLowerRange <> rsUpperRange <> "]"
  rsFitz = "\\x{1f3fb}-\\x{1f3ff}"
  rsModifier = "(?:" <> rsCombo <> "|" <> rsFitz <> ")"
  rsNonAstral = "[^" <> rsAstralRange <> "]"
  rsRegional = "(?:\\x{1f1e6}-\\x{1f1ff}){2}"
  rsSurrPair = "[\\x{10000}-\\x{10ffff}]"
  rsUpper = "[" <> rsUpperRange <> "]"
  rsZWJ = "\\x{200d}"

  -- Used to compose unicode regexes.
  rsMiscLower = "(?:" <> rsLower <> "|" <> rsMisc <> ")"
  rsMiscUpper = "(?:" <> rsUpper <> "|" <> rsMisc <> ")"
  rsOptContrLower = "(?:" <> rsApos <> "(?:d|ll|m|re|s|t|ve))?"
  rsOptContrUpper = "(?:" <> rsApos <> "(?:D|LL|M|RE|S|T|VE))?"
  reOptMod = rsModifier <> "?"
  rsOptVar = "[" <> rsVarRange <> "]?"
  rsOptJoin = "(?:" <> rsZWJ <> "(?:" <> rsNonAstral <> "|" <> rsRegional <> "|" <> rsSurrPair <> ")" <> rsOptVar <> reOptMod <> ")*"
  rsOrdLower = "\\d*(?:1st|2nd|3rd|(?![123])\\dth)(?=\\b|[A-Z_])"
  rsOrdUpper = "\\d*(?:1ST|2ND|3RD|(?![123])\\dTH)(?=\\b|[a-z_])"
  rsSeq = rsOptVar <> reOptMod <> rsOptJoin
  rsEmoji = "(?:" <> rsDingbat <> "|" <> rsRegional <> "|" <> rsSurrPair <> ")" <> rsSeq

unicodeWords :: String -> Array String
unicodeWords =
  Regex.match regexUnicodeWords >>> foldMap NonEmptyArray.catMaybes
