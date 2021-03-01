{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.IO (Handle, withFile, IOMode (ReadMode))
import qualified Data.Attoparsec.Text as P
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (ignore)
import qualified Data.Text as T
import qualified Data.Set as Set


data State
    = State
    { budgetOrgS :: !(Set.Set T.Text)
    , lineNum :: !Int
    , numBad :: !Int
    }


initState :: State
initState =
    State
    { budgetOrgS = Set.empty
    , lineNum = 0
    , numBad = 0
    }


main :: IO ()
main =
    withFile "../data.csv" ReadMode $ \handle ->
    mainHelp initState handle


mainHelp :: State -> Handle -> IO ()
mainHelp state handle =
    do
    rawRow <- B.hGetLine handle
    let rowText = decodeUtf8With ignore rawRow
    case P.parseOnly rowP rowText of
        Left _ ->
            do
            putStrLn $ show $ lineNum state
            mainHelp
                (state { numBad = numBad state + 1, lineNum = lineNum state + 1 }) handle

        Right row ->
            mainHelp (updateState state row) handle


updateState :: State -> Row -> State
updateState state row =
    state
        { budgetOrgS =
            Set.insert (budgetOrgR row) (budgetOrgS state)
        , lineNum = lineNum state + 1
        }


data Row
    = Row
    { budgetOrgR :: !T.Text
    }


rowP :: P.Parser Row
rowP =
    do
    _ <- yearNoP
    barP
    _ <- yearShortNameP
    barP
    _ <- quarterShortNameP
    barP
    _ <- monthShortNameP
    barP
    budgetOrg <- budgetOrgP
    barP
    _ <- depGroupCodeP
    barP
    _ <- depGroupLongP
    barP
    _ <- orgCodeP
    barP
    _ <- orgLongP
    barP
    _ <- orgTypeP
    barP
    _ <- orgTypeLongP
    barP
    _ <- orgTypeCodeP
    barP
    _ <- orgTypeLongP
    barP
    _ <- pesaGroupP
    barP
    _ <- pesaGroupLongP
    barP
    _ <- srGroupP
    barP
    _ <- srGroupLongP
    barP
    _ <- bxValidCodeP
    barP
    _ <- bxValidGroupLongP
    barP
    _ <- foValidCodeP
    barP
    _ <- foValidLongP
    barP
    _ <- accountArrangeCodeP
    barP
    _ <- accountArrangeLongP
    barP
    _ <- counterCodeP
    barP
    _ <- counterLongP
    barP
    _ <- segCodeP
    barP
    _ <- segLongP
    barP
    _ <- cofog0CodeP
    barP
    _ <- cofog0LongP
    barP
    _ <- cofog1CodeP
    barP
    _ <- cofog1LongP
    barP
    _ <- cofog2CodeP
    barP
    _ <- cofog2LongP
    barP
    _ <- control0LongP
    barP
    _ <- control1LongP
    barP
    _ <- coverageLongP
    barP
    _ <- estimatesCodeP
    barP
    _ <- estimatesLongP
    barP
    _ <- netSubheadLongP
    barP
    _ <- pesa11CodeP
    barP
    _ <- pesaGrantsCodeP
    barP
    _ <- pesaLgCodeP
    barP
    _ <- pesaServicesLongP
    barP
    _ <- pesaRegionalCodeP
    barP
    _ <- policyRingfenceP
    barP
    _ <- accountAuthL0CodeP
    barP
    _ <- accountAuthL1LongP
    barP
    _ <- subFuncCodeP
    barP
    _ <- subFuncLongP
    barP
    _ <- funcCodeP
    barP
    _ <- funcLongP
    barP
    _ <- accountsLongP
    barP
    _ <- chartL5CodeP
    barP
    _ <- chartL5LongP
    barP
    _ <- economicBudgetCodeP
    barP
    _ <- economicRingfenceCodeP
    barP
    _ <- economicGroupCodeP
    barP
    _ <- economicGroupLongP
    barP
    _ <- economicCatCodeP
    barP
    _ <- economicCatLongP
    barP
    _ <- sectorCodeP
    barP
    _ <- sectorLongP
    barP
    _ <- tesCodeP
    barP
    _ <- tesLongP
    barP
    _ <- esaCodeP
    barP
    _ <- esaLongP
    barP
    _ <- esaGroupCodeP
    barP
    _ <- esaGroupLongP
    barP
    _ <- psatCodeP
    barP
    _ <- psatLongP
    barP
    _ <- naAggregateCodeP
    barP
    _ <- naAggregateLongP
    barP
    _ <- estimatesCatCodeP
    barP
    _ <- estimatesSubCatCodeP
    barP
    _ <- estimatesColCodeP
    barP
    _ <- pesaEconomicBudgetCodeP
    barP
    _ <- pesaEconomicGroupCodeP
    barP
    _ <- incomeCatShortP
    barP
    _ <- usageCodeP
    barP
    _ <- statusCodeP
    barP
    _ <- typeCodeP
    barP
    _ <- typeLongP
    barP
    _ <- typeGroupCodeP
    barP
    _ <- typeGroupLongP
    barP
    _ <- versionCodeP
    barP
    _ <- fctLoadTypeCodeP
    barP
    _ <- fctLoadTypeLongP
    barP
    _ <- rowDescriptionP
    barP
    _ <- dataIdP
    barP
    _ <- amountP
    barP
    _ <- datetimeP
    return $ Row
        { budgetOrgR = budgetOrg
        }


barP :: P.Parser ()
barP =
    do
    _ <- P.char '|'
    return ()


yearNoP :: P.Parser T.Text
yearNoP =
    stringP


yearShortNameP :: P.Parser T.Text
yearShortNameP =
    stringP


quarterShortNameP :: P.Parser T.Text
quarterShortNameP =
    stringP


monthShortNameP :: P.Parser T.Text
monthShortNameP =
    stringP


budgetOrgP :: P.Parser T.Text
budgetOrgP =
    stringP


depGroupCodeP :: P.Parser T.Text
depGroupCodeP =
    stringP


depGroupLongP :: P.Parser T.Text
depGroupLongP =
    stringP


orgCodeP :: P.Parser T.Text
orgCodeP =
    stringP


orgLongP :: P.Parser T.Text
orgLongP =
    stringP


orgTypeP :: P.Parser T.Text
orgTypeP =
    stringP


orgTypeLongP :: P.Parser T.Text
orgTypeLongP =
    stringP


orgTypeCodeP :: P.Parser T.Text
orgTypeCodeP =
    stringP


pesaGroupP :: P.Parser T.Text
pesaGroupP =
    stringP


pesaGroupLongP :: P.Parser T.Text
pesaGroupLongP =
    stringP


srGroupP :: P.Parser T.Text
srGroupP =
    stringP


srGroupLongP :: P.Parser T.Text
srGroupLongP =
    stringP


bxValidCodeP :: P.Parser T.Text
bxValidCodeP =
    stringP


bxValidGroupLongP :: P.Parser T.Text
bxValidGroupLongP =
    stringP


foValidCodeP :: P.Parser T.Text
foValidCodeP =
    stringP


foValidLongP :: P.Parser T.Text
foValidLongP =
    stringP


accountArrangeCodeP :: P.Parser T.Text
accountArrangeCodeP =
    stringP


accountArrangeLongP :: P.Parser T.Text
accountArrangeLongP =
    stringP


counterCodeP :: P.Parser T.Text
counterCodeP =
    stringP


counterLongP :: P.Parser T.Text
counterLongP =
    stringP


segCodeP :: P.Parser T.Text
segCodeP =
    stringP


segLongP :: P.Parser T.Text
segLongP =
    stringP


cofog0CodeP :: P.Parser T.Text
cofog0CodeP =
    stringP


cofog0LongP :: P.Parser T.Text
cofog0LongP =
    stringP


cofog1CodeP :: P.Parser T.Text
cofog1CodeP =
    stringP


cofog1LongP :: P.Parser T.Text
cofog1LongP =
    stringP


cofog2CodeP :: P.Parser T.Text
cofog2CodeP =
    stringP


cofog2LongP :: P.Parser T.Text
cofog2LongP =
    stringP


control0LongP :: P.Parser T.Text
control0LongP =
    stringP


control1LongP :: P.Parser T.Text
control1LongP =
    stringP


coverageLongP :: P.Parser T.Text
coverageLongP =
    stringP


estimatesCodeP :: P.Parser T.Text
estimatesCodeP =
    stringP


estimatesLongP :: P.Parser T.Text
estimatesLongP =
    stringP


netSubheadLongP :: P.Parser T.Text
netSubheadLongP =
    stringP


pesa11CodeP :: P.Parser T.Text
pesa11CodeP =
    stringP


pesaGrantsCodeP :: P.Parser T.Text
pesaGrantsCodeP =
    stringP


pesaLgCodeP :: P.Parser T.Text
pesaLgCodeP =
    stringP


pesaServicesLongP :: P.Parser T.Text
pesaServicesLongP =
    stringP


pesaRegionalCodeP :: P.Parser T.Text
pesaRegionalCodeP =
    stringP


policyRingfenceP :: P.Parser T.Text
policyRingfenceP =
    stringP


accountAuthL0CodeP :: P.Parser T.Text
accountAuthL0CodeP =
    stringP


accountAuthL1LongP :: P.Parser T.Text
accountAuthL1LongP =
    stringP


subFuncCodeP :: P.Parser T.Text
subFuncCodeP =
    stringP


subFuncLongP :: P.Parser T.Text
subFuncLongP =
    stringP


funcCodeP :: P.Parser T.Text
funcCodeP =
    stringP


funcLongP :: P.Parser T.Text
funcLongP =
    stringP


accountsLongP :: P.Parser T.Text
accountsLongP =
    stringP


chartL5CodeP :: P.Parser T.Text
chartL5CodeP =
    stringP


chartL5LongP :: P.Parser T.Text
chartL5LongP =
    stringP


economicBudgetCodeP :: P.Parser T.Text
economicBudgetCodeP =
    stringP


economicRingfenceCodeP :: P.Parser T.Text
economicRingfenceCodeP =
    stringP


economicGroupCodeP :: P.Parser T.Text
economicGroupCodeP =
    stringP


economicGroupLongP :: P.Parser T.Text
economicGroupLongP =
    stringP


economicCatCodeP :: P.Parser T.Text
economicCatCodeP =
    stringP


economicCatLongP :: P.Parser T.Text
economicCatLongP =
    stringP


sectorCodeP :: P.Parser T.Text
sectorCodeP =
    stringP


sectorLongP :: P.Parser T.Text
sectorLongP =
    stringP


tesCodeP :: P.Parser T.Text
tesCodeP =
    stringP


tesLongP :: P.Parser T.Text
tesLongP =
    stringP


esaCodeP :: P.Parser T.Text
esaCodeP =
    stringP


esaLongP :: P.Parser T.Text
esaLongP =
    stringP


esaGroupCodeP :: P.Parser T.Text
esaGroupCodeP =
    stringP


esaGroupLongP :: P.Parser T.Text
esaGroupLongP =
    stringP


psatCodeP :: P.Parser T.Text
psatCodeP =
    stringP


psatLongP :: P.Parser T.Text
psatLongP =
    stringP


naAggregateCodeP :: P.Parser T.Text
naAggregateCodeP =
    stringP


naAggregateLongP :: P.Parser T.Text
naAggregateLongP =
    stringP


estimatesCatCodeP :: P.Parser T.Text
estimatesCatCodeP =
    stringP


estimatesSubCatCodeP :: P.Parser T.Text
estimatesSubCatCodeP =
    stringP


estimatesColCodeP :: P.Parser T.Text
estimatesColCodeP =
    stringP


pesaEconomicBudgetCodeP :: P.Parser T.Text
pesaEconomicBudgetCodeP =
    stringP


pesaEconomicGroupCodeP :: P.Parser T.Text
pesaEconomicGroupCodeP =
    stringP


incomeCatShortP :: P.Parser T.Text
incomeCatShortP =
    stringP


usageCodeP :: P.Parser T.Text
usageCodeP =
    stringP


statusCodeP :: P.Parser T.Text
statusCodeP =
    stringP


typeCodeP :: P.Parser T.Text
typeCodeP =
    stringP


typeLongP :: P.Parser T.Text
typeLongP =
    stringP


typeGroupCodeP :: P.Parser T.Text
typeGroupCodeP =
    stringP


typeGroupLongP :: P.Parser T.Text
typeGroupLongP =
    stringP


versionCodeP :: P.Parser T.Text
versionCodeP =
    stringP


fctLoadTypeCodeP :: P.Parser T.Text
fctLoadTypeCodeP =
    stringP


fctLoadTypeLongP :: P.Parser T.Text
fctLoadTypeLongP =
    stringP


rowDescriptionP :: P.Parser T.Text
rowDescriptionP =
    stringP


dataIdP :: P.Parser T.Text
dataIdP =
    stringP


amountP :: P.Parser T.Text
amountP =
    stringP


datetimeP :: P.Parser T.Text
datetimeP =
    stringP


stringP :: P.Parser T.Text
stringP =
    do
    _ <- P.char '"'
    str <- P.takeWhile (/= '"')
    _ <- P.char '"'
    return str
