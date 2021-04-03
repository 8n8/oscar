{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.IO (Handle, withFile, IOMode (ReadMode))
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as B
import qualified Data.Set as Set
import qualified Control.Exception as E


data State
    = State
    { budgetOrgS :: !(Set.Set B.ByteString)
    , lineNum :: !Int
    , numBad :: !Int
    , badS :: Maybe String
    }
    deriving Show


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
    case badS state of
    Just err ->
        putStrLn err

    Nothing ->
        do
        eitherRawRow <- E.try $ B.hGetLine handle
        case (eitherRawRow :: Either IOError B.ByteString) of
            Left err ->
                print err

            Right rawRow ->
                mainHelp (parse state rawRow) handle


parse :: State -> B.ByteString -> State
parse state raw =
    



takeEnd n xs = B.drop (B.length xs -n) xs


updateState :: State -> Row -> State
updateState state row =
    state
        { budgetOrgS =
            if lineNum state == 0 then
            budgetOrgS state
            else
            Set.insert (budgetOrgR row) (budgetOrgS state)
        , lineNum = lineNum state + 1
        }


data Row
    = Row
    { budgetOrgR :: !B.ByteString
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
    _ <- P.word8 124 -- '|'
    return ()


yearNoP :: P.Parser B.ByteString
yearNoP =
    stringP


yearShortNameP :: P.Parser B.ByteString
yearShortNameP =
    stringP


quarterShortNameP :: P.Parser B.ByteString
quarterShortNameP =
    stringP


monthShortNameP :: P.Parser B.ByteString
monthShortNameP =
    stringP


budgetOrgP :: P.Parser B.ByteString
budgetOrgP =
    stringP


depGroupCodeP :: P.Parser B.ByteString
depGroupCodeP =
    stringP


depGroupLongP :: P.Parser B.ByteString
depGroupLongP =
    stringP


orgCodeP :: P.Parser B.ByteString
orgCodeP =
    stringP


orgLongP :: P.Parser B.ByteString
orgLongP =
    stringP


orgTypeP :: P.Parser B.ByteString
orgTypeP =
    stringP


orgTypeLongP :: P.Parser B.ByteString
orgTypeLongP =
    stringP


orgTypeCodeP :: P.Parser B.ByteString
orgTypeCodeP =
    stringP


pesaGroupP :: P.Parser B.ByteString
pesaGroupP =
    stringP


pesaGroupLongP :: P.Parser B.ByteString
pesaGroupLongP =
    stringP


srGroupP :: P.Parser B.ByteString
srGroupP =
    stringP


srGroupLongP :: P.Parser B.ByteString
srGroupLongP =
    stringP


bxValidCodeP :: P.Parser B.ByteString
bxValidCodeP =
    stringP


bxValidGroupLongP :: P.Parser B.ByteString
bxValidGroupLongP =
    stringP


foValidCodeP :: P.Parser B.ByteString
foValidCodeP =
    stringP


foValidLongP :: P.Parser B.ByteString
foValidLongP =
    stringP


accountArrangeCodeP :: P.Parser B.ByteString
accountArrangeCodeP =
    stringP


accountArrangeLongP :: P.Parser B.ByteString
accountArrangeLongP =
    stringP


counterCodeP :: P.Parser B.ByteString
counterCodeP =
    stringP


counterLongP :: P.Parser B.ByteString
counterLongP =
    stringP


segCodeP :: P.Parser B.ByteString
segCodeP =
    stringP


segLongP :: P.Parser B.ByteString
segLongP =
    stringP


cofog0CodeP :: P.Parser B.ByteString
cofog0CodeP =
    stringP


cofog0LongP :: P.Parser B.ByteString
cofog0LongP =
    stringP


cofog1CodeP :: P.Parser B.ByteString
cofog1CodeP =
    stringP


cofog1LongP :: P.Parser B.ByteString
cofog1LongP =
    stringP


cofog2CodeP :: P.Parser B.ByteString
cofog2CodeP =
    stringP


cofog2LongP :: P.Parser B.ByteString
cofog2LongP =
    stringP


control0LongP :: P.Parser B.ByteString
control0LongP =
    stringP


control1LongP :: P.Parser B.ByteString
control1LongP =
    stringP


coverageLongP :: P.Parser B.ByteString
coverageLongP =
    stringP


estimatesCodeP :: P.Parser B.ByteString
estimatesCodeP =
    stringP


estimatesLongP :: P.Parser B.ByteString
estimatesLongP =
    stringP


netSubheadLongP :: P.Parser B.ByteString
netSubheadLongP =
    stringP


pesa11CodeP :: P.Parser B.ByteString
pesa11CodeP =
    stringP


pesaGrantsCodeP :: P.Parser B.ByteString
pesaGrantsCodeP =
    stringP


pesaLgCodeP :: P.Parser B.ByteString
pesaLgCodeP =
    stringP


pesaServicesLongP :: P.Parser B.ByteString
pesaServicesLongP =
    stringP


pesaRegionalCodeP :: P.Parser B.ByteString
pesaRegionalCodeP =
    stringP


policyRingfenceP :: P.Parser B.ByteString
policyRingfenceP =
    stringP


accountAuthL0CodeP :: P.Parser B.ByteString
accountAuthL0CodeP =
    stringP


accountAuthL1LongP :: P.Parser B.ByteString
accountAuthL1LongP =
    stringP


subFuncCodeP :: P.Parser B.ByteString
subFuncCodeP =
    stringP


subFuncLongP :: P.Parser B.ByteString
subFuncLongP =
    stringP


funcCodeP :: P.Parser B.ByteString
funcCodeP =
    stringP


funcLongP :: P.Parser B.ByteString
funcLongP =
    stringP


accountsLongP :: P.Parser B.ByteString
accountsLongP =
    stringP


chartL5CodeP :: P.Parser B.ByteString
chartL5CodeP =
    stringP


chartL5LongP :: P.Parser B.ByteString
chartL5LongP =
    stringP


economicBudgetCodeP :: P.Parser B.ByteString
economicBudgetCodeP =
    stringP


economicRingfenceCodeP :: P.Parser B.ByteString
economicRingfenceCodeP =
    stringP


economicGroupCodeP :: P.Parser B.ByteString
economicGroupCodeP =
    stringP


economicGroupLongP :: P.Parser B.ByteString
economicGroupLongP =
    stringP


economicCatCodeP :: P.Parser B.ByteString
economicCatCodeP =
    stringP


economicCatLongP :: P.Parser B.ByteString
economicCatLongP =
    stringP


sectorCodeP :: P.Parser B.ByteString
sectorCodeP =
    stringP


sectorLongP :: P.Parser B.ByteString
sectorLongP =
    stringP


tesCodeP :: P.Parser B.ByteString
tesCodeP =
    stringP


tesLongP :: P.Parser B.ByteString
tesLongP =
    stringP


esaCodeP :: P.Parser B.ByteString
esaCodeP =
    stringP


esaLongP :: P.Parser B.ByteString
esaLongP =
    stringP


esaGroupCodeP :: P.Parser B.ByteString
esaGroupCodeP =
    stringP


esaGroupLongP :: P.Parser B.ByteString
esaGroupLongP =
    stringP


psatCodeP :: P.Parser B.ByteString
psatCodeP =
    stringP


psatLongP :: P.Parser B.ByteString
psatLongP =
    stringP


naAggregateCodeP :: P.Parser B.ByteString
naAggregateCodeP =
    stringP


naAggregateLongP :: P.Parser B.ByteString
naAggregateLongP =
    stringP


estimatesCatCodeP :: P.Parser B.ByteString
estimatesCatCodeP =
    stringP


estimatesSubCatCodeP :: P.Parser B.ByteString
estimatesSubCatCodeP =
    stringP


estimatesColCodeP :: P.Parser B.ByteString
estimatesColCodeP =
    stringP


pesaEconomicBudgetCodeP :: P.Parser B.ByteString
pesaEconomicBudgetCodeP =
    stringP


pesaEconomicGroupCodeP :: P.Parser B.ByteString
pesaEconomicGroupCodeP =
    stringP


incomeCatShortP :: P.Parser B.ByteString
incomeCatShortP =
    stringP


usageCodeP :: P.Parser B.ByteString
usageCodeP =
    stringP


statusCodeP :: P.Parser B.ByteString
statusCodeP =
    stringP


typeCodeP :: P.Parser B.ByteString
typeCodeP =
    stringP


typeLongP :: P.Parser B.ByteString
typeLongP =
    stringP


typeGroupCodeP :: P.Parser B.ByteString
typeGroupCodeP =
    stringP


typeGroupLongP :: P.Parser B.ByteString
typeGroupLongP =
    stringP


versionCodeP :: P.Parser B.ByteString
versionCodeP =
    stringP


fctLoadTypeCodeP :: P.Parser B.ByteString
fctLoadTypeCodeP =
    stringP


fctLoadTypeLongP :: P.Parser B.ByteString
fctLoadTypeLongP =
    stringP


rowDescriptionP :: P.Parser B.ByteString
rowDescriptionP =
    stringP


dataIdP :: P.Parser B.ByteString
dataIdP =
    stringP


amountP :: P.Parser B.ByteString
amountP =
    stringP


datetimeP :: P.Parser B.ByteString
datetimeP =
    stringP


stringP :: P.Parser B.ByteString
stringP =
    do
    _ <- P.word8 34 -- '"'
    str <- P.takeWhile (/= 34) -- '"')
    _ <- P.word8 34 -- '"'
    return str
