{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import qualified Data.ByteString.Lazy as Bl
import qualified Data.Csv.Streaming as Csv
import qualified Data.Set as Set
import qualified Data.IntSet as Iset
import Data.Csv (FromRecord, defaultDecodeOptions, decDelimiter)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Char (ord)
import Control.DeepSeq (deepseq, NFData)


instance NFData Accum


main :: IO ()
main =
    do
    csvData <- Bl.readFile "../data.csv"
    decoder initAccum $ Csv.decodeWith options Csv.HasHeader csvData


options =
    defaultDecodeOptions { decDelimiter = fromIntegral (ord '|') }


initAccum =
    Accum
    { year_noS = Set.empty
    , year_short_nameS = Set.empty
    , quarter_short_nameS = Set.empty
    , month_short_nameS = Set.empty
    , budgeting_organisations_codeS = Set.empty
    , department_group_codeS = Set.empty
    , department_group_long_nameS = Set.empty
    , organisation_codeS = Set.empty
    , organisation_long_nameS = Set.empty
    , organisation_type_codeS = Set.empty
    , organisation_type_long_nameS = Set.empty
    , organisation_type_l1_codeS = Set.empty
    , organisation_type_l1_long_nameS = Set.empty
    , pesa_group_codeS = Set.empty
    , pesa_group_long_nameS = Set.empty
    , sr_group_codeS = Set.empty
    , sr_group_long_nameS = Set.empty
    , bx_validation_group_codeS = Set.empty
    , bx_validation_group_long_nameS = Set.empty
    , fo_validation_group_codeS = Set.empty
    , fo_validation_group_long_nameS = Set.empty
    , accounting_arrangements_codeS = Set.empty
    , accounting_arrangements_long_nameS = Set.empty
    , counter_organisation_codeS = Set.empty
    , counter_organisation_long_nameS = Set.empty
    , segment_l4_codeS = Set.empty
    , segment_l4_long_nameS = Set.empty
    , cofog_l0_codeS = Set.empty
    , cofog_l0_long_nameS = Set.empty
    , cofog_l1_codeS = Set.empty
    , cofog_l1_long_nameS = Set.empty
    , cofog_l2_codeS = Set.empty
    , cofog_l2_long_nameS = Set.empty
    , control_budget_l0_long_nameS = Set.empty
    , control_budget_l1_long_nameS = Set.empty
    , coverage_long_nameS = Set.empty
    , estimates_row_codeS = Set.empty
    , estimates_row_long_nameS = Set.empty
    , net_subhead_long_nameS = Set.empty
    , pesa11_codeS = Set.empty
    , pesa_la_grants_codeS = Set.empty
    , pesa_lg_codeS = Set.empty
    , pesa_services_long_nameS = Set.empty
    , pesa_regional_codeS = Set.empty
    , policy_ringfence_codeS = Set.empty
    , accounting_authority_l0_codeS = Set.empty
    , accounting_authority_l1_long_nameS = Set.empty
    , sub_function_codeS = Set.empty
    , sub_function_long_nameS = Set.empty
    , function_codeS = Set.empty
    , function_long_nameS = Set.empty
    , accounts_long_nameS = Set.empty
    , chart_of_accounts_l5_codeS = Iset.empty
    , chart_of_accounts_l5_long_nameS = Set.empty
    , economic_budget_codeS = Set.empty
    , economic_ringfence_codeS = Set.empty
    , economic_group_codeS = Set.empty
    , economic_group_long_nameS = Set.empty
    , economic_category_codeS = Set.empty
    , economic_category_long_nameS = Set.empty
    , sector_codeS = Set.empty
    , sector_long_nameS = Set.empty
    , tes_codeS = Set.empty
    , tes_long_nameS = Set.empty
    , esa_codeS = Set.empty
    , esa_long_nameS = Set.empty
    , esa_group_codeS = Set.empty
    , esa_group_long_nameS = Set.empty
    , psat_codeS = Set.empty
    , psat_long_nameS = Set.empty
    , na_aggregate_codeS = Set.empty
    , na_aggregate_long_nameS = Set.empty
    , estimates_category_codeS = Set.empty
    , estimates_sub_category_codeS = Set.empty
    , estimates_column_codeS = Set.empty
    , pesa_economic_budget_codeS = Set.empty
    , pesa_economic_group_codeS = Set.empty
    , income_category_short_nameS = Set.empty
    , usage_codeS = Set.empty
    , status_codeS = Set.empty
    , type_codeS = Set.empty
    , type_long_nameS = Set.empty
    , type_group_codeS = Set.empty
    , type_group_long_nameS = Set.empty
    , version_codeS = Set.empty
    , fct_load_type_codeS = Set.empty
    , fct_load_type_long_nameS = Set.empty
    , row_descriptionS = Set.empty
    , data_idS = Iset.empty
    , amountS = Iset.empty
    , data_effective_datetimeS = Set.empty
    }


data Accum
    = Accum
    { year_noS :: !(Set.Set Text)
    , year_short_nameS :: !(Set.Set Text)
    , quarter_short_nameS :: !(Set.Set Text)
    , month_short_nameS :: !(Set.Set Text)
    , budgeting_organisations_codeS :: !(Set.Set Text)
    , department_group_codeS :: !(Set.Set Text)
    , department_group_long_nameS :: !(Set.Set Text)
    , organisation_codeS :: !(Set.Set Text)
    , organisation_long_nameS :: !(Set.Set Text)
    , organisation_type_codeS :: !(Set.Set Text)
    , organisation_type_long_nameS :: !(Set.Set Text)
    , organisation_type_l1_codeS :: !(Set.Set Text)
    , organisation_type_l1_long_nameS :: !(Set.Set Text)
    , pesa_group_codeS :: !(Set.Set Text)
    , pesa_group_long_nameS :: !(Set.Set Text)
    , sr_group_codeS :: !(Set.Set Text)
    , sr_group_long_nameS :: !(Set.Set Text)
    , bx_validation_group_codeS :: !(Set.Set Text)
    , bx_validation_group_long_nameS :: !(Set.Set Text)
    , fo_validation_group_codeS :: !(Set.Set Text)
    , fo_validation_group_long_nameS :: !(Set.Set Text)
    , accounting_arrangements_codeS :: !(Set.Set Text)
    , accounting_arrangements_long_nameS :: !(Set.Set Text)
    , counter_organisation_codeS :: !(Set.Set Text)
    , counter_organisation_long_nameS :: !(Set.Set Text)
    , segment_l4_codeS :: !(Set.Set Text)
    , segment_l4_long_nameS :: !(Set.Set Text)
    , cofog_l0_codeS :: !(Set.Set Text)
    , cofog_l0_long_nameS :: !(Set.Set Text)
    , cofog_l1_codeS :: !(Set.Set Text)
    , cofog_l1_long_nameS :: !(Set.Set Text)
    , cofog_l2_codeS :: !(Set.Set Text)
    , cofog_l2_long_nameS :: !(Set.Set Text)
    , control_budget_l0_long_nameS :: !(Set.Set Text)
    , control_budget_l1_long_nameS :: !(Set.Set Text)
    , coverage_long_nameS :: !(Set.Set Text)
    , estimates_row_codeS :: !(Set.Set Text)
    , estimates_row_long_nameS :: !(Set.Set Text)
    , net_subhead_long_nameS :: !(Set.Set Text)
    , pesa11_codeS :: !(Set.Set Text)
    , pesa_la_grants_codeS :: !(Set.Set Text)
    , pesa_lg_codeS :: !(Set.Set Text)
    , pesa_services_long_nameS :: !(Set.Set Text)
    , pesa_regional_codeS :: !(Set.Set Text)
    , policy_ringfence_codeS :: !(Set.Set Text)
    , accounting_authority_l0_codeS :: !(Set.Set Text)
    , accounting_authority_l1_long_nameS :: !(Set.Set Text)
    , sub_function_codeS :: !(Set.Set Text)
    , sub_function_long_nameS :: !(Set.Set Text)
    , function_codeS :: !(Set.Set Text)
    , function_long_nameS :: !(Set.Set Text)
    , accounts_long_nameS :: !(Set.Set Text)
    , chart_of_accounts_l5_codeS :: !Iset.IntSet
    , chart_of_accounts_l5_long_nameS :: !(Set.Set Text)
    , economic_budget_codeS :: !(Set.Set Text)
    , economic_ringfence_codeS :: !(Set.Set Text)
    , economic_group_codeS :: !(Set.Set Text)
    , economic_group_long_nameS :: !(Set.Set Text)
    , economic_category_codeS :: !(Set.Set Text)
    , economic_category_long_nameS :: !(Set.Set Text)
    , sector_codeS :: !(Set.Set Text)
    , sector_long_nameS :: !(Set.Set Text)
    , tes_codeS :: !(Set.Set Text)
    , tes_long_nameS :: !(Set.Set Text)
    , esa_codeS :: !(Set.Set Text)
    , esa_long_nameS :: !(Set.Set Text)
    , esa_group_codeS :: !(Set.Set Text)
    , esa_group_long_nameS :: !(Set.Set Text)
    , psat_codeS :: !(Set.Set Text)
    , psat_long_nameS :: !(Set.Set Text)
    , na_aggregate_codeS :: !(Set.Set Text)
    , na_aggregate_long_nameS :: !(Set.Set Text)
    , estimates_category_codeS :: !(Set.Set Text)
    , estimates_sub_category_codeS :: !(Set.Set Text)
    , estimates_column_codeS :: !(Set.Set Text)
    , pesa_economic_budget_codeS :: !(Set.Set Text)
    , pesa_economic_group_codeS :: !(Set.Set Text)
    , income_category_short_nameS :: !(Set.Set Text)
    , usage_codeS :: !(Set.Set Text)
    , status_codeS :: !(Set.Set Text)
    , type_codeS :: !(Set.Set Text)
    , type_long_nameS :: !(Set.Set Text)
    , type_group_codeS :: !(Set.Set Text)
    , type_group_long_nameS :: !(Set.Set Text)
    , version_codeS :: !(Set.Set Text)
    , fct_load_type_codeS :: !(Set.Set Text)
    , fct_load_type_long_nameS :: !(Set.Set Text)
    , row_descriptionS :: !(Set.Set Text)
    , data_idS :: !Iset.IntSet
    , amountS :: !Iset.IntSet
    , data_effective_datetimeS :: !(Set.Set Text)
    }
    deriving (Show, Generic)


decoder :: Accum -> Csv.Records Row -> IO ()
decoder accum records =
    case records of
        Csv.Cons (Left err) _ ->
            putStrLn $ "Cons error: " ++ err

        Csv.Cons (Right row) moreRecords ->
            let
            newAccum = update accum row
            in
            decoder newAccum $ deepseq newAccum moreRecords

        Csv.Nil Nothing _ ->
            output accum

        Csv.Nil (Just err) remainder ->
            putStrLn $ "Nil error: " ++ err ++ ": " ++ (show $ Bl.take 1000 remainder)


update :: Accum -> Row -> Accum
update accum row =
    Accum
    { year_noS =
        Set.insert
            (year_no row)
            (year_noS accum)
    , year_short_nameS =
        Set.insert
            (year_short_name row)
            (year_short_nameS accum)
    , quarter_short_nameS =
        Set.insert
            (quarter_short_name row)
            (quarter_short_nameS accum)
    , month_short_nameS =
        Set.insert
            (month_short_name row)
            (month_short_nameS accum)
    , budgeting_organisations_codeS =
        Set.insert
            (budgeting_organisations_code row)
            (budgeting_organisations_codeS accum)
    , department_group_codeS =
        Set.insert
            (department_group_code row)
            (department_group_codeS accum)
    , department_group_long_nameS =
        Set.insert
            (department_group_long_name row)
            (department_group_long_nameS accum)
    , organisation_codeS =
        Set.insert
            (organisation_code row)
            (organisation_codeS accum)
    , organisation_long_nameS =
        Set.insert
            (organisation_long_name row)
            (organisation_long_nameS accum)
    , organisation_type_codeS =
        Set.insert
            (organisation_type_code row)
            (organisation_type_codeS accum)
    , organisation_type_long_nameS =
        Set.insert
            (organisation_type_long_name row)
            (organisation_type_long_nameS accum)
    , organisation_type_l1_codeS =
        Set.insert
            (organisation_type_l1_code row)
            (organisation_type_l1_codeS accum)
    , organisation_type_l1_long_nameS =
        Set.insert
            (organisation_type_l1_long_name row)
            (organisation_type_l1_long_nameS accum)
    , pesa_group_codeS =
        Set.insert
            (pesa_group_code row)
            (pesa_group_codeS accum)
    , pesa_group_long_nameS =
        Set.insert
            (pesa_group_long_name row)
            (pesa_group_long_nameS accum)
    , sr_group_codeS =
        Set.insert
            (sr_group_code row)
            (sr_group_codeS accum)
    , sr_group_long_nameS =
        Set.insert
            (sr_group_long_name row)
            (sr_group_long_nameS accum)
    , bx_validation_group_codeS =
        Set.insert
            (bx_validation_group_code row)
            (bx_validation_group_codeS accum)
    , bx_validation_group_long_nameS =
        Set.insert
            (bx_validation_group_long_name row)
            (bx_validation_group_long_nameS accum)
    , fo_validation_group_codeS =
        Set.insert
            (fo_validation_group_code row)
            (fo_validation_group_codeS accum)
    , fo_validation_group_long_nameS =
        Set.insert
            (fo_validation_group_long_name row)
            (fo_validation_group_long_nameS accum)
    , accounting_arrangements_codeS =
        Set.insert
            (accounting_arrangements_code row)
            (accounting_arrangements_codeS accum)
    , accounting_arrangements_long_nameS =
        Set.insert
            (accounting_arrangements_long_name row)
            (accounting_arrangements_long_nameS accum)
    , counter_organisation_codeS =
        Set.insert
            (counter_organisation_code row)
            (counter_organisation_codeS accum)
    , counter_organisation_long_nameS =
        Set.insert
            (counter_organisation_long_name row)
            (counter_organisation_long_nameS accum)
    , segment_l4_codeS =
        Set.insert
            (segment_l4_code row)
            (segment_l4_codeS accum)
    , segment_l4_long_nameS =
        Set.insert
            (segment_l4_long_name row)
            (segment_l4_long_nameS accum)
    , cofog_l0_codeS =
        Set.insert
            (cofog_l0_code row)
            (cofog_l0_codeS accum)
    , cofog_l0_long_nameS =
        Set.insert
            (cofog_l0_long_name row)
            (cofog_l0_long_nameS accum)
    , cofog_l1_codeS =
        Set.insert
            (cofog_l1_code row)
            (cofog_l1_codeS accum)
    , cofog_l1_long_nameS =
        Set.insert
            (cofog_l1_long_name row)
            (cofog_l1_long_nameS accum)
    , cofog_l2_codeS =
        Set.insert
            (cofog_l2_code row)
            (cofog_l2_codeS accum)
    , cofog_l2_long_nameS =
        Set.insert
            (cofog_l2_long_name row)
            (cofog_l2_long_nameS accum)
    , control_budget_l0_long_nameS =
        Set.insert
            (control_budget_l0_long_name row)
            (control_budget_l0_long_nameS accum)
    , control_budget_l1_long_nameS =
        Set.insert
            (control_budget_l1_long_name row)
            (control_budget_l1_long_nameS accum)
    , coverage_long_nameS =
        Set.insert
            (coverage_long_name row)
            (coverage_long_nameS accum)
    , estimates_row_codeS =
        Set.insert
            (estimates_row_code row)
            (estimates_row_codeS accum)
    , estimates_row_long_nameS =
        Set.insert
            (estimates_row_long_name row)
            (estimates_row_long_nameS accum)
    , net_subhead_long_nameS =
        Set.insert
            (net_subhead_long_name row)
            (net_subhead_long_nameS accum)
    , pesa11_codeS =
        Set.insert
            (pesa11_code row)
            (pesa11_codeS accum)
    , pesa_la_grants_codeS =
        Set.insert
            (pesa_la_grants_code row)
            (pesa_la_grants_codeS accum)
    , pesa_lg_codeS =
        Set.insert
            (pesa_lg_code row)
            (pesa_lg_codeS accum)
    , pesa_services_long_nameS =
        Set.insert
            (pesa_services_long_name row)
            (pesa_services_long_nameS accum)
    , pesa_regional_codeS =
        Set.insert
            (pesa_regional_code row)
            (pesa_regional_codeS accum)
    , policy_ringfence_codeS =
        Set.insert
            (policy_ringfence_code row)
            (policy_ringfence_codeS accum)
    , accounting_authority_l0_codeS =
        Set.insert
            (accounting_authority_l0_code row)
            (accounting_authority_l0_codeS accum)
    , accounting_authority_l1_long_nameS =
        Set.insert
            (accounting_authority_l1_long_name row)
            (accounting_authority_l1_long_nameS accum)
    , sub_function_codeS =
        Set.insert
            (sub_function_code row)
            (sub_function_codeS accum)
    , sub_function_long_nameS =
        Set.insert
            (sub_function_long_name row)
            (sub_function_long_nameS accum)
    , function_codeS =
        Set.insert
            (function_code row)
            (function_codeS accum)
    , function_long_nameS =
        Set.insert
            (function_long_name row)
            (function_long_nameS accum)
    , accounts_long_nameS =
        Set.insert
            (accounts_long_name row)
            (accounts_long_nameS accum)
    , chart_of_accounts_l5_codeS =
        Iset.insert
            (chart_of_accounts_l5_code row)
            (chart_of_accounts_l5_codeS accum)
    , chart_of_accounts_l5_long_nameS =
        Set.insert
            (chart_of_accounts_l5_long_name row)
            (chart_of_accounts_l5_long_nameS accum)
    , economic_budget_codeS =
        Set.insert
            (economic_budget_code row)
            (economic_budget_codeS accum)
    , economic_ringfence_codeS =
        Set.insert
            (economic_ringfence_code row)
            (economic_ringfence_codeS accum)
    , economic_group_codeS =
        Set.insert
            (economic_group_code row)
            (economic_group_codeS accum)
    , economic_group_long_nameS =
        Set.insert
            (economic_group_long_name row)
            (economic_group_long_nameS accum)
    , economic_category_codeS =
        Set.insert
            (economic_category_code row)
            (economic_category_codeS accum)
    , economic_category_long_nameS =
        Set.insert
            (economic_category_long_name row)
            (economic_category_long_nameS accum)
    , sector_codeS =
        Set.insert
            (sector_code row)
            (sector_codeS accum)
    , sector_long_nameS =
        Set.insert
            (sector_long_name row)
            (sector_long_nameS accum)
    , tes_codeS =
        Set.insert
            (tes_code row)
            (tes_codeS accum)
    , tes_long_nameS =
        Set.insert
            (tes_long_name row)
            (tes_long_nameS accum)
    , esa_codeS =
        Set.insert
            (esa_code row)
            (esa_codeS accum)
    , esa_long_nameS =
        Set.insert
            (esa_long_name row)
            (esa_long_nameS accum)
    , esa_group_codeS =
        Set.insert
            (esa_group_code row)
            (esa_group_codeS accum)
    , esa_group_long_nameS =
        Set.insert
            (esa_group_long_name row)
            (esa_group_long_nameS accum)
    , psat_codeS =
        Set.insert
            (psat_code row)
            (psat_codeS accum)
    , psat_long_nameS =
        Set.insert
            (psat_long_name row)
            (psat_long_nameS accum)
    , na_aggregate_codeS =
        Set.insert
            (na_aggregate_code row)
            (na_aggregate_codeS accum)
    , na_aggregate_long_nameS =
        Set.insert
            (na_aggregate_long_name row)
            (na_aggregate_long_nameS accum)
    , estimates_category_codeS =
        Set.insert
            (estimates_category_code row)
            (estimates_category_codeS accum)
    , estimates_sub_category_codeS =
        Set.insert
            (estimates_sub_category_code row)
            (estimates_sub_category_codeS accum)
    , estimates_column_codeS =
        Set.insert
            (estimates_column_code row)
            (estimates_column_codeS accum)
    , pesa_economic_budget_codeS =
        Set.insert
            (pesa_economic_budget_code row)
            (pesa_economic_budget_codeS accum)
    , pesa_economic_group_codeS =
        Set.insert
            (pesa_economic_group_code row)
            (pesa_economic_group_codeS accum)
    , income_category_short_nameS =
        Set.insert
            (income_category_short_name row)
            (income_category_short_nameS accum)
    , usage_codeS =
        Set.insert
            (usage_code row)
            (usage_codeS accum)
    , status_codeS =
        Set.insert
            (status_code row)
            (status_codeS accum)
    , type_codeS =
        Set.insert
            (type_code row)
            (type_codeS accum)
    , type_long_nameS =
        Set.insert
            (type_long_name row)
            (type_long_nameS accum)
    , type_group_codeS =
        Set.insert
            (type_group_code row)
            (type_group_codeS accum)
    , type_group_long_nameS =
        Set.insert
            (type_group_long_name row)
            (type_group_long_nameS accum)
    , version_codeS =
        Set.insert
            (version_code row)
            (version_codeS accum)
    , fct_load_type_codeS =
        Set.insert
            (fct_load_type_code row)
            (fct_load_type_codeS accum)
    , fct_load_type_long_nameS =
        Set.insert
            (fct_load_type_long_name row)
            (fct_load_type_long_nameS accum)
    , row_descriptionS =
        Set.insert
            (row_description row)
            (row_descriptionS accum)
    , data_idS =
        Iset.insert
            (data_id row)
            (data_idS accum)
    , amountS =
        Iset.insert
            (truncate $ amount row)
            (amountS accum)
    , data_effective_datetimeS =
        Set.insert
            (data_effective_datetime row)
            (data_effective_datetimeS accum)
    }


output :: Accum -> IO ()
output accum =
    print accum


data Row
    = Row
    { year_no :: !Text
    , year_short_name :: !Text
    , quarter_short_name :: !Text
    , month_short_name :: !Text
    , budgeting_organisations_code :: !Text
    , department_group_code :: !Text
    , department_group_long_name :: !Text
    , organisation_code :: !Text
    , organisation_long_name :: !Text
    , organisation_type_code :: !Text
    , organisation_type_long_name :: !Text
    , organisation_type_l1_code :: !Text
    , organisation_type_l1_long_name :: !Text
    , pesa_group_code :: !Text
    , pesa_group_long_name :: !Text
    , sr_group_code :: !Text
    , sr_group_long_name :: !Text
    , bx_validation_group_code :: !Text
    , bx_validation_group_long_name :: !Text
    , fo_validation_group_code :: !Text
    , fo_validation_group_long_name :: !Text
    , accounting_arrangements_code :: !Text
    , accounting_arrangements_long_name :: !Text
    , counter_organisation_code :: !Text
    , counter_organisation_long_name :: !Text
    , segment_l4_code :: !Text
    , segment_l4_long_name :: !Text
    , cofog_l0_code :: !Text
    , cofog_l0_long_name :: !Text
    , cofog_l1_code :: !Text
    , cofog_l1_long_name :: !Text
    , cofog_l2_code :: !Text
    , cofog_l2_long_name :: !Text
    , control_budget_l0_long_name :: !Text
    , control_budget_l1_long_name :: !Text
    , coverage_long_name :: !Text
    , estimates_row_code :: !Text
    , estimates_row_long_name :: !Text
    , net_subhead_long_name :: !Text
    , pesa11_code :: !Text
    , pesa_la_grants_code :: !Text
    , pesa_lg_code :: !Text
    , pesa_services_long_name :: !Text
    , pesa_regional_code :: !Text
    , policy_ringfence_code :: !Text
    , accounting_authority_l0_code :: !Text
    , accounting_authority_l1_long_name :: !Text
    , sub_function_code :: !Text
    , sub_function_long_name :: !Text
    , function_code :: !Text
    , function_long_name :: !Text
    , accounts_long_name :: !Text
    , chart_of_accounts_l5_code :: !Int
    , chart_of_accounts_l5_long_name :: !Text
    , economic_budget_code :: !Text
    , economic_ringfence_code :: !Text
    , economic_group_code :: !Text
    , economic_group_long_name :: !Text
    , economic_category_code :: !Text
    , economic_category_long_name :: !Text
    , sector_code :: !Text
    , sector_long_name :: !Text
    , tes_code :: !Text
    , tes_long_name :: !Text
    , esa_code :: !Text
    , esa_long_name :: !Text
    , esa_group_code :: !Text
    , esa_group_long_name :: !Text
    , psat_code :: !Text
    , psat_long_name :: !Text
    , na_aggregate_code :: !Text
    , na_aggregate_long_name :: !Text
    , estimates_category_code :: !Text
    , estimates_sub_category_code :: !Text
    , estimates_column_code :: !Text
    , pesa_economic_budget_code :: !Text
    , pesa_economic_group_code :: !Text
    , income_category_short_name :: !Text
    , usage_code :: !Text
    , status_code :: !Text
    , type_code :: !Text
    , type_long_name :: !Text
    , type_group_code :: !Text
    , type_group_long_name :: !Text
    , version_code :: !Text
    , fct_load_type_code :: !Text
    , fct_load_type_long_name :: !Text
    , row_description :: !Text
    , data_id :: !Int
    , amount :: !Double
    , data_effective_datetime :: !Text
    }
    deriving (Generic)

instance FromRecord Row
