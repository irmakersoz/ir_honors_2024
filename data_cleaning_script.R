### Stanford University ###
### IR Honors Thesis ###
##### 2023-2024 #####
#### Irmak Ersoz #### 

# Data Cleaning Script:
# Use this script to replicate the dataset used in the thesis. 
# Original dataset: Dan Honig, PPD 2.0, April 2022. 

# Load the PPD
# (Source: https://img1.wsimg.com/blobby/go/79440b61-e70b-4220-b67f-e5edb0157a24/downloads/PPD%202.1%20April%201%202022.zip?ver=1695027061680)
library(readr)
ppd_april <- read_csv("Desktop/ppd_april.csv")


# Remove aiddata columns 
# These columns are included for AidData purposes and replicate organization-
# specific information not required for the purposes of this analysis. 

library(dplyr)

ppd_april <- ppd_april %>%
  select(-c("aiddata_disbursement_amount", 
            "aiddata_id", 
            "aiddata_longdescription", 
            "aiddata_purposecode",
            "aiddata_purposename",
            "aiddata_sectorcode",
            "aiddata_sectorname",
            "aiddata_shortdescription",
            "aiddata_title"))


# Remove multinational projects
# Projects that span multiple countries do not fit the requirements of analysis
# for state fragility and other good governance measures. 
ppd_april <- ppd_april[ppd_april$countryname_COW != "Multinational", ]
ppd_april <- ppd_april[!grepl("Multicountry|,| and ", ppd_april$countryname_COW), ]


# Remove column "ccode"
# This column has NAs for most part and is not significant for analysis. 
ppd_april <- ppd_april %>%
  select(-c("ccode"))

# Remove duplicate columns that all contain country names 
ppd_april <- ppd_april %>%
  select(-c("countryname_WB",
            "country_code_WB",
            "country_code_COW",
            "country_temp"))

# Remove indexing columns 
ppd_april <- ppd_april %>%
  select(-c("...1"))

# Remove duplicate columns that all contain sector information (non-org specific)
ppd_april <- ppd_april %>%
  select(-c("wb_sectorboard",
            "wb_sector_board",
            "sector_code"))

# Remove columns with NAs for six_overall_rating
# View rows with six_overall_rating as NA
na_scores <- ppd_april[which(is.na(ppd_april$six_overall_rating)), ]
View(na_scores)

# Remove those rows 
ppd_april <- ppd_april[!is.na(ppd_april$six_overall_rating), ]


# Generate start_year column using start_date
ppd_april$start_year <- year(as.Date(ppd_april$start_date, format="%Y-%m-%d"))


# Turn variables into categorical
ppd_april$countryname_COW <- as.factor(ppd_april$countryname_COW)

ppd_april$donor <- as.factor(ppd_april$donor)

ppd_april$external_evaluator <- as.factor(ppd_april$external_evaluator)

ppd_april$office_presence <- as.factor(ppd_april$office_presence)

ppd_april$purpose_code <- as.factor(ppd_april$purpose_code)

ppd_april$aid_type <- as.factor(ppd_april$aid_type)

ppd_april$region <- as.factor(ppd_april$region)

ppd_april$effect <- as.factor(ppd_april$effect)

ppd_april$legit <- as.factor(ppd_april$legit)

ppd_april$seceff <- as.factor(ppd_april$seceff)

ppd_april$secleg <- as.factor(ppd_april$secleg)

ppd_april$poleff <- as.factor(ppd_april$poleff)

ppd_april$ecoeff <- as.factor(ppd_april$ecoeff)

ppd_april$ecoleg <- as.factor(ppd_april$ecoleg)

ppd_april$soceff <- as.factor(ppd_april$soceff)

ppd_april$socleg <- as.factor(ppd_april$socleg)

ppd_april$sfi <- as.factor(ppd_april$sfi)

ppd_april$cpi <- as.factor(ppd_april$cpi)



# Turn variables into numeric

ppd_april$evalyear <- as.numeric(ppd_april$evalyear)

ppd_april$duration <- as.numeric(ppd_april$duration)

ppd_april$start_year <- as.numeric(ppd_april$start_year)

ppd_april$projectsize_original <- as.numeric(ppd_april$projectsize_original)

ppd_april$completion_year <- as.numeric(ppd_april$completion_year)

ppd_april$eval_lag <- as.numeric(ppd_april$eval_lag)

ppd_april$wb_startyear <- as.numeric(ppd_april$wb_startyear)

ppd_april$wb_completionyear <- as.numeric(ppd_april$wb_completionyear)

ppd_april$afdb_impl_staff_evalyear <- as.numeric(ppd_april$afdb_impl_staff_evalyear)

ppd_april$ifad_yearincludedinARRI <- as.numeric(ppd_april$ifad_yearincludedinARRI)



# Format dates 
ppd_april$start_date <- as.character(ppd_april$start_date)
ppd_april$start_date <- as.Date(ppd_april$start_date, "%Y-%D-%M")

ppd_april$evaluation_date <- as.character(ppd_april$evaluation_date)
ppd_april$evaluation_date <- as.Date(ppd_april$evaluation_date, "%Y-%D-%M")

ppd_april$completion_date <- as.character(ppd_april$completion_date)
ppd_april$completion_date <- as.Date(ppd_april$completion_date, "%Y-%D-%M")

ppd_april$kfw_officeopening_date <- as.Date(ppd_april$kfw_officeopening_date, "%Y-%D-%M")

ppd_april$kfw_reportcompletion_date <- as.Date(ppd_april$kfw_reportcompletion_date, "%Y-%D-%M")

ppd_april$cdb_terminal_disb_date <- as.Date(ppd_april$cdb_terminal_disb_date, "%Y-%D-%M")

ppd_april$kfw_evaluation_date <- as.Date(ppd_april$kfw_evaluation_date, "%Y-%D-%M")

ppd_april$cdb_approval_date <- as.Date(ppd_april$cdb_approval_date, "%Y-%D-%M")

ppd_april$ifad_approval_date <- as.Date(ppd_april$ifad_approval_date, "%Y-%D-%M")

ppd_april$ifad_effective_date <- as.Date(ppd_april$ifad_effective_date, "%Y-%D-%M")

ppd_april$ifad_closing_date <- as.Date(ppd_april$ifad_closing_date, "%Y-%D-%M")



ppd_april$wb_approvaldate <- as.Date(ppd_april$wb_approvaldate, "%Y-%D-%M")

ppd_april$wb_ieg_evaldate <- as.Date(ppd_april$wb_ieg_evaldate, "%Y-%D-%M")

ppd_april$gef_ProjectStart <- as.numeric(ppd_april$gef_ProjectStart)



      
# Format IDO specific columns as factors

columns_to_factor <- c("asdb_approvaldate",
                       "asdb_approvedamount",
                       "asdb_countryclassification",
                       "asdb_departmentname",
                       "asdb_funds_source_name",
                       "asdb_pcrrating",
                       "asdb_pperrating",
                       "asdb_pperyear",
                       "asdb_project_id",
                       "asdb_projecttype",
                       "asdb_pvrrating",
                       "asdb_pvryear",
                       "crs_purpose_code",
                       "crs_purpose_sector",
                       "dfid_deptofficename",
                       "dfid_divisionname",
                       "dfid_outputriskscore",
                       "dfid_overallriskscore",
                       "dfid_principalsector",
                       "dfid_projectpurpose",
                       "dfid_projectpurposescore",
                       "dfid_reviewstyle",
                       "dfid_reviewtype",
                       "dfid_sectorgroup",
                       "dfid_totalimpactscore",
                       "giz_effectiveness_rating",
                       "giz_efficiency_rating",
                       "giz_impact_rating",
                       "giz_leadexecagency_country",
                       "giz_leadexecagency_name",
                       "giz_leadexecagency_type",
                       "giz_leadimplementingorg_country",
                       "giz_leadimplementingorg_name",
                       "giz_leadimplementingorg_type",
                       "giz_relevance_rating",
                       "giz_sustainability_rating",
                       "jica_borrowerimplementername",
                       "jica_borrowerimplementertype",
                       "jica_effectiveness_rating",
                       "jica_efficiency_rating",
                       "jica_impact_rating",
                       "jica_primarycontractorcountry",
                       "jica_primarycontractortype",
                       "jica_ratingsimputed",
                       "jica_relevance_rating",
                       "jica_sustainability_rating",
                       "kfw_effectiveness_rating",         
                       "kfw_efficiency_rating",
                       "kfw_impact_rating",
                       "kfw_significance_rating",
                       "kfw_sustainability_rating",
                       "dfat_QualityReportType",
                       "dfat_EfficiencyRating",
                       "dfat_RelevanceRating",
                       "dfat_GenderEqualityRating",
                       "dfat_MERating",
                       "dfat_SustainabilityRating",
                       "dfat_ConnectednessRating",
                       "dfat_ProtectionRating",
                       "dfat_Ratedeffectiveness",
                       "dfat_Ratedefficiency",
                       "dfat_Ratedrelevance",
                       "dfat_Ratedgender",
                       "dfat_RatedME",
                       "dfat_Ratedsustainability",         
                       "dfat_InvestmentPriorityArea",
                       "country_code",
                       "Code",
                       "selfgen_code",
                       "_merge_cow_wb",
                       "_merge",
                       "cdb_StrategicRelevance",
                       "cdb_PovertyRelevance",
                       "cdb_Effectiveness",
                       "cdb_Efficiency",
                       "cdb_ThematicAreas",
                       "cdb_Sustainability",
                       "cdb_AggregateScore",
                       "cdb_BorrowerPerformance",
                       "cdb_CDBPerformance",
                       "cdb_QualityofPCR",
                       "cdb_StrategicRelevance_rating",
                       "cdb_PovertyRelevance_rating",
                       "cdb_Effectiveness_rating",
                       "cdb_Efficiency_rating",
                       "cdb_ThematicAreas_rating",
                       "cdb_Sustainability_rating",
                       "cdb_AggregateScore_rating",
                       "cdb_BorrowerPerformance_rating",
                       "cdb_CDBPerformance_rating",
                       "cdb_QualityofPCR_rating",
                       "ifad_REGION",
                       "ifad_REGIONCODE",
                       "ifad_Subsector",
                       "ifad_ProjectID",
                       "ifad_CoFinancing",
                       "ifad_CooperatingInstitution",
                       "ifad_CPEPEPPAPCRVIE",
                       "ifad_PE",
                       "ifad_rating_relevance",
                       "ifad_rating_effectiveness",
                       "ifad_rating_efficiency",
                       "ifad_rating_sustainability",
                       "ifad_rating_avg_project_perf",
                       "ifad_rating_IFAD",
                       "ifad_rating_corp_Institution",
                       "ifad_rating_government",
                       "ifad_rating_NGO_other",
                       "ifad_rating_cofinanc",
                       "ifad_rating_combined_partner",
                       "ifad_rating_physical_assets",
                       "ifad_rating_financial_assets",
                       "ifad_rating_food_security",
                       "ifad_rating_agr_productivity",
                       "ifad_rating_env_rec_mgmt",
                       "ifad_rating_adap_climatech",
                       "ifad_rating_human_assets",
                       "ifad_rating_social_cap_emp",
                       "ifad_rating_institus_policies",
                       "ifad_rating_markets",
                       "ifad_rating_ruralpoverty",
                       "ifad_rating_innovation_scaling",
                       "ifad_rating_gender_women_emp",
                       "gfatm_Region",
                       "gfatm_project_component",
                       "gfatm_LastPerformanceRating",
                       "gfatm_grantcurrentstatus",
                       "gfatm_Performancetop10indicators",
                       "gfatm_Performanceallindicators",
                       "gfatm_newstatus",
                       "wb_region",
                       "wb_countrycode",
                       "wb_agreementtype",
                       "wb_lendinginstrumenttype",
                       "wb_lendinginstrument",
                       "wb_productline_code",
                       "wb_productline_name",
                       "wb_ieg_evalfy",
                       "wb_ieg_evaluationtype",
                       "wb_erratappraisal",
                       "wb_erratcompletion",
                       "wb_ieg_outcome",
                       "wb_ieg_rdoclassification",
                       "wb_ieg_idimpactclassification",
                       "wb_ieg_bankqualityatentry",
                       "wb_ieg_bankqltyofsuperv",
                       "wb_ieg_overallbankperf",
                       "wb_discieg_borrprep",
                       "wb_ieg_implagencyperf",
                       "wb_ieg_governmentperf",
                       "wb_ieg_overallborrperf",
                       "wb_ieg_icrqualityclassification",
                       "wb_sustainability_classification",
                       "wb_ieg_mequalityclassification",
                       "wb_bankqualityatentry_rating",
                       "wb_supervisionquality_rating",
                       "wb_ieg_overallbankperf_rating",
                       "wb_discieg_borrprep_rating",
                       "wb_ieg_implagencyperf_rating",
                       "wb_ieg_governmentperf_rating",
                       "wb_ieg_overallborrperf_rating",
                       "wb_ieg_icrquality_rating",
                       "gef_ImplementingAgency",
                       "gef_LeadImplementingAgency",
                       "gef_Region",
                       "gef_ProjectSize",
                       "gef_TrustFund",
                       "gef_FocalArea",
                       "gef_phase",
                       "gef_SourceofRating",
                       "gef_OutcomesBinary",
                       "gef_Outcomes6point",
                       "gef_SustainabilityBinary",
                       "gef_Sustainability4point",
                       "gef_MEDesignBinary",
                       "gef_MEDesign6point",
                       "gef_MEImplementationBinary",
                       "gef_MEImplementation6point",
                       "gef_ImplementationQualityBinary",
                       "gef_ImplementationQuality6point",
                       "gef_ExecutionQualityBinary",
                       "gef_ExecutionQuality6point",
                       "gef_terminal_eval_overall",
                       "d",
                       "m",
                       "afdb_score_C1",
                       "afdb_score_C101",
                       "afdb_score_C102",
                       "afdb_score_C103",
                       "afdb_score_C104",
                       "afdb_score_C105",
                       "afdb_score_C106",
                       "afdb_score_C107",
                       "afdb_score_C2",
                       "afdb_score_C201",
                       "afdb_score_C202",
                       "afdb_score_C203",
                       "afdb_score_C204",
                       "afdb_score_C3",
                       "afdb_score_C301",
                       "afdb_score_C302",
                       "afdb_score_C303",
                       "afdb_score_C304",
                       "afdb_score_C305",
                       "afdb_score_C306",
                       "afdb_score_C307",
                       "afdb_score_C308",
                       "afdb_score_C309",
                       "afdb_score_C310",
                       "afdb_score_C4",
                       "afdb_score_C401",
                       "afdb_score_C402",
                       "afdb_score_C403",
                       "afdb_score_C404",
                       "afdb_score_C5",
                       "afdb_score_C501",
                       "afdb_score_C502",
                       "afdb_score_C503",
                       "afdb_score_C504",
                       "afdb_score_C505",
                       "afdb_score_C506",
                       "afdb_score_C507",
                       "afdb_score_C508",
                       "afdb_score_C6",
                       "afdb_score_C601",
                       "afdb_score_C602",
                       "afdb_score_C7",
                       "afdb_score_D1",
                       "afdb_score_D101",
                       "afdb_score_D102",
                       "afdb_score_D2",
                       "afdb_score_D201",
                       "afdb_score_D202",
                       "afdb_score_D203",
                       "afdb_score_D204",
                       "afdb_score_D205",
                       "afdb_score_D206",
                       "afdb_score_D207",
                       "afdb_score_D208",
                       "afdb_score_D209",
                       "afdb_score_D210",
                       "afdb_score_D211",
                       "afdb_score_D212",
                       "afdb_score_D213",
                       "afdb_score_D214",
                       "afdb_score_D215",
                       "afdb_score_D216",
                       "afdb_score_D217",
                       "afdb_score_D218",
                       "afdb_score_D219",
                       "afdb_score_D220",
                       "afdb_score_D221",
                       "afdb_score_D222",
                       "afdb_score_D223",
                       "afdb_score_D224",
                       "afdb_score_D225",
                       "afdb_score_D3",
                       "afdb_score_D301",
                       "afdb_score_D302",
                       "afdb_score_D303",
                       "afdb_score_D304",
                       "afdb_score_D305",
                       "afdb_score_D306",
                       "afdb_score_D307",
                       "afdb_score_D308",
                       "afdb_score_D309",
                       "afdb_score_D310",
                       "afdb_score_D311",
                       "afdb_score_D312",
                       "afdb_score_D313",
                       "afdb_score_D314",
                       "afdb_score_D315",
                       "afdb_score_D316",
                       "afdb_score_D317",
                       "afdb_score_D318",
                       "afdb_score_D319",
                       "afdb_score_D320",
                       "afdb_score_D321",
                       "afdb_score_D322",
                       "afdb_score_D323",
                       "afdb_score_D324",
                       "afdb_score_D325",
                       "afdb_score_D4",
                       "afdb_score_D401",
                       "afdb_score_D402",
                       "afdb_score_D403",
                       "afdb_score_D404",
                       "afdb_score_D405",
                       "afdb_score_D406",
                       "afdb_score_D407",
                       "afdb_score_D5",
                       "afdb_score_D501",
                       "afdb_score_D502",
                       "afdb_score_D503",
                       "afdb_score_D6",
                       "afdb_score_D7",
                       "afdb_score_D701",
                       "afdb_score_D702",
                       "afdb_score_D8",
                       "afdb_score_D801",
                       "afdb_score_D802",
                       "afdb_score_D9",
                       "afdb_score_D901",
                       "afdb_score_D902",
                       "afdb_score_D903",
                       "afdb_score_E1",
                       "afdb_score_E101",
                       "afdb_score_E102",
                       "afdb_score_E103",
                       "afdb_score_E104",
                       "afdb_score_E105",
                       "afdb_score_E106",
                       "afdb_score_E107",
                       "afdb_score_E108",
                       "afdb_score_E109",
                       "afdb_score_F1",
                       "afdb_score_F101",
                       "afdb_score_F102",
                       "afdb_score_F103",
                       "afdb_score_F104",
                       "afdb_score_F105",
                       "afdb_score_F106",
                       "afdb_score_F107",
                       "afdb_score_F108",
                       "afdb_score_F109",
                       "afdb_score_F110",
                       "afdb_score_F2",
                       "afdb_score_F201",
                       "afdb_score_F202",
                       "afdb_score_F203",
                       "afdb_score_O1",
                       "afdb_score_O101",
                       "afdb_score_O102",
                       "afdb_score_O2",
                       "afdb_score_O201",
                       "afdb_score_O202",
                       "afdb_score_O203",
                       "afdb_score_O3",
                       "afdb_score_O301",
                       "afdb_score_O302",
                       "afdb_score_O303",
                       "afdb_score_O304",
                       "afdb_score_O4",
                       "afdb_score_O401",
                       "afdb_score_O402",
                       "afdb_score_O403",
                       "afdb_score_O404",
                       "afdb_score_O405",
                       "afdb_score_O406",
                       "afdb_score_O407",
                       "afdb_score_O5",
                       "afdb_score_P1",
                       "afdb_score_P101",
                       "afdb_score_P102",
                       "afdb_score_P103",
                       "afdb_score_P104",
                       "afdb_score_P105",
                       "afdb_score_P106",
                       "afdb_score_P107",
                       "afdb_score_P108",
                       "afdb_score_P109",
                       "afdb_score_P110",
                       "afdb_score_P2",
                       "afdb_score_P201",
                       "afdb_score_P202",
                       "afdb_score_P203",
                       "afdb_ProjectSapCode",
                       "afdb_FormatType",
                       "afdb_SectorNames",
                       "afdb_SubSectorNames",
                       "afdb_impl_staff_CountryNames",
                       "afdb_impl_staff_sector",
                       "afdb_impl_staff_department",
                       "afdb_impl_staff_relevance",
                       "afdb_impl_staff_a1",
                       "afdb_impl_staff_a2",
                       "afdb_impl_staff_effectiveness",
                       "afdb_impl_staff_b4",
                       "afdb_impl_staff_efficiency",
                       "afdb_impl_staff_c1",
                       "afdb_impl_staff_c2",
                       "afdb_impl_staff_c3",
                       "afdb_impl_staff_c4",
                       "afdb_impl_staff_durability",
                       "afdb_impl_staff_d1",
                       "afdb_impl_staff_d2",
                       "afdb_impl_staff_d3",
                       "afdb_impl_staff_d4",
                       "afdb_impl_staff_overall_rating",
                       "afdb_evaluator",
                       "_merge_afdbprojectslist",
                       "afdb_relevance",
                       "afdb_efficiency",
                       "afdb_sustainability",
                       "dac5_code",
                       "mmg_purpose_sector",
                       "mmg_purpose_sectorname",
                       "mmg_purpose_code",
                       "mmg_purpose_codename",
                       "region",
                       "effect",
                       "legit",
                       "seceff",
                       "secleg",
                       "poleff",
                       "polleg",
                       "ecoeff",
                       "ecoleg",
                       "soceff",
                       "socleg")

ppd_april[columns_to_factor] <- lapply(ppd_april[columns_to_factor], factor)




# Alternative governance measures: WGI

# Import WGI
wgi <- read_csv("wgi.csv")

# Remove rows from WGI that don't contain scores 
wgi_scores <- wgi[grep("Estimate", wgi$`Series Name`, ignore.case = TRUE), ]

# Remove rows that have abbreviation explanations
wgi_scores <- wgi_scores[-c(1285:1302), ]

# Store WGI years in a dataset after cleaning brackets
wgi_years <- names(wgi)[5:28]
split_wgi_years <- as.data.frame(strsplit(wgi_years, " "))
wgi_years_cleaned <- as.numeric(unlist(split_wgi_years[1, ]))
wgi_years_final <- data.frame(wgi_years_cleaned)


# Transform WGI from wide to long to add year column
wgi_scores <- wgi_scores %>% pivot_longer(cols = "1996 [YR1996]": "2022 [YR2022]",
                                          names_to = "year",
                                          values_to = "score")

# Remove bracketed text from cells in year column in WGI
wgi_scores$year <- gsub("\\[.*\\]", "", wgi_scores$year)
wgi_scores$year <- trimws(wgi_scores$year)
wgi_scores$year <- as.numeric(wgi_scores$year)

# The wgi_scores dataset is now ready for merge. 

# Separate wgi_scores by score type 
wgi_corruption <- wgi_scores[which(wgi_scores$`Series Name` == "Control of Corruption: Estimate"), c("Country Name", "year", "score")]
names(wgi_corruption)[names(wgi_corruption) == "score"] <- "wgi_corruption_score"

wgi_goveff <- wgi_scores[which(wgi_scores$`Series Name` == "Government Effectiveness: Estimate"), c("Country Name", "year", "score")]
names(wgi_goveff)[names(wgi_goveff) == "score"] <- "wgi_goveff_score"

wgi_polstab <- wgi_scores[which(wgi_scores$`Series Name` == "Political Stability and Absence of Violence/Terrorism: Estimate"), c("Country Name", "year", "score")]
names(wgi_polstab)[names(wgi_polstab) == "score"] <- "wgi_polstability_score"

wgi_regqual <- wgi_scores[which(wgi_scores$`Series Name` == "Regulatory Quality: Estimate"), c("Country Name", "year", "score")]
names(wgi_regqual)[names(wgi_regqual) == "score"] <- "wgi_regquality_score"

wgi_rolaw <- wgi_scores[which(wgi_scores$`Series Name` == "Rule of Law: Estimate"), c("Country Name", "year", "score")]
names(wgi_rolaw)[names(wgi_rolaw) == "score"] <- "wgi_ruleoflaw_score"

wgi_voacc <- wgi_scores[which(wgi_scores$`Series Name` == "Voice and Accountability: Estimate"), c("Country Name", "year", "score")]
names(wgi_voacc)[names(wgi_voacc) == "score"] <- "wgi_voiceacct_score"

ppd_april$countryname_COW <- as.character(ppd_april$countryname_COW)

# Merge WGI sets with PPD
ppd_wgi_merge <- merge(ppd_april, wgi_corruption, by.x=c("countryname_COW", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_merge <- merge(ppd_wgi_merge, wgi_goveff, by.x=c("countryname_COW", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_merge <- merge(ppd_wgi_merge, wgi_polstab, by.x=c("countryname_COW", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_merge <- merge(ppd_wgi_merge, wgi_regqual, by.x=c("countryname_COW", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_merge <- merge(ppd_wgi_merge, wgi_rolaw, by.x=c("countryname_COW", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_merge <- merge(ppd_wgi_merge, wgi_voacc, by.x=c("countryname_COW", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)



ppd_wgi_merge$wgi_corruption_score <- as.numeric(ppd_wgi_merge$wgi_corruption_score)
ppd_wgi_merge$wgi_goveff_score <- as.numeric(ppd_wgi_merge$wgi_goveff_score)
ppd_wgi_merge$wgi_polstability_score <- as.numeric(ppd_wgi_merge$wgi_polstability_score)
ppd_wgi_merge$wgi_regquality_score <- as.numeric(ppd_wgi_merge$wgi_regquality_score)
ppd_wgi_merge$wgi_ruleoflaw_score <- as.numeric(ppd_wgi_merge$wgi_ruleoflaw_score)
ppd_wgi_merge$wgi_voiceacct_score <- as.numeric(ppd_wgi_merge$wgi_voiceacct_score)





# Create merged column for project size in USD
names(ppd_wgi_merge)[names(ppd_wgi_merge) == "projectsize_original"] <- "project_size_original"
project_size_usd <- numeric(nrow(ppd_wgi_merge))

for (i in 1:length(ppd_wgi_merge$project_size_original)) {
  if (ppd_wgi_merge$donor[i] == "DFID") {
    project_size_usd[i] <- ppd_wgi_merge$project_size_original[i] * 1.35
  }
  else if (ppd_wgi_merge$donor[i] == "AsianDB") {
    project_size_usd[i] <- ppd_wgi_merge$project_size_original[i] * 1000000
  }
  else if (ppd_wgi_merge$donor[i] == "GEF") {
    project_size_usd[i] <- ppd_wgi_merge$project_size_original[i] * 1000000
  }
  else if (ppd_wgi_merge$donor[i] == "GiZ") {
    project_size_usd[i] <- ppd_wgi_merge$project_size_original[i] * 1000
  }
  else if (ppd_wgi_merge$donor[i] == "IFAD") {
    project_size_usd[i] <- ppd_wgi_merge$project_size_original[i] * 1000000
  }
  else if (ppd_wgi_merge$donor[i] == "JICA") {
    project_size_usd[i] <- ppd_wgi_merge$project_size_original[i] * 10687
  }
  else if (ppd_wgi_merge$donor[i] == "KfW") {
    project_size_usd[i] <- ppd_wgi_merge$project_size_original[i] * 1.20
  }
  else {
    project_size_usd[i] <- ppd_wgi_merge$project_size_original[i]
  }
}

ppd_wgi_merge$project_size_usd <- project_size_usd


# Create binary outcome variable 
bi_overall_rating <- rep(NA, length(ppd_wgi_merge$six_overall_rating))

for (i in 1:length(ppd_wgi_merge$six_overall_rating)) {
  if (ppd_wgi_merge$six_overall_rating[i] > 3) {
    bi_overall_rating[i] <- 1
  }
  else {
    bi_overall_rating[i] <- 0
  }
}

ppd_wgi_merge$bi_overall_rating <- bi_overall_rating



# Create finalized dataset ppd_wgi_merge_final that has columns of interest. 
# Complete list of variables from the above merge can be found in the codebook.
# List of variables used for the model are included separately. 


# Rename columns of interest for interpretability

names(ppd_wgi_merge)[names(ppd_wgi_merge) == "countryname_COW"] <- "country_name"
names(ppd_wgi_merge)[names(ppd_wgi_merge) == "projectname"] <- "project_name"
names(ppd_wgi_merge)[names(ppd_wgi_merge) == "sector_description"] <- "sector"
names(ppd_wgi_merge)[names(ppd_wgi_merge) == "purpose_description"] <- "project_purpose"



# Remove columns with NAs, duplicates, or organization specifics that have been combined
ppd_wgi_final <- ppd_wgi_merge %>%
  select(c("project_id",
           "country_name",
           "start_year",
           "donor",
           "six_overall_rating",
           "project_duration",
           "project_name",
           "bi_overall_rating",
           "office_presence",
           "project_purpose",
           "region",
           "sector",
           "project_size_usd",
           "wgi_corruption_score",
           "wgi_goveff_score",
           "wgi_polstability_score",
           "wgi_regquality_score",
           "wgi_ruleoflaw_score",
           "wgi_voiceacct_score"))


# Alternative governance measures: World Bank CPIA

# Import WGI
cpia <- read_csv("cpia.csv")

# Remove Series Code column
cpia <- cpia %>% 
  select(-c("Series Code"))


# Store WGI years in a dataset after cleaning brackets
cpia_years <- names(cpia)[4:18]
split_cpia_years <- as.data.frame(strsplit(cpia_years, " "))
cpia_years_cleaned <- as.numeric(unlist(split_cpia_years[1, ]))
cpia_years_final <- data.frame(cpia_years_cleaned)


# Transform WGI from wide to long to add year column
cpia <- cpia %>% pivot_longer(cols = "2005 [YR2005]": "2022 [YR2022]",
                                          names_to = "year",
                                          values_to = "score")

# Remove bracketed text from cells in year column in WGI
cpia$year <- gsub("\\[.*\\]", "", cpia$year)
cpia$year <- trimws(cpia$year)
cpia$year <- as.numeric(cpia$year)


# Remove text in parentheses in cells in column 'Series Name'
cpia$`Series Name` <- gsub("\\(.*?\\)", "", cpia$`Series Name`)
cpia$`Series Name`<- trimws(cpia$`Series Name`)

# Replace .. with NA in score column
for (i in 1:length(cpia$score)) {
  if (!is.na(cpia$score[i]) && cpia$score[i] == "..") {
    cpia$score[i] <- NA
  }
}

# The CPIA dataset is now ready for merge with the PPD. 

# Separate cpia by score type 
cpia_build_hr_score <- cpia[which(cpia$`Series Name` == "CPIA building human resources rating"), c("Country Name", "year", "score")]
names(cpia_build_hr_score)[names(cpia_build_hr_score) == "score"] <- "cpia_build_hr_score"

cpia_bus_reg_env_score <- cpia[which(cpia$`Series Name` == "CPIA business regulatory environment rating"), c("Country Name", "year", "score")]
names(cpia_bus_reg_env_score)[names(cpia_bus_reg_env_score) == "score"] <- "cpia_bus_reg_env_score"

cpia_debt_pol_score <- cpia[which(cpia$`Series Name` == "CPIA debt policy rating"), c("Country Name", "year", "score")]
names(cpia_debt_pol_score)[names(cpia_debt_pol_score) == "score"] <- "cpia_debt_pol_score"

cpia_econmgt_clust_score <- cpia[which(cpia$`Series Name` == "CPIA economic management cluster average"), c("Country Name", "year", "score")]
names(cpia_econmgt_clust_score)[names(cpia_econmgt_clust_score) == "score"] <- "cpia_econmgt_clust_score"

cpia_revmob_eff_score <- cpia[which(cpia$`Series Name` == "CPIA efficiency of revenue mobilization rating"), c("Country Name", "year", "score")]
names(cpia_revmob_eff_score)[names(cpia_revmob_eff_score) == "score"] <- "cpia_revmob_eff_score"

cpia_publres_eqty_score <- cpia[which(cpia$`Series Name` == "CPIA equity of public resource use rating"), c("Country Name", "year", "score")]
names(cpia_publres_eqty_score)[names(cpia_publres_eqty_score) == "score"] <- "cpia_publres_eqty_score"

cpia_fin_sector_score <- cpia[which(cpia$`Series Name` == "CPIA financial sector rating"), c("Country Name", "year", "score")]
names(cpia_fin_sector_score)[names(cpia_fin_sector_score) == "score"] <- "cpia_fin_sector_score"

cpia_fisc_pol_score <- cpia[which(cpia$`Series Name` == "CPIA fiscal policy rating"), c("Country Name", "year", "score")]
names(cpia_fisc_pol_score)[names(cpia_fisc_pol_score) == "score"] <- "cpia_fisc_pol_score"

cpia_gend_eq_score <- cpia[which(cpia$`Series Name` == "CPIA gender equality rating"), c("Country Name", "year", "score")]
names(cpia_gend_eq_score)[names(cpia_gend_eq_score) == "score"] <- "cpia_gend_eq_score"

cpia_macecon_mgt_score <- cpia[which(cpia$`Series Name` == "CPIA macroeconomic management rating"), c("Country Name", "year", "score")]
names(cpia_macecon_mgt_score)[names(cpia_macecon_mgt_score) == "score"] <- "cpia_macecon_mgt_score"

cpia_social_policy_eqty_score <- cpia[which(cpia$`Series Name` == "CPIA policies for social inclusion/equity cluster average"), c("Country Name", "year", "score")]
names(cpia_social_policy_eqty_score)[names(cpia_social_policy_eqty_score) == "score"] <- "cpia_social_policy_eqty_score"

cpia_env_policy_score <- cpia[which(cpia$`Series Name` == "CPIA policy and institutions for environmental sustainability rating"), c("Country Name", "year", "score")]
names(cpia_env_policy_score)[names(cpia_env_policy_score) == "score"] <- "cpia_env_policy_score"

cpia_prop_rbg_score <- cpia[which(cpia$`Series Name` == "CPIA property rights and rule-based governance rating"), c("Country Name", "year", "score")]
names(cpia_prop_rbg_score)[names(cpia_prop_rbg_score) == "score"] <- "cpia_prop_rbg_score"

cpia_publ_sct_mgt_score <- cpia[which(cpia$`Series Name` == "CPIA public sector management and institutions cluster average"), c("Country Name", "year", "score")]
names(cpia_publ_sct_mgt_score)[names(cpia_publ_sct_mgt_score) == "score"] <- "cpia_publ_sct_mgt_score"

cpia_finmgt_qual_score <- cpia[which(cpia$`Series Name` == "CPIA quality of budgetary and financial management rating"), c("Country Name", "year", "score")]
names(cpia_finmgt_qual_score)[names(cpia_finmgt_qual_score) == "score"] <- "cpia_finmgt_qual_score"

cpia_public_admin_score <- cpia[which(cpia$`Series Name` == "CPIA quality of public administration rating"), c("Country Name", "year", "score")]
names(cpia_public_admin_score)[names(cpia_public_admin_score) == "score"] <- "cpia_public_admin_score"

cpia_social_prot_score <- cpia[which(cpia$`Series Name` == "CPIA social protection rating"), c("Country Name", "year", "score")]
names(cpia_social_prot_score)[names(cpia_social_prot_score) == "score"] <- "cpia_social_prot_score"

cpia_struc_pol_score <- cpia[which(cpia$`Series Name` == "CPIA structural policies cluster average"), c("Country Name", "year", "score")]
names(cpia_struc_pol_score)[names(cpia_struc_pol_score) == "score"] <- "cpia_struc_pol_score"

cpia_trade_score <- cpia[which(cpia$`Series Name` == "CPIA trade rating"), c("Country Name", "year", "score")]
names(cpia_trade_score)[names(cpia_trade_score) == "score"] <- "cpia_trade_score"

cpia_publ_corr_score <- cpia[which(cpia$`Series Name` == "CPIA transparency, accountability, and corruption in the public sector rating"), c("Country Name", "year", "score")]
names(cpia_publ_corr_score)[names(cpia_publ_corr_score) == "score"] <- "cpia_publ_corr_score"

cpia_ida_res_score <- cpia[which(cpia$`Series Name` == "IDA resource allocation index"), c("Country Name", "year", "score")]
names(cpia_ida_res_score)[names(cpia_ida_res_score) == "score"] <- "cpia_ida_res_score"


# Merge with PPD

ppd_wgi_final <- merge(ppd_wgi_final, cpia_build_hr_score, by.x=c("country_name", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_final <- merge(ppd_wgi_final, cpia_bus_reg_env_score, by.x=c("country_name", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_final <- merge(ppd_wgi_final, cpia_debt_pol_score, by.x=c("country_name", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_final <- merge(ppd_wgi_final, cpia_econmgt_clust_score, by.x=c("country_name", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_final <- merge(ppd_wgi_final, cpia_revmob_eff_score, by.x=c("country_name", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_final <- merge(ppd_wgi_final, cpia_publres_eqty_score, by.x=c("country_name", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_final <- merge(ppd_wgi_final, cpia_fin_sector_score, by.x=c("country_name", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_final <- merge(ppd_wgi_final, cpia_fisc_pol_score, by.x=c("country_name", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_final <- merge(ppd_wgi_final, cpia_gend_eq_score, by.x=c("country_name", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_final <- merge(ppd_wgi_final, cpia_macecon_mgt_score, by.x=c("country_name", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_final <- merge(ppd_wgi_final, cpia_social_policy_eqty_score, by.x=c("country_name", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_final <- merge(ppd_wgi_final, cpia_env_policy_score, by.x=c("country_name", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_final <- merge(ppd_wgi_final, cpia_prop_rbg_score, by.x=c("country_name", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_final <- merge(ppd_wgi_final, cpia_publ_sct_mgt_score, by.x=c("country_name", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_final <- merge(ppd_wgi_final, cpia_finmgt_qual_score, by.x=c("country_name", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_final <- merge(ppd_wgi_final, cpia_public_admin_score, by.x=c("country_name", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_final <- merge(ppd_wgi_final, cpia_social_prot_score, by.x=c("country_name", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_final <- merge(ppd_wgi_final, cpia_struc_pol_score, by.x=c("country_name", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_final <- merge(ppd_wgi_final, cpia_trade_score, by.x=c("country_name", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_final <- merge(ppd_wgi_final, cpia_publ_corr_score, by.x=c("country_name", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)
ppd_wgi_final <- merge(ppd_wgi_final, cpia_ida_res_score, by.x=c("country_name", "start_year"), by.y=c("Country Name", "year"), all.x=TRUE)



# DEALING WITH NAs 

# Factor finalized columns 
factor_cols <- c("country_name", "donor", "region", "bi_overall_rating")
ppd_wgi_final[factor_cols] <- lapply(ppd_wgi_final[factor_cols], factor)

# Turn numeric columns into numeric
numeric_cols <- c("start_year", "six_overall_rating", "project_duration", "project_size_usd")
ppd_wgi_final[numeric_cols] <- lapply(ppd_wgi_final[numeric_cols], function(x) as.numeric(as.character(x)))


# Replace NAs with "Other" and "Unknown"
ppd_wgi_final <- ppd_wgi_final %>%
  mutate(
    sector = ifelse(is.na(sector), "Other", sector),
    project_purpose = ifelse(is.na(project_purpose), "Other", project_purpose),
    project_name = ifelse(is.na(project_name), "Unknown", project_name),
    project_size_usd = ifelse(is.na(project_size_usd), "Unknown", project_size_usd)
  )


# Impute office presence and other variables using random forest
# Random forest chosen due to complexity of data
install.packages("missRanger")
library(missRanger)
predictor_variables <- c("country_name", 
                         "start_year", 
                         "donor", 
                         "six_overall_rating", 
                         "project_duration",
                         "office_presence",
                         "project_purpose",
                         "region",
                         "sector",
                         "project_size_usd")

impute_formula <- as.formula(paste("office_presence ~", paste(predictor_variables, collapse = " + ")))

imputed_ppd_wgi_cpia <- missRanger(ppd_wgi_final, formula = impute_formula, pmm.k = 3, num.trees = 100)
View(imputed_ppd_wgi_cpia)


# Rename final dataset 
ppd_final <- imputed_ppd_wgi_cpia

# Remove other NAs 
ppd_final <- ppd_final[complete.cases(ppd_final[, c("six_overall_rating", 
                                                    "start_year", 
                                                    "project_duration", 
                                                    "region")]), ]

ppd_final <- ppd_final %>%
  mutate(six_overall_rating_int = floor(six_overall_rating), # Create a temporary column for the integer part
         multi_overall_rating = case_when(
           six_overall_rating_int %in% c(0, 1) ~ 0,
           six_overall_rating_int %in% c(2, 3, 4) ~ 1,
           six_overall_rating_int %in% c(5, 6) ~ 2,
           TRUE ~ NA_real_  # Handles cases outside 0-6, if any
         )) %>%
  select(-six_overall_rating_int)

ppd_final$multi_overall_rating <- factor(ppd_final$multi_overall_rating)

# The ppd_final dataset contains both WGI and CPIA scores. 
# For the purposes of running multiple models to assess model performance,
# this compiled dataset will be split into 3: 
# ppd_wgi <- Project rows with only WGI scores 
# ppd_cpia <- Project rows with only CPIA scores 
# ppd_only <- Project rows only 

# Subset for WGI
# Subset to include columns 1 through 13
proj_columns <- ppd_final[c("country_name", 
                            "start_year",
                            "project_id",
                            "project_name",
                            "bi_overall_rating",
                            "multi_overall_rating",
                            "donor", 
                            "six_overall_rating", 
                            "project_duration",
                            "office_presence",
                            "project_purpose",
                            "region",
                            "sector",
                            "project_size_usd")]

# Find columns that contain the string "wgi"
wgi_columns <- grep("wgi", names(ppd_final), value = TRUE)

# Subset to include columns with "wgi"
columns_wgi <- ppd_final[, wgi_columns]

# Combine the two subsets
ppd_wgi <- cbind(proj_columns, columns_wgi)


# Repeat for CPIA
cpia_columns <- grep("cpia", names(ppd_final), value = TRUE)

# Subset to include columns with "wgi"
columns_cpia <- ppd_final[, cpia_columns]

# Combine the two subsets
ppd_cpia <- cbind(proj_columns, columns_cpia)

# Create subset without governance metrics 
ppd_only <- proj_columns[which(proj_columns$start_year > 1994), ]

# Check for NAs
colSums(is.na(ppd_only))

# Convert all columns starting with "cpia" to numeric and "sector" to a factor
ppd_cpia <- ppd_cpia %>%
  mutate(across(starts_with("cpia"), as.numeric), # Convert cpia columns to numeric
         sector = factor(sector)) # Convert sector to factor

# Check the structure to confirm the changes
str(ppd_cpia)


write.csv(ppd_only, "ppd_only.csv")
write.csv(ppd_cpia, "ppd_cpia.csv")
write.csv(ppd_wgi, "ppd_wgi.csv")




ppd_only_all <- ppd_final_withna[, 1:13]
