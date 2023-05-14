
'DATA PREPROCESSING MODULE          

                       '
# Back old names for the purpose of simulation 

colnames(SIMULATION) <- column_names

suppressMessages({ 
# setwd(path)
# getwd()
# 1. CONSOLIDATED SIMULATION TABLE  --------

# colnames(SIMULATION) <- c("PRODUCT_INDUSTRY_CODE","CPA_DIVISION",
#                   "Current_Policy_Exempt","Current_Policy_Reduced_Rate","Current_Policy_Fully_Taxable",
#                   "Simulation_Toggles_Exempt","Simulation_Toggles_Reduced_Rate",
#                   "Standard_VAT_Rate","Preferential_VAT_Rate")


# colnames(SIMULATION) <- c("PRODUCT_INDUSTRY_CODE",
#                   "Current_Policy_Exempt","Current_Policy_Reduced_Rate","Current_Policy_Fully_Taxable",
#                   "Simulation_Toggles_Exempt","Simulation_Toggles_Reduced_Rate",
#                   "Standard_VAT_Rate","Preferential_VAT_Rate")


SIMULATION <- SIMULATION %>%
  dplyr::mutate(Simulated_Policy_Exempt = ifelse(is.na(Simulation_Toggles_Exempt), Current_Policy_Exempt, Simulation_Toggles_Exempt),
                Simulated_Policy_Reduced_Rate = ifelse(is.na(Simulation_Toggles_Reduced_Rate), Current_Policy_Reduced_Rate, Simulation_Toggles_Reduced_Rate),
                Simulated_Policy_Fully_Taxable = 1-Simulated_Policy_Exempt-Simulated_Policy_Reduced_Rate)

TE_EXEMPT<-0.00
TE_REDUCED_RATE<-0.00

# 2. DEFINE FUNCTIONS ----


#  The function creates an ntile group vector:
qgroup = function(numvec, n, na.rm=TRUE){
  qtile = quantile(numvec, probs = seq(0, 1, 1/n), na.rm)  
  out = sapply(numvec, function(x) sum(x >= qtile[-(n+1)]))
  return(out)
}




#  to extract only English names from SUTs
trim <- function (x) gsub("^\\s+|\\s+$", "", x) 
input_output_matrix_to_long_data <- function(matrix){
  
  matrix <- matrix %>%
    dplyr::filter(...2 != "NA")
  
  
  colnames(matrix) <- matrix[1,]
  
  data <- matrix[c(-1,-2),c(-1,-2)] %>% as.matrix() %>% melt()
  
  product_industry_name <- matrix[[2]][c(-1,-2)]
  product_industry_code <- matrix[[1]][c(-1,-2)]
  industry_code <-  matrix[2,c(-1,-2)] %>% as.character()
  
  data$Var1 <- rep(product_industry_name, time = length(industry_code))
  
  data <- data %>% 
    dplyr::rename(PRODUCT_INDUSTRY_NAME = Var1,
                  INDUSTRY_NAME = Var2)
  
  
  data$PRODUCT_INDUSTRY_CODE <- rep(product_industry_code, time = length(industry_code))
  data$INDUSTRY_CODE <- rep(industry_code, each = length(product_industry_code))
  
  data <- data %>% 
    dplyr::select(PRODUCT_INDUSTRY_NAME, PRODUCT_INDUSTRY_CODE, INDUSTRY_NAME, INDUSTRY_CODE, value)
  
  
  # Leave the names only in English
  data$PRODUCT_INDUSTRY_NAME<-gsub("^.*\\/", "",  data$PRODUCT_INDUSTRY_NAME) %>% trim()
  data$INDUSTRY_NAME<-gsub("^.*\\/", "",  data$INDUSTRY_NAME) %>% trim()
  
  data$value <- as.numeric(as.character(data$value))
  
  return(data)
  
}


# 3. RAW DATA IMPORT AND PREPROCESS  ----- 
' 
                    In this section data are imported from three files:
                    
                    VAT_Model_v9.15a.xlsx
                    Merged_VAT_data.xlsx 
                    NACE_SUT_table.xlsx
                    
                    '

# 3.1 SUTs ------------------------------------


SUPPLY <- read_excel("DATA/INPUT/VAT_Model_v9.15b.xlsx", sheet = "Supply", col_names = F)[c(-1,-2,-3,-4),] %>%
  input_output_matrix_to_long_data()

USE_PURCHASER <- read_excel("DATA/INPUT/VAT_Model_v9.15b.xlsx", sheet = "Use_Purchaser", col_names = F)[c(-1,-2,-3,-4),] %>%
  input_output_matrix_to_long_data()

USE_VAT <- read_excel("DATA/INPUT/VAT_Model_v9.15b.xlsx", sheet = "Use_VAT", col_names = F)[c(-1,-2,-3,-4),] %>%
  input_output_matrix_to_long_data()

USE_SUBSIDIES <- read_excel("DATA/INPUT/VAT_Model_v9.15b.xlsx", sheet = "Use_Subsidies", col_names = F)[c(-1,-2,-3,-4),] %>%
  input_output_matrix_to_long_data()

USE_OTHERTAXES <- read_excel("DATA/INPUT/VAT_Model_v9.15b.xlsx", sheet = "Use_OtherTaxes", col_names = F)[c(-1,-2,-3,-4),] %>%
  input_output_matrix_to_long_data()

USE_BASIC <- read_excel("DATA/INPUT/VAT_Model_v9.15b.xlsx", sheet = "Use_Basic", col_names = F)[c(-1,-2,-3,-4),] %>%
  input_output_matrix_to_long_data()

USE_IMPORTS <- read_excel("DATA/INPUT/VAT_Model_v9.15b.xlsx", sheet = "Use_Imports", col_names = F)[c(-1,-2,-3,-4),] %>%
  input_output_matrix_to_long_data()


# 3.2 COICOP table ------------------------------------------------------------


## Data from COICOP table

VAT_COICOP_NAMES <- read_excel("DATA/INPUT/VAT_Model_v9.15b.xlsx", sheet = "COICOP", col_names = F)[-c(1:2),c(2)]
VAT_COICOP_NAMES<-VAT_COICOP_NAMES[1:178,1]


VAT_COICOP_FC <- read_excel("DATA/INPUT/VAT_Model_v9.15b.xlsx", sheet = "COICOP", col_names = F)[-c(1:2),c(4:9)]
VAT_COICOP_FC<-VAT_COICOP_FC[1:178,1:6]


VAT_COICOP_FINAL_RAW<-cbind(VAT_COICOP_NAMES,VAT_COICOP_FC)

VAT_COICOP_FINAL_RAW<-VAT_COICOP_FINAL_RAW%>%
  dplyr:: rename(c("COICOP_Descriptions"= "...2",
                   "FC"="...4",
                   "EX"= "...5",
                   "Reduced_Rate_5"="...6",
                   "Standard_Rate_18"= "...7",
                   "VAT_Revenue_5"="...8",
                   "VAT_Revenue_18"="...9"
                   
  ))

#Select NACE industries on four digits for calculation  

VAT_COICOP_FINAL_RAW<-subset(VAT_COICOP_FINAL_RAW, grepl("^\\d{2}\\.\\d\\.\\d\\s+", COICOP_Descriptions))

# Extract NACE codes
VAT_COICOP_FINAL_RAW$Four_digits<-substr(VAT_COICOP_FINAL_RAW$COICOP_Descriptions, 1, 6)
VAT_COICOP_FINAL_RAW$Two_digits<-substr(VAT_COICOP_FINAL_RAW$COICOP_Descriptions, 1, 2)
VAT_COICOP_FINAL_RAW[is.na(VAT_COICOP_FINAL_RAW)] <- 0
VAT_COICOP_FINAL_RAW$EX<-as.numeric(VAT_COICOP_FINAL_RAW$EX)
VAT_COICOP_FINAL_RAW$VAT_Revenue_5<-as.numeric(VAT_COICOP_FINAL_RAW$VAT_Revenue_5)


# Input raw concordance table
VAT_Concordance <- read_excel("DATA/INPUT/VAT_Model_v9.15b.xlsx", sheet = "Concordance", col_names = T)

# Merging table
VAT_COICOP_FINAL<-left_join(VAT_COICOP_FINAL_RAW,VAT_Concordance,by = c("COICOP_Descriptions"))


# Adjustment of CPA codes with Concordance table
COICOP <- read_excel("DATA/INPUT/VAT_Model_v9.15b.xlsx", sheet = "COICOP", col_names = F)[-c(1,2),-c(1:19)]
COICOP[1,1] <- "PRODUCT_INDUSTRY_CODE"
COICOP[1,5] <- "Negative"

colnames(COICOP) <- c("PRODUCT_INDUSTRY_CODE", "Base",  "Exempt_Levels", "Reduced_Rate_Levels", "Negative", "Exempt_Adjustment", "Reduced_Rate_Adjustment", 
                      "Exempt_Levels_2", "Reduced_Rate_Levels_2",
                      "Exempt_Raw_perc", "Reduced_Rate_Raw_perc", "Exempt_Capped_perc", "Reduced_Rate_Capped_perc")

COICOP <- COICOP[-c(1, 66:nrow(COICOP)),]

COICOP <- COICOP %>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE)


# 3.3 Data from import (Merged_VAT_data)------------------------------------

# Input structure by CPA and HS

Merged_VAT_data <- read_excel("DATA/INPUT/Merged_VAT_data.xlsx", sheet = "VAT_IMPORT_HS")



# 3.4 Taxable proportion -------------------------------------------
'This table will change taxabile proportion for estimation. Data here are estimated from 
                           data from import for goods and from data from COICOP for services
                          '


# 3.5 Concordance table NACE_SUT ----------------------------------

NACE_SUT_table <- read_excel("DATA/INPUT/NACE_SUT_table.xlsx", sheet = "NACE")

CPA_COICOP_CONCORDANCE <- read_excel("DATA/INPUT/NACE_SUT_table.xlsx", sheet = "CPA_COICOP_CONCORDANCE")


NACE_SUT_table<-NACE_SUT_table%>%
  dplyr::select(divisions_sut,nace)

# Detect import with reduced and exempted tax rates

Merged_VAT_data_Group_5_18<-Merged_VAT_data%>%
  dplyr::select(cpa_code,mkd_import,vat_rate)%>%
  dplyr::group_by(cpa_code, vat_rate) %>%
  dplyr::summarise(Total_import = sum(mkd_import, na.rm = T))


# Remove NaN. In data base there are some goods that are classified under code 9888770000	Undivided. This code can't not be calculated
Merged_VAT_data_Group=Merged_VAT_data_Group_5_18[!(is.na(Merged_VAT_data_Group_5_18$vat_rate) | Merged_VAT_data_Group_5_18$vat_rate==""), ]


# Pivot longer
Merged_VAT_data_Group_5_18_Pivot<-Merged_VAT_data_Group %>%
  pivot_wider(names_from = vat_rate , values_from = Total_import,values_fill = 0)


# Detect import exempt from VAT
Merged_VAT_data_Group_Exempt<-Merged_VAT_data%>%
  dplyr::select(cpa_code,mkd_import,vat_rate,effective_vat)%>%
  dplyr:: filter(effective_vat<=2.5)%>%
  dplyr::group_by(cpa_code,vat_rate) %>%
  dplyr::summarise(Total_import = sum(mkd_import, na.rm = T))


# Remove NaN. In data base there are some goods that are classified under code 9888770000	Undivided. This code can't not be calculated
Merged_VAT_data_Group_Exempt=Merged_VAT_data_Group_Exempt[!(is.na(Merged_VAT_data_Group_Exempt$vat_rate) | Merged_VAT_data_Group_Exempt$vat_rate==""), ]

# Pivot longer
Merged_VAT_data_Group_Exempt_Pivot<-Merged_VAT_data_Group_Exempt %>%
  pivot_wider(names_from = vat_rate , values_from = Total_import,values_fill = 0)

# Merged data 
VAT_STRUCTURE<-left_join(Merged_VAT_data_Group_5_18_Pivot,Merged_VAT_data_Group_Exempt_Pivot,by = c("cpa_code"="cpa_code"))
VAT_STRUCTURE[is.na(VAT_STRUCTURE)] <- 0

VAT_STRUCTURE_FINAL<-VAT_STRUCTURE%>%
  dplyr::mutate(
    EX=`5.y`+`18.y`,
    Reduced_Rate_5 = `5.x`-`5.y`,
    Standard_Rate_18 =`18.x`-`18.y`
  )%>%
  dplyr::select(-c(`5.x`,`18.x`,`5.y`,`18.y`))


VAT_STRUCTURE_FINAL<-left_join(VAT_STRUCTURE_FINAL,NACE_SUT_table,by = c("cpa_code"="nace"))

VAT_STRUCTURE_FINAL<-VAT_STRUCTURE_FINAL%>%
  dplyr::select(divisions_sut,EX,Reduced_Rate_5,Standard_Rate_18)%>%
  dplyr::group_by(divisions_sut) %>%
  dplyr::summarise(EX = sum(EX, na.rm = T),Reduced_Rate_5 = sum(Reduced_Rate_5, na.rm = T),Standard_Rate_18 = sum(Standard_Rate_18, na.rm = T))

# 3.6 HBS  ----------------------------------------
# Import data
data4_hbs <- read_excel("DATA/INPUT/Data4_hbs2019.xlsx")


# Setting columns names
data4_hbs<-data4_hbs%>%
  dplyr::select(-c('kvartal','Year'))


colnames(data4_hbs)<-c("number_hh","01","02","03","04","05","06","07","08","09","10","11","12","Consumption_own")

# Preparing data for merging 
data4_hbs_long<-data4_hbs%>%
  pivot_longer(!number_hh, names_to = "COICOP_section", values_to = "Expenditures")

# 3.7 MACRO-FISCAL INDICATORS ---------------------------------------------
MACRO_FISCAL_INDICATORS <- read_excel("DATA/INPUT/MACRO_FISCAL_INDICATORS.xlsx")

# 4. AGGREGATE DATA IN LISTS -----

CPA_PRODUCTS <- as.list(c(1:4))
names(CPA_PRODUCTS) = c("Supply", "Use_Purchaser", "Use_VAT", "Use_Basic")

NACE_INDUSTRIES <- as.list(c(1:4))
names(NACE_INDUSTRIES) = c("Supply", "Use_Purchaser", "Use_VAT", "Use_Basic")

RC_prc_of_Constructions_and_construction_works = 0.3
#Locked_Calibration_Factor <-0.872971938101163     
vat_rate_on_residential_construction = 0.05

# 4.1 Supply matrix -----

CPA_PRODUCTS$Supply <- SUPPLY %>% 
  dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
  dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
  dplyr::summarise(Total_output = sum(value, na.rm = T))

CPA_PRODUCTS$Supply <- SUPPLY %>%
  dplyr::filter(INDUSTRY_NAME == "Imports CIF") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
  merge.data.frame(CPA_PRODUCTS$Supply, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::rename(Imports_CIF = value)

CPA_PRODUCTS$Supply <- SUPPLY %>%
  dplyr::filter(INDUSTRY_NAME == "Trade and transport margins") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
  merge.data.frame(CPA_PRODUCTS$Supply, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::rename(Trade_and_transport_margins = value)

CPA_PRODUCTS$Supply <- SUPPLY %>%
  dplyr::filter(INDUSTRY_NAME == "Taxes less subsidies on products") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
  merge.data.frame(CPA_PRODUCTS$Supply, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::rename(Taxes_less_subsidies_on_products = value)

CPA_PRODUCTS$Supply <- CPA_PRODUCTS$Supply %>%
  dplyr::mutate(Total_supply_at_basic_prices = psum(Total_output,Imports_CIF, na.rm=TRUE),
                Total_supply_at_purchasers_prices = psum(Total_supply_at_basic_prices,
                                                         Trade_and_transport_margins,
                                                         Taxes_less_subsidies_on_products, na.rm=TRUE))

NACE_INDUSTRIES$Supply <- SUPPLY %>% 
  dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
  dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
  dplyr::summarise(Total_output_by_industries_at_basic_prices = sum(value, na.rm = T))

# 4.2 Use Purchaser matrix ----

CPA_PRODUCTS$Use_Purchaser <- USE_PURCHASER %>% 
  dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
  dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
  dplyr::summarise(Total_intermediate_consumption_at_purchasers_prices = sum(value, na.rm = T))

CPA_PRODUCTS$Use_Purchaser <-  USE_PURCHASER %>%
  dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by households") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
  merge.data.frame(CPA_PRODUCTS$Use_Purchaser, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::rename(Final_consumption_expenditure_by_households = value)

CPA_PRODUCTS$Use_Purchaser <- USE_PURCHASER %>%
  dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by non-profit organisations serving households (NPISH)") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
  merge.data.frame(CPA_PRODUCTS$Use_Purchaser, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::rename(Final_consumption_expenditure_NPISH = value)

CPA_PRODUCTS$Use_Purchaser <- USE_PURCHASER %>%
  dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by government") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
  merge.data.frame(CPA_PRODUCTS$Use_Purchaser, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::rename(Final_consumption_expenditure_by_government = value)

CPA_PRODUCTS$Use_Purchaser <- USE_PURCHASER %>%
  dplyr::filter(INDUSTRY_NAME == "Gross fixed capital formation") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
  merge.data.frame(CPA_PRODUCTS$Use_Purchaser, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::rename(Gross_fixed_capital_formation = value)

CPA_PRODUCTS$Use_Purchaser <- USE_PURCHASER %>%
  dplyr::filter(INDUSTRY_NAME == "Changes in inventories and acquisition less disposals of valuables") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
  merge.data.frame(CPA_PRODUCTS$Use_Purchaser, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::rename(Changes_in_inventories_and_acquisition_less_disposals_of_valuables = value)

CPA_PRODUCTS$Use_Purchaser <- USE_PURCHASER %>%
  dplyr::filter(INDUSTRY_NAME == "Exports FOB") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
  merge.data.frame(CPA_PRODUCTS$Use_Purchaser, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::rename(Exports_FOB = value)


CPA_PRODUCTS$Use_Purchaser <- CPA_PRODUCTS$Use_Purchaser %>%
  dplyr::mutate(Total_final_consumption_expenditure_at_purchasers_prices = psum(Final_consumption_expenditure_by_households,
                                                                                Final_consumption_expenditure_NPISH,
                                                                                Final_consumption_expenditure_by_government, na.rm=TRUE),
                Gross_capital_formation = psum(Gross_fixed_capital_formation,
                                               Changes_in_inventories_and_acquisition_less_disposals_of_valuables, na.rm=TRUE),
                Total_final_uses_at_purchasers_prices = psum(Total_final_consumption_expenditure_at_purchasers_prices,
                                                             Gross_capital_formation,
                                                             Exports_FOB, na.rm=TRUE),
                Total_use_at_purchasers_prices = psum(Total_intermediate_consumption_at_purchasers_prices,
                                                      Total_final_uses_at_purchasers_prices, na.rm=TRUE))


NACE_INDUSTRIES$Use_Purchaser <- USE_PURCHASER %>% 
  dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
  dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
  dplyr::summarise(Total_intermediate_consumption_by_industries_at_purchasers_prices = sum(value, na.rm = T))


# 4.3 Use Basic matrix ----

CPA_PRODUCTS$Use_Basic <- USE_BASIC %>% 
  dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
  dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
  dplyr::summarise(Total_intermediate_consumption_at_basic_prices = sum(value, na.rm = T))

CPA_PRODUCTS$Use_Basic <- USE_BASIC %>%
  dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by households") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
  merge.data.frame(CPA_PRODUCTS$Use_Basic, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::rename(Final_consumption_expenditure_by_households = value)

CPA_PRODUCTS$Use_Basic <- USE_BASIC %>%
  dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by non-profit organisations serving households (NPISH)") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
  merge.data.frame(CPA_PRODUCTS$Use_Basic, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::rename(Final_consumption_expenditure_NPISH = value)

CPA_PRODUCTS$Use_Basic <- USE_BASIC %>%
  dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by government") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
  merge.data.frame(CPA_PRODUCTS$Use_Basic, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::rename(Final_consumption_expenditure_by_government = value)

CPA_PRODUCTS$Use_Basic <- USE_BASIC %>%
  dplyr::filter(INDUSTRY_NAME == "Gross fixed capital formation") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
  merge.data.frame(CPA_PRODUCTS$Use_Basic, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::rename(Gross_fixed_capital_formation = value)

CPA_PRODUCTS$Use_Basic <- USE_BASIC %>%
  dplyr::filter(INDUSTRY_NAME == "Changes in inventories and acquisition less disposals of valuables") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
  merge.data.frame(CPA_PRODUCTS$Use_Basic, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::rename(Changes_in_inventories_and_acquisition_less_disposals_of_valuables = value)

CPA_PRODUCTS$Use_Basic <- USE_BASIC %>%
  dplyr::filter(INDUSTRY_NAME == "Exports FOB") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
  merge.data.frame(CPA_PRODUCTS$Use_Basic, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::rename(Exports_FOB = value)

CPA_PRODUCTS$Use_Basic$Exports_FOB[is.na(CPA_PRODUCTS$Use_Basic$Exports_FOB)] <- 0


CPA_PRODUCTS$Use_Basic <- CPA_PRODUCTS$Use_Basic %>%
  dplyr::mutate(Total_final_consumption_expenditure_at_basic_prices = psum(
    Final_consumption_expenditure_by_households,
    Final_consumption_expenditure_NPISH,
    Final_consumption_expenditure_by_government,
    na.rm=TRUE),
    Gross_capital_formation = psum(
      Gross_fixed_capital_formation,
      Changes_in_inventories_and_acquisition_less_disposals_of_valuables,
      na.rm=TRUE),
    Total_final_uses_at_basic_prices = psum(Total_final_consumption_expenditure_at_basic_prices,
                                            Gross_capital_formation,
                                            Exports_FOB,
                                            na.rm=TRUE),
    Total_use_at_basic_prices = psum(Total_intermediate_consumption_at_basic_prices,
                                     Total_final_uses_at_basic_prices,
                                     na.rm=TRUE))


NACE_INDUSTRIES$Use_Basic <- USE_BASIC %>% 
  dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
  dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
  dplyr::summarise(Total_intermediate_consumption_by_industries_at_basic_prices = sum(value, na.rm = T))


# 4.4 Use VAT matrix----

CPA_PRODUCTS$Use_VAT <- USE_VAT %>% 
  dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
  dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
  dplyr::summarise(Total_intermediate_consumption_at_basic_prices = sum(value, na.rm = T))

CPA_PRODUCTS$Use_VAT <- USE_VAT %>%
  dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by households") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
  merge.data.frame(CPA_PRODUCTS$Use_VAT, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::rename(Final_consumption_expenditure_by_households = value)


CPA_PRODUCTS$Use_VAT <- USE_VAT %>%
  dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by non-profit organisations serving households (NPISH)") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
  merge.data.frame(CPA_PRODUCTS$Use_VAT, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::rename(Final_consumption_expenditure_NPISH = value)


CPA_PRODUCTS$Use_VAT <- USE_VAT %>%
  dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by government") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
  merge.data.frame(CPA_PRODUCTS$Use_VAT, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::rename(Final_consumption_expenditure_by_government = value)


CPA_PRODUCTS$Use_VAT <- USE_VAT %>%
  dplyr::filter(INDUSTRY_NAME == "Gross fixed capital formation") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
  merge.data.frame(CPA_PRODUCTS$Use_VAT, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::rename(Gross_fixed_capital_formation = value)


CPA_PRODUCTS$Use_VAT <- USE_VAT %>%
  dplyr::filter(INDUSTRY_NAME == "Changes in inventories and acquisition less disposals of valuables") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
  merge.data.frame(CPA_PRODUCTS$Use_VAT, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::rename(Changes_in_inventories_and_acquisition_less_disposals_of_valuables = value)


CPA_PRODUCTS$Use_VAT <- USE_VAT %>%
  dplyr::filter(INDUSTRY_NAME == "Exports FOB") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
  merge.data.frame(CPA_PRODUCTS$Use_VAT, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::rename(Exports_FOB = value)


CPA_PRODUCTS$Use_VAT <- CPA_PRODUCTS$Use_VAT %>%
  dplyr::mutate(Total_final_consumption_expenditure_at_basic_prices = psum(Final_consumption_expenditure_by_households,
                                                                           Final_consumption_expenditure_NPISH,
                                                                           Final_consumption_expenditure_by_government,
                                                                           na.rm=TRUE),
                Gross_capital_formation = psum(Gross_fixed_capital_formation,
                                               Changes_in_inventories_and_acquisition_less_disposals_of_valuables,
                                               na.rm=TRUE),
                Total_final_uses_at_basic_prices = psum(Total_final_consumption_expenditure_at_basic_prices,
                                                        Gross_capital_formation,
                                                        Exports_FOB,
                                                        na.rm=TRUE),
                Total_use_at_basic_prices = psum(Total_intermediate_consumption_at_basic_prices,
                                                 Total_final_uses_at_basic_prices,
                                                 na.rm=TRUE))

NACE_INDUSTRIES$Use_VAT <- USE_VAT %>% 
  dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
  dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
  dplyr::summarise(Total_VAT = sum(value, na.rm = T))


})
