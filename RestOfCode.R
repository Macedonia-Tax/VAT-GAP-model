# Rest of code





# 2. Define FUNCTIONS ----



#  The function creates an ntile group vector:
qgroup = function(numvec, n, na.rm=TRUE){
  qtile = quantile(numvec, probs = seq(0, 1, 1/n), na.rm)  # will pick up the value from the default setting
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



SIMULATION <- SIMULATION %>%
  dplyr::mutate(Simulated_Policy_Exempt = ifelse(is.na(Simulation_Toggles_Exempt), Current_Policy_Exempt, Simulation_Toggles_Exempt),
                Simulated_Policy_Reduced_Rate = ifelse(is.na(Simulation_Toggles_Reduced_Rate), Current_Policy_Reduced_Rate, Simulation_Toggles_Reduced_Rate),
                Simulated_Policy_Fully_Taxable = 1-Simulated_Policy_Exempt-Simulated_Policy_Reduced_Rate)



# This data are stored in the Excel file  from VAT_Model_v9.15a

# SUPPLY <- read_excel("VAT_Model_v9.15a.xlsx", sheet = "Supply", col_names = F)[c(-1,-2,-3,-4),] %>%
#   input_output_matrix_to_long_data()

SUPPLY <- read_excel(version_vat_model, sheet = "Supply", col_names = F)[c(-1,-2,-3,-4),] %>%
  input_output_matrix_to_long_data()

USE_PURCHASER <- read_excel(version_vat_model, sheet = "Use_Purchaser", col_names = F)[c(-1,-2,-3,-4),] %>%
  input_output_matrix_to_long_data()

USE_VAT <- read_excel(version_vat_model, sheet = "Use_VAT", col_names = F)[c(-1,-2,-3,-4),] %>%
  input_output_matrix_to_long_data()

USE_SUBSIDIES <- read_excel(version_vat_model, sheet = "Use_Subsidies", col_names = F)[c(-1,-2,-3,-4),] %>%
  input_output_matrix_to_long_data()

USE_OTHERTAXES <- read_excel(version_vat_model, sheet = "Use_OtherTaxes", col_names = F)[c(-1,-2,-3,-4),] %>%
  input_output_matrix_to_long_data()

USE_BASIC <- read_excel(version_vat_model, sheet = "Use_Basic", col_names = F)[c(-1,-2,-3,-4),] %>%
  input_output_matrix_to_long_data()

USE_IMPORTS <- read_excel(version_vat_model, sheet = "Use_Imports", col_names = F)[c(-1,-2,-3,-4),] %>%
  input_output_matrix_to_long_data()



# 3.2 COICOP table ------------------------------------------------------------


## Data from COICOP table

VAT_COICOP_NAMES <- read_excel(version_vat_model, sheet = "COICOP", col_names = F)[-c(1:2),c(2)]
VAT_COICOP_NAMES<-VAT_COICOP_NAMES[1:178,1]


VAT_COICOP_FC <- read_excel(version_vat_model, sheet = "COICOP", col_names = F)[-c(1:2),c(4:9)]
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


## Input raw concordance table

VAT_Concordance <- read_excel(version_vat_model, sheet = "Concordance", col_names = T)

# Merging table
VAT_COICOP_FINAL<-left_join(VAT_COICOP_FINAL_RAW,VAT_Concordance,by = c("COICOP_Descriptions"))


# Adjustment of CPA codes with Concordance table

COICOP <- read_excel(version_vat_model, sheet = "COICOP", col_names = F)[-c(1,2),-c(1:19)]
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

Merged_VAT_data <- read_excel("Merged_VAT_data.xlsx", sheet = "VAT_IMPORT_HS")



# 3.4 Taxable proportion -------------------------------------------
'This table will change taxabile proportion for estimation. Data here are estimated from 
                           data from import for goods and from data from COICOP for services
                          '


# 3.5 Concordance table NACE_SUT ----------------------------------

NACE_SUT_table <- read_excel("NACE_SUT_table.xlsx", sheet = "NACE")

CPA_COICOP_CONCORDANCE <- read_excel("NACE_SUT_table.xlsx", sheet = "CPA_COICOP_CONCORDANCE")


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
#data4_hbs <- read_excel("HSB/2016/data4_hbs.xls")
#data3_hbs2016<- read_excel("HSB/2016/data3_hbs2016.xls")

data4_hbs <- read_excel("HSB/2016/Data4_hbs2019.xlsx")
#data3_hbs2016<- read_excel("HSB/2016/Data3_hbs2019.xlsx")

# Setting columns names
data4_hbs<-data4_hbs%>%
  dplyr::select(-c('kvartal','Year'))


colnames(data4_hbs)<-c("number_hh","01","02","03","04","05","06","07","08","09","10","11","12","Consumption_own")

# Preparing data for merging 

data4_hbs_long<-data4_hbs%>%
  pivot_longer(!number_hh, names_to = "COICOP_section", values_to = "Expenditures")

# 4. AGGREGATE DATA IN LISTS -----

CPA_PRODUCTS <- as.list(c(1:4))
names(CPA_PRODUCTS) = c("Supply", "Use_Purchaser", "Use_VAT", "Use_Basic")

NACE_INDUSTRIES <- as.list(c(1:4))
names(NACE_INDUSTRIES) = c("Supply", "Use_Purchaser", "Use_VAT", "Use_Basic")



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
                                     # Exports_FOB,
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

# 5. ESTIMATION -----
# 5.1 Arrange Aggregate Industries and Products ---------------------------
# Arrange Aggregate Industries
NACE_INDUSTRIES$Supply <- NACE_INDUSTRIES$Supply %>%
  dplyr::arrange(INDUSTRY_CODE)
NACE_INDUSTRIES$Use_Purchaser <- NACE_INDUSTRIES$Use_Purchaser %>%
  dplyr::arrange(INDUSTRY_CODE)
NACE_INDUSTRIES$Use_VAT <- NACE_INDUSTRIES$Use_VAT %>%
  dplyr::arrange(INDUSTRY_CODE)
NACE_INDUSTRIES$Use_Basic <- NACE_INDUSTRIES$Use_Basic %>%
  dplyr::arrange(INDUSTRY_CODE)

# View(NACE_INDUSTRIES)

# Arrange Aggregate Products

CPA_PRODUCTS$Supply <- CPA_PRODUCTS$Supply %>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE)
CPA_PRODUCTS$Use_Purchaser <- CPA_PRODUCTS$Use_Purchaser %>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE)
CPA_PRODUCTS$Use_VAT <- CPA_PRODUCTS$Use_VAT %>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE)
CPA_PRODUCTS$Use_Basic <- CPA_PRODUCTS$Use_Basic %>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE)

# View(CPA_PRODUCTS)

# 5.2 Domestic Share matrix-----

CPA_PRODUCTS$Dom_Share <- CPA_PRODUCTS$Supply %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE)

CPA_PRODUCTS$Dom_Share$Domestic_share_of_output_commodity <- 
  pmax(1-CPA_PRODUCTS$Use_Basic$Exports_FOB/CPA_PRODUCTS$Supply$Total_output,0)


DOM_SHARE <- SUPPLY %>%
  dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
  dplyr::group_by(PRODUCT_INDUSTRY_CODE) %>%
  dplyr::arrange(INDUSTRY_CODE) %>%
  dplyr::mutate(value = value/NACE_INDUSTRIES$Supply$Total_output_by_industries_at_basic_prices) %>%
  merge.data.frame(CPA_PRODUCTS$Dom_Share, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::mutate(value = value*Domestic_share_of_output_commodity) %>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)


NACE_INDUSTRIES$Dom_Share <- DOM_SHARE %>% 
  dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
  dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
  dplyr::summarise(Domestic_Share_of_Output_Industry = sum(value, na.rm = T))


# 5.3 Net purchaser prices -----------

USE_NETPURCH <- USE_PURCHASER %>%
  dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)


USE_VAT_temp <- USE_VAT %>% 
  dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)

USE_NETPURCH$value[is.na(USE_NETPURCH$value)] <- 0
USE_VAT_temp$value[is.na(USE_VAT_temp$value)] <- 0

USE_NETPURCH$value <- USE_NETPURCH$value - USE_VAT_temp$value 

CPA_PRODUCTS$Use_NetPurch <- USE_NETPURCH %>% 
  dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
  dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
  dplyr::summarise(Total_intermediate_consumption_net_purchasers_prices = sum(value, na.rm = T))


CPA_PRODUCTS$Use_NetPurch <- CPA_PRODUCTS$Use_NetPurch %>% 
  dplyr::arrange(PRODUCT_INDUSTRY_CODE)


CPA_PRODUCTS$Use_Purchaser$Gross_fixed_capital_formation[is.na(CPA_PRODUCTS$Use_Purchaser$Gross_fixed_capital_formation)] <- 0
CPA_PRODUCTS$Use_VAT$Gross_fixed_capital_formation[is.na(CPA_PRODUCTS$Use_VAT$Gross_fixed_capital_formation)] <- 0


CPA_PRODUCTS$Use_NetPurch$Gross_fixed_capital_formation = 
  CPA_PRODUCTS$Use_Purchaser$Gross_fixed_capital_formation - 
  CPA_PRODUCTS$Use_VAT$Gross_fixed_capital_formation    

# Here I set the missing values of "Gross_fixed_capital_formation" to zero!
CPA_PRODUCTS$Use_NetPurch$Gross_fixed_capital_formation[is.na(CPA_PRODUCTS$Use_NetPurch$Gross_fixed_capital_formation)] <- 0

CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_by_households[is.na( CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_by_households)] <- 0
CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_by_households[is.na(CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_by_households)] <- 0

CPA_PRODUCTS$Use_NetPurch$Final_consumption_expenditure_by_households = 
  CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_by_households - 
  CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_by_households

CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_NPISH[is.na(CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_NPISH)] <- 0
CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_NPISH[is.na(CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_NPISH)] <- 0


CPA_PRODUCTS$Use_NetPurch$Final_consumption_expenditure_NPISH = 
  CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_NPISH - 
  CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_NPISH

CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_by_government[is.na(CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_by_government)] <- 0
CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_by_government[is.na( CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_by_government)] <- 0

CPA_PRODUCTS$Use_NetPurch$Final_consumption_expenditure_by_government =
  CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_by_government - 
  CPA_PRODUCTS$Use_VAT$Final_consumption_expenditure_by_government


# 5.4 Capital Shift Map ----


CPA_PRODUCTS$K_Shift_Map <- USE_NETPURCH %>% 
  dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
  dplyr::distinct(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
  dplyr::mutate(Share_of_K_in_HH_FD = 0)

CPA_PRODUCTS$K_Shift_Map$Share_of_K_in_HH_FD[grepl("Constructions and construction works", CPA_PRODUCTS$K_Shift_Map$PRODUCT_INDUSTRY_NAME)] = 
  RC_prc_of_Constructions_and_construction_works


# WARNING: the formula includes (1-$BUx) but this seems to be not clear how it is caclulated. 
# It is zero for all cases but one where it takes value from the simulation of 0.7.
K_SHIFT_MAP <- USE_NETPURCH %>%
  merge.data.frame(CPA_PRODUCTS$Use_NetPurch, key = "PRODUCT_INDUSTRY_CODE") %>%
  merge.data.frame(CPA_PRODUCTS$K_Shift_Map, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::mutate(value = value/Total_intermediate_consumption_net_purchasers_prices*(1-Share_of_K_in_HH_FD)) %>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE) %>%
  dplyr::select(-Share_of_K_in_HH_FD)


CPA_PRODUCTS$K_Shift_Map$Final_consumption_expenditure_by_households = CPA_PRODUCTS$K_Shift_Map$Share_of_K_in_HH_FD
CPA_PRODUCTS$K_Shift_Map$Final_consumption_expenditure_NPISH = 0
CPA_PRODUCTS$K_Shift_Map$Final_consumption_expenditure_by_government = 0

# 5.5 Use Capital net purchaser prices -----

USE_K_NETPURCH <- K_SHIFT_MAP %>%
  merge.data.frame(CPA_PRODUCTS$Use_NetPurch, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::mutate(value = value*Gross_fixed_capital_formation) %>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)

USE_NETPURCH <- USE_NETPURCH %>% dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)


USE_K_NETPURCH$value <- USE_K_NETPURCH$value + USE_NETPURCH$value 


CPA_PRODUCTS$Use_NetPurch <- CPA_PRODUCTS$Use_NetPurch %>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME)

CPA_PRODUCTS$K_Shift_Map <- CPA_PRODUCTS$K_Shift_Map %>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME)


CPA_PRODUCTS$Use_K_NetPurch <- CPA_PRODUCTS$Use_NetPurch %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME)

CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_by_households <-
  CPA_PRODUCTS$Use_NetPurch$Final_consumption_expenditure_by_households+
  CPA_PRODUCTS$K_Shift_Map$Final_consumption_expenditure_by_households*
  CPA_PRODUCTS$Use_NetPurch$Gross_fixed_capital_formation

CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_NPISH <-
  CPA_PRODUCTS$Use_NetPurch$Final_consumption_expenditure_NPISH+
  CPA_PRODUCTS$K_Shift_Map$Final_consumption_expenditure_NPISH*
  CPA_PRODUCTS$Use_NetPurch$Gross_fixed_capital_formation


# Replace NAs with zero
CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_NPISH[is.na(CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_NPISH)] <- 0


CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_by_government <-
  CPA_PRODUCTS$Use_NetPurch$Final_consumption_expenditure_by_government+
  CPA_PRODUCTS$K_Shift_Map$Final_consumption_expenditure_by_government*
  CPA_PRODUCTS$Use_NetPurch$Gross_fixed_capital_formation

# Replace NAs with zero
CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_by_government[is.na(CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_by_government)] <- 0

# 5.6 Use_K_Dom_NetPurch -----

USE_K_DOM_NETPURCH <- USE_K_NETPURCH %>%
  dplyr::select(PRODUCT_INDUSTRY_NAME, PRODUCT_INDUSTRY_CODE, INDUSTRY_NAME, INDUSTRY_CODE, value) %>%
  merge.data.frame(NACE_INDUSTRIES$Dom_Share, key = "INDUSTRY_CODE") %>%
  dplyr::mutate(value = value*Domestic_Share_of_Output_Industry) %>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)


# 5.7 Supply_Dom -----

SUPPLY_DOM <- SUPPLY %>%
  dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
  merge.data.frame(CPA_PRODUCTS$Dom_Share, key = "PRODUCT_INDUSTRY_CODE") %>%
  dplyr::mutate(value = value*Domestic_share_of_output_commodity) %>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)

NACE_INDUSTRIES$Supply_Dom <- SUPPLY_DOM %>% 
  dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
  dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
  dplyr::summarise(Total_output_by_industries_at_basic_prices = sum(value, na.rm = T))

# 6. SIMULATION ----
# 6.1 Pre-processing COICOP and setting parameters for simulation ------------------------------------------
# 6.1.1 Aggregate table for COICOP----------------------------------------

# New  Aggregation for FINAL COICOP TABLE
SUPPLY_DOM_AGGREGATE_1<-SUPPLY_DOM%>%
  dplyr::select(PRODUCT_INDUSTRY_CODE,value)%>%
  dplyr::group_by(PRODUCT_INDUSTRY_CODE)%>%
  dplyr::summarize(value=sum(value,na.rm = TRUE))%>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE)

SUPPLY_DOM_AGGREGATE_1$value[is.na(SUPPLY_DOM_AGGREGATE_1$value)] <- 0

CPA_PRODUCTS_1<-CPA_PRODUCTS$Supply%>%
  dplyr::select(PRODUCT_INDUSTRY_CODE,Imports_CIF)%>%
  dplyr::group_by(PRODUCT_INDUSTRY_CODE )%>%
  dplyr::summarize(value=sum(Imports_CIF,na.rm = TRUE))

CPA_PRODUCTS_1$value[is.na(CPA_PRODUCTS_1$value)] <- 0

# Sum of columns for Base
COICOP$Base <- SUPPLY_DOM_AGGREGATE_1$value + CPA_PRODUCTS_1$value

# Adding columns for Exempt	and Reduced-Rate Levels
VAT_COICOP_FINAL_GROUPED<-VAT_COICOP_FINAL%>%
  dplyr::select(Reduce_or_Exempted,EX,Reduced_Rate_5,Standard_Rate_18)%>%
  dplyr::group_by(Reduce_or_Exempted )%>%
  dplyr::summarize(EX=sum(as.numeric(EX),na.rm = TRUE),
                   Reduced_Rate_5=sum(Reduced_Rate_5,na.rm = TRUE))

COICOP_MERGED<-left_join(COICOP,VAT_COICOP_FINAL_GROUPED,by = c("PRODUCT_INDUSTRY_CODE"="Reduce_or_Exempted"))

# This column is calculated
COICOP_MERGED$Negatives<-pmin((COICOP_MERGED$Base-COICOP_MERGED$EX-COICOP_MERGED$Reduced_Rate_5),0)

# Adjustment-This column is not calculated  <---- WARNING !!!!!
COICOP_MERGED$Exempt_Adjustment<-as.numeric(as.character(COICOP_MERGED$Exempt_Adjustment))
COICOP_MERGED$Reduced_Rate_Adjustment <-as.numeric(as.character(COICOP_MERGED$Reduced_Rate_Adjustment ))

# Levels - This column is calculated
COICOP_MERGED$Exempt_Adjustment[is.na(COICOP_MERGED$Exempt_Adjustment)] <- 0
COICOP_MERGED$Exempt_Levels_2<-as.numeric(COICOP_MERGED$Exempt_Levels)+as.numeric(COICOP_MERGED$Exempt_Adjustment)

COICOP_MERGED$Reduced_Rate_Adjustment[is.na(COICOP_MERGED$Reduced_Rate_Adjustment)] <- 0
COICOP_MERGED$Reduced_Rate_Levels_2<-as.numeric(COICOP_MERGED$Reduced_Rate_Levels)+as.numeric(COICOP_MERGED$Reduced_Rate_Adjustment)

# Raw - This column is calculated
COICOP_MERGED$Exempt_Raw_perc<-COICOP_MERGED$Exempt_Levels_2/COICOP_MERGED$Base
COICOP_MERGED$Reduced_Rate_Raw_perc<-COICOP_MERGED$Reduced_Rate_Levels_2/COICOP_MERGED$Base

# Capped - This column is calculated
COICOP_MERGED$Exempt_Capped_perc<-pmax(pmin(COICOP_MERGED$Exempt_Raw_perc,1),0)
COICOP_MERGED$Reduced_Rate_Capped_perc<-pmax(pmin(COICOP_MERGED$Reduced_Rate_Raw_perc,1),0)

COICOP_MERGED[is.na(COICOP_MERGED)] <- 0

# 6.1.3 Estimation of TE part -----------------------------------------------------
'
    Warning
    This part begins with copy-paste of the parameters entered previously and from this point begins with their new calculation with the aim of obtaining TE
    '


SIMULATION_1 = copy(SIMULATION)


# 6.1.4 Estimation of effective VAT RATE ----------------------------------------------------------

SIMULATION_2 = copy(SIMULATION)

SIMULATION_2$Simulation_Toggles_Reduced_Rate<-TE_EXEMPT



SIMULATION_2 <- SIMULATION_2 %>%
  dplyr::mutate(Simulated_Policy_Exempt = ifelse(is.na(Simulation_Toggles_Exempt), Current_Policy_Exempt, Simulation_Toggles_Exempt),
                Simulated_Policy_Reduced_Rate = ifelse(is.na(Simulation_Toggles_Reduced_Rate), Current_Policy_Reduced_Rate, Simulation_Toggles_Reduced_Rate),
                Simulated_Policy_Fully_Taxable = 1-Simulated_Policy_Exempt-Simulated_Policy_Reduced_Rate)


# 6.2 Benchmark revenues -----

benchmark_tax_rate <- benchmark_tax_rate
# I use the data from "Use_K_NetPurch" instead of "Use_K_Dom_NetPurch" to avoid redundancies because it is the same data.

CPA_PRODUCTS$BM_Rev <- CPA_PRODUCTS$Use_K_NetPurch %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, Final_consumption_expenditure_by_households) %>%
  dplyr::mutate(Final_Demand_HH = Final_consumption_expenditure_by_households*benchmark_tax_rate) %>%
  dplyr::select(-Final_consumption_expenditure_by_households)

# I set NA as well as the value of industry "Imputed rents of owner-occupied dwellings" to zero following the excel document.
CPA_PRODUCTS$BM_Rev$Final_Demand_HH[is.na(CPA_PRODUCTS$BM_Rev$Final_Demand_HH)] <- 0
CPA_PRODUCTS$BM_Rev$Final_Demand_HH[grepl("Imputed rents of owner-occupied dwellings", CPA_PRODUCTS$BM_Rev$PRODUCT_INDUSTRY_NAME)] <- 0

# I use the data from "Use_K_NetPurch" instead of "Use_K_Dom_NetPurch" to avoid redundancies because it is the same data.
CPA_PRODUCTS$BM_Rev$Final_Demand_NPISH <- CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_NPISH*benchmark_tax_rate

# I set NA as well as the value of industry "Imputed rents of owner-occupied dwellings" to zero following the excel document.
CPA_PRODUCTS$BM_Rev$Final_Demand_NPISH[is.na(CPA_PRODUCTS$BM_Rev$Final_Demand_NPISH)] <- 0
CPA_PRODUCTS$BM_Rev$Final_Demand_NPISH[grepl("Imputed rents of owner-occupied dwellings", CPA_PRODUCTS$BM_Rev$PRODUCT_INDUSTRY_NAME)] <- 0


# I use the data from "Use_K_NetPurch" instead of "Use_K_Dom_NetPurch" to avoid redundancies because it is the same data.
CPA_PRODUCTS$BM_Rev$Final_Demand_Government <- CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_by_government*benchmark_tax_rate


# I set NA as well as the value of industry "Imputed rents of owner-occupied dwellings" to zero following the excel document.
CPA_PRODUCTS$BM_Rev$Final_Demand_Government[is.na(CPA_PRODUCTS$BM_Rev$Final_Demand_Government)] <- 0
CPA_PRODUCTS$BM_Rev$Final_Demand_Government[grepl("Imputed rents of owner-occupied dwellings", CPA_PRODUCTS$BM_Rev$PRODUCT_INDUSTRY_NAME)] <- 0


# Make the Toal Final Demand
# Note that "Total Revenues from Intermediate Inputs" is not included in the "Final_Demand_Total" because the all values of "BM Rev" are zero

# We set "Final_Demand_HH" to zero for "Constructions and construction works" following the model given in excel
CPA_PRODUCTS$BM_Rev$Final_Demand_HH[grepl("Constructions and construction works", CPA_PRODUCTS$BM_Rev$PRODUCT_INDUSTRY_NAME)] = 0


CPA_PRODUCTS$BM_Rev <- CPA_PRODUCTS$BM_Rev %>%
  dplyr::mutate(Final_Demand_Total = psum(Final_Demand_HH, Final_Demand_NPISH, Final_Demand_Government))

Sum_of_Final_Demand_Total = sum(CPA_PRODUCTS$BM_Rev$Final_Demand_Total)

# 6.3 Est.IS ----
# 6.3.1 Main estimation  --------------------------------------------------

EST.IS <- merge.data.frame(SUPPLY_DOM, SIMULATION, key = "PRODUCT_INDUSTRY_CODE")
EST.IS$value = EST.IS$value*EST.IS$Simulated_Policy_Exempt

EST.IS <- EST.IS %>% select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, INDUSTRY_CODE, INDUSTRY_NAME, value)
EST.IS$value[is.na(EST.IS$value)] <- 0

NACE_INDUSTRIES$Est.IS <- EST.IS %>% 
  dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
  dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
  dplyr::summarise(Industry_Share = sum(value, na.rm = T))

NACE_INDUSTRIES$Est.IS <- NACE_INDUSTRIES$Est.IS %>%
  dplyr::arrange(INDUSTRY_CODE)

NACE_INDUSTRIES$Supply_Dom <- NACE_INDUSTRIES$Supply_Dom %>%
  dplyr::arrange(INDUSTRY_CODE)

NACE_INDUSTRIES$Est.IS$Industry_Share <- NACE_INDUSTRIES$Est.IS$Industry_Share/NACE_INDUSTRIES$Supply_Dom$Total_output_by_industries_at_basic_prices

NACE_INDUSTRIES$Est.IS$Industry_Share[is.na(NACE_INDUSTRIES$Est.IS$Industry_Share)] <- 0

# 6.3.2 Estimation of TE part ---------------------------------------------------

# Copy paste table
SUPPLY_DOM_1 = copy(SUPPLY_DOM)
NACE_INDUSTRIES_1 = copy(NACE_INDUSTRIES)

EST.IS_1 <- merge.data.frame(SUPPLY_DOM_1, SIMULATION_1, key = "PRODUCT_INDUSTRY_CODE")
EST.IS_1$value = EST.IS_1$value*EST.IS_1$Simulated_Policy_Exempt

EST.IS_1 <- EST.IS_1 %>% select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, INDUSTRY_CODE, INDUSTRY_NAME, value)
EST.IS_1$value[is.na(EST.IS_1$value)] <- 0

NACE_INDUSTRIES_1$Est.IS <- EST.IS_1 %>% 
  dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
  dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
  dplyr::summarise(Industry_Share = sum(value, na.rm = T))

NACE_INDUSTRIES_1$Est.IS <- NACE_INDUSTRIES_1$Est.IS %>%
  dplyr::arrange(INDUSTRY_CODE)

NACE_INDUSTRIES_1$Supply_Dom <- NACE_INDUSTRIES_1$Supply_Dom %>%
  dplyr::arrange(INDUSTRY_CODE)

NACE_INDUSTRIES_1$Est.IS$Industry_Share <- NACE_INDUSTRIES_1$Est.IS$Industry_Share/NACE_INDUSTRIES_1$Supply_Dom$Total_output_by_industries_at_basic_prices

NACE_INDUSTRIES_1$Est.IS$Industry_Share[is.na(NACE_INDUSTRIES_1$Est.IS$Industry_Share)] <- 0

# 6.3.4 Estimation of effective VAT RATE ----------------------------------------
# Copy paste table
SUPPLY_DOM_2 = copy(SUPPLY_DOM)
NACE_INDUSTRIES_2 = copy(NACE_INDUSTRIES)

EST.IS_2 <- merge.data.frame(SUPPLY_DOM_2, SIMULATION_2, key = "PRODUCT_INDUSTRY_CODE")
EST.IS_2$value = EST.IS_2$value*EST.IS_2$Simulated_Policy_Exempt

EST.IS_2 <- EST.IS_2 %>% select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, INDUSTRY_CODE, INDUSTRY_NAME, value)
EST.IS_2$value[is.na(EST.IS_2$value)] <- 0

NACE_INDUSTRIES_2$Est.IS <- EST.IS_2 %>% 
  dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
  dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
  dplyr::summarise(Industry_Share = sum(value, na.rm = T))

NACE_INDUSTRIES_2$Est.IS <- NACE_INDUSTRIES_2$Est.IS %>%
  dplyr::arrange(INDUSTRY_CODE)

NACE_INDUSTRIES_2$Supply_Dom <- NACE_INDUSTRIES_2$Supply_Dom %>%
  dplyr::arrange(INDUSTRY_CODE)

NACE_INDUSTRIES_2$Est.IS$Industry_Share <- NACE_INDUSTRIES_2$Est.IS$Industry_Share/NACE_INDUSTRIES_2$Supply_Dom$Total_output_by_industries_at_basic_prices

NACE_INDUSTRIES_2$Est.IS$Industry_Share[is.na(NACE_INDUSTRIES_2$Est.IS$Industry_Share)] <- 0

# 6.4 Est Rev ----
# 6.4.1 Main estimation ----------------------------------------------------
EST_REV <- USE_K_DOM_NETPURCH %>% 
  merge.data.frame(NACE_INDUSTRIES$Est.IS, key = "INDUSTRY_NAME") %>%
  merge.data.frame(SIMULATION, key = "PRODUCT_INDUSTRY_NAME") %>%
  dplyr::mutate(value = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*Industry_Share*value) %>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)

CPA_PRODUCTS$Est_Rev <- EST_REV %>% 
  dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
  dplyr::summarise(Total_Revenues_from_Intermediate_Inputs = sum(value, na.rm = T)) %>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE)

CPA_PRODUCTS$Est_Rev <- CPA_PRODUCTS$Use_K_NetPurch %>% 
  merge.data.frame(SIMULATION, key = "PRODUCT_INDUSTRY_NAME") %>%
  dplyr::mutate(Final_Demand_HH = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                  Final_consumption_expenditure_by_households,
                Final_Demand_NPISH = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                  Final_consumption_expenditure_NPISH,
                Final_Demand_Government = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                  Final_consumption_expenditure_by_government) %>%
  merge.data.frame(CPA_PRODUCTS$Est_Rev, key = "PRODUCT_INDUSTRY_NAME") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, Total_Revenues_from_Intermediate_Inputs, Final_Demand_HH, Final_Demand_NPISH, Final_Demand_Government) %>%
  dplyr::arrange(PRODUCT_INDUSTRY_NAME)

# Selection is for Constructions and construction works industry and number 62 should be replace by industry name 
CPA_PRODUCTS$Est_Rev$Final_Demand_HH[CPA_PRODUCTS$Est_Rev$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"] <- 
  CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_by_households[CPA_PRODUCTS$Use_K_NetPurch$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"]*
  (1-SIMULATION$Simulated_Policy_Exempt[62])*vat_rate_on_residential_construction




CPA_PRODUCTS$Est_Rev$Final_Demand_Total = psum(CPA_PRODUCTS$Est_Rev$Total_Revenues_from_Intermediate_Inputs, 
                                               CPA_PRODUCTS$Est_Rev$Final_Demand_HH, 
                                               CPA_PRODUCTS$Est_Rev$Final_Demand_NPISH, 
                                               CPA_PRODUCTS$Est_Rev$Final_Demand_Government, na.rm = T)  

# Extracting NACE names
NACE_NAMES<-CPA_PRODUCTS$Est_Rev%>%
  select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME)

# Note that the variable "Final_Demand_HH" for industry "Constructions and construction works" is caclulated using different formula in the excel document.

# Test for calibration factor

#sum(CPA_PRODUCTS$Est_Rev$Final_Demand_Total,na.rm = TRUE)/sum(CPA_PRODUCTS$BM_Rev$Final_Demand_Total,na.rm = TRUE)



# 6.4.2 Estimation of TE part ---------------------------------------------------
USE_K_DOM_NETPURCH_1 = copy(USE_K_DOM_NETPURCH)
CPA_PRODUCTS_1 = copy(CPA_PRODUCTS)

EST_REV_1 <- USE_K_DOM_NETPURCH_1 %>% 
  merge.data.frame(NACE_INDUSTRIES_1$Est.IS, key = "INDUSTRY_NAME") %>%
  merge.data.frame(SIMULATION_1, key = "PRODUCT_INDUSTRY_NAME") %>%
  dplyr::mutate(value = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*Industry_Share*value) %>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)

CPA_PRODUCTS_1$Est_Rev <- EST_REV_1 %>% 
  dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
  dplyr::summarise(Total_Revenues_from_Intermediate_Inputs = sum(value, na.rm = T)) %>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE)

CPA_PRODUCTS_1$Est_Rev <- CPA_PRODUCTS_1$Use_K_NetPurch %>% 
  merge.data.frame(SIMULATION_1, key = "PRODUCT_INDUSTRY_NAME") %>%
  dplyr::mutate(Final_Demand_HH = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                  Final_consumption_expenditure_by_households,
                Final_Demand_NPISH = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                  Final_consumption_expenditure_NPISH,
                Final_Demand_Government = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                  Final_consumption_expenditure_by_government) %>%
  merge.data.frame(CPA_PRODUCTS_1$Est_Rev, key = "PRODUCT_INDUSTRY_NAME") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, Total_Revenues_from_Intermediate_Inputs, Final_Demand_HH, Final_Demand_NPISH, Final_Demand_Government) %>%
  dplyr::arrange(PRODUCT_INDUSTRY_NAME)

# Selection is for Constructions and construction works industry and number 62 should be replace by industry name 
CPA_PRODUCTS_1$Est_Rev$Final_Demand_HH[CPA_PRODUCTS_1$Est_Rev$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"] <- 
  CPA_PRODUCTS_1$Use_K_NetPurch$Final_consumption_expenditure_by_households[CPA_PRODUCTS_1$Use_K_NetPurch$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"]*
  (1-SIMULATION$Simulated_Policy_Exempt[62])*vat_rate_on_residential_construction

CPA_PRODUCTS_1$Est_Rev$Final_Demand_Total = psum(CPA_PRODUCTS_1$Est_Rev$Total_Revenues_from_Intermediate_Inputs, 
                                                 CPA_PRODUCTS_1$Est_Rev$Final_Demand_HH, 
                                                 CPA_PRODUCTS_1$Est_Rev$Final_Demand_NPISH, 
                                                 CPA_PRODUCTS_1$Est_Rev$Final_Demand_Government, na.rm = T) 

# 6.4.3 Estimation of effective VAT RATE ----------------------------------------

USE_K_DOM_NETPURCH_2 = copy(USE_K_DOM_NETPURCH)
CPA_PRODUCTS_2 = copy(CPA_PRODUCTS)

EST_REV_2 <- USE_K_DOM_NETPURCH_2 %>% 
  merge.data.frame(NACE_INDUSTRIES_2$Est.IS, key = "INDUSTRY_NAME") %>%
  merge.data.frame(SIMULATION_2, key = "PRODUCT_INDUSTRY_NAME") %>%
  dplyr::mutate(value = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*Industry_Share*value) %>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)

CPA_PRODUCTS_2$Est_Rev <- EST_REV_2 %>% 
  dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
  dplyr::summarise(Total_Revenues_from_Intermediate_Inputs = sum(value, na.rm = T)) %>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE)

CPA_PRODUCTS_2$Est_Rev <- CPA_PRODUCTS_2$Use_K_NetPurch %>% 
  merge.data.frame(SIMULATION_2, key = "PRODUCT_INDUSTRY_NAME") %>%
  dplyr::mutate(Final_Demand_HH = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                  Final_consumption_expenditure_by_households,
                Final_Demand_NPISH = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                  Final_consumption_expenditure_NPISH,
                Final_Demand_Government = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                  Final_consumption_expenditure_by_government) %>%
  merge.data.frame(CPA_PRODUCTS_2$Est_Rev, key = "PRODUCT_INDUSTRY_NAME") %>%
  dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, Total_Revenues_from_Intermediate_Inputs, Final_Demand_HH, Final_Demand_NPISH, Final_Demand_Government) %>%
  dplyr::arrange(PRODUCT_INDUSTRY_NAME)

# Selection is for Constructions and construction works industry and number 62 should be replace by industry name 
CPA_PRODUCTS_2$Est_Rev$Final_Demand_HH[CPA_PRODUCTS_2$Est_Rev$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"] <- 
  CPA_PRODUCTS_2$Use_K_NetPurch$Final_consumption_expenditure_by_households[CPA_PRODUCTS_2$Use_K_NetPurch$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"]*
  (1-SIMULATION$Simulated_Policy_Exempt[62])*vat_rate_on_residential_construction

CPA_PRODUCTS_2$Est_Rev$Final_Demand_Total = psum(CPA_PRODUCTS_2$Est_Rev$Total_Revenues_from_Intermediate_Inputs, 
                                                 CPA_PRODUCTS_2$Est_Rev$Final_Demand_HH, 
                                                 CPA_PRODUCTS_2$Est_Rev$Final_Demand_NPISH, 
                                                 CPA_PRODUCTS_2$Est_Rev$Final_Demand_Government, na.rm = T) 


# 7. SIMULATION RESULTS ----  
# 7.1.1 Main estimation --------------------------------------------------

Results <- as.list(c("VAT_Gap", "Simulation"))
names(Results) <- c("VAT_Gap", "Simulation")


Results$VAT_Gap <- as.data.frame(sum(CPA_PRODUCTS$BM_Rev$Final_Demand_Total))
colnames(Results$VAT_Gap) <- "Benchmark_VAT_M_of_denars"

Results$VAT_Gap$Uncalibrated_VAT_Est.M_of_denars <- sum(CPA_PRODUCTS$Est_Rev$Final_Demand_Total, na.rm = T)



Results$VAT_Gap$Calibrated_VAT_Est.M_of_denars <- sum(CPA_PRODUCTS$Est_Rev$Final_Demand_Total, na.rm = T)*Locked_Calibration_Factor

Results$VAT_Gap$VAT_Control_Total.M_of_denars <- sum(CPA_PRODUCTS$Use_VAT$Total_use_at_basic_prices, na.rm = T) - sum(CPA_PRODUCTS$Use_VAT$Exports_FOB, na.rm = T)

Results$VAT_Gap$Total_VAT_Gap.M_of_denars <- Results$VAT_Gap$Benchmark_VAT_M_of_denars - Results$VAT_Gap$VAT_Control_Total.M_of_denars
Results$VAT_Gap$Total_VAT_Gap.Prc <- Results$VAT_Gap$Total_VAT_Gap.M_of_denars/Results$VAT_Gap$VAT_Control_Total.M_of_denars

# Manual input 

Results$VAT_Gap$Policy_Gap.M_of_denars <- 24407  # <------- Manual input in simulation



Results$VAT_Gap$Policy_Gap.Prc <- Results$VAT_Gap$Policy_Gap.M_of_denars/Results$VAT_Gap$VAT_Control_Total.M_of_denars

Results$VAT_Gap$Compliance_Gap.M_of_denars <- Results$VAT_Gap$Total_VAT_Gap.M_of_denars-Results$VAT_Gap$Policy_Gap.M_of_denars

Results$VAT_Gap$Compliance_Gap.Prc <- Results$VAT_Gap$Compliance_Gap.M_of_denars/Results$VAT_Gap$VAT_Control_Total.M_of_denars

Results$VAT_Gap$Calibration_Factor <- Locked_Calibration_Factor #0.755207774941413



# Final output - Change in Revenues


Results$Simulation <- as.data.frame(as.integer(Results$VAT_Gap$Calibrated_VAT_Est.M_of_denars-Results$VAT_Gap$VAT_Control_Total.M_of_denars))
colnames(Results$Simulation) <- "Simulated_Change_in_Revenues.M_of_denars"

Results$Simulation$Simulated_Change_in_Revenues.Prc <- (Results$Simulation$Simulated_Change_in_Revenues.M_of_denars/Results$VAT_Gap$VAT_Control_Total.M_of_denars)*100

# View(Results)


# 7.1.2 Estimation of TE part ---------------------------------------------------

Results_1 <- as.list(c("VAT_Gap", "Simulation"))
names(Results_1) <- c("VAT_Gap", "Simulation")


Results_1$VAT_Gap <- as.data.frame(sum(CPA_PRODUCTS_1$BM_Rev$Final_Demand_Total))
colnames(Results_1$VAT_Gap) <- "Benchmark_VAT_M_of_denars"

Results_1$VAT_Gap$Uncalibrated_VAT_Est.M_of_denars <- sum(CPA_PRODUCTS_1$Est_Rev$Final_Demand_Total, na.rm = T)



Results_1$VAT_Gap$Calibrated_VAT_Est.M_of_denars <- sum(CPA_PRODUCTS_1$Est_Rev$Final_Demand_Total, na.rm = T)*Locked_Calibration_Factor

Results_1$VAT_Gap$VAT_Control_Total.M_of_denars <- sum(CPA_PRODUCTS_1$Use_VAT$Total_use_at_basic_prices, na.rm = T) - sum(CPA_PRODUCTS_1$Use_VAT$Exports_FOB, na.rm = T)

Results_1$VAT_Gap$Total_VAT_Gap.M_of_denars <- Results_1$VAT_Gap$Benchmark_VAT_M_of_denars - Results_1$VAT_Gap$VAT_Control_Total.M_of_denars
Results_1$VAT_Gap$Total_VAT_Gap.Prc <- Results_1$VAT_Gap$Total_VAT_Gap.M_of_denars/Results_1$VAT_Gap$VAT_Control_Total.M_of_denars

# Manual input 

Results_1$VAT_Gap$Policy_Gap.M_of_denars <- 24407  # <------- Manual input in simulation

Results_1$VAT_Gap$Policy_Gap.Prc <- Results_1$VAT_Gap$Policy_Gap.M_of_denars/Results_1$VAT_Gap$VAT_Control_Total.M_of_denars

Results_1$VAT_Gap$Compliance_Gap.M_of_denars <- Results_1$VAT_Gap$Total_VAT_Gap.M_of_denars-Results_1$VAT_Gap$Policy_Gap.M_of_denars

Results_1$VAT_Gap$Compliance_Gap.Prc <- Results_1$VAT_Gap$Compliance_Gap.M_of_denars/Results_1$VAT_Gap$VAT_Control_Total.M_of_denars


Results_1$VAT_Gap$Calibration_Factor <- Locked_Calibration_Factor #0.755207774941413


# Final output - Change in Revenues


Results_1$Simulation <- as.data.frame(as.integer(Results_1$VAT_Gap$Calibrated_VAT_Est.M_of_denars-Results_1$VAT_Gap$VAT_Control_Total.M_of_denars))
colnames(Results_1$Simulation) <- "Simulated_Change_in_Revenues.M_of_denars"

Results_1$Simulation$Simulated_Change_in_Revenues.Prc <- (Results_1$Simulation$Simulated_Change_in_Revenues.M_of_denars/Results_1$VAT_Gap$VAT_Control_Total.M_of_denars)*100


# Extracting part foR TE
# Results$VAT_Gap$Policy_Gap.M_of_denars<-Results_1$VAT_Gap$Policy_Gap.M_of_denars

#View(Results_1)

# 7.1.3 Estimation of effective VAT RATE------------------------------

Results_2 <- as.list(c("VAT_Gap", "Simulation"))
names(Results_2) <- c("VAT_Gap", "Simulation")


Results_2$VAT_Gap <- as.data.frame(sum(CPA_PRODUCTS_2$BM_Rev$Final_Demand_Total))
colnames(Results_2$VAT_Gap) <- "Benchmark_VAT_M_of_denars"

Results_2$VAT_Gap$Uncalibrated_VAT_Est.M_of_denars <- sum(CPA_PRODUCTS_2$Est_Rev$Final_Demand_Total, na.rm = T)



Results_2$VAT_Gap$Calibrated_VAT_Est.M_of_denars <- sum(CPA_PRODUCTS_2$Est_Rev$Final_Demand_Total, na.rm = T)*Locked_Calibration_Factor

Results_2$VAT_Gap$VAT_Control_Total.M_of_denars <- sum(CPA_PRODUCTS_2$Use_VAT$Total_use_at_basic_prices, na.rm = T) - sum(CPA_PRODUCTS_2$Use_VAT$Exports_FOB, na.rm = T)

Results_2$VAT_Gap$Total_VAT_Gap.M_of_denars <- Results_2$VAT_Gap$Benchmark_VAT_M_of_denars - Results_2$VAT_Gap$VAT_Control_Total.M_of_denars
Results_2$VAT_Gap$Total_VAT_Gap.Prc <- Results_2$VAT_Gap$Total_VAT_Gap.M_of_denars/Results_2$VAT_Gap$VAT_Control_Total.M_of_denars

# Manual input 

Results_2$VAT_Gap$Policy_Gap.M_of_denars <- 24407  # <------- Manual input in simulation

Results_2$VAT_Gap$Policy_Gap.Prc <- Results_2$VAT_Gap$Policy_Gap.M_of_denars/Results_2$VAT_Gap$VAT_Control_Total.M_of_denars

Results_2$VAT_Gap$Compliance_Gap.M_of_denars <- Results_2$VAT_Gap$Total_VAT_Gap.M_of_denars-Results_2$VAT_Gap$Policy_Gap.M_of_denars

Results_2$VAT_Gap$Compliance_Gap.Prc <- Results_2$VAT_Gap$Compliance_Gap.M_of_denars/Results_2$VAT_Gap$VAT_Control_Total.M_of_denars

Results_2$VAT_Gap$Calibration_Factor <- Locked_Calibration_Factor #0.755207774941413



# Final output - Change in Revenues

#Results_2$Simulation <- as.data.frame(Results_2$VAT_Gap$Calibrated_VAT_Est.M_of_denars-Results_2$VAT_Gap$VAT_Control_Total.M_of_denars)
Results_2$Simulation <- as.data.frame(as.integer(Results_2$VAT_Gap$Calibrated_VAT_Est.M_of_denars-Results_2$VAT_Gap$VAT_Control_Total.M_of_denars))
colnames(Results_2$Simulation) <- "Simulated_Change_in_Revenues.M_of_denars"

Results_2$Simulation$Simulated_Change_in_Revenues.Prc <- (Results_2$Simulation$Simulated_Change_in_Revenues.M_of_denars/Results_2$VAT_Gap$VAT_Control_Total.M_of_denars)*100


# 7.1.4 Other files -------------------------------------------------------
# 9.1 Result from main estimation
Export_Main_Results<- Results$VAT_Gap 
Simulation_Results<-Results$Simulation


Revenue_VAT_TOTAL<-CPA_PRODUCTS$Est_Rev%>%
  dplyr::select(PRODUCT_INDUSTRY_CODE,Total_Revenues_from_Intermediate_Inputs,Final_Demand_HH,Final_Demand_NPISH,Final_Demand_Government,Final_Demand_Total)

# Without text column
Revenue_VAT_TOTAL[2:6]<-Revenue_VAT_TOTAL[2:6]*Locked_Calibration_Factor

# 9.2  Result from tax expenditures estimation
# Result 1

Simulation_Results_1<-Results_1$Simulation
Simulation_Results_1_te<-Results_1$VAT_Gap$Policy_Gap.M_of_denars

Est_Rev1<-CPA_PRODUCTS_1$Est_Rev


# 8. ESTIMATION OF EFFECTIVE VAT RATE BY SECTORS -------------------------------------
# 8.1 Effective rate for : Industries,HH,NPISH and GOVERMENT First approach ----------------------------


#TAX_BASE_TOTAL<-CPA_PRODUCTS_3$Est_Rev%>%
TAX_BASE_TOTAL<-CPA_PRODUCTS$Est_Rev%>%
  dplyr::select(PRODUCT_INDUSTRY_CODE,Final_Demand_Total,Total_Revenues_from_Intermediate_Inputs,Final_Demand_HH,Final_Demand_NPISH,Final_Demand_Government)%>%
  dplyr::mutate(
    tax_base_INDUSTRIES=Total_Revenues_from_Intermediate_Inputs/standard_VAT_rate,
    tax_base_HH=Final_Demand_HH/standard_VAT_rate,
    tax_base_NPISH=Final_Demand_NPISH/standard_VAT_rate,
    tax_base_GOVERMENT=Final_Demand_Government/standard_VAT_rate,
    tax_base_TOTAL=Final_Demand_Total/standard_VAT_rate)%>%
  dplyr::select(PRODUCT_INDUSTRY_CODE,tax_base_INDUSTRIES,tax_base_HH,tax_base_NPISH,tax_base_GOVERMENT,tax_base_TOTAL)



# EFFECTIVE_VAT_RATES<-left_join(TAX_BASE_TOTAL,SIMULATION_3,by = c("PRODUCT_INDUSTRY_CODE"))%>%
EFFECTIVE_VAT_RATES<-left_join(TAX_BASE_TOTAL,SIMULATION,by = c("PRODUCT_INDUSTRY_CODE"))%>%
  dplyr::select(PRODUCT_INDUSTRY_CODE,Standard_VAT_Rate,Preferential_VAT_Rate,tax_base_INDUSTRIES,
                tax_base_HH,tax_base_NPISH,tax_base_GOVERMENT,tax_base_TOTAL,Current_Policy_Reduced_Rate,Current_Policy_Fully_Taxable,
                Simulated_Policy_Reduced_Rate,Simulated_Policy_Fully_Taxable
  )%>%
  dplyr::mutate(
    # VAT FROM PREFERENTIAL VAT RATES
    VAT_PREFERENTIAL_R_INDUSTRIES=tax_base_INDUSTRIES*Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate,
    VAT_PREFERENTIAL_R_HH=tax_base_HH*Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate,
    VAT_PREFERENTIAL_R_NPISH=tax_base_NPISH*Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate,
    VAT_PREFERENTIAL_R_GOVERMENT=tax_base_GOVERMENT*Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate,
    VAT_PREFERENTIAL_R_TOTAL=tax_base_TOTAL*Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate,
    # VAT FROM STANDARD RATES
    VAT_STANDARD_R_INDUSTRIES=tax_base_INDUSTRIES*Simulated_Policy_Fully_Taxable*Standard_VAT_Rate,
    VAT_STANDARD_R_HH=tax_base_HH*Simulated_Policy_Fully_Taxable*Standard_VAT_Rate,
    VAT_STANDARD_R_NPISH=tax_base_NPISH*Simulated_Policy_Fully_Taxable*Standard_VAT_Rate,
    VAT_STANDARD_R_GOVERMENT=tax_base_GOVERMENT*Simulated_Policy_Fully_Taxable*Standard_VAT_Rate,
    VAT_STANDARD_R_TOTAL=tax_base_TOTAL*Simulated_Policy_Fully_Taxable*Standard_VAT_Rate,
    
    # SUM VAT REVENUES FROM PREFERENTIAL PLUS VAT FROM STANDARD RATE
    VAT_R_INDUSTRIES= VAT_PREFERENTIAL_R_INDUSTRIES+VAT_STANDARD_R_INDUSTRIES,
    VAT_R_HH=VAT_PREFERENTIAL_R_HH+VAT_STANDARD_R_HH,
    VAT_R_NPISH=VAT_PREFERENTIAL_R_NPISH+VAT_STANDARD_R_NPISH,
    VAT_R_GOVERMENT=VAT_PREFERENTIAL_R_GOVERMENT+VAT_STANDARD_R_GOVERMENT,
    VAT_TOTAL_R_TOTAL=VAT_PREFERENTIAL_R_TOTAL+VAT_STANDARD_R_TOTAL,
    #  Effective VAT rate by NACE DIVISION
    EFFECTIVE_VAT_RATE_INDUSTRIES=VAT_R_INDUSTRIES/tax_base_INDUSTRIES,
    EFFECTIVE_VAT_RATE_HH= VAT_R_HH/tax_base_HH,
    EFFECTIVE_VAT_RATE_NPISH=VAT_R_NPISH/tax_base_NPISH,
    EFFECTIVE_VAT_RATE_GOVERMENT=VAT_R_GOVERMENT/tax_base_GOVERMENT,
    EFFECTIVE_VAT_RATE_TOTAL=VAT_TOTAL_R_TOTAL/tax_base_TOTAL)%>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE)

EFFECTIVE_VAT_RATES[is.na(EFFECTIVE_VAT_RATES)] <- 0


EFFECTIVE_VAT_RATES_HH<-EFFECTIVE_VAT_RATES%>%
  dplyr::select(PRODUCT_INDUSTRY_CODE,tax_base_HH,EFFECTIVE_VAT_RATE_HH)    



effective_vat_rates<-EFFECTIVE_VAT_RATES

effective_vat_rates_bu=effective_vat_rates
# 9. HBS Analysis ------------------------------------------------------------
# 9.1 Applying effective VAT rates with HBS ------------------------------------
'In this part, only VAT base from households are used.

                These bases come from the SUT table and are distributed by CPA (64 NACE divisions).
                Because of this, the COICOP structure was first calculated, and then the same percentages were applied to VAT Revenues from CPA. 
                In the end, effective VAT rates are applied.
                '
Revenue_VAT_TOTAL_HH<-Revenue_VAT_TOTAL%>%
  dplyr::select(PRODUCT_INDUSTRY_CODE,Final_Demand_HH)

VAT_COICOP_PROPORTIONS<-VAT_COICOP_FINAL_RAW %>%
  dplyr::select(Two_digits,FC,EX,Reduced_Rate_5,Standard_Rate_18) %>%
  dplyr::group_by(Two_digits) %>%
  dplyr::summarise(FC = sum(FC, na.rm = T),
                   VAT_BASE_COICOP_EX= sum(EX, na.rm = T),
                   VAT_BASE_COICOP_5 = sum(Reduced_Rate_5, na.rm = T),
                   VAT_BASE_COICOP_18 = sum(Standard_Rate_18, na.rm = T)
  ) %>%
  dplyr::mutate(VAT_BASE_COICOP_5_18=VAT_BASE_COICOP_5+VAT_BASE_COICOP_18)

VAT_COICOP_PROPORTIONS<-left_join(VAT_COICOP_PROPORTIONS,CPA_COICOP_CONCORDANCE,by = c("Two_digits"="COICOP_Division"))


# New
EFFECTIVE_VAT_RATES_HH_BASE<-EFFECTIVE_VAT_RATES_HH%>%
  dplyr::select(PRODUCT_INDUSTRY_CODE,tax_base_HH,EFFECTIVE_VAT_RATE_HH)

#View(EFFECTIVE_VAT_RATES_HH_BASE)

# Concordance
VAT_COICOP_PROPORTIONS_CPA_1<-left_join(VAT_COICOP_PROPORTIONS,EFFECTIVE_VAT_RATES_HH_BASE,by = c("CPA_COICOP"="PRODUCT_INDUSTRY_CODE"))%>%
  dplyr::select(CPA_COICOP,Two_digits,tax_base_HH,VAT_BASE_COICOP_5_18,EFFECTIVE_VAT_RATE_HH)%>%
  dplyr::group_by(CPA_COICOP) %>%
  dplyr::mutate(PCT = VAT_BASE_COICOP_5_18/sum(VAT_BASE_COICOP_5_18))%>%
  ungroup

#View(VAT_COICOP_PROPORTIONS_CPA_1)


# Sum of VAT new 18-11.2022
Revenue_VAT_TOTAL_HH_CONCORDANCE_FINAL<-VAT_COICOP_PROPORTIONS_CPA_1%>%
  #dplyr::mutate(PROXY_TAX_BASE_HH=tax_base_HH*PCT)%>%
  dplyr::mutate(PROXY_TAX_BASE_HH=VAT_BASE_COICOP_5_18*PCT)%>%
  #dplyr::mutate(VAT_ESTIMATED=VAT_BASE_COICOP_5_18*EFFECTIVE_VAT_RATE_HH)%>%
  dplyr::mutate(VAT_ESTIMATED=PROXY_TAX_BASE_HH*EFFECTIVE_VAT_RATE_HH)%>%
  dplyr::group_by(Two_digits)%>%
  #dplyr::summarise(PROXY_TAX_BASE_HH = sum(PROXY_TAX_BASE_HH, na.rm = T),VAT_ESTIMATED = sum(VAT_ESTIMATED, na.rm = T))
  #dplyr::summarise(PROXY_TAX_BASE_HH = sum(PROXY_TAX_BASE_HH, na.rm = T),VAT_ESTIMATED = sum(VAT_ESTIMATED, na.rm = T),EFFECTIVE_VAT_RATE_HH=mean(EFFECTIVE_VAT_RATE_HH, na.rm = T))
  dplyr::summarise(PROXY_TAX_BASE_HH = sum(PROXY_TAX_BASE_HH, na.rm = T),VAT_ESTIMATED = sum(VAT_ESTIMATED, na.rm = T))%>%
  dplyr::mutate(EFFECTIVE_VAT_RATE_HH=VAT_ESTIMATED/PROXY_TAX_BASE_HH)








#View(Revenue_VAT_TOTAL_HH_CONCORDANCE_FINAL)

# Effective VAT rate
Revenue_VAT_TOTAL_HH_CONCORDANCE_FINAL_1<-Revenue_VAT_TOTAL_HH_CONCORDANCE_FINAL%>%
  ##dplyr::mutate(EFFECTIVE_VAT_RATE_HH=VAT_ESTIMATED/PROXY_TAX_BASE_HH)
  #dplyr::mutate(EFFECTIVE_VAT_RATE_HH=round(VAT_ESTIMATED/PROXY_TAX_BASE_HH,1))
  dplyr::mutate(EFFECTIVE_VAT_RATE_HH=round(EFFECTIVE_VAT_RATE_HH,2))

Revenue_VAT_TOTAL_HH_CONCORDANCE_FINAL_1[is.na(Revenue_VAT_TOTAL_HH_CONCORDANCE_FINAL_1)] <- 0


# Merging with HBS
data4_hbs_long_merged<-left_join(data4_hbs_long,Revenue_VAT_TOTAL_HH_CONCORDANCE_FINAL_1,by = c("COICOP_section"="Two_digits"))%>%
  dplyr::select(-c(PROXY_TAX_BASE_HH,VAT_ESTIMATED))%>%
  dplyr::mutate(VAT_BASE_HH=Expenditures/(1+EFFECTIVE_VAT_RATE_HH),
                VAT_REVENUES_HH=VAT_BASE_HH*EFFECTIVE_VAT_RATE_HH)


# 9.2 Merging with HBS Available assets --------------------------------------------------------


#  Merging with other base

# Adding new
data4_hbs$Consumption_own<-NULL


data4_hbs<-data4_hbs%>%
  dplyr::mutate(total_consumption=`01`+`02`+`03`+`04`+`05`+`06`+`07`+`08`+`09`+`10`+`11`+`12`)

# Adding centiles and deciles groups 
FINAL_UNWEIGHTED_SAMPLE<-mutate(data4_hbs,
                                deciles=qgroup(total_consumption, 10),
                                centiles=qgroup(total_consumption, 100),)


data4_hbs_long_merged_deciles<-left_join(data4_hbs_long_merged,FINAL_UNWEIGHTED_SAMPLE,by = c("number_hh"="number_hh"))%>%
  dplyr::select(number_hh,COICOP_section,VAT_REVENUES_HH,centiles,deciles,total_consumption)

data4_hbs_wider_merged_deciles1<-data4_hbs_long_merged_deciles%>%    
  pivot_wider(
    names_from = COICOP_section,
    values_from = c(VAT_REVENUES_HH))%>%
  dplyr::mutate(VAT_TOTAL=`01`+`02`+`03`+`04`+`05`+`06`+`07`+`08`+`09`+`10`+`11`+`12`)%>%
  dplyr::select(-c("Consumption_own"))




# 10. EXPORT RESULTS IN EXCEL -----------------------------------------------
gc(TRUE)
library(xlsx)
# Export data for business as usual. Suffix for business as usual is 'bu'. Results are estimated without any changes in VAT Rates.
#For this data set this estimation is only need to done one time and after that this result can be compared as benchmark
# 
# write.xlsx(as.data.frame(Simulation_Results_1), file="export_data_bu.xlsx", sheetName="Results_1_bu", row.names=FALSE)
# write.xlsx(as.data.frame(Est_Rev1), file="export_data_bu.xlsx", sheetName="Est_Rev1_bu",append=TRUE, row.names=FALSE)
# write.xlsx(as.data.frame(effective_vat_rates_bu), file="export_data_bu.xlsx", sheetName="effective_vat_rates_bu",append=TRUE, row.names=FALSE)
# write.xlsx(as.data.frame(data4_hbs_wider_merged_deciles1), file="export_data_bu.xlsx", sheetName="hbs_bu",append=TRUE, row.names=FALSE)
# write.xlsx(as.data.frame(Revenue_VAT_TOTAL), file="export_data_bu.xlsx", sheetName="revenue_vat_total_bu",append=TRUE, row.names=FALSE)
# write.xlsx(as.data.frame(Simulation_Results_1_te), file="export_data_bu.xlsx", sheetName="te_bu",append=TRUE, row.names=FALSE)



# Unweight sample
write.xlsx(as.data.frame(Export_Main_Results), file="export_data.xlsx", sheetName="Main_Results", row.names=FALSE)
write.xlsx(as.data.frame(Revenue_VAT_TOTAL), file="export_data.xlsx", sheetName="Revenue_VAT_TOTAL", append=TRUE, row.names=FALSE)
write.xlsx(as.data.frame(Simulation_Results), file="export_data.xlsx", sheetName="Result_Simulation",append=TRUE, row.names=FALSE)
write.xlsx(as.data.frame(Simulation_Results_1), file="export_data.xlsx", sheetName="Results_1",append=TRUE, row.names=FALSE)
write.xlsx(as.data.frame(Est_Rev1), file="export_data.xlsx", sheetName="Est_Rev1",append=TRUE, row.names=FALSE)
write.xlsx(as.data.frame(effective_vat_rates), file="export_data.xlsx", sheetName="effective_vat_rates",append=TRUE, row.names=FALSE)

write.xlsx(as.data.frame(data4_hbs_wider_merged_deciles1), file="export_data.xlsx", sheetName="hbs",append=TRUE, row.names=FALSE)


write.xlsx(as.data.frame(SIMULATION), file="simulation.xlsx", sheetName="simulation", row.names=FALSE)
write.xlsx(as.data.frame(NACE_NAMES), file="simulation.xlsx", sheetName="NACE_NAMES",append=TRUE, row.names=FALSE)


