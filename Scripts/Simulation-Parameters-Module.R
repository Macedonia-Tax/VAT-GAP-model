# I.Importing libraries------------------------------------------------

library(DataEditR)
library(data.table)
library(tidyverse)
#library(tidyr)
#options(tidyr.quiet = TRUE)
library(readxl)
library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)
library(reshape2)
library(rccmisc) 
library(flexdashboard)
library(plotly)
library(data.table)
library(shades)
library(knitr)
library(DT)
library(rpivotTable)
library(openintro)
library(highcharter)
library(ggvis)
library(DataCombine)
library(viridisLite)
library(readxl)
library(reshape2)
options(warn=-1)
library(sm)
library(ks)
library(plyr)
library(openxlsx)



# II. Simulation parameters ------------------------------------------------



benchmark_tax_rate <- 0.18
standard_VAT_rate = 0.18
preferential_VAT_rate = 0.05

# # Name of the version of model
#version_vat_model<-c("VAT_Model_v9.15b.xlsx")

suppressMessages({ 
TAXABLE_PROPORTION_IMPORT <- read_excel("DATA/INPUT/VAT_Model_v9.15b.xlsx", sheet = "Simulation", col_names = F)[c(-1,-2,-3),] %>%
  data.frame()
})



# This is apply only on share on import
# Original 
# TAXABLE_PROPORTION_IMPORT<-TAXABLE_PROPORTION_IMPORT%>%
#   dplyr::select("...1","...10","...11","...12")%>%
#   dplyr:: rename(c("PRODUCT_INDUSTRY_CODE"= "...1",
#                    "Current_Policy_Exempt"="...10",
#                    "Current_Policy_Reduced_Rate"= "...11",
#                    "Current_Policy_Fully_Taxable"="...12"))%>%
#   dplyr::arrange(PRODUCT_INDUSTRY_CODE)



TAXABLE_PROPORTION_IMPORT<-TAXABLE_PROPORTION_IMPORT%>%
  dplyr::select("...1","...2","...10","...11","...12")%>%
  dplyr:: rename(c("PRODUCT_INDUSTRY_CODE"= "...1",
                   "Products(CPA)"="...2",
                   "Current_Policy_Exempt"="...10",
                   "Current_Policy_Reduced_Rate"= "...11",
                   "Current_Policy_Fully_Taxable"="...12"))%>%
  dplyr::arrange(PRODUCT_INDUSTRY_CODE)

#Original
#TAXABLE_PROPORTION_IMPORT[2:4]<-as.numeric(unlist(TAXABLE_PROPORTION_IMPORT[2:4]))

TAXABLE_PROPORTION_IMPORT[3:5]<-as.numeric(unlist(TAXABLE_PROPORTION_IMPORT[3:5]))





SIMULATION<-TAXABLE_PROPORTION_IMPORT
SIMULATION$Current_Policy_Exempt[42] = 0 #  Imputed rents of owner-occupied dwellings with the industry code: 68A/ 68A
SIMULATION$Standard_VAT_Rate = standard_VAT_rate
SIMULATION$Standard_VAT_Rate[42] = 0 # This is the industry: Imputed rents of owner-occupied dwellings with the industry code: 68A/ 68A
SIMULATION$Preferential_VAT_Rate = preferential_VAT_rate
SIMULATION$Simulation_Toggles_Exempt = NA
SIMULATION$Simulation_Toggles_Exempt[SIMULATION$PRODUCT_INDUSTRY_CODE == 85] = 1
SIMULATION$Simulation_Toggles_Exempt[SIMULATION$PRODUCT_INDUSTRY_CODE == 86] = 1
SIMULATION$Simulation_Toggles_Reduced_Rate = NA

# # Base for calculation 
SIMULATION_CALIBRATION_FACTOR<-SIMULATION


# Extract column names from SIMULATION
column_names <- colnames(SIMULATION_CALIBRATION_FACTOR)


# III. Interactive table -------------------------------------------------------


SIMULATION<-SIMULATION %>%
dplyr:: rename(c("CPA"="PRODUCT_INDUSTRY_CODE",
                 "Products(CPA)"="Products(CPA)",
                 "Exempt Share"="Current_Policy_Exempt",
                 "Reduced Rate Share"= "Current_Policy_Reduced_Rate",
                 "Fully Taxable Share"="Current_Policy_Fully_Taxable",
                 "Standard VAT Rate"="Standard_VAT_Rate",
                 "Reduced VAT Rate"="Preferential_VAT_Rate",
                 "Exempt Proportion"="Simulation_Toggles_Exempt",
                 "Reduced Rate Proportion"="Simulation_Toggles_Reduced_Rate"
                    ))


# Download pic
logo_pic <- 'https://1000logos.net/wp-content/uploads/2020/04/The_World_Bank_logo_PNG4.png'

#logo_pic <- 'https://raw.githubusercontent.com/DillonHammill/DataEditR/master/vignettes/DataEditR/DataEditR-Car.png'

SIMULATION<-data_edit(SIMULATION,logo =logo_pic,
                      logo_size = 80,
                      logo_side = "left",
                      title="Input policy parametars",
                      viewer_height = 1000,
                      viewer_width = 4900,
                      col_stretch = TRUE,
                      col_readonly = c("CPA","Products(CPA)","Exempt Share","Reduced Rate Share","Fully Taxable Share")
                      ) 

