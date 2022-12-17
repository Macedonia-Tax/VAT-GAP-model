
'
    Forcast VAT revenues
'

setwd(path)
getwd()


      # Results from main simulation
      DS<-Results$Simulation$Simulated_Change_in_Revenues.M_of_denars
      TE<-Results$VAT_Gap$Policy_Gap.M_of_denars
      # Adding new column
      MACRO_FISCAL_INDICATORS<-MACRO_FISCAL_INDICATORS%>%
        dplyr::mutate(
                      Result_Simulation=0,
                      TE_Simulation=0
                      )

# Simulation_VAT_NET=Nominal_VAT_NET,

        MACRO_FISCAL_INDICATORS[20,4]<-DS
        MACRO_FISCAL_INDICATORS[20,5]<-TE
        
        MACRO_FISCAL_INDICATORS<-MACRO_FISCAL_INDICATORS%>%
          dplyr::mutate(
            DS_Share_VAT=Result_Simulation/Nominal_GDP,
            TE_Share_VAT=TE_Simulation/Nominal_GDP
            )
          
        DS_Share_VAT<-MACRO_FISCAL_INDICATORS[20,6]
        TE_Share_VAT<-MACRO_FISCAL_INDICATORS[20,7]
        
        MACRO_FISCAL_INDICATORS[21,6]<-DS_Share_VAT$DS_Share_VAT
        MACRO_FISCAL_INDICATORS[22,6]<-DS_Share_VAT$DS_Share_VAT

        MACRO_FISCAL_INDICATORS[21,7]<-TE_Share_VAT$TE_Share_VAT
        MACRO_FISCAL_INDICATORS[22,7]<-TE_Share_VAT$TE_Share_VAT
        
        MACRO_FISCAL_INDICATORS[23,7]<-TE_Share_VAT$TE_Share_VAT
        MACRO_FISCAL_INDICATORS[24,7]<-TE_Share_VAT$TE_Share_VAT
        MACRO_FISCAL_INDICATORS[25,7]<-TE_Share_VAT$TE_Share_VAT
        MACRO_FISCAL_INDICATORS[26,7]<-TE_Share_VAT$TE_Share_VAT
        MACRO_FISCAL_INDICATORS[27,7]<-TE_Share_VAT$TE_Share_VAT
        MACRO_FISCAL_INDICATORS[28,7]<-TE_Share_VAT$TE_Share_VAT
        
        
        MACRO_FISCAL_INDICATORS<-MACRO_FISCAL_INDICATORS%>%
          dplyr::mutate(
            New_VAT=Nominal_GDP*DS_Share_VAT,
            #New_Simulation_VAT=Nominal_VAT_NET-New_VAT)
            New_Simulation_VAT=Nominal_VAT_NET+New_VAT,
            Tax_expenditures=Nominal_GDP*TE_Share_VAT
            )
            
        
        MACRO_FISCAL_INDICATORS_TE<-MACRO_FISCAL_INDICATORS%>%
        dplyr::select(Year,Tax_expenditures)

# 1.First calculation -------------------------------------------------------


      FINAL_FORECASTING0<-MACRO_FISCAL_INDICATORS%>%
        select(Year,Nominal_GDP,Nominal_VAT_NET)%>%
        arrange(desc(Year))
        
      function_estimation <- function(x){ ((x) /lead(x))*100-100}
        
      
      FINAL_FORECASTING1<-mutate(FINAL_FORECASTING0,
                                 Nominal_GDP_GROWTH=function_estimation(Nominal_GDP)) %>%
                                arrange((Year))
      
      FINAL_FORECASTING2<-mutate(FINAL_FORECASTING1,
                                 CALCULATION_VAT_NET=1+(1/100*Nominal_GDP_GROWTH)
                               )



          for (i in 1:length(which(is.na(FINAL_FORECASTING2$Nominal_VAT_NET)))){
            FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(Nominal_VAT_NET=if_else(is.na(Nominal_VAT_NET),(CALCULATION_VAT_NET*lag(Nominal_VAT_NET)),Nominal_VAT_NET))
          }
          
          #View(FINAL_FORECASTING2)

          FINAL_FORECASTING_BU<-FINAL_FORECASTING2%>%
            dplyr::select(Year,Nominal_VAT_NET)
          

# 2. Second calculation------------------------------------
      
          FINAL_FORECASTING0<-MACRO_FISCAL_INDICATORS%>%
            select(Year,Nominal_GDP,New_Simulation_VAT)%>%
            arrange(desc(Year))
          
          function_estimation <- function(x){ ((x) /lead(x))*100-100}
          
          
          FINAL_FORECASTING1<-mutate(FINAL_FORECASTING0,
                                     Nominal_GDP_GROWTH=function_estimation(Nominal_GDP)) %>%
            arrange((Year))
          
          FINAL_FORECASTING2<-mutate(FINAL_FORECASTING1,
                                     CALCULATION_VAT_NET=1+(1/100*Nominal_GDP_GROWTH)
          )
          
          
         
          
          
          
          
          
          for (i in 1:length(which(is.na(FINAL_FORECASTING2$New_Simulation_VAT)))){
            FINAL_FORECASTING2=FINAL_FORECASTING2%>%mutate(New_Simulation_VAT=if_else(is.na(New_Simulation_VAT),(CALCULATION_VAT_NET*lag(New_Simulation_VAT)),New_Simulation_VAT))
          }
          
          #View(FINAL_FORECASTING2)
          
          FINAL_FORECASTING_SIM<-FINAL_FORECASTING2%>%
            dplyr::select(Year,Nominal_GDP,New_Simulation_VAT)
          
          
          
          FINAL_COMPARISON_VAT_REVENUES_0<-inner_join(FINAL_FORECASTING_BU,FINAL_FORECASTING_SIM, by = "Year")
          
          FINAL_COMPARISON_VAT_REVENUES<-left_join(FINAL_COMPARISON_VAT_REVENUES_0,MACRO_FISCAL_INDICATORS_TE, by = "Year")
          
          

          # Estimation of share of GDP
          FINAL_COMPARISON_VAT_REVENUES<-FINAL_COMPARISON_VAT_REVENUES%>%
            dplyr::mutate(share_gdp=Nominal_VAT_NET/Nominal_GDP )
          
         # View(FINAL_COMPARISON)


