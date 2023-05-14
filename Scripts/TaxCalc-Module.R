'
TAX CALCULATOR-MODULE

'

                    # 1. SIMULATION ----
                        # 1.1 Pre-processing COICOP and setting parameters for simulation ------------------------------------------
                          # 1.1.0 Estimation of Calibration factor -----------------------------------------------------
                        '
                              Warning
                              This part begins with copy-paste of the parameters entered previously and from this point begins with their new calculation with the aim of obtaining TE
                              '
                        SIMULATION_0 = copy(SIMULATION_CALIBRATION_FACTOR)
                        
                        # colnames(SIMULATION_0) <- c("PRODUCT_INDUSTRY_CODE","CPA_DIVISION",
                        #                           "Current_Policy_Exempt","Current_Policy_Reduced_Rate","Current_Policy_Fully_Taxable",
                        #                           "Simulation_Toggles_Exempt","Simulation_Toggles_Reduced_Rate",
                        #                           "Standard_VAT_Rate","Preferential_VAT_Rate")

                        
                        SIMULATION_0 <- SIMULATION_0 %>%
                          dplyr::mutate(Simulated_Policy_Exempt = ifelse(is.na(Simulation_Toggles_Exempt), Current_Policy_Exempt, Simulation_Toggles_Exempt),
                                        Simulated_Policy_Reduced_Rate = ifelse(is.na(Simulation_Toggles_Reduced_Rate), Current_Policy_Reduced_Rate, Simulation_Toggles_Reduced_Rate),
                                        Simulated_Policy_Fully_Taxable = 1-Simulated_Policy_Exempt-Simulated_Policy_Reduced_Rate)
                        
                        
                          # 1.1.1 Aggregate table for COICOP----------------------------------------
                          
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
                          
                          # 1.1.2 Estimation of TE part -----------------------------------------------------
                          '
                              Warning
                              This part begins with copy-paste of the parameters entered previously and from this point begins with their new calculation with the aim of obtaining TE
                              '
                          SIMULATION_1 = copy(SIMULATION)
                          
                          SIMULATION_1$Simulation_Toggles_Reduced_Rate<-TE_EXEMPT
                          SIMULATION_1$Simulation_Toggles_Exempt<-TE_REDUCED_RATE
                          
                          
                          SIMULATION_1 <- SIMULATION_1 %>%
                            dplyr::mutate(Simulated_Policy_Exempt = ifelse(is.na(Simulation_Toggles_Exempt), Current_Policy_Exempt, Simulation_Toggles_Exempt),
                                          Simulated_Policy_Reduced_Rate = ifelse(is.na(Simulation_Toggles_Reduced_Rate), Current_Policy_Reduced_Rate, Simulation_Toggles_Reduced_Rate),
                                          Simulated_Policy_Fully_Taxable = 1-Simulated_Policy_Exempt-Simulated_Policy_Reduced_Rate)
                          
                          
                          # 1.1.3 Estimation of effective VAT RATE ----------------------------------------------------------
                          
                          SIMULATION_2 = copy(SIMULATION)
                          SIMULATION_2$Simulation_Toggles_Reduced_Rate<-TE_EXEMPT
                          
                          SIMULATION_2 <- SIMULATION_2 %>%
                            dplyr::mutate(Simulated_Policy_Exempt = ifelse(is.na(Simulation_Toggles_Exempt), Current_Policy_Exempt, Simulation_Toggles_Exempt),
                                          Simulated_Policy_Reduced_Rate = ifelse(is.na(Simulation_Toggles_Reduced_Rate), Current_Policy_Reduced_Rate, Simulation_Toggles_Reduced_Rate),
                                          Simulated_Policy_Fully_Taxable = 1-Simulated_Policy_Exempt-Simulated_Policy_Reduced_Rate)
                          
                        # 1.2 Benchmark revenues -----
                        
                        benchmark_tax_rate <- benchmark_tax_rate
                       
                        
                        CPA_PRODUCTS$BM_Rev <- CPA_PRODUCTS$Use_K_NetPurch %>%
                          dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, Final_consumption_expenditure_by_households) %>%
                          dplyr::mutate(Final_Demand_HH = Final_consumption_expenditure_by_households*benchmark_tax_rate) %>%
                          dplyr::select(-Final_consumption_expenditure_by_households)
                        
                       
                        CPA_PRODUCTS$BM_Rev$Final_Demand_HH[is.na(CPA_PRODUCTS$BM_Rev$Final_Demand_HH)] <- 0
                        CPA_PRODUCTS$BM_Rev$Final_Demand_HH[grepl("Imputed rents of owner-occupied dwellings", CPA_PRODUCTS$BM_Rev$PRODUCT_INDUSTRY_NAME)] <- 0
                        
                        
                        CPA_PRODUCTS$BM_Rev$Final_Demand_NPISH <- CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_NPISH*benchmark_tax_rate
                        
                        
                        CPA_PRODUCTS$BM_Rev$Final_Demand_NPISH[is.na(CPA_PRODUCTS$BM_Rev$Final_Demand_NPISH)] <- 0
                        CPA_PRODUCTS$BM_Rev$Final_Demand_NPISH[grepl("Imputed rents of owner-occupied dwellings", CPA_PRODUCTS$BM_Rev$PRODUCT_INDUSTRY_NAME)] <- 0
                        
                        CPA_PRODUCTS$BM_Rev$Final_Demand_Government <- CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_by_government*benchmark_tax_rate
                        
                        
                        
                        CPA_PRODUCTS$BM_Rev$Final_Demand_Government[is.na(CPA_PRODUCTS$BM_Rev$Final_Demand_Government)] <- 0
                        CPA_PRODUCTS$BM_Rev$Final_Demand_Government[grepl("Imputed rents of owner-occupied dwellings", CPA_PRODUCTS$BM_Rev$PRODUCT_INDUSTRY_NAME)] <- 0
                        
                        
                        CPA_PRODUCTS$BM_Rev$Final_Demand_HH[grepl("Constructions and construction works", CPA_PRODUCTS$BM_Rev$PRODUCT_INDUSTRY_NAME)] = 0
                        
                        
                        CPA_PRODUCTS$BM_Rev <- CPA_PRODUCTS$BM_Rev %>%
                          dplyr::mutate(Final_Demand_Total = psum(Final_Demand_HH, Final_Demand_NPISH, Final_Demand_Government))
                        
                        Sum_of_Final_Demand_Total = sum(CPA_PRODUCTS$BM_Rev$Final_Demand_Total)
                        
                        # 1.3 Est.IS ----
                          # 2.3.0 Estimation of Calibration factor ---------------------------------------------------
                        
                        # Copy paste table
                        SUPPLY_DOM_0 = copy(SUPPLY_DOM)
                        NACE_INDUSTRIES_0 = copy(NACE_INDUSTRIES)
                        
                        EST.IS_0 <- merge.data.frame(SUPPLY_DOM_0, SIMULATION_0, key = "PRODUCT_INDUSTRY_CODE")
                        EST.IS_0$value = EST.IS_0$value*EST.IS_0$Simulated_Policy_Exempt
                        
                        EST.IS_0 <- EST.IS_0 %>% select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, INDUSTRY_CODE, INDUSTRY_NAME, value)
                        EST.IS_0$value[is.na(EST.IS_0$value)] <- 0
                        
                        NACE_INDUSTRIES_0$Est.IS <- EST.IS_0 %>% 
                          dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
                          dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
                          dplyr::summarise(Industry_Share = sum(value, na.rm = T))
                        
                        NACE_INDUSTRIES_0$Est.IS <- NACE_INDUSTRIES_0$Est.IS %>%
                          dplyr::arrange(INDUSTRY_CODE)
                        
                        NACE_INDUSTRIES_0$Supply_Dom <- NACE_INDUSTRIES_0$Supply_Dom %>%
                          dplyr::arrange(INDUSTRY_CODE)
                        
                        NACE_INDUSTRIES_0$Est.IS$Industry_Share <- NACE_INDUSTRIES_0$Est.IS$Industry_Share/NACE_INDUSTRIES_0$Supply_Dom$Total_output_by_industries_at_basic_prices
                        
                        NACE_INDUSTRIES_0$Est.IS$Industry_Share[is.na(NACE_INDUSTRIES_0$Est.IS$Industry_Share)] <- 0
                        
                          # 2.3.1 Main estimation  --------------------------------------------------
                          
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
                          
                          # 2.3.2 Estimation of TE part ---------------------------------------------------
                          
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
                          
                          # 2.3.3 Estimation of effective VAT RATE ----------------------------------------
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
                          
                        # 1.4 Est Rev ----
                          # 2.4.0 Estimation of Calibration factor ---------------------------------------------------
                          USE_K_DOM_NETPURCH_0 = copy(USE_K_DOM_NETPURCH)
                          CPA_PRODUCTS_0 = copy(CPA_PRODUCTS)
                          
                          EST_REV_0 <- USE_K_DOM_NETPURCH_0 %>% 
                            merge.data.frame(NACE_INDUSTRIES_0$Est.IS, key = "INDUSTRY_NAME") %>%
                            merge.data.frame(SIMULATION_0, key = "PRODUCT_INDUSTRY_NAME") %>%
                            dplyr::mutate(value = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*Industry_Share*value) %>%
                            dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)
                          
                          CPA_PRODUCTS_0$Est_Rev <- EST_REV_0 %>% 
                            dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
                            dplyr::summarise(Total_Revenues_from_Intermediate_Inputs = sum(value, na.rm = T)) %>%
                            dplyr::arrange(PRODUCT_INDUSTRY_CODE)
                          
                          CPA_PRODUCTS_0$Est_Rev <- CPA_PRODUCTS_0$Use_K_NetPurch %>% 
                            merge.data.frame(SIMULATION_0, key = "PRODUCT_INDUSTRY_NAME") %>%
                            dplyr::mutate(Final_Demand_HH = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                                            Final_consumption_expenditure_by_households,
                                          Final_Demand_NPISH = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                                            Final_consumption_expenditure_NPISH,
                                          Final_Demand_Government = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                                            Final_consumption_expenditure_by_government) %>%
                            merge.data.frame(CPA_PRODUCTS_0$Est_Rev, key = "PRODUCT_INDUSTRY_NAME") %>%
                            dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, Total_Revenues_from_Intermediate_Inputs, Final_Demand_HH, Final_Demand_NPISH, Final_Demand_Government) %>%
                            dplyr::arrange(PRODUCT_INDUSTRY_NAME)
                          
                          
                          CPA_PRODUCTS_0$Est_Rev$Final_Demand_HH[CPA_PRODUCTS_0$Est_Rev$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"] <- 
                            CPA_PRODUCTS_0$Use_K_NetPurch$Final_consumption_expenditure_by_households[CPA_PRODUCTS_0$Use_K_NetPurch$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"]*
                            (1-SIMULATION$Simulated_Policy_Exempt[62])*vat_rate_on_residential_construction
                          
                          CPA_PRODUCTS_0$Est_Rev$Final_Demand_Total = psum(CPA_PRODUCTS_0$Est_Rev$Total_Revenues_from_Intermediate_Inputs, 
                                                                           CPA_PRODUCTS_0$Est_Rev$Final_Demand_HH, 
                                                                           CPA_PRODUCTS_0$Est_Rev$Final_Demand_NPISH, 
                                                                           CPA_PRODUCTS_0$Est_Rev$Final_Demand_Government, na.rm = T) 
                          
                        
                          # 2.4.1 Main estimation ----------------------------------------------------
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
        
                          # 2.4.2 Estimation of TE part ---------------------------------------------------
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
                          
                         
                          CPA_PRODUCTS_1$Est_Rev$Final_Demand_HH[CPA_PRODUCTS_1$Est_Rev$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"] <- 
                            CPA_PRODUCTS_1$Use_K_NetPurch$Final_consumption_expenditure_by_households[CPA_PRODUCTS_1$Use_K_NetPurch$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"]*
                            (1-SIMULATION$Simulated_Policy_Exempt[62])*vat_rate_on_residential_construction
                          
                          CPA_PRODUCTS_1$Est_Rev$Final_Demand_Total = psum(CPA_PRODUCTS_1$Est_Rev$Total_Revenues_from_Intermediate_Inputs, 
                                                                           CPA_PRODUCTS_1$Est_Rev$Final_Demand_HH, 
                                                                           CPA_PRODUCTS_1$Est_Rev$Final_Demand_NPISH, 
                                                                           CPA_PRODUCTS_1$Est_Rev$Final_Demand_Government, na.rm = T) 
                          
                          # 2.4.3 Estimation of effective VAT RATE ----------------------------------------
                          
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
                          
                          
                          CPA_PRODUCTS_2$Est_Rev$Final_Demand_HH[CPA_PRODUCTS_2$Est_Rev$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"] <- 
                            CPA_PRODUCTS_2$Use_K_NetPurch$Final_consumption_expenditure_by_households[CPA_PRODUCTS_2$Use_K_NetPurch$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"]*
                            (1-SIMULATION$Simulated_Policy_Exempt[62])*vat_rate_on_residential_construction
                          
                          CPA_PRODUCTS_2$Est_Rev$Final_Demand_Total = psum(CPA_PRODUCTS_2$Est_Rev$Total_Revenues_from_Intermediate_Inputs, 
                                                                           CPA_PRODUCTS_2$Est_Rev$Final_Demand_HH, 
                                                                           CPA_PRODUCTS_2$Est_Rev$Final_Demand_NPISH, 
                                                                           CPA_PRODUCTS_2$Est_Rev$Final_Demand_Government, na.rm = T) 
                          
                          
                    # 2. SIMULATION RESULTS ----  
                          # 2.1.0 Estimation of Calibration factor ---------------------------------------------------
                          Results_0 <- as.list(c("VAT_Gap", "Simulation"))
                          names(Results_0) <- c("VAT_Gap", "Simulation")
                          
                          # NEW
                          Results_0$VAT_Gap <- as.data.frame(sum(CPA_PRODUCTS_0$BM_Rev$Final_Demand_Total))
                          colnames(Results_0$VAT_Gap) <- "Benchmark_VAT_M_of_denars"

                          # Results_0$VAT_Gap$Uncalibrated_VAT_Est.M_of_denars <- sum(CPA_PRODUCTS_0$Est_Rev$Final_Demand_Total, na.rm = T)
                          # Results_0$VAT_Gap$VAT_Control_Total.M_of_denars <- sum(CPA_PRODUCTS_0$Use_VAT$Total_use_at_basic_prices, na.rm = T) - sum(CPA_PRODUCTS_0$Use_VAT$Exports_FOB, na.rm = T)
                          
                          Uncalibrated_VAT<- sum(CPA_PRODUCTS_0$Est_Rev$Final_Demand_Total, na.rm = T)
                          VAT_control<-sum(CPA_PRODUCTS_0$Use_VAT$Total_use_at_basic_prices, na.rm = T) - sum(CPA_PRODUCTS_0$Use_VAT$Exports_FOB, na.rm = T)
                          
                          Locked_Calibration_Factor_TEST<-VAT_control/Uncalibrated_VAT
                          
                          Locked_Calibration_Factor<-Locked_Calibration_Factor_TEST
                          
                          #View(Locked_Calibration_Factor_TEST)
                          Results_0$VAT_Gap$Calibrated_VAT_Est.M_of_denars <- sum(CPA_PRODUCTS_0$Est_Rev$Final_Demand_Total, na.rm = T)*Locked_Calibration_Factor
                          Results_0$VAT_Gap$VAT_Control_Total.M_of_denars <- sum(CPA_PRODUCTS_0$Use_VAT$Total_use_at_basic_prices, na.rm = T) - sum(CPA_PRODUCTS_0$Use_VAT$Exports_FOB, na.rm = T)
                          Results_0$VAT_Gap$Total_VAT_Gap.M_of_denars <- Results_0$VAT_Gap$Benchmark_VAT_M_of_denars - Results_0$VAT_Gap$VAT_Control_Total.M_of_denars
                          Results_0$VAT_Gap$Total_VAT_Gap.Prc <- Results_0$VAT_Gap$Total_VAT_Gap.M_of_denars/Results_0$VAT_Gap$VAT_Control_Total.M_of_denars

                          # Manual input
                          # Test without manual input
                          #Results_0$VAT_Gap$Policy_Gap.M_of_denars <- 24407  # <------- Manual input in simulation
                          # #New
                          # Results_0$VAT_Gap$Policy_Gap.Prc <- Results_0$VAT_Gap$Policy_Gap.M_of_denars/Results_0$VAT_Gap$VAT_Control_Total.M_of_denars
                          # Results_0$VAT_Gap$Compliance_Gap.M_of_denars <- Results_0$VAT_Gap$Total_VAT_Gap.M_of_denars-Results_0$VAT_Gap$Policy_Gap.M_of_denars
                          # Results_0$VAT_Gap$Compliance_Gap.Prc <- Results_0$VAT_Gap$Compliance_Gap.M_of_denars/Results_0$VAT_Gap$VAT_Control_Total.M_of_denars
                          # Results_0$VAT_Gap$Calibration_Factor <- Locked_Calibration_Factor
                          # 
                          # 
                          # # Final output - Change in Revenues
                          # 
                          # 
                          # Results_0$Simulation <- as.data.frame(as.integer(Results_0$VAT_Gap$Calibrated_VAT_Est.M_of_denars-Results_0$VAT_Gap$VAT_Control_Total.M_of_denars))
                          # colnames(Results_0$Simulation) <- "Simulated_Change_in_Revenues.M_of_denars"
                          # 
                          # Results_0$Simulation$Simulated_Change_in_Revenues.Prc <- (Results_0$Simulation$Simulated_Change_in_Revenues.M_of_denars/Results_0$VAT_Gap$VAT_Control_Total.M_of_denars)*100

                          #View(Results_0)
                          
                          # 2.1.1 Main estimation --------------------------------------------------
                          
                          Results <- as.list(c("VAT_Gap", "Simulation"))
                          names(Results) <- c("VAT_Gap", "Simulation")
                          
                          
                          Results$VAT_Gap <- as.data.frame(sum(CPA_PRODUCTS$BM_Rev$Final_Demand_Total))
                          colnames(Results$VAT_Gap) <- "Benchmark_VAT_M_of_denars"
                          
                          Results$VAT_Gap$Uncalibrated_VAT_Est.M_of_denars <- sum(CPA_PRODUCTS$Est_Rev$Final_Demand_Total, na.rm = T)
                          
                          
                          
                          Results$VAT_Gap$Calibrated_VAT_Est.M_of_denars <- sum(CPA_PRODUCTS$Est_Rev$Final_Demand_Total, na.rm = T)*Locked_Calibration_Factor
                          Results$VAT_Gap$VAT_Control_Total.M_of_denars <- sum(CPA_PRODUCTS$Use_VAT$Total_use_at_basic_prices, na.rm = T) - sum(CPA_PRODUCTS$Use_VAT$Exports_FOB, na.rm = T)
                          Results$VAT_Gap$Total_VAT_Gap.M_of_denars <- Results$VAT_Gap$Benchmark_VAT_M_of_denars - Results$VAT_Gap$VAT_Control_Total.M_of_denars
                          Results$VAT_Gap$Total_VAT_Gap.Prc <- Results$VAT_Gap$Total_VAT_Gap.M_of_denars/Results$VAT_Gap$VAT_Control_Total.M_of_denars
                          
                    
                          # This part is replaced below
                          # # Results$VAT_Gap$Policy_Gap.M_of_denars <- 24407  # <------- Manual input in simulation
                          # Results$VAT_Gap$Policy_Gap.M_of_denars <-Results_1$Simulation$Simulated_Change_in_Revenues.M_of_denars  
                          # Results$VAT_Gap$Policy_Gap.Prc <- Results$VAT_Gap$Policy_Gap.M_of_denars/Results$VAT_Gap$VAT_Control_Total.M_of_denars
                          # Results$VAT_Gap$Compliance_Gap.M_of_denars <- Results$VAT_Gap$Total_VAT_Gap.M_of_denars-Results$VAT_Gap$Policy_Gap.M_of_denars
                          # Results$VAT_Gap$Compliance_Gap.Prc <- Results$VAT_Gap$Compliance_Gap.M_of_denars/Results$VAT_Gap$VAT_Control_Total.M_of_denars
                          # Results$VAT_Gap$Calibration_Factor <- Locked_Calibration_Factor 
                          # 
                          
                          
                          # Final output - Change in Revenues
                          
                          
                          Results$Simulation <- as.data.frame(as.integer(Results$VAT_Gap$Calibrated_VAT_Est.M_of_denars-Results$VAT_Gap$VAT_Control_Total.M_of_denars))
                          colnames(Results$Simulation) <- "Simulated_Change_in_Revenues.M_of_denars"
                          
                          Results$Simulation$Simulated_Change_in_Revenues.Prc <- (Results$Simulation$Simulated_Change_in_Revenues.M_of_denars/Results$VAT_Gap$VAT_Control_Total.M_of_denars)*100
                          
                          
                          
                          
                          # 2.1.2 Estimation of TE part ---------------------------------------------------
                          
                          Results_1 <- as.list(c("VAT_Gap", "Simulation"))
                          names(Results_1) <- c("VAT_Gap", "Simulation")
                          
                          # NEW
                          Results_1$VAT_Gap <- as.data.frame(sum(CPA_PRODUCTS_1$BM_Rev$Final_Demand_Total))
                          colnames(Results_1$VAT_Gap) <- "Benchmark_VAT_M_of_denars"

                          Results_1$VAT_Gap$Uncalibrated_VAT_Est.M_of_denars <- sum(CPA_PRODUCTS_1$Est_Rev$Final_Demand_Total, na.rm = T)
                          Results_1$VAT_Gap$Calibrated_VAT_Est.M_of_denars <- sum(CPA_PRODUCTS_1$Est_Rev$Final_Demand_Total, na.rm = T)*Locked_Calibration_Factor
                          Results_1$VAT_Gap$VAT_Control_Total.M_of_denars <- sum(CPA_PRODUCTS_1$Use_VAT$Total_use_at_basic_prices, na.rm = T) - sum(CPA_PRODUCTS_1$Use_VAT$Exports_FOB, na.rm = T)
                          Results_1$VAT_Gap$Total_VAT_Gap.M_of_denars <- Results_1$VAT_Gap$Benchmark_VAT_M_of_denars - Results_1$VAT_Gap$VAT_Control_Total.M_of_denars
                          Results_1$VAT_Gap$Total_VAT_Gap.Prc <- Results_1$VAT_Gap$Total_VAT_Gap.M_of_denars/Results_1$VAT_Gap$VAT_Control_Total.M_of_denars

                          # Manual input 
                          # Test without manual input
                         # Results_1$VAT_Gap$Policy_Gap.M_of_denars <- 24407  # <------- Manual input in simulation
                          # New
                          #Results_1$VAT_Gap$Policy_Gap.Prc <- Results_1$VAT_Gap$Policy_Gap.M_of_denars/Results_1$VAT_Gap$VAT_Control_Total.M_of_denars
                          #Results_1$VAT_Gap$Compliance_Gap.M_of_denars <- Results_1$VAT_Gap$Total_VAT_Gap.M_of_denars-Results_1$VAT_Gap$Policy_Gap.M_of_denars
                          #Results_1$VAT_Gap$Compliance_Gap.Prc <- Results_1$VAT_Gap$Compliance_Gap.M_of_denars/Results_1$VAT_Gap$VAT_Control_Total.M_of_denars
                          #Results_1$VAT_Gap$Calibration_Factor <- Locked_Calibration_Factor 
                          
                          
                          # Final output - Change in Revenues
                          
                          
                          Results_1$Simulation <- as.data.frame(as.integer(Results_1$VAT_Gap$Calibrated_VAT_Est.M_of_denars-Results_1$VAT_Gap$VAT_Control_Total.M_of_denars))
                          colnames(Results_1$Simulation) <- "Simulated_Change_in_Revenues.M_of_denars"
                          
                          Results_1$Simulation$Simulated_Change_in_Revenues.Prc <- (Results_1$Simulation$Simulated_Change_in_Revenues.M_of_denars/Results_1$VAT_Gap$VAT_Control_Total.M_of_denars)*100
                          
                          
                          # Extracting part foR TE
                          # Results$VAT_Gap$Policy_Gap.M_of_denars<-Results_1$VAT_Gap$Policy_Gap.M_of_denars
                          
                          #View(Results_1)
                          
                          
                          # 2.1.1 Adding TE'S ----------------------------------------------------------------
                             # This part is from 1.1.1 main estimation
                          Results$VAT_Gap$Policy_Gap.M_of_denars <-Results_1$Simulation$Simulated_Change_in_Revenues.M_of_denars
                          Results$VAT_Gap$Policy_Gap.Prc <- Results$VAT_Gap$Policy_Gap.M_of_denars/Results$VAT_Gap$VAT_Control_Total.M_of_denars
                          Results$VAT_Gap$Compliance_Gap.M_of_denars <- Results$VAT_Gap$Total_VAT_Gap.M_of_denars-Results$VAT_Gap$Policy_Gap.M_of_denars
                          Results$VAT_Gap$Compliance_Gap.Prc <- Results$VAT_Gap$Compliance_Gap.M_of_denars/Results$VAT_Gap$VAT_Control_Total.M_of_denars
                          Results$VAT_Gap$Calibration_Factor <- Locked_Calibration_Factor
                          
                         # This part is for calibration factor
                          Results_0$VAT_Gap$Policy_Gap.M_of_denars <- Results_1$Simulation$Simulated_Change_in_Revenues.M_of_denars
                          
                          #New
                          Results_0$VAT_Gap$Policy_Gap.Prc <- Results_0$VAT_Gap$Policy_Gap.M_of_denars/Results_0$VAT_Gap$VAT_Control_Total.M_of_denars
                          Results_0$VAT_Gap$Compliance_Gap.M_of_denars <- Results_0$VAT_Gap$Total_VAT_Gap.M_of_denars-Results_0$VAT_Gap$Policy_Gap.M_of_denars
                          Results_0$VAT_Gap$Compliance_Gap.Prc <- Results_0$VAT_Gap$Compliance_Gap.M_of_denars/Results_0$VAT_Gap$VAT_Control_Total.M_of_denars
                          Results_0$VAT_Gap$Calibration_Factor <- Locked_Calibration_Factor
                          
                          
                          # Final output - Change in Revenues
                          
                          
                          Results_0$Simulation <- as.data.frame(as.integer(Results_0$VAT_Gap$Calibrated_VAT_Est.M_of_denars-Results_0$VAT_Gap$VAT_Control_Total.M_of_denars))
                          colnames(Results_0$Simulation) <- "Simulated_Change_in_Revenues.M_of_denars"
                          
                          Results_0$Simulation$Simulated_Change_in_Revenues.Prc <- (Results_0$Simulation$Simulated_Change_in_Revenues.M_of_denars/Results_0$VAT_Gap$VAT_Control_Total.M_of_denars)*100
                          
                          
                          
                          
                          
                          # 2.1.3 Estimation of effective VAT RATE ------------------------------
                          
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
                          
                         # Results_2$VAT_Gap$Policy_Gap.M_of_denars <- 24407  # <------- Manual input in simulation
                          
                          Results_2$VAT_Gap$Policy_Gap.M_of_denars <-Results_1$Simulation$Simulated_Change_in_Revenues.M_of_denars  
                          Results_2$VAT_Gap$Policy_Gap.Prc <- Results_2$VAT_Gap$Policy_Gap.M_of_denars/Results_2$VAT_Gap$VAT_Control_Total.M_of_denars
                          Results_2$VAT_Gap$Compliance_Gap.M_of_denars <- Results_2$VAT_Gap$Total_VAT_Gap.M_of_denars-Results_2$VAT_Gap$Policy_Gap.M_of_denars
                          Results_2$VAT_Gap$Compliance_Gap.Prc <- Results_2$VAT_Gap$Compliance_Gap.M_of_denars/Results_2$VAT_Gap$VAT_Control_Total.M_of_denars
                          Results_2$VAT_Gap$Calibration_Factor <- Locked_Calibration_Factor 
                          
                          
                          
                          # Final output - Change in Revenues
                          
                        
                          Results_2$Simulation <- as.data.frame(as.integer(Results_2$VAT_Gap$Calibrated_VAT_Est.M_of_denars-Results_2$VAT_Gap$VAT_Control_Total.M_of_denars))
                          colnames(Results_2$Simulation) <- "Simulated_Change_in_Revenues.M_of_denars"
                          Results_2$Simulation$Simulated_Change_in_Revenues.Prc <- (Results_2$Simulation$Simulated_Change_in_Revenues.M_of_denars/Results_2$VAT_Gap$VAT_Control_Total.M_of_denars)*100
                          
                          
                          # 2.1.4 Other files -------------------------------------------------------
                          
                          Export_Main_Results<- Results$VAT_Gap 
                          Simulation_Results<-Results$Simulation
                          
                          
                          Revenue_VAT_TOTAL<-CPA_PRODUCTS$Est_Rev%>%
                            dplyr::select(PRODUCT_INDUSTRY_CODE,Total_Revenues_from_Intermediate_Inputs,Final_Demand_HH,Final_Demand_NPISH,Final_Demand_Government,Final_Demand_Total)
                          
                          # Without text column
                          Revenue_VAT_TOTAL[2:6]<-Revenue_VAT_TOTAL[2:6]*Locked_Calibration_Factor
                          
                          #  Result from tax expenditures estimation
                          # Result 1
                          # NEW
                          Simulation_Results_1<-Results_1$Simulation
                          #Simulation_Results_1_te<-Results_1$VAT_Gap$Policy_Gap.M_of_denars
                          Simulation_Results_1_te<-Results_1$Simulation$Simulated_Change_in_Revenues.M_of_denars 
                          
                          
                          Est_Rev1<-CPA_PRODUCTS_1$Est_Rev
                          
                          
                          
                          