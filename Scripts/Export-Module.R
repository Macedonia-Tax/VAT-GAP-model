'Export-Module'

#  EXPORT RESULTS IN EXCEL -----------------------------------------------
    setwd(path)
      getwd()
  
      wb <- createWorkbook()
      addWorksheet(wb, "Main_Results")
      addWorksheet(wb, "Revenue_VAT_TOTAL")
      addWorksheet(wb, "Result_Simulation")
      addWorksheet(wb, "Results_1")
      addWorksheet(wb, "Est_Rev1")
      addWorksheet(wb, "effective_vat_rates")
      addWorksheet(wb, "hbs")
      
      # Name of worksheet in Excel
      writeData(wb, "Main_Results", Export_Main_Results, startRow = 1, startCol = 1)
      writeData(wb, "Revenue_VAT_TOTAL", Revenue_VAT_TOTAL, startRow = 1, startCol = 1)
      writeData(wb, "Result_Simulation", Simulation_Results, startRow = 1, startCol = 1)
      writeData(wb, "Results_1", Simulation_Results_1, startRow = 1, startCol = 1)
      writeData(wb, "Est_Rev1", Est_Rev1, startRow = 1, startCol = 1)
      writeData(wb, "effective_vat_rates", effective_vat_rates, startRow = 1, startCol = 1)   
      writeData(wb, "hbs", data4_hbs_wider_merged_deciles1, startRow = 1, startCol = 1)   
      # Export and save 
      saveWorkbook(wb, file = "export_data.xlsx", overwrite = TRUE)


  wb1 <- createWorkbook()
      addWorksheet(wb1, "simulation")
      addWorksheet(wb1, "NACE_NAMES")
      addWorksheet(wb1, "FINAL_VAT_REVENUES")

      # Name of worksheet in Excel
      writeData(wb1, "simulation", SIMULATION, startRow = 1, startCol = 1)
      writeData(wb1, "NACE_NAMES", NACE_NAMES, startRow = 1, startCol = 1)
      writeData(wb1, "FINAL_VAT_REVENUES", FINAL_COMPARISON_VAT_REVENUES, startRow = 1, startCol = 1)
      # Export and save 
      saveWorkbook(wb1, file = "simulation.xlsx", overwrite = TRUE)


      
      
      rm(list=ls()[! ls() %in% c("path","path1")])


