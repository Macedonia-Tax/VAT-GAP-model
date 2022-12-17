
setwd(path)
getwd()


# 10. EXPORT RESULTS IN EXCEL -----------------------------------------------
#gc(TRUE)
#library(xlsx)
# Export data for business as usual. Suffix for business as usual is 'bu'. Results are estimated without any changes in VAT Rates.
#For this data set this estimation is only need to done one time and after that this result can be compared as benchmark
# 
# write.xlsx(as.data.frame(Simulation_Results_1), file="export_data_bu.xlsx", sheetName="Results_1_bu", row.names=FALSE)
# write.xlsx(as.data.frame(Est_Rev1), file="export_data_bu.xlsx", sheetName="Est_Rev1_bu",append=TRUE, row.names=FALSE)
# write.xlsx(as.data.frame(effective_vat_rates_bu), file="export_data_bu.xlsx", sheetName="effective_vat_rates_bu",append=TRUE, row.names=FALSE)
#write.xlsx(as.data.frame(data4_hbs_wider_merged_deciles1), file="export_data_bu1.xlsx", sheetName="hbs_bu1",append=TRUE, row.names=FALSE)
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
write.xlsx(as.data.frame(FINAL_COMPARISON_VAT_REVENUES), file="simulation.xlsx", sheetName="FINAL_VAT_REVENUES",append=TRUE, row.names=FALSE)

#rm(list=(ls()))

