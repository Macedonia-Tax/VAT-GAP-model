'VAT-GAP MODEL 

TIP:
    There is a two-step procedure for executing this code and running this model.
    Press on the below arrows to see the instruction
    
    Before running this code, first, run the script named Install-packages. R and install the necessary libraries to work. 
    If you are already done with this, please continue with the following steps.
    
    '          

# I. Step 1 (INPUT SIMULATION PARAMTERS) ----------------------
'Step 1. Before executing these lines of code please import path 1 to the directory where your model is saved,  and also import the path where your data is saved.
After entering these paths, the code needs to be selected, and then CTRL+Enter must be pressed to execute it. During this step, all necessary packages will be loaded into the R environment. Also, simulation parameters will be uploaded in this step through an interactive table for editing. 
Here simulation parameters can be changed for VAT rates and Taxable proportion. These parameters you can find in the following columns with titles :

Changing VAT rates
 - Standard_VAT_Rate;
 - Preferential_VAT_Rate

Changing Taxable proportion
- Simulation toggle exempt (TP_Exempt )
- Reduced rate (TP_Reduced_Rate)
              
New taxable proportions can not be changed independently. You must change both at the same time.
After changing policy parameters, you need to press the button for Synchronizing, and the button Done.
'

      path<-"C:/Users/wb591157/OneDrive - WBG/Documents/Models/VAT-GAP-model/DATA/INPUT" #<----Set your path here for data 
      path1<-"C:/Users/wb591157/OneDrive - WBG/Documents/Models/VAT-GAP-model" #<----Set your path here for the model
      source("./Scripts/Simulation-Parameters-Module.R")


      
      
# II.Step 2 (SIMULATION AND VIZUALIZATION) ------------------------------------------
      'After the successful execution of Step 1, next is Step 2. To execute it, you need to select the code and then press CTRL+Enter. 
      Code will continue to iterate through other modules and produce a VAT Dashboard in HTML format. 
      The dashboard contains interactive charts, which can be separately downloaded for further use, such as presentations or analysis.'
      
      
      source("./Scripts/Data preprocessing-Module.R")
      source("./Scripts/VAT estimation-Module.R")
      source("./Scripts/TaxCalc-Module.R")
      source("./Scripts/Estimation effective VAT rates-Module.R")
      source("./Scripts/Forecast-Module.R")
      source("./Scripts/Export-Module.R")
      dashboard <- rmarkdown::render((file=paste(path1,"/Scripts//VAT-Dashboard.Rmd",sep="")),params=list(args = myarg))
      rstudioapi::viewer(dashboard)
      rm(list=ls()[! ls() %in% c("path","path1")])
      gc(TRUE)
      setwd(path1)
      getwd()

