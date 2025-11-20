##* Xiaopei Zhang
##* 
##* 8/18/2025
##* 
##* V2
##* 
##* 


library(shiny)
library(bslib)

##********************************************************##
##******************** Prepare ***************************##
##********************************************************##

# Step 1. Prepare datasets under / datasets
# Step 2. Prepare "Datasets infomation.xlsx"
# Step 3. run all codes below

source("XZ_DB_functions.r")

# A. import one by one
indexfile<-read_xlsx("Datasets infomation.xlsx")

#Read files under /datasets, generate index information under new folder /IndexData
xiaopei.input("JCM_NCBP2_OE_DEG_2024_PMID.csv",Sheet = 1)
xiaopei.input("BCK_Ezr_RIP_2025_PMID.csv",Sheet = 1)
xiaopei.input("MG_eIF4E_KD_DSG_2023_PMID.xlsx",Sheet = 1)
xiaopei.input("MG_eIF4E_KD_DSG_2023_PMID.xlsx",Sheet = 3)
xiaopei.input("MG_eIF4E_KD_DSG_2023_PMID.xlsx",Sheet = 2)


# B. import all together
xiaopei.input.all("Datasets infomation.xlsx")






# base on the current files under /IndexData, generate searching csv under current location.
xiaopei.prepare.GeneID()
xiaopei.prepare.geneSymbol()









##********************************************************##
##******************** USING *****************************##
##********************************************************##
# run the source code first, then can start to search.
source("XZ_DB_functions.r")

# quick search function

xiaopei.search.geneSymbol("NCBP2")

xiaopei.search.GeneID("ENSG00000166689")


##********************************************************##
# print result function

xiaopei.print.geneSymbol("NCBP2")
xiaopei.print.geneSymbol("CDK13")
xiaopei.print.geneSymbol("FXYD5")

xiaopei.print.geneSymbol("MYC")
xiaopei.print.geneSymbol("DMPK")


xiaopei.print.GeneID("ENSG00000166689")







##********************************************************##
#app
shiny::runApp(".")








