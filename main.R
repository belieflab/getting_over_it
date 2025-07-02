# Main analysis script for this wizaRdry project

# Load wizaRdry library:
if(!require(wizaRdry)) {install.packages('wizaRdry')}; library(wizaRdry)

# Check available REDCap forms:
# redcap.index()

# Example NDA request:
# Create remediation script in nda/
# nda("cde_dsm5crossad01")

# Your analysis code here
# What collections are available in the database specified by config.yml if arg = NULL, in this case sing
mongo.index()

social_prl<- mongo("social_prl") 

to.csv(social_prl)


