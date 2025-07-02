# Load wizaRdry library:
if(!require(wizaRdry)) {install.packages('wizaRdry')}; library(wizaRdry)

demo_phase1 <- qualtrics("demo_phase1")

clean("demo_phase1")

#removing duplicates
unique(demo_phase1$src_subject_id)
#removing lines with NA or NAN

#removing entries without sing_id
