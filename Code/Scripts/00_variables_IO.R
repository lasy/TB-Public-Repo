

IO = list()

IO$public_output_data = "../Data/"
IO$out_Rdata = paste0(IO$public_output_data, "Rdata/")
IO$out_csv = paste0(IO$public_output_data, "CSV/")

if(!dir.exists(IO$public_output_data)){dir.create(IO$public_output_data)}
if(!dir.exists(IO$out_Rdata)){dir.create(IO$out_Rdata)}
if(!dir.exists(IO$out_csv)){dir.create(IO$out_csv)}

source("../../TB-Restricted-Access-Repo/Scripts/00_variables_restricted_IO.R")


IO$panels = "../Figures Tables Media/Figures/panels/"
