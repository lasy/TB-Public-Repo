

IO = list()

IO$output_data = "../Data/"
IO$out_Rdata = paste0(IO$output_data, "Rdata/")
IO$out_csv = paste0(IO$output_data, "CSV/")

if(!dir.exists(IO$output_data)){dir.create(IO$output_data)}
if(!dir.exists(IO$out_Rdata)){dir.create(IO$out_Rdata)}
if(!dir.exists(IO$out_csv)){dir.create(IO$out_csv)}

source("../../TB-Restricted-Access-Repo/Scripts/00_variables_restricted_IO.R")


IO$panels = "../Figures Tables Media/Figures/panels/"
