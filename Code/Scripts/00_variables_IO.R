
IO = list()

IO$public_output_data = "../Data/"
IO$out_Rdata = paste0(IO$public_output_data, "Rdata/")
IO$out_csv = paste0(IO$public_output_data, "CSV/")

if(!dir.exists(IO$public_output_data)){dir.create(IO$public_output_data)}
if(!dir.exists(IO$out_Rdata)){dir.create(IO$out_Rdata)}
if(!dir.exists(IO$out_csv)){dir.create(IO$out_csv)}


if(par$local_user == "laurasymul"){
  source("../../TB-Restricted-Access-Repo/Scripts/00_variables_restricted_IO.R")
}else{
  IO$r_Data = "../../TB-Data/"
}


if(par$subset){
  IO$input_data = paste0(IO$r_Data,"input_data/Clue_20180119_subset/")
}else{
  IO$input_data = paste0(IO$r_Data,"input_data/Clue_20180119/")
}

IO$tmp_data = paste0(IO$r_Data, "tmp_data/")
IO$output_data = paste0(IO$r_Data,"output_data/")



IO$panels = "../Figures Tables Media/Figures/panels/"
