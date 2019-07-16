
IO = list()

# INPUT AND OUTPUT DATA

if((par$local_user == "laurasymul") & (par$data_type != "synthetic")){
  source("../../TB-Restricted-Access-Repo/Scripts/00_variables_restricted_IO.R") # assign value to IO$r_Data
}else{
  IO$r_Data = "../../TB-Data/"
}


if(par$data_type == "synthetic"){
  folder_name = "synthetic/"
}else if(par$data_type ==  "subset"){
  folder_name = "Clue_20180119_subset/"
}else{
  folder_name = "Clue_20180119/"
}

IO$input_data = paste0(IO$r_Data,folder_name,"input_data/")
IO$tmp_data = paste0(IO$r_Data,folder_name, "tmp_data/")
IO$output_data = paste0(IO$r_Data,folder_name,"output_data/")
if(!dir.exists(IO$tmp_data)){dir.create(IO$tmp_data, recursive = TRUE)}
if(!dir.exists(IO$output_data)){dir.create(IO$output_data, recursive = TRUE)}


# PUBLIC OUTPUT FIGURES AND DATA


IO$panels = paste0("../Figures Tables Media/Figures/panels/",folder_name)
if(!dir.exists(IO$panels)){dir.create(IO$panels, recursive = TRUE)}


IO$tables = paste0("../Figures Tables Media/Tables/",folder_name)
if(!dir.exists(IO$tables)){dir.create(IO$tables, recursive = TRUE)}



IO$public_output_data = paste0("../Data/",folder_name)
IO$out_Rdata = paste0(IO$public_output_data, "Rdata/")
IO$out_csv = paste0(IO$public_output_data, "CSV/")

if(!dir.exists(IO$public_output_data)){dir.create(IO$public_output_data, recursive = TRUE)}
if(!dir.exists(IO$out_Rdata)){dir.create(IO$out_Rdata)}
if(!dir.exists(IO$out_csv)){dir.create(IO$out_csv)}

