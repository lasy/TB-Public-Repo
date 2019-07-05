par = list()
par$data_type = "full" # "synthetic" "subset" "full"
par$local_user = Sys.getenv("LOGNAME")
par$run_optimal_nb_of_cluster = ifelse(par$data_type == "synthetic",FALSE,TRUE)
par$n_cores = detectCores() - 1
par$reset = FALSE
par$reset_steps = c("","","")

source("Scripts/00_variables_IO.R")

TB = "tender_breasts"

breaks = list()
breaks$age = seq(10,45, by = 5)
breaks$height = seq(100,240,by = 5)
breaks$bmi = c(0,seq(15,33,by = 3), Inf)
breaks$n_logs


# VIZ variables

cols = list()
cols$age_cat <- scales::seq_gradient_pal("seagreen1", "steelblue4")(seq(0,1,length.out=5))
cols$menstrual = rgb(255,0,98, max = 255)
cols$premenstrual =   rgb(1,161,247, max = 255)  
cols$clusters = c(cols$menstrual,cols$premenstrual) 
#cols$clusters_clara = c("brown1","lightskyblue","goldenrod1","royalblue1")
#cols$clusters_4 = c(cols$clusters, rgb(170,162,224, max = 255),"gray")
cols$cl_1 = rgb(246,106,98, max = 255)
cols$cl_2 = rgb(214,189,90, max = 255)
cols$cl_3 = rgb(78,212,172, max = 255)
cols$cl_4 = rgb(108,181,247, max = 255)

cols$clusters_4 = c(cols$cl_1,cols$cl_2,cols$cl_3,cols$cl_4)
cols$clusters_5 = c("gray60",cols$clusters_4 )
cols$clusters_6 = c("gray40","gray65",cols$clusters_4 )


cols$pill = rgb(14,179,208, max = 255)
cols$NC = rgb(253,135,115, max = 255)
cols$BC = c(cols$NC, cols$pill)
cols$BC3 = c(cols$BC, "gray")
cols$on_pill = rgb(14,179,208, max = 400)
cols$off_pill = rgb(253,135,115, max = 400)
cols$off_on_pill = rgb(14,179,208, max = 500)
cols$on_off_pill = rgb(253,135,115, max = 500)
cols$BC_users = c(cols$BC, cols$on_pill,cols$off_pill,cols$off_on_pill,cols$on_off_pill,"black")

cols$age_cat_BC <- scales::seq_gradient_pal("gray80", "black")(seq(0,1,length.out=5))
cols$age_x_BC <- c(scales::seq_gradient_pal(cols$NC, "black")(seq(0,1,length.out=6))[1:5],
                   scales::seq_gradient_pal(cols$pill, "black")(seq(0,1,length.out=6))[1:5],
                   scales::seq_gradient_pal("gray", "black")(seq(0,1,length.out=6))[1:5])


cols_feature = data.frame(type = c("heavy","medium","light","spotting",
                                   "tender_breasts",
                                   "taken","late","missed","double",
                                   "n_logs"),
                          col = c("tomato4","tomato3","tomato2","tomato1",
                                  "lightblue",
                                  "steelblue3","orange","red","black",
                                  "gray70"),
                          stringsAsFactors = FALSE)


cols$init_TB_group = c("skyblue3","skyblue4","black")
N = 3
cols$BC_x_init_TB_group = c(scales::seq_gradient_pal(cols$NC, "black")(seq(0,1,length.out=N+1))[1:N],
                            scales::seq_gradient_pal(cols$pill, "black")(seq(0,1,length.out=N+1))[1:N],
                            scales::seq_gradient_pal("gray", "black")(seq(0,1,length.out=N+1))[1:N])
rm(N)


cols$pill_trans = c("gray40","royalblue1")

viz = list()
viz$scale = 1.1
viz$full_width = 12




par$n_cluster_default = 2
par$selected_countries = c("United States","United Kingdom", "Brazil","Germany","France", "Unknown")
par$x.axis = seq(-14,7,by = 7)
par$max_batch_size = 1000
par$min_n_batches = 10

par$BC_dict = data.frame(name = c("none / condoms","pill"),
                         binary = c(0,1))


par$users_BC_dict = data.frame(
  name = c("none / condoms","pill","on pill","off pill","off-on pill", "on-off pill","mult. trans."),
  num = c(0:6))




par$cycles_m = list()
par$cycles_m$BC_dict = data.frame(
  value = c("none / condoms","pill","none / condoms,pill","pill,none / condoms"),
  new_value = c("none / condoms","pill","on-pill","off-pill"))


sigmoid = function(x, ix, s){ 1/(1+exp(-(x-ix)*s))}

par$w  = 0.5 + (sigmoid(-18:7,ix = -12,s = 0.5) - sigmoid(-18:7, ix = 8, s = 0.6))/2
par$ww = data.frame(cycleday_m_D = -18:7, w = par$w)

par$r = 0.75

par$D = 18
par$Df = 7
par$cycleday_m_D = c(-par$D:par$Df)

par$bmi_cat_exclude = c("(0,15]")
par$age_cat_exclude = c("(10,15]","(40,45]")

par$clara = list()
par$clara$samples = 50
par$clara$sampsize = 200



special_pill_user = "6ef947aeb9cfe0468bd1efd26b8a10dca0a1e645"


