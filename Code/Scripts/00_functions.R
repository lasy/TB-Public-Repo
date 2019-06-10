source("Scripts/00_functions_viz.R")


copy_days_tmp2out = function(dir_path){
  # remove the days folder that is in the output_data
  day_folder_output = paste0(IO$output_data,"days/")
  if(dir.exists(day_folder_output)){unlink(day_folder_output, recursive = TRUE)}
  folder_name = gsub("^.*/", "", substr(dir_path, 1, nchar(dir_path)-1) )
  file.copy(from = dir_path, to = IO$output_data, recursive=TRUE)
  file.rename(paste0(IO$output_data,folder_name),day_folder_output)
}



tac = function(chunck_name = "chunk_name"){
  elapsed = toc()
  df = data.frame(datetime = Sys.time(),
                  local_user = par$local_user,
                  chunck_name = chunck_name,
                  duration_sec = round(elapsed$toc - elapsed$tic),
                  duration_min = round((elapsed$toc - elapsed$tic)/60,2),
                  duration_hour = round((elapsed$toc - elapsed$tic)/60/60,4))
  
  write_csv(df, path = "time_log.csv", append = TRUE,col_names = FALSE)
}


is_file_open <- function(path) {
  suppressWarnings(
    "try-error" %in% class(
      try(file(path, 
               open = "w"), 
          silent = TRUE
      )
    )
  )
}



lu = function(x){
  length(unique(x))
}


rle_min = function(x){
  #rle_BC = rle(as.character(x))
  #j = which(rle_BC$value %in% par$BC_dict$name)
  rle_length = rle(as.numeric(x))$length
  if(length(rle_length) == 1){return(0)}else{return(min(rle_length))}
}


replace_NAs_with_latest_value = function(x) {   # repeats the last non NA value. Keeps leading NA
  ind = which(!is.na(x))      # get positions of nonmissing values
  if(is.na(x[1]))             # if it begins with a missing, add the 
    ind = c(1,ind)            # first position to the indices
  rep(x[ind], times = diff(   # repeat the values at these indices
    c(ind, length(x) + 1) ))  # diffing the indices + length yields how often 
}                               # they need to be repeated

# copied from: https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value

find_on_off_pill_transitions = function(x, n_cycles = 4){
  x = as.character(x)
  on_pill_pattern = c(rep("none / condoms",n_cycles),"on-pill",rep("pill",n_cycles))
  off_pill_pattern = c(rep("pill",n_cycles), "off-pill",rep("none / condoms",n_cycles))
  y = c()
  for(i in 1:length(x)){
    if((i <= n_cycles)|(i >(length(x)-n_cycles))){
      new_y = NA
    }else{
      check = c(all(x[(i-n_cycles):(i+n_cycles)] == on_pill_pattern),
                all(x[(i-n_cycles):(i+n_cycles)] == off_pill_pattern))
      if(!any(is.na(check)) & any(check)){
        new_y = c("on-pill","off-pill")[check]
      }else{
        new_y = NA
      }
    }
    y = c(y, new_y)
  } 
  return(y)
}





#loadv = function(x){load(x, verbose = TRUE)}


ncosh.dict = data.frame(sac.value = c(2,3,4,5), Factor.value = c(1, 0.5, 0.38, 0.24))

impute = function(obs = c(-1,-1,-1,0,1,-1,0,1,-1,-1,-1,-1,1,-1,1,-1,-1,-1,1,-1,0)){
  obs = as.vector(obs)
  new_obs = obs
  if(any(obs == -1)){
    # get the indices of missing data
    j = which(new_obs == -1)
    #removing leading sequences of missing data
    j = j[!(j == (1:length(j)))]
    #removing ending sequences of missing data
    j = j[!(j == (length(obs)-length(j)+1):length(obs))]
    #removing sequence longer than 3
    x = c(0,diff(j)) == 1
    ac = ave(x, cumsum(x == 0), FUN = cumsum)
    y = (1:length(j)) - ac
    irm = which(y %in% y[ac>=3])
    if(length(irm)>0){j = j[-irm]}
    
    if(length(j)>0){
      # obs_approx (interpolation of values)
      i = 1:length(obs)
      obs_approx = approx(x = i[-j],y = obs[-j],xout = i)$y
      
      # Factor
      x = 1*(obs<0)
      ac = ave(x, cumsum(x == 0), FUN = cumsum)
      rac = rev(ave(rev(x), cumsum(rev(x) == 0), FUN = cumsum))
      sac = ac+rac+((ac == 2)&(rac == 2))
      Factor = x * ncosh.dict$Factor.value[match(sac, ncosh.dict$sac.value)]
      
      new_obs[j] = obs_approx[j] * Factor[j] * 0.75
      #plot(obs, type = "b")
      #points(new_obs_tmp, type = "b", col = "blue")
      #points(new_obs, type = "b", col = "tomato")
    }
  }
  return(t(new_obs))
}



reshape_and_impute = function(days){
  #prepare : only keep the rows with n_logs or TB
  row_keep = which(((days$type == "n_logs") |  (days$type == "tender_breasts")) & (!is.na(days$cycleday_m_D)) & (!is.na(days$cycle_id_m)))
  col_keep = c("user_id","cycle_nb","cycle_id","cycle_id_m","cycleday","cycleday_m_D","type","number")
  d = days[row_keep,col_keep]
  
  #
  d$day_id = paste0(d$cycle_id_m,"_",d$cycleday_m_D)
  dup = duplicated(d$day_id)
  d_before_dup = d
  d = d[!dup,]
  d$n = 0
  d$n[d$day_id %in% d_before_dup$day_id[dup]]= 1
  
  #reshape
  d_wide = reshape(as.data.frame(d[,c("cycle_id_m","cycleday_m_D","n")]),
                          idvar = "cycle_id_m", timevar = "cycleday_m_D",
                          direction = "wide")
  d_wide[is.na(d_wide)] = -1
  o = order(as.numeric(sapply(strsplit(colnames(d_wide)[2:ncol(d_wide)],"\\."), "[[", 2)))
  d_wide = data.frame(user_id = d$user_id[match(d_wide$cycle_id_m,d$cycle_id_m)], cycle_id_m = d_wide$cycle_id_m, d_wide[,o+1] )
  
  #impute
  d_wide_imputed = t(apply(d_wide[,-c(1,2)], 1, impute))
  d_wide_imputed_df = data.frame(user_id = d_wide$user_id, cycle_id_m = d_wide$cycle_id_m, d_wide_imputed)
  colnames(d_wide_imputed_df) = colnames(d_wide)
  return(d_wide_imputed_df)
}



compute_average_distance_new_jaccard = function(d){
  col_excl = grep("id",colnames(d))
  if(length(col_excl) == 0){M = d}else{M = d[,-col_excl]}
  dist_mat = new_jaccard(M = M, r = par$r, w = par$w)
  dist_val = dist_mat[upper.tri(dist_mat)]
  avg_dist =  mean(dist_val)
  median_dist = median(dist_val)
  sd_dist = sd(dist_val)
  return(list(mean = avg_dist, median = median_dist, sd = sd_dist))
}


compute_average_distance = function(d){
  col_excl = grep("id",colnames(d))
  if(length(col_excl) == 0){M = d}else{M = d[,-col_excl]}
  dist_mat = TB_distance(M = M, r = par$r, w = par$w)
  dist_val = dist_mat[upper.tri(dist_mat)]
  avg_dist =  mean(dist_val)
  median_dist = median(dist_val)
  sd_dist = sd(dist_val)
  return(list(mean = avg_dist, median = median_dist, sd = sd_dist))
}


reshape_distance_matrix_to_long = function(D){
  D[lower.tri(D, diag = TRUE)] = NA
  if(!is.data.frame(D)){D = as.data.frame(D)}
  D$i = 1:nrow(D)
  D = reshape(D, direction = "long",idvar = "i", varying = list(1:nrow(D)))
  colnames(D) = c("i","j","dist")
  D$i = factor(D$i)
  D$j = factor(D$j, levels = sort(unique(D$j),decreasing = TRUE))
  return(D)
}

