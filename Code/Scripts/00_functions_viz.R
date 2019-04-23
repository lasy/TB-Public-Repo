


ggplot_user_example = function(d = days[1:10,], title = TRUE,size_factor = 1){
  #ordering the cycles
  d$cycle_number = factor(d$cycle_nb_m, levels = min(d$cycle_nb_m, na.rm = TRUE):max(d$cycle_nb_m, na.rm = TRUE))
  # for size
  d$number[d$type == "tender_breasts"] = 2
  d$number[d$category == "period"] = match(d$type[d$category == "period"], c("spotting","light","medium","heavy"))
  d$label = d$category
  d$label[d$label == "pain"] = "TB"
  d$label[d$label == "n_logs"] = "# logs"
  d$label = factor(d$label, levels = c("period","TB","# logs"))
  
  d$type = factor(d$type, levels = c("tender_breasts","spotting","light","medium","heavy","n_logs"))
  d$category = factor(d$category, levels = c("period","pain","n_logs"))
  
  
  # plot itself
  g = ggplot(d, aes(x = cycleday_m, y = label, col = type, shape = category, size = number)) + 
    geom_point()+ scale_size_continuous(range = c(1, 2.5)*size_factor)+
    facet_grid(cycle_number ~.) + 
    guides(shape = FALSE, size = FALSE, color = FALSE) + 
    ylab("") + xlab("cycleday from 1st day of menstruation") + scale_x_continuous(breaks = par$x.axis, limits = c(-14,14))+
    scale_color_manual(values = c("skyblue3","tomato1","tomato2","tomato3","tomato4","gray80"))+
    theme(strip.text.y = element_text(angle = 0))
  if(title){g = g + ggtitle(d$user_id[1])}
  g
  
  return(g)
  
}



ggplot_imputed_TB = function(sel_d, facet_grid = NULL, cycle_id = TRUE, col = NA, full_cycle_id = FALSE){
  
  mandatory_columns = c("tender_breasts","cycle_nb_m","cycleday_m_D", "user_id")
  if(!all(mandatory_columns  %in% colnames(sel_d))){
    stop(paste0("tracking tables must contain: ",paste0(mandatory_columns,collapse = ", "),"\n"))
  }
  
  if("cluster_num" %in% colnames(sel_d)){clusters_exist = TRUE}else{clusters_exist = FALSE}
  
  sel_d$alpha = pmax(sel_d$tender_breasts, 0)
  sel_d$stroke = 1*(sel_d$tender_breasts == 0)
  #sel_d$alpha_stroke = pmax(sel_d$tender_breasts,0)+(sel_d$tender_breasts ==0)
  
  #color
  if(clusters_exist){sel_d$color = as.factor(sel_d$cluster_num)}else{sel_d$color = as.factor(sel_d$user_id)}
  if(!is.na(col)){eval(parse(text = paste0("sel_d$color = as.factor(sel_d$",col,")")))}
  
  sel_d$short_user_id = paste0(substr(sel_d$user_id,1,4),"...",substr(sel_d$user_id ,37,40))
  
  
  #y axis (cycle_id or cycle_nb)
  if(cycle_id){
    ylab = ""
    if(full_cycle_id){
      sel_d$y = as.factor(sel_d$cycle_id_m); 
    }else{
      if(!is.factor(sel_d$cycle_id_m)){sel_d$cycle_id_m = factor(sel_d$cycle_id_m, levels = rev(unique(sel_d$cycle_id_m)))}
      levels = levels(sel_d$cycle_id_m)
      levels_short = paste0(substr(levels,1,4),"...",substr(levels ,nchar(levels)-4,nchar(levels)))
      cycle_ids = as.character(sel_d$cycle_id_m)
      sel_d$short_cycle_ids =paste0(substr(cycle_ids,1,4),"...",substr(cycle_ids ,nchar(cycle_ids)-4,nchar(cycle_ids)))
      sel_d$y = factor(sel_d$short_cycle_ids, levels = levels_short)
    }
  }else{sel_d$y = as.factor(sel_d$cycle_nb_m); ylab = "cycle number"}
  
  g = ggplot(sel_d, 
             aes(x = cycleday_m_D, y = y, col = color, size = 1))+
    scale_x_continuous(breaks = par$x.axis)+
    geom_vline(xintercept = 0, col = "gray90", size = 1.5)+
    geom_point(aes(alpha = stroke), shape = 1) + # stroke
    geom_point(aes(alpha = alpha), shape = 16)+ # fill
    scale_alpha(range = c(0, 1))+
    xlab("cycleday from 1st day of menstruation") + ylab(ylab)+
    guides(col = FALSE, alpha = FALSE, size = FALSE)
  
  if(length(facet_grid)>0){
    if(typeof(facet_grid) != "character"){stop("facet_grid must be NULL or a character vector \n")}
    if(!all(facet_grid %in% colnames(sel_d))){stop("facet_grid must be colnames of sel_d \n")}
    if("user_id" %in% facet_grid){facet_grid[facet_grid == "user_id"] = "short_user_id"}
    facet_grid = paste(facet_grid, collapse = " + ")
    eval(parse(text = paste0("g = g + facet_grid(",facet_grid," ~ .,scales = 'free_y')")))
    #if(clusters_exist){g = g + facet_grid( cluster_num + short_user_id  ~ ., scales = "free_y")}else{g = g + facet_grid(short_user_id ~ ., scales = "free_y")}
  }
  
  return(g)
}


