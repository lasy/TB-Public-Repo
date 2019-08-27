


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



ggplot_imputed_TB = function(sel_d, facet_grid = NULL, facet_grid_x = NULL, facet_grid_y = NULL, cycle_id = TRUE, col = NA, full_cycle_id = FALSE){
  
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
  short_user_id_levels = paste0(substr(levels(sel_d$user_id),1,4),"...",substr(levels(sel_d$user_id) ,37,40))
  sel_d$short_user_id = factor(sel_d$short_user_id, levels = short_user_id_levels)
 
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
      sel_d$y = factor(sel_d$short_cycle_ids, levels = unique(levels_short))
    }
  }else{sel_d$y = factor(sel_d$cycle_nb_m, levels = sort(unique(as.numeric(sel_d$cycle_nb_m)), decreasing = TRUE)); ylab = "cycle number"}
  

  
  g = ggplot(sel_d, 
             aes(x = cycleday_m_D, y = y, col = color, size = 1))+
    scale_x_continuous(breaks = par$x.axis)+
    geom_vline(xintercept = 0, col = "gray90", size = 1.5)+
    geom_point(aes(alpha = stroke), shape = 1) + # stroke
    geom_point(aes(alpha = alpha), shape = 16)+ # fill
    scale_alpha(range = c(0, 1))+
    xlab("cycleday from 1st day of menstruation") + ylab(ylab)+
    guides(col = FALSE, alpha = FALSE, size = FALSE)
  
  
  if((length(facet_grid) == 0) & ((length(facet_grid_x)>0)|(length(facet_grid_y)>0))){facet_grid = c(facet_grid_x,facet_grid_y)}
  
  if(length(facet_grid)>0){
    if(typeof(facet_grid) != "character"){stop("facet_grid must be NULL or a character vector \n")}
    if(!all(facet_grid %in% colnames(sel_d))){stop("facet_grid must be colnames of sel_d \n")}
    if("user_id" %in% facet_grid){
      facet_grid[facet_grid == "user_id"] = "short_user_id"; 
      facet_grid_x[facet_grid_x == "user_id"] = "short_user_id";
      facet_grid_y[facet_grid_y == "user_id"] = "short_user_id"
    }
    if((length(facet_grid_x)==0) & (length(facet_grid_y)==0)){facet_grid_y = facet_grid}
    if((length(facet_grid_x)>0) & (length(facet_grid_y)==0)){facet_grid_y = "."}
    if(length(facet_grid_x) == 0){facet_grid_x = "."}
    facet_grid_x = paste(facet_grid_x, collapse = " + ")
    facet_grid_y = paste(facet_grid_y, collapse = " + ")
    eval(parse(text = paste0("g = g + facet_grid(",facet_grid_y," ~ ", facet_grid_x,",scales = 'free_y')")))
    #if(clusters_exist){g = g + facet_grid( cluster_num + short_user_id  ~ ., scales = "free_y")}else{g = g + facet_grid(short_user_id ~ ., scales = "free_y")}
  }
  
  return(g)
}



ggplot_user_history = function(d, pill_transition = c("no trans","on pill","off pill"),
                               replace_pain_by_symptom = TRUE, 
                               print_x_lab = TRUE, print_title = TRUE, make_x_axis_symmetrical = FALSE){
  
  if(uniqueN(d$user_id)>1){stop("this function is only suited for a single user table\n")}
  
  pill_transition = pill_transition[1]
  if(pill_transition == "on pill"){
    ref_date = min(d$date[d$BC == "pill"])
  }else if(pill_transition == "off pill"){
    ref_date = min(d$date[d$BC == "none / condoms"])
  }else{
    ref_date = min(d$date)
  }
  d$rel_date = as.numeric(d$date - ref_date)
  if(!is.na(pill_transition)){
    d = d[d$rel_date %in% -180:180,]
  }
  
  number_max = max(d$number[which(d$type == "n_logs")], na.rm = TRUE)
  d = d[d$type %in% cols_feature$type,]
  d$number[is.na(d$number)] = 0.75*number_max
  d$number[d$type == "heavy"] = number_max
  d$number[d$type == "medium"] = 0.75*number_max
  d$number[d$type == "light"] = 0.5*number_max
  d$number[d$type == "spotting"] = 0.25*number_max
  
  d$type = factor(d$type, levels = cols_feature$type)
  colors = cols_feature$col[cols_feature$type %in% unique(d$type)]
  
  if(replace_pain_by_symptom){
    d$category2 = gsub("pain",TB,d$category)
    d$category2 = factor(d$category2, levels = rev(c("pill_hbc","period",TB,"n_logs")))
  }else{
    d$category2 = d$category
  }
  
  if(print_x_lab){x_lab = "relative dates (in days)"}else{x_lab = ""}
  if(print_title){title = unique(d$user_id)}else{title = ""}
  if(make_x_axis_symmetrical){x_limits = c(-max(abs(d$rel_date)),max(abs(d$rel_date)))}else{x_limits = range(d$rel_date )}
  
  
  g = ggplot(d, aes(x = rel_date, y = category2, col = type, size = number)) + 
    geom_vline(xintercept = d$rel_date[d$cycleday == 1], col = "gray90")+
    geom_point(shape = "|")+
    scale_color_manual(values = colors)+
    scale_size(range = c(1,5))+
    scale_x_continuous(breaks = seq(-10*30,10*30,by = 30), limits = x_limits)+
    theme(axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position="bottom")+
    ggtitle(title)+
    ylab("")+xlab(x_lab)+
    guides(size = FALSE, col = FALSE)
  
  return(g)
}


