
# The function below cmpute the distance between 2 cycles as
# - a weighted average between the Symptoms distance and the Tracking behavior distance (Symptom distance has more weight)
# - the Symptom distance is the Jaccard distance between the smoothed symptoms (the Symtoms are smoothed to allow a little bit of flexibility regarding the cycleday)
# - the Tracking behavior distance is the binary distance between the days with logs or no logs


cppFunction(
  'NumericMatrix TB_distance_Rccp(NumericMatrix M_symptoms, NumericMatrix M_any_log , double r) {
  int N = M_symptoms.nrow();
  NumericMatrix J(N,N);
  for(int i = 0; i < (N-1); i++){
  for(int j = i+1; j < (N); j++){
  
  double Tracking_binary = sum(M_any_log(i,_) == M_any_log(j,_));
  double vector_size = (M_any_log(i,_).size());
  double Tracking_index = Tracking_binary/vector_size;
  
  NumericVector Symptom_overlap = pmin(M_symptoms(i,_),M_symptoms(j,_));
  NumericVector Symptom_union = pmax(M_symptoms(i,_),M_symptoms(j,_));
  double Symptom_index;
  if(sum(Symptom_union) == 0) {Symptom_index = Tracking_index/2;} 
  else{ Symptom_index = sum(Symptom_overlap) / sum(Symptom_union); }
  
  double d  = 1 - r*Symptom_index - (1-r)*Tracking_index; 
  
  J(i,j) = d;
  J(j,i) = d;
  }
  }
  return J;
  }'
)


TB_distance = function(M, r = 0.75, w = NA, smooth = TRUE, filter = c(1/4,1/2,1/4)){
  if(any(is.na(w))){w = rep(1,ncol(M))}
  if(any((w>1)|(w<0))){stop("w must be between 0 and 1\n")}
  if(length(w) != ncol(M)){stop("w must be NA or the same length as the number of columns of M\n")}
  M = as.matrix(M)
  
  
  M_any_log = 1*(M>=0);
  M_any_log = sweep(M_any_log,MARGIN=2,w,`*`)
  
  M_symptoms = M;
  M_symptoms[M_symptoms<0] = 0
  M_symptoms = sweep(M_symptoms,MARGIN=2,w,`*`)
  
  if(smooth){
    M_tmp = cbind(M_symptoms[,1],M_symptoms,M_symptoms[,ncol(M_symptoms)])
    M_tmp = t(apply(M_tmp, MARGIN = 1, FUN = function(x, f) { y = stats::filter(x, filter = f); y = y[!is.na(y)]; return(y)}, filter))
    M_symptoms = M_tmp
  }
  
  J = TB_distance_Rccp(M_symptoms, M_any_log, r)
  return(J)
}




# The function below is similar to the above one (earlier version) BUT
#  - the tracking distance is also a jaccard > that gave artificially smaller distance for users that were tracking a lot
#  - the symptoms are not smoothed, so that didn't allow for flexibility regarding the cycleday, especially for users that logged few symptoms around their menstruation


cppFunction(
'NumericMatrix new_jaccard_Rcpp(NumericMatrix M_symptoms, NumericMatrix M_any_log , double r) {
  int N = M_symptoms.nrow();
  NumericMatrix J(N,N);
  for(int i = 0; i < (N-1); i++){
    for(int j = i+1; j < (N); j++){
  
      double I1 = sum(pmin(M_symptoms(i,_),M_symptoms(j,_)))/sum(pmax(M_symptoms(i,_),M_symptoms(j,_))); 

      double I0 = sum(pmin(M_any_log(i,_),M_any_log(j,_)))/sum(pmax(M_any_log(i,_),M_any_log(j,_))); 
      if(sum(pmax(M_symptoms(i,_),M_symptoms(j,_))) == 0) I1 = I0; 

      
      double d  = 1 - r*I1 - (1-r)*I0; 
  
      J(i,j) = d;
      J(j,i) = d;
    }
  }
  return J;
}'
)


new_jaccard = function(M, r = 0.8, w = NA){
  if(any(is.na(w))){w = rep(1,ncol(M))}
  if(any((w>1)|(w<0))){stop("w must be between 0 and 1\n")}
  if(length(w) != ncol(M)){stop("w must be NA or the same length as the number of columns of M\n")}
  M = as.matrix(M)
  
  M_symptoms = M;
  M_symptoms[M_symptoms<0] = 0
  M_symptoms = sweep(M_symptoms,MARGIN=2,w,`*`)
  
  M_any_log = 1*(M>=0);
  M_any_log = sweep(M_any_log,MARGIN=2,w,`*`)
  
  J = new_jaccard_Rcpp(M_symptoms, M_any_log, r)
  return(J)
}








# deprecated


new_jaccard_R = function(M_symptoms, M_any_log, r, w){
  N = nrow(M_symptoms)
  J = matrix(0, N, N)
  for(i in 1:(N-1)){
    for(j in (i+1):N){
      I1 = sum(pmin(M_symptoms[i,],M_symptoms[j,]))/sum(pmax(M_symptoms[i,],M_symptoms[j,]))
      I0 = sum(pmin(M_any_log[i,],M_any_log[j,]))/sum(pmax(M_any_log[i,],M_any_log[j,]))
      d = 1 - r*I1 - (1-r)*I0
      J[i,j] = d  
      J[j,i] = d
    }
  }
  
}




cppFunction(
  'NumericMatrix new_jaccard_Rcpp_1(NumericMatrix M , double r , NumericVector w) {
  int N = M.nrow();
  NumericMatrix J(N,N);
  for(int i = 0; i < (N-1); i++){
  for(int j = i+1; j < (N); j++){
  NumericVector x = M(i,_);
  NumericVector y = M(j,_);
  
  NumericVector x1 = w*pmax(x,0); 
  NumericVector y1 = w*pmax(y,0); 
  double I1 = sum(pmin(x1,y1))/sum(pmax(x1,y1)); 
  NumericVector x0 = w ; 
  NumericVector y0 = w ;
  x0[x0<0] = 0;
  y0[y0<0] = 0;
  double I0 = sum(pmin(x0,y0))/sum(pmax(x0,y0)); 
  double d  = 1 - r*I1 - (1-r)*I0; 
  
  J(i,j) = d;
  J(j,i) = d;
  }
  }
  return J;
  }'
)


