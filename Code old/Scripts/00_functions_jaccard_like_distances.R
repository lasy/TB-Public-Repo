


new_jaccard_vecs = function(x, y, r = 0.8, w = NA){
  x1 = w*pmax(x,0); y1 = w*pmax(y, 0)
  I1 = sum(pmin(x1,y1))/sum(pmax(x1,y1))
  x0 = w*(x>=0) ; y0 = w*(y>=0)
  I0 = sum(pmin(x0,y0))/sum(pmax(x0,y0))
  D = 1 - r*I1 - (1-r)*I0
  return(D)
}

new_jaccard_loop = function(M, r = 0.8, w = NA){
  if(any(is.na(w))){w = rep(1,ncol(M))}
  if(any((w>1)|(w<0))){stop("w must be between 0 and 1\n")}
  
  N = nrow(M)
  J = matrix(0, nrow = N, ncol =N)
  for(i in 1:(N-1)){
    #cat(i,"\n")
    for(j in min((i+1),N):N){
      #cat("\t",j,"\n")
      d = new_jaccard_vecs(x = M[i,], y = M[j,], r = r, w = w)
      J[i,j] = d
      J[j,i] = d
    }
  }
  return(J)
}





new_jaccard_vectorized = function(v, r = r, w = w){
  return(new_jaccard_vecs(x = v[1:(length(v)/2)], y = v[((length(v)/2)+1):length(v)], r = r, w = w))
}


new_jaccard_no_Rcpp = function(M, r = 0.8, w = NA){
  if(any(is.na(w))){w = rep(1,ncol(M))}
  if(any((w>1)|(w<0))){stop("w must be between 0 and 1\n")}
  
  N = nrow(M)
  J = matrix(0, nrow = N, ncol =N)
  
  is = rep(1:(N-1), times = (N-1):1)
  js = N+2-sequence(N:1); js = js[-which(js == (N+1))]
  M2 = cbind(M[is,], M[js,])
  
  dist = apply(M2, 1, FUN = new_jaccard_vectorized,r = r, w = w)
  J[is+N*(js-1)] = dist
  J[js+N*(is-1)] = dist
  
  return(J)
}





cppFunction(
  'double new_jaccard_vecs_Rcpp_m(NumericVector m , double r , NumericVector w) {
  NumericVector x = m[seq_len(m.size()/2)-1]; 
  NumericVector y = m[seq_len(m.size()/2)-1+(m.size()/2)]; 
  NumericVector x1 = w*pmax(x,0); 
  NumericVector y1 = w*pmax(y,0); 
  double I1 = sum(pmin(x1,y1))/sum(pmax(x1,y1)); 
  NumericVector x0 = x ; 
  NumericVector y0 = y ;
  int n = x.size();
  for(int i = 0; i < n; i++){
  if(x0[i]>=0){x0[i] = w[i];}else{x0[i] = 0;}
  if(y0[i]>=0){y0[i] = w[i];}else{y0[i] = 0;}
  }
  double I0 = sum(pmin(x0,y0))/sum(pmax(x0,y0)); 
  double D = 1 - r*I1 - (1-r)*I0; 
  return D;
  }'
)


new_jaccard_with_vecs_Rcpp = function(M, r = 0.8, w = NA){
  if(any(is.na(w))){w = rep(1,ncol(M))}
  if(any((w>1)|(w<0))){stop("w must be between 0 and 1\n")}
  
  N = nrow(M)
  J = matrix(0, nrow = N, ncol =N)
  
  is = rep(1:(N-1), times = (N-1):1)
  js = N+2-sequence(N:1); js = js[-which(js == (N+1))]
  M2 = cbind(M[is,], M[js,])
  
  dist = apply(M2, 1, FUN = new_jaccard_vecs_Rcpp_m,r = r, w = w)
  J[is+N*(js-1)] = dist
  J[js+N*(is-1)] = dist
  
  return(J)
}








cppFunction(
  'double new_jaccard_vecs_Rcpp(NumericVector x, NumericVector y , double r , NumericVector w) {
  NumericVector x1 = w*pmax(x,0); 
  NumericVector y1 = w*pmax(y,0); 
  double I1 = sum(pmin(x1,y1))/sum(pmax(x1,y1)); 
  NumericVector x0 = x ; 
  NumericVector y0 = y ;
  int n = x.size();
  for(int i = 0; i < n; i++){
  if(x0[i]>=0){x0[i] = w[i];}else{x0[i] = 0;}
  if(y0[i]>=0){y0[i] = w[i];}else{y0[i] = 0;}
  }
  double I0 = sum(pmin(x0,y0))/sum(pmax(x0,y0)); 
  double D = 1 - r*I1 - (1-r)*I0; 
  return D;
  }'
)






########### TESTS

cppFunction(
  'NumericMatrix test_Rcpp(NumericMatrix M , double r , NumericVector w) {
  int N = M.nrow();
  NumericMatrix J(N,N);
  return J;
  }'
)


test_Rcpp(matrix(nrow = 20, ncol = 2), 2,1)


