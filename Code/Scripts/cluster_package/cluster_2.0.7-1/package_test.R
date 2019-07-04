
load("../../Rdata/d_wide_imputed.Rdata", verbose = TRUE)

source("../../00_libraries.R")

source("../../00_functions_jaccard_like_distance.R")





M = d_wide[1:10,-1]

#c1 = c(-1,-1,-1,0,0,0,1,1,1, 0.5, 0.5, 0.5)
#c2 = c(-1, 0, 1,-1,0, 1, -1, 0, 1,  -1, 0, 1 )
#M = matrix(c(c1, c2), nrow = 2, ncol = length(c1),byrow = TRUE)

w = (1:ncol(M))/ncol(M) # rep(0.5, ncol(M))
r = 0.7


### CLARA 
clara_n = clara(M , k = 1, metric = "custom_jaccard", w = w, cjratio = r)
clara_n$diss


### RCCP
jaccard_dist = new_jaccard(M ,w = w, r = r)
jaccard_dist


round(as.matrix(clara_n$diss), digits = 4)

all(round(as.matrix(clara_n$diss), digits = 4) == round(jaccard_dist, digits = 4))

rm(clara_n, jaccard_dist )