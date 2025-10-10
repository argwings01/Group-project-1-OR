set.seed(0)
n<-1000
h_max=5
h <- sample(rep(1:n,sample(1:h_max,n,replace = T))[1:n])
h
get.net<-function(beta,h,nc=15){
  av_beta<-mean(beta)
  beta2<-beta
  constant<-nc/((av_beta^2) *(n-1))
  for (i in n:2) {
    temp<-beta2[-1]
    beta2[i+1]<-beta2[i]
    beta2[1]<-temp
  }
    
#    for (j in 1:length(n)) {
 #   if (h[i]==h[j]){
  # beta_j
    
    
  }return(result_matrix)
}

get.net(50, 3, nc = 15)