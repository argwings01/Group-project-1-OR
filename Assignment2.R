set.seed(0)
h_max<-5

h<-rep(0,1000)
for (i in 1:1000) {
  m<-sample(1:h_max)[[1]]  #size of each household
  h[i]<-rep(rep("i",sample(h_max)[1]),1000)
  }
h<-sample(1:h_max, rep(sample(n)))

set.seed(0)
n<- rep(1:200,5); sample(n, 1000, replace = T)

n


# 1. Create a sequence of unique numbers (e.g., from 1 to 200).
unique_values <- 1:200

# 2. Repeat each unique number 5 times. This results in a vector of length 1000.
repeated_values <- rep(unique_values, each = 5)

# 3. Randomly shuffle the order of the 1000 numbers.
random_numbers <- sample(repeated_values, size = 1000, replace = FALSE)



n<-sample(5,1000,replace = T);rep(1000)
n
get.net<-function(beta,h,nc=15){
  beta=n
  bat<-("")
  for (i in 1:length(beta)) {
    if (i!=i){
      result[i]<-beta 
    }
    
  }return(result_matrix)
}

get.net(50, 3, nc = 15)