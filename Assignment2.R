set.seed(0)
n<-1000          #number of people
h_max=5          #maximum number of people living in each household
h <- sample(rep(1:n,sample(1:h_max,n,replace = T))[1:n])          #SHIKUKU, ADD THIS COMMENT FOR QUESTION 1

#now we make a function that recieves the vector beta of sociability parameters of all the people, 
#the vector h indicating in which household each person belongs and nc which is the average number of contacts per person,
#and it returns a list of which the i_th element is the vector of the probabilities of the i person 
#stumbling across the j person, where j is being represented as the j_th element of each vector of the list
#Such a probability is defined by: (nc*βi*βj)\(((β^_)^2)*(n − 1)), where β^_ is the mean of all the sociability parameters
#In these vectors, flatmates are not taken into consideration
get.net<-function(beta,h,nc=15){
  av_beta<-mean(beta)                    #mean of all the betas
  constant<-nc/((av_beta^2)*(n-1))       #we find the constant that is multiplied by the product of each βi and βj 
  list<-vector("list",length(beta))      #we create an empty list sized n that will be returned after it is amended
  for (i in 1:length(beta)) {            #we start finding the probabilities of stambling across to someone else of each person independently
                                         #beta2 is the vector that we will use in order get all the βj_s for the βi at once
    beta2<-beta                          #at the start of each loop, beta2 must be defined as beta again so that the next results are not affected by the previous ones
    beta2[which(h==h[i])]<-0             #the which function finds who are the people who live in the same apartment, and we set their βj to zero in order to not be taken into account
                                         #So, whenever we get an element of a vector in the list being zero, that means that i and j are flatmates
    list[[i]]<-(constant*beta[i])*beta2  #we create the desired probability by multiplying the constant and βi with the vector of the βj_s,
                                         #as each βj is multiplyed with βi (if they are not flatmates) and the constant
  }
  return(list)
}