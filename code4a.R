a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,
          + fileEncoding="UTF-8")
brace_places<-rep(0,2*length(grep("[[]",a))) #vector with length equal to the number of words containing braces

b<-grep("[[]",a) #positions of all left [
wrong_br<-rep(" ",length(b)) #vector to store the words with left [ and no pair

#for (i in 1:length(b)) {
#  brace_places[2*i-1]<-b[i]
#  next100<-b[i]:(b[i]+100)
#  sec_br<-grep("[]]",a[next100])
#  brace_places[2*i]<-next100[sec_br[1]] }

for (i in 1:length(b)) {
  brace_places[2*i-1]<-b[i] #odd slots represent the positions of all left [
  next100<-b[i]:(b[i]+100) #vector containing the next 100 words from a word with left [
  sec_br<-grep("[]]",a[next100]) #positions of right ] in the following 100 words from left ]
  if (length(sec_br)>0) {  #we take into account the possibility of a right ] not existing
    brace_places[2*i]<-next100[sec_br[1]] #addition of the positions of all the right ] to even slots, next to the position of their left [ pair
  }else {
    brace_places[2*i]<-0
    brace_places[2*i-1] <-0   #in case of an absent right ], we ignore the position of the left one
  }
}
numb_words_del <-0
for (i in 1:length(b)) {
  if (brace_places[2*i-1]!=0) {
    numb_words_del<-(numb_words_del + brace_places[2*i]-brace_places[2*i-1]+1)
  }
}

reverse_bp <- rep(0,length(brace_places))
for (i in 1:length(brace_places)) {
  reverse_bp[i] <- brace_places[length(brace_places)-i+1] #creates a vector of all the aforementioned positions with the oposite order
}

for (i in 1:length(b)) {
  
}