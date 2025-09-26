a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,
          + fileEncoding="UTF-8")
brace_places<-rep(0,2*length(grep("[[]",a))) #vector with length equal to the number of words containing braces

#first we will remove all the words that are inside of braces [...]

b<-grep("[[]",a) #positions of all left [
c<-grep("[]]",a) #positions of all right ]

# now we will make a vector brace_places which will indicate the positions of all words starting with [ and next to them the word in which the braces close with the right ]
for (i in 1:length(b)) {
  brace_places[2*i-1]<-b[i] #odd slots represent the positions of all left [
  next100<-b[i]:(b[i]+100) #vector containing the next 100 words from a word with left [
  sec_br<-grep("[]]",a[next100]) #sec_br contains the positions of all right ] in the following 100 words from left ]s
  if (length(sec_br)>0) {  #we take into account the possibility of a right ] not existing, so if it exists sec_br has at least 1 element
    brace_places[2*i]<-next100[sec_br[1]] #addition of the positions of all the right ] to even slots, next to the position of their left [ pair
  }else {
    a[b[i]]<-gsub("[[]","",a[b[i]]) #delete of the unnecessary left [
    brace_places[2*i]<-0  
    brace_places[2*i-1] <-0   #in case of an absent right ], we ignore the position of the left one and we recognize the error with 2 zeros side by side instead of the positions of the braces
  }
}


numb_words_del <-0  #we enumerate the number of words that are going to be deleted because they are inside braces
for (i in 1:length(b)) {
  if (brace_places[2*i-1]!=0) { #previously we determined as 0 the braces without a pair, so we omit those
    numb_words_del<-(numb_words_del + brace_places[2*i]-brace_places[2*i-1]+1) #the difference of the numbers representing the position of the left and right braces +1 represent the number of words inside the 2 braces, left braces in odd number positions and right ones in even positions
  }
}

a_new <- rep(" ",length(a)-numb_words_del) #vector sized as much as the number of words that are not contained in braces
k<-1 #this will represent the position of the element that is being added in the new vector
m<-1 #this represents the pair of braces that we test if we are inside of it
n<-0 #when this is equal to zero then we are outside braces, when it is positive then we are inside braces and we should omit these words from being added and it will represent the number of words left inside the braces
for (i in 1:length(a)) {
  if (i >= brace_places[m] & i <=brace_places[m+1] & n==0) { #here we enter a pair of braces, we don't add a new word in the new vector and there are still (original number of words inside the braces)-1 words remaining to be omitted
    n<-length(brace_places[m]:brace_places[m+1])-1
    m<-m+2 #we move to next pair of braces
  } else if (n>0){ #we are still inside braces, we don't add new words to the new vector and there is one less word remaining to be omitted
    n<-n-1
  } else { #we are outside of braces and we add a new word, k increases in order to keep up with i
    a_new[k]<-a[i]
    k<-k+1
  }
}







