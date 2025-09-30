setwd("C:/Users/Alex/Documents/University/University_of_Edinburgh/1st_semester/statistical_programming/stats_assignment1")
a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8")
brace_places<-rep(0,2*length(grep("[[]",a))) #vector with length equal to the number of words containing braces

#first we will remove all the words that are inside of braces [...]

b<-grep("[[]",a) #positions of all left [
c<-grep("[]]",a) #positions of all right ]

# now we will make a vector brace_places which will indicate the positions of all words starting with [ and next to them the word in which the braces close with the right ]
for (i in 1:length(b)) {
  brace_places[2*i-1]<-b[i]               #odd slots represent the positions of all left [
  next100<-b[i]:(b[i]+100)                #vector containing the next 100 words from a word with left [
  sec_br<-grep("[]]",a[next100])          #sec_br contains the positions of all right ] in the following 100 words from left ]s
  if (length(sec_br)>0) {                 #we take into account the possibility of a right ] not existing, so if it exists sec_br has at least 1 element
    brace_places[2*i]<-next100[sec_br[1]] #addition of the positions of all the right ] to even slots, next to the position of their left [ pair
  }else {
    a[b[i]]<-gsub("[[]","",a[b[i]])       #delete of the unnecessary left [
    brace_places[2*i]<-0  
    brace_places[2*i-1] <-0               #in case of an absent right ], we ignore the position of the left one and we recognize the error with 2 zeros side by side instead of the positions of the braces
  }
}


numb_words_del <-0                     #we enumerate the number of words that are going to be deleted because they are inside braces
for (i in 1:length(b)) {
  if (brace_places[2*i-1]!=0) {        #previously we determined as 0 the braces without a pair, so we omit those
    numb_words_del<-(numb_words_del + brace_places[2*i]-brace_places[2*i-1]+1) #the difference of the numbers representing the position of the left and right braces +1 represent the number of words inside the 2 braces, left braces in odd number positions and right ones in even positions
  }
}

a_new <- rep(" ",length(a)-numb_words_del) #vector sized as much as the number of words that are not contained in braces
k<-1                  #this will represent the position of the element that is being added in the new vector
m<-1                  #this represents the pair of braces that we test if we are inside of it
n<-0                  #when this is equal to zero then we are outside braces, when it is positive then we are inside braces and we should omit these words from being added and it will represent the number of words left inside the braces
for (i in 1:length(a)) {
  if (i >= brace_places[m] & i <=brace_places[m+1] & n==0) { #here we enter a pair of braces, we don't add a new word in the new vector and there are still (original number of words inside the braces)-1 words remaining to be omitted
    n<-length(brace_places[m]:brace_places[m+1])-1
    m<-m+2             #we move to next pair of braces
  } else if (n>0){     #we are still inside braces, we don't add new words to the new vector and there is one less word remaining to be omitted
    n<-n-1
  } else {             #we are outside of braces and we add a new word 
    if (a[i]==toupper(a[i]) & a[i] != "I" & a[i] != "A") {    #we omit all the upper case words/Arabic numbers except of "A" and "I"
      next
    }
    else {
      a_new[k]<-a[i]
      k<-k+1             #k increases in order to keep up with i, so in the next step a new word can be added
    }
  }
}
a_new<-gsub("_","",a_new) #removes _ from all the words
a_new<-gsub("-","",a_new) #removes - from all the words


#now we make a function that can locate the words that are stored with a punctuation mark, and splits them into "word" and "punctuation mark"
#we assume that all punctuation marks are the last character of the word that they are attached to
split_punct<-function(x) {
  ii<-grep("\\,|\\.|\\;|\\!|\\:|\\?",x)                    #first we create a vector containing the positions of all the words with punctuation marks in the passage
  xs<-rep("",length(ii)+length(x))                         #vector to store all the words separated from the punctuation marks
  k<-0                                                     #k defines the number of punctuation marks that has been separated from their respective word
  for (i in 1:length(x)){
    if (i %in% ii){                                                  #checks if each word has a punctuation mark attached to it
      punc <- length(strsplit(x[i],"")[[1]])                         #strsplit(x[i],"") takes the word x[i] and breaks it down character by character, and via [[1]] we take the first character which is the vector containing all the characters in this word, so punc is the length of the word+punctuation mark
      xs[i+k]<-paste(strsplit(x[i],"")[[1]][1:punc-1],collapse="")   #we keep only the characters that consist the word without the punctuation mark and with collapse="", it compiles the word again and then it places the word in the proper location, which is k places ahead of its original one
      xs[i+k+1]<-paste(strsplit(x[i],"")[[1]][punc],collapse="")     #Similarly now it keeps the last character which is the punctuation mark, and places it to the proper location in the vector, on the right of its former attached word
      k<-k+1                                                         #number of punctuation marks separated increased by one
    }
    else {
      xs[i+k]<-x[i]                                                #no punctuation marks found, so we just add the word as it is
    }
  }
  return(xs)                   #returns the amended passage with splitted words and punctuation marks
}

a_new<-split_punct(a_new)      #we separate all the punctuation marks from their respective words in the amended Shakespeare passage
a<-tolower(a_new)              #we make all the words to be lower cased for most accurate results and we re-use the old a variable as it was not used anymore, and we finally have a cleaned passage


# Finding the unique elements in vector a of our cleaned data

b <- unique(a, incomparables = FALSE, fromLast = FALSE, nmax = NA)
b
# Matching each unique words to the vector a to know there positions

index_vector <- match(a,b,nomatch = NA_integer_)
index_vector <- match(a,b)

# the number of times Each unique word occurs in the text
words_n <- tabulate (index_vector, nbins = length(b)) # normal use of tabulate

# Most common words used which are approximately 1000 words

# Since using rank was tough to understand, we used the alternative order()
word_freq <- data.frame( words=b, words_n)        #Putting them in dataframe to for easy access to words
sort_descend <- word_freq[order(word_freq$words_n, decreasing = T), ]  # arranging the frequencies of words from most to least
common <- min(1000, nrow(sort_descend))       # Finding the 1000 most common words from the arranging
b <- sort_descend$words[1:common]    # Vector b with 1000 most common words

# making tokens of length a using match and making sure it has all common words

tokens <- match( a, b)

# Constructing the matrix M

mlag<-4
n <-length(tokens)  
R <- c(n - mlag)

# Giving the matrix dimensions 

M <- matrix(NA, nrow = R, ncol = mlag + 1)

# Fill the matrix column by column (for each lag)

for (j in 0:mlag) {
  start_index <- 1 + j                         # The starting index is 1 for the first column (lag 0) and increases with lag
  end_index <- n - (mlag - j)                  # The ending index is n for the first column and decreases with lag
  shifted_vector <- tokens[start_index:end_index]    # Extract the slice of the token vector
  M[, j + 1] <- shifted_vector                   # Assign the slice to the correct column (j+1)
}

M

#now we will make a function which returns the token of the most likely following word, by giving the following:
#key is the word sequence in tokens for which the next word is to be generated
#M is the token matrix made above 
#M1 is the vector of word tokens for the whole text
#w is the vector of mixture weights
next.word<-function(key,M,M1,w=rep(1,ncol(M)-1)) {
  mlag <- ncol(M)-1                                 #mlag represents the maximum length a key can be, so that the token for the next word can be taken from the matrix
  if (length(key)>mlag) {
    key2<-key[(length(key)-mlag+1):length(key)]     #if the key is too long we consider only the final words of the key of maximum accepted length
   } else {
       key2<-key                 #otherwise we work with it as it is
    }

  u<-matrix(0,length(key2),nrow(M))                 #matrix where we will store all the tokens
   
  #in the following loops, we search for the tokens, placing at the i_th row of the matrix those that we find by taking into account only the last i words of the key given
  adding_to_row<-1                                                                                
  for (i in 1:length(key2)) {                       
     key3<-key2[(length(key2)-i+1):length(key2)]               #every time we look at the last i words of our key
     ii<-colSums(!(t(M[,(mlag-length(key3)+1):mlag,drop=FALSE])==key3))       #the key is checked if it matches with any of the columns, this function returns a vector with zeros in the positions that represent a match at the same numbered column as long as they are finite
     v<-which(ii==0 & is.finite(ii) & !(M[,mlag+1] %in% u[adding_to_row, ]))  #v is a vector containing all the locations of ii where the value there is equal to zero and finite, and it also omits the locations that have already been added to this specific row
     if (length(v)>0) {
       u[adding_to_row,][1:length(v)]<- M[v,mlag+1]                           #If any matches were found, then v contains at least one element, and then we add all the elements to the respective row in u
     }
     adding_to_row<-adding_to_row+1                                           #we continue to the next row
  }
  #now we have a matrix with all the possible tokens, as mentioned above, and all the rest positions indicating the value "0"
  random_token<-0
  if (any(na.rm = TRUE)) {                                      #we omit any potential NAs
    while (random_token==0) {
      random_row<-sample(1:nrow(u), prob=w[1:nrow(u)]/sum(w), size=1) #We randomly select a row using the given weights as the row indicates the number of words used by the keys and the weights are used as it follows: Σ_{i=1}^{m} w_i * P(next word | v[i:m])
      random_column<-sample(1:ncol(u),size=1)                         #Then we pick a column at random which will indicate which one of the tokens in the row it returns
      random_token<-u[random_row,random_column,drop=TRUE]             #we define a new variable as the token located in the random row and column that were picked. If a "0" was picked we repeat the process until we pick a real token
  }
} else {
  random_token<-sample(M1[!is.na(M1)],size=1)                         #if we had a NA then the algorithm picks one token at random from the text that is not a NA 
  }
 return(random_token)                                                 #the function returns the token
}

  
  
  
  
  
  
