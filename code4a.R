
#This work was done by students Alexandros Zachakos and Argwings Shikuku of Univeristy of Edinburgh


#The following code takes all the works of Shakespeare, and it generates the most Shakespearean sentence using Probabilities
#The goal is to start from a single random word and generate randomly a potential next word, and then take the new sentence and generate a third one with the same way, until it generates a full sentence
#The scheme of this code is the following:
# 1) It downloads the text of all his works and it clears it from all the useless elements, that being:
#       i)  The removal of all the sentences being contained inside braces [...], which are staging descriptions
#       ii) The removal of all words and numbers indicating either the name of the person speaking, or headings of various sorts
#       iii)The removal of -, _, — symbols from each and every word so that the code doesn't consider a word and the same word with such symbol as different things
#       iv) The separation of words and their potential punctuation marks attached to them, considering them as two different words
#       v)  All the remaining words are getting turned into lower case so that "The" and "the" for example are not considered to be different
# 2)  Finds the unique words from the cleaned vector, and manipulating the resulting data to find approximately the first 1000 unique words fron the passage.
# 3) Makes the matrices of common word token sequences. Here, we make a vector containing the tokens for representing the whole text.
#    We put the vector of tokens into a matrix , shifting the words from one column to another.
# 4) We create a function that takes a sentence in the form of tokens of words, the matrix M, the vector of the text in the form of tokens and the probabilistic weights  
#    which determine the preference we have of generating a word after taking into account a specific number of words of the key given and it returns a token for the next word
# 5) Finally, it picks a word at random to start with, turns into a token, and through the aforementioned function it generates the token for the next word and appends it to the text.
#    This procedure is repeated until a full stop is reached where we have the result we wanted.



setwd("put/your/local/repo/location/here")
a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8")                 #First we scan the text
brace_places<-rep(0,2*length(grep("[[]",a)))     #vector with length equal to the number of words containing braces, both left and right, and we have made the assumption that if there is a left [ then there also exist a right ], hence the 2*

#first we will remove all the words that are inside of braces [...]

b<-grep("[[]",a) #grep locates the positions of all left [ in the text
c<-grep("[]]",a) #positions of all right ]

# now we will make a vector brace_places which will indicate the positions of all words starting with [ and next to them the word in which the braces close with the right ]
for (i in 1:length(b)) {
  brace_places[2*i-1]<-b[i]               #odd slots represent the positions of all left [
  next100<-b[i]:(b[i]+100)                #vector containing the next 100 words from a word with left [
  sec_br<-grep("[]]",a[next100])          #sec_br contains the positions of all right ] in the 100 words that follow the left ]s
  if (length(sec_br)>0) {                 #we take into account the possibility of a right ] not existing, so if it exists sec_br has at least 1 element
    brace_places[2*i]<-next100[sec_br[1]] #addition of the positions of all the right ] to even slots, next to the position of their left [ pair
  }else {
    a[b[i]]<-gsub("[[]","",a[b[i]])       #In case of an accidental right ] without a pair, we use gsub to delete the unnecessary left [, and a[b[i]] is the word containing the wrong brace
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

a_new <- rep(" ",length(a)-numb_words_del) #vector sized as much as the number of words that are not contained in braces, here we will store each and every word of the text except of those that are inside of braces
k<-1                  #this will represent the position of the element that is being added in the new vector
m<-1                  #this represents the pair of braces that we test if we are inside of it
n<-0                  #when this is equal to zero then we are outside braces, when it is positive then we are inside braces and we should omit these words from being added and it will represent the number of words left inside the braces
for (i in 1:length(a)) {
  if (i >= brace_places[m] & i <=brace_places[m+1] & n==0) { #in this case, we enter a pair of braces, we don't add a new word in the new vector and there are still (original number of words inside the braces)-1 words remaining to be omitted
    n<-length(brace_places[m]:brace_places[m+1])-1   #n becomes equal to the words that are contained inside the braces minus 1, as we dont add the word with the left [ in the new text so we have already omitted it
    m<-m+2             #we move to next pair of braces
  } else if (n>0){     #in this case, we are still inside braces, we don't add new words to the new vector and there is one less word remaining to be omitted
    n<-n-1
  } else {             #in this case, we are outside of braces and we most likely add a new word 
    if (a[i]==toupper(a[i]) & a[i] != "I" & a[i] != "A") {    #but if that word is upper case (we check that with the function toupper which turns all letters of the word into capital letters), we omit them as upper case words/Arabic numbers but not the "A"s and "I"s
      next                                                    #with next we do nothing and we continue
    }
    else {             #if the word is not in capital letters we add it to the new text
      a_new[k]<-a[i]
      k<-k+1           #k increases in order to keep up with i, so in the next step a new word can be added to the next slot of the new vector
    }
  }
}
a_new<-gsub("_","",a_new) #removes _ from all the words
a_new<-gsub("-","",a_new) #removes - from all the words
a_new<-gsub("—","",a_new) #removes — from all the words


#now we make a function that can locate the words that are stored with a punctuation mark, and splits them into "word" and "punctuation mark"
#we assume that all punctuation marks are the last character of the word that they are attached to
split_punct<-function(x) {
  ii<-grep("\\,|\\.|\\;|\\!|\\:|\\?",x)                    #first we create a vector containing the positions of all the words with punctuation marks in the passage
  xs<-rep("",length(ii)+length(x))                         #vector to store all the words separated from the punctuation marks
  iis<-ii+1:length(ii)                                     #where should punctuation marks go in xs
  xs[iis]<-substr(x[ii],nchar(x[ii]),nchar(x[ii]))         #nchar takes the word x[ii] which contains a punctuation mark and it returns the number of letters it contains (including the punctuation mark)
                                                           #substr takes the word x[ii] and it keeps only the letters from nchar(x[ii]) to nchar(x[ii]), so only the last character which is the punctuation mark, and then it is added to its respective position
  xs[-iis] <- x                                            #then we add all the words to the remaining positions. The words containing punctuation marks are added without the removal of the mark only temporarily
  #we know that all the punctuation marks are in the ii+(1:length(ii)) positions of the matrix, so the words that they came from are located to the exact previous one
  #with substr we take the words from this position and we keep only all the characters from the first one until the nchar(xs[ii+(1:length(ii))-1])-1_th one, which is the second last, and the result is the word itself without the last character which was the punctuation mark
  xs[ii+(1:length(ii))-1]<-substr(xs[ii+(1:length(ii))-1],1,nchar(xs[ii+(1:length(ii))-1])-1)
  return(xs)                   #the function returns the amended passage with splitted words and punctuation marks
}  

a_new<-split_punct(a_new)      #we separate all the punctuation marks from their respective words in the amended Shakespeare passage
a<-tolower(a_new)              #we make all the words to be lower cased for most accurate results and we re-use the old a variable as it was not used anymore, and we finally have a cleaned passage


# Finding the unique elements in vector a of our cleaned data

b <- unique(a, incomparables = FALSE, fromLast = FALSE, nmax = NA)

# Matching each unique words to the vector a to know there positions
# finding the vector of indices indicating which element in the unique word vector each element in the text corresponds to

index_vector <- match(a,b,nomatch = NA_integer_)
index_vector <- match(a,b)

# counting how many time each unique word occurs in the text.

words_n <- tabulate (index_vector, nbins = length(b)) # normal use of tabulate

# Most common words used which are approximately 1000 words

# Since using rank was tough to understand, we used the alternative order()

word_freq <- data.frame( words=b, words_n)        #Putting them in dataframe to for easy access to words
sort_descend <- word_freq[order(word_freq$words_n, decreasing = T), ]  # arranging the frequencies of words from most to least
common <- min(1000, nrow(sort_descend))       # Finding the 1000 most common words from the arranging
b <- sort_descend$words[1:common]    # Vector b with 1000 most common words

# making tokens of length a using match and making sure it has all common words

tokens <- match( a, b)

# Constructing the matrix M of common word token sequences

mlag<-4
n <-length(tokens)  
R <- c(n - mlag)

# Giving the matrix dimensions to show the lags. Basically we are fitting all unique indexes in 4 columns.

M <- matrix(NA, nrow = R, ncol = mlag + 1)

# Fill the matrix column by column (for each lag)

for (i in 0:mlag) {
  start_ <- 1 + i                         # The starting index is 1 for the first column (lag 0) and increases with lag
  end_ <- n - (mlag - i)                  # The ending index is n for the first column and decreases with lag
  shifted_vector <- tokens[start_:end_]    # Extract the slice of the token vector
  M[, i + 1] <- shifted_vector                   # Assign the slice to the correct column 
}

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
    key3<-key2[(length(key2)-i+1):length(key2)]               #every time we consider the last i words of our key, and after some loops we will have found all the possible tokens of words that are likely to appear after our sentence by taking into consideration every possible sub-length of the key, emanating from its end
    ii<-colSums(!(t(M[,(mlag-length(key3)+1):mlag,drop=FALSE])==key3))       #this function returns a vector with zeros in the positions that represent a match at the same numbered column as long as they are finite. The key is checked if it matches with any of the columns of the matrix M, 
                                                                             #and the columns that are checked to be matched each time are the mlag-length(key3)+1):mlag.This choice is because the number of columns should be equal to the length of key3, in order for them to be comparable with it,and the last column is always the second last of the matrix M, as the last one contains the token of the next word 
        v<-which(ii==0 & is.finite(ii) & !(M[,mlag+1] %in% u[adding_to_row, ]))  #v is a vector containing all the locations of ii where the value there is equal to zero and finite, and it also omits the locations that have already been added to this specific row
    if (length(v)>0) {
      u[adding_to_row,][1:length(v)]<- M[v,mlag+1]                           #If any matches were found, then v contains at least one element, and then we add all the elements to the respective row in u, if no matches were found, we do nothing
    }
    adding_to_row<-adding_to_row+1                                           #In the next loop we will take into account one more word from the key, so in order to separate the tokens according to the number of words used from the key to be found, we continue adding them to the next row
  }
  #now we have a matrix with all the possible tokens, as mentioned above, and all the remaining elements have the value "0", and we are ready to choose one of them at random
  random_token<-0                 #random interim value for the variable that the function is going to return
  if (any(na.rm = TRUE)) {        #we omit any potential NAs in the matrix of the tokens
    while (random_token==0) {
      random_row<-sample(1:nrow(u), prob=w[1:nrow(u)]/sum(w), size=1) #We randomly select a row using the given weights as the row indicates the number of words used from the end of the key that was given, and the weights are used as it follows: Σ_{i=1}^{m} w_i * P(next word | v[i:m])
                                                                      #The function sample is able to choose an element at random, we type size=1 in order to return only one element, and this happens with a probability of (the weight that was given for the row)/(sum of all weights)
      random_column<-sample(1:ncol(u),size=1)                         #Then we pick a column at random which will indicate which one of the tokens in the row it returns
      random_token<-u[random_row,random_column,drop=TRUE]             #we define a new variable as the token located in the random row and column that were picked. If a "0" was picked we repeat the process until we pick a real token
    }                                                                 #drop=TRUE indicates that the value we define is the one located at the random rows and columns we chose
  } else {
    random_token<-sample(M1[!is.na(M1)],size=1)                         #if we had a NA then the algorithm picks one token at random from the text that is not a NA 
  }
  return(random_token)                                                  #the function returns the token
}

sentence<-","                                                           #we define the sentence that is going to be printed to be a punctuation mark
while (sentence %in% c(",",".",";","!",":","?"," ")) {                  #That is because we want the program to select a word at random from the text to start with, that is not a punctuation mark, so when it starts the while loop, it is forced to pick a first word
  sentence<-sample(a,size=1)                                            #With the function sample we choose one word from the text (hence size=1) and if it is a punctuation mark it picks again, until it finds a real word
}

new_word<-"abcde"                                            #we define the new_word that is going to be generated as something random to begin with
while (!(new_word %in% c("."))) {                    #while the new word is NOT a full stop, a question mark or an explanation mark, it repeats the following procedure, otherwise it stops the loop, resulting to the desirable outcome
  token_position<-rep("",length(sentence))                   #We make a vector of length equal to the length of the sentence that is going to store all the positions of the tokens representing the words in the sentence
                          
  token_of_word<-rep("",length(sentence))                    #vector that will contain all the tokens of the sentence representing its words
  token_of_word<-tokens[token_position]                      #As the vector "tokens" contains all the tokens of the passage in order, the location of a token in this vector is the same as the location of the respective word in the text, so now we have a vector filled with the tokens of our sentence in order
  next_token<-next.word(token_of_word,M,tokens)              #We find the token of the next work via our function
  position_of_next_token<-grep(next_token,tokens)[1]         #Using grep we locate where the new token is located in the vector "tokens" as its position represents the position of its respective word in the text, grep finds all the potential positions and we pick the first one (we could pick anyone, not the first specifically, we only want one at random and the first one is the safest choice)
  new_word<-a[position_of_next_token]                        #We find the new word in our text using the position of the token in the vector "tokens"
  print(new_word)                                            
  sentence[length(sentence)+1]<-new_word                     #We append the new word to our sentence, as the (n+1)_th element in the already existing vector of length n
}
cat(sentence, sep=" ")                                       #We print the full generated sentence nicely using cat, and placing spaces " " between all the words
  
#This work was done by students Alexandros Zachakos and Argwings Shikuku of Univeristy of Edinburgh (It was supposed to be a 3 person work but we failed to find a 3rd one, so all the work was done by only the two of us).
#The steps that were given as instructions in the notes were completed as followed:
# 1) Argwings Shikuku
# 2) Collaboratively
# 3) Collaboratively
# 4) Alexandros Zachakos
# 5) Argwings Shikuku
# 6) Argwings Shikuku
# 7) Alexandros Zachakos
# 8) Collaboratively
# 9) Alexandros Zachakos





