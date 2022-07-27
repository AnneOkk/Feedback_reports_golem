#' prepare_words 
#'
#' @description A function to create term matrix from event descriptions
#'
#' @return a term matrix
#' 
#' @import memoise
#' @import tm
#'
#' @noRd


# Using "memoise" to automatically cache the results
getTermMatrix <- memoise::memoise(function(event) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  text <- readLines(event,
                    encoding = "UTF-8")
  
  myCorpus = tm::Corpus(tm::VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})