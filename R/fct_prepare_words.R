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
  text <- event
  myCorpus = tm::Corpus(tm::VectorSource(text))
  myCorpus = tm::tm_map(myCorpus, tm::content_transformer(tolower))
  myCorpus = tm::tm_map(myCorpus, tm::removePunctuation)
  myCorpus = tm::tm_map(myCorpus, tm::removeNumbers)
  myCorpus = tm::tm_map(myCorpus, tm::removeWords,
                    c(tm::stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  
  myDTM = tm::TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})
