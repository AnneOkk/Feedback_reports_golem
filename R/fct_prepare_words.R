#' prepare_words 
#'
#' @description A function to create term matrix from event descriptions
#'
#' @return a term matrix
#' 
#' @import memoise
#' @import tm textstem
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
  myCorpus = tm::tm_map(myCorpus, textstem::lemmatize_strings)
  myCorpus = tm::tm_map(myCorpus, tm::removeWords,
                    c(tm::stopwords("SMART"), "the", 
                      "and", "but", 
                      "cwe", "govt", "make", "problem",
                      "work", "part", "tough",
                      "find", "month", "march", "july",
                      "complete", "process", "absence", "small",
                      "issue", "hes", "spin-dry", "happen",
                      "meet", "mistake", "business", "apital",
                      "payfabricate", "task", "good", 
                      "luckily", "difficult"
                      ))
  
  myDTM = tm::TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})
