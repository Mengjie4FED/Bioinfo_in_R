wordsDictionary <- function(fileName, removeStopWords = TRUE) {
  # check valid txt file url
  if (is.null(fileName)) {
    stop("URL of the file cannot be null.")
  }
  if (!is.character(fileName)) {
    stop("Please type URL in character.")
  }
  if (substr(fileName, nchar(fileName)-2, nchar(fileName)) != 'txt') {
    stop("Please import a txt file.")
  }
  if (!is.logical(removeStopWords)) {
    stop("removeStopWords shouble be logical (TRUE or FALSE).")
  }
  
  # define stop words
  stopWords = c("me","my","myself", "we","our","ours","ourselves",
                "you","your","yours","yourself","yourselves","he",
                "him","his","himself","she","her","hers","herself",
                "it","its","itself","they","them","their","theirs",
                "themselves","what","which","who","whom","this","am",
                "is","are","was","were","be","been","being","have",
                "has","had","having","do","does","did","doing","would",
                "should","could","ought","re","ve","ll","isn","aren",
                "wasn","weren","hasn","haven","hadn","doesn","don",
                "didn","won","wouldn","shan","shouldn","can","cannot",
                "couldn","mustn","let","here","there","when","where",
                "why","how","an","the","and","but","if","or","because",
                "as","until","while","of","at","by","for","with","about",
                "against","between","into","through","during","before",
                "after","above","below","to","from","up","down","in",
                "out","on","off","over","under","again","further","then",
                "once","all","any","both","each","few","more","most","such",
                "no","nor","not","only","own","same","so","than","too",
                "very")
  
  # read in txt file
  listChar = readLines(fileName)
  # clean characters and transform to lower case
  listChar = tolower(gsub("[[:punct:]0-9[:space:][:blank:]]+", " ", listChar))
  # get total words, remove single character
  totalWords = unlist(strsplit(listChar, "[[:blank:]\t]+"))
  totalWords = totalWords[ nchar(totalWords) > 1]
  # unique words
  uniqueWords = unique(totalWords)
  
  # remove stop words or not
  if (removeStopWords) {
    totalWords = totalWords[ !( totalWords %in% stopWords) ]
    uniqueWords = uniqueWords[ !( uniqueWords %in% stopWords) ]
  }
  
  # initialize a matrix to implement dictionary 
  wordTable = matrix(0, nrow = 2, ncol = length(uniqueWords), 
                     dimnames = list(c("frequency", "ratio"), uniqueWords))
  # count frequency for each word
  for ( i in 1:length(totalWords)) {
    wordTable["frequency", totalWords[i]] = wordTable["frequency", totalWords[i]] + 1
  }
  # get the ratio of each word
  wordTable["ratio", ] = wordTable["frequency", ]/length(totalWords)
  
  # set the class and return value
  this = list(bagOfWords = uniqueWords, dictionary = wordTable)
  class(this) <- append(class(this),"wordsDictionary")
  return (this)
}

getFrequency = function(object, ...) {
  UseMethod("getFrequency", object)
}
getFrequency.wordsDictionary = function(object, word, type = "frequency", ...) {
  # some corner cases
  if (is.null(word)) {
    stop("Target word cannot be null.")
  }
  if (!is.character(word)) {
    stop("Target word should be character.")
  }
  if (!is.character(type)) {
    stop("type should be character.")
  }
  if (!(type == "frequency" | type == "ratio")) {
    stop("type should be frequency or ratio")
  }
  
  if (!( word %in% object$bagOfWords)) {
    return("Cannot find this word in dictionary.")
  } else {
    # return frequency count or ratio based on type
    return(object$dictionary[type, word])
  } 
}


plotDic = function(object, ...) {
  UseMethod("plotDic", object)
}
plotDic.wordsDictionary = function(object, yType = "frequency", minFeq = 1, ...) {
  if (!is.character(yType)) {
    stop("yType should be character ( \"frequency\" or \"ratio\").")
  }
  if (!(is.numeric(minFeq) & minFeq >= 0)) {
    stop("minFeq should be a positive number.")
  }
  if (minFeq > max(object$dictionary["frequency" ,])) {
    stop("No word has bigger frequency. Type in a smaller minFeq.")
  }
  if (!(yType == "frequency" | yType == "ratio")) {
    stop("yType should be \"frequency\" or \"ratio\".")
  }
  
  # use minimal frequency to select words into plot
  index = object$dictionary["frequency" ,] > minFeq
  # set y to frequency count or ratio based on yType
  y = object$dictionary[yType ,][index]
  # sort y in descending order
  y = sort(y, TRUE)
  wordsX = labels(y)
  
  # label top 3 words 
  labelY1 = y[1:3]
  labelX1 = labels(labelY1)
  X1 = c(which(wordsX == labelX1[1]), which(wordsX == labelX1[2]), which(wordsX == labelX1[3]))
  
  plot(y, xlab = 'words', ylab = yType, main = paste("Plot of words'", yType, sep = " "), xaxt='n')
  axis(1, at = 1:length(wordsX), labels = wordsX)
  text(X1, labelY1, labelX1, pos = 4)
}