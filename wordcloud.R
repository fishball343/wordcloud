library(tm)
library(RColorBrewer)

parse_txt = function(filename) {
  ## Parse_txt converts a txt file into a string.
  data = readChar(filename, file.info(filename)$size)
  return (data)
}

filter_stopword = function(text) {
  ## Filter_stopword removes stop words from a vector of words.
  stopword = c(stopwords("english"), "shall", "-", "--", "will", "can")
  text = text[!text %in% stopword]
  return (text)
}

tokenize = function(text) {
  ## Tokenize takes a string and removes all punctatuation and numbers except for
  ## hyphens and apostrophes. However if the hyphen is not embedded between two 
  ## words then the then the hyphen is removed. It then converts the string into
  ## lowercases and then splits the string into a vector by spaces. 
  text = gsub("( |^)-+|-+( |$)", "\\1", gsub("[^[:alpha:]’'-]", " ", text))
  text = tolower(text)
  text = strsplit(text, "\\s+")[[1]]
  return(text)
}

uniquify = function(stemed_text) {
  ## Uniquify takes a data frame of words and removes all duplicated words by
  ## aggregating their occurence. It returns a dataframe of the word and the new
  ## counts/frequencies. 
  unique = aggregate(stemed_text$count, by = list(as.factor(stemed_text$word)), 
                     sum)
  colnames(unique) = c("word", "count")
  return (unique)
}

stem = function(tokened_data) {
  ## Stem reads a dataframe of the tokened words and converts it to another
  ## data frame that aggregates the words on their stems. Then it completes the
  ## stems using the stemCompletion function to obtain the final word. In some 
  ## cases the stems that end with "y" were not able to be completed by the 
  ## stemCompletion function so I had to append "y" using a forloop.
  dtf = as.data.frame(table(stemDocument(tokened_data)))
  colnames(dtf) = c("stem", "count")
  dtf[, "word"] = stemCompletion(dtf[, "stem"], tokened_data)
  index_of_y = grep("^$", dtf[, "word"])
  for (missing_index in index_of_y) {
    stem = as.character(dtf[, "stem"][missing_index])
    dtf[, "word"][missing_index] = paste(substr(stem, 1, nchar(stem) - 1), "y", 
                                         sep = "")
  }
  return (dtf)
}
  
weight_by_count = function(filename) {
  ## Weight_by_count is the final data processing function that parses the text,
  ## tokenize the text, then find the stem words and then uniquify the final 
  ## stemmed words. It returns a data frame where one column is the word and the 
  ## other column is the frequency. The words are sorted by decreasing order of 
  ## frequency.
  text = parse_txt(filename)
  tokened = filter_stopword(tokenize(text))
  stem_data = stem(tokened)
  wordcounts = uniquify(stem_data)
  wordcounts$count = as.numeric(wordcounts$count)
  wordcounts = wordcounts[order(wordcounts$count, decreasing = TRUE), ]
  return (wordcounts)
}

bounding_box = function(text, size, font) {
  ## Bounding_box is a function that returns the width and height of the 
  ## bounding box given the arguments of word, size and font. 
  width <- strwidth(text,
                    cex = size,
                    family = font,
                    units = "inches")
  height <- strheight(text,
                      cex = size,
                      family = font,
                      units = "inches")
  return (c(width, height))
}

overlap = function (x1, y1, width, height, boxes) {
  ## Function that checks if a box overlaps with other boxes stored in a list.
  ## The arguments are the x1 and y1 which are the bottom left coordinates of 
  ## the bounding box, width and height of the box and the list of all other 
  ## boxes which also contain values of their respective x1, y1, width and 
  ## height. 
  if (length(boxes) == 0) {
    return (FALSE)
  }
  for (box in 1:length(boxes)) {
    box_bound = boxes[[box]]
    x2 = box_bound[1]
    y2 = box_bound[2]
    width2 = box_bound[3]
    height2 = box_bound[4]
    if (!(x1 >= x2 + width2 ||
          y1 >= y2 + height2 || y1 + height <= y2 || x1 + width <= x2)) {
      return (TRUE)
    }
  }
  return (FALSE)
}

wordcloudspiral = function (wordframe, wordscount, wordsize = 3.3, 
                            propRotate = 0.2, font) {
  ## Wordcloudspiral takes in a dataframe containing the word and their 
  ## respective counts, the wordscount argument which is the number of words we 
  ## want to plot and the font of the text that we are plotting. Then it 
  ## computes the position of the words and their bounding box in the plot using
  ## an spiral greedy algorithm, the size of the word scaled by its frequency
  ## and the rotation of the word determined randomly. The function returns a 
  ## data frame containing the bottom left coordinates, width and height of 
  ## each box, the word, size of the word and an indicator of rotation.
  if (wordscount > length(wordframe$word)) {
    wordscount = length(wordframe$word)
  }
  words = wordframe$word[1:wordscount]
  count = wordframe$count[1:wordscount]
  thetaStep = 0.1
  rStep = 0.8
  size = wordsize * count / max(count) + 0.4
  bounding_boxes = list()
  for (i in 1:length(words)) {
    isRotated = rbinom(1, 1, propRotate)
    rads = 0
    x1 = 0
    y1 = 0
    theta = runif(1, 0, 2 * pi)
    width = bounding_box(words[i], size[i], font = font)[1]
    height = bounding_box(words[i], size[i], font = font)[2]
    if (isRotated) {
      tmp = height
      height = width
      width = tmp
    }
    while (overlap(x1, y1, width, height, bounding_boxes)) {
      theta = theta + thetaStep
      rads = rads + rStep * thetaStep / (2 * pi)
      x1 = 0 + rads * cos(theta)
      y1 = 0 + rads * sin(theta)
    }
    bounding_boxes[[length(bounding_boxes) + 1]] = c(x1, y1, width, height,
                                                         size[i], isRotated)
    }
  spiraldata = as.data.frame(matrix(
    unlist(bounding_boxes),
    nrow = length(bounding_boxes),
    byrow = TRUE), stringsAsFactors = FALSE)
  colnames(spiraldata) = c("x", "y", "width", "height", "size", "rotate")
  spiraldata$word = words
  return (spiraldata)
}

wordcloudplot = function(text, coltheme, numwords, font) {
  ## Wordcloudplot plots the wordcloud by using the coordinates of the text and
  ## bounding box using the wordcloudspiral function. It also finds the
  ## plot window to encompass all words into the plot. Finally it uses the 
  ## Rcolorbrewer packages to asign random colors and a theme to the words in
  ## the plot. 
  spiraldf = wordcloudspiral(weight_by_count(text), wordscount = numwords, 
                             font = font)
  leftside = min(spiraldf$x - spiraldf$width)
  rightside = max(spiraldf$x + spiraldf$width)
  topside = max(spiraldf$y + spiraldf$height)
  botside = min(spiraldf$y - spiraldf$height)
  plot.new()
  plot.window(c(leftside, rightside), c(botside, topside))
  colorscheme = brewer.pal(8, coltheme)[3:8]
  for (i in 1:length(spiraldf$x)) {
    text(
      spiraldf$x[i] + 0.5 * spiraldf$width[i],
      spiraldf$y[i] + 0.5 * spiraldf$height[i],
      spiraldf$word[i],
      cex = spiraldf$size[i],
      srt = spiraldf$rotate[i] * 90,
      family = font,
      col = sample(colorscheme, 1)
    )
  }
}