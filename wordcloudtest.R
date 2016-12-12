#!/usr/bin/env Rscript

source("wordcloud.R")
source("randomtextgenerator.R")
## Writes random.txt into root directory for purposes of randomized testing
library(testthat)

test_that("weight_by_count, check the word frequncy are in decreasing order", {
  ## Function weight_by_count should return data frame that is sorted by freq in 
  ## decreasing order.
  test_dtf = weight_by_count("trump.txt")  
  random_dtf = weight_by_count("random.txt")
  expect_true(identical(as.numeric(test_dtf$count), 
                        sort(as.numeric(test_dtf$count), decreasing = TRUE)))
  expect_true(identical(as.numeric(random_dtf$count), 
                        sort(as.numeric(random_dtf$count), decreasing = TRUE)))
})

test_that("weight_by_count, check the stopwords have been removed from data", {
  ## Function weight_by_count should return a data frame that does not have 
  ## stop words in it. 
  test_dtf = weight_by_count("trump.txt")
  random_dtf = weight_by_count("random.txt")
  expect_false(all(stopwords("english") %in% test_dtf$word))
  expect_false(all(stopwords("english") %in% random_dtf$word))
})

test_that("weight_by_count, check that words are unique", {
  ## Function weight_by_count should return a data frame that does not have 
  ## repeated words so that only unique words can be plotted. 
  test_dtf = weight_by_count("trump.txt")
  random_dtf = weight_by_count("random.txt")
  expect_true(identical(test_dtf$word, unique(test_dtf$word)))
  expect_true(identical(random_dtf$word, unique(random_dtf$word)))
})

test_that("bounding_box, check that bounding box for words for longer words", {
  ## Function bounding_box returns larger width for words that 
  ## are longer 
  word1 = bounding_box("hello_hello", size = 1, font = "Times")
  word2 = bounding_box("hello", size = 1, font = "Times")
  expect_true(word1[1] > word2[1])
})

test_that("wordcloudspiral, check if bounding box is largest for frequent
          words", {
  ## Function wordcloudspiral gives a larger strheight and strwidth for more
  ## frequent words. Most frequent words are sorted on top of the returned 
  ## dataframe. We will compare similar words of different frequency and font.
  NUM_WORDS = 50
  FONT = "Arial"
  test_dtf = data.frame(word = rep("testword", 4), count = c(5, 4, 3, 2))
  spiraldf = wordcloudspiral(test_dtf, wordscount = NUM_WORDS, font = FONT, 
                             propRotate = 0)
  expect_true(abs(spiraldf$width[1] - max(spiraldf$width)) < 0.001)
  expect_true(abs(spiraldf$height[1] - max(spiraldf$height)) < 0.001)
})

test_that("wordcloudspiral, check that number of words does not exceed user 
          demands or maximum words in text file", {
  ## Function wordscloudspiral should return a data frame of words equal to the 
  ## request of the user or if the user request if too large, the maximum number 
  ## of unique words in the text
  FONT = "Arial"
  test_dtf = weight_by_count("JFK.txt")
  random_dtf = weight_by_count("random.txt")
  spiraldf = wordcloudspiral(test_dtf, wordscount = 50, font = FONT, 
                             propRotate = 0)
  expect_equal(length(spiraldf$x) , 50)
  spiraldf2 = wordcloudspiral(test_dtf, wordscount = 600, font = FONT, 
                              propRotate = 0)
  expect_equal(length(spiraldf2$x) , length(test_dtf$word))
  randomspiraldf = wordcloudspiral(random_dtf, wordscount = 50, font = FONT, 
                             propRotate = 0)
  expect_equal(length(randomspiraldf$x) , 50)
})

test_that("overlap, check that boxes in the spiral do not overlap", {
  ## Function overlap should return false for all boxes in the word cloud to 
  ## verify that no boxes in the spiral should overlap
  FONT = "Times"
  WORDNUM = 40
  test_dtf = weight_by_count("random.txt")   
  spiraldf = wordcloudspiral(test_dtf, wordscount = WORDNUM, font = FONT, 
                             propRotate = 0)
  for (ii in 1:WORDNUM){
    first_box= spiraldf[ii, ]
    box.list=split(spiraldf[-ii, ], seq(nrow(  spiraldf[-ii, ])))
    expect_false(overlap(first_box$x, first_box$y, first_box$width, 
                                    first_box$height, box.list))
  }
})