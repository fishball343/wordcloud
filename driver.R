#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(argparse))
source("wordcloud.R")

parser <- ArgumentParser()
parser$add_argument("-f", "--input", type = "character", default = "trump.txt",
                    help = "Insert URL or filename")
parser$add_argument("-c", "--color", type = "character", default = "Blues", 
                    help = "Color theme from ColorBrewer pallete")
parser$add_argument("-n", "--numword", type = "integer", default = "50",
                    help = "Number of words in plot")
parser$add_argument("-t", "--font", type = "character", default = "Times", 
                    help = "Font of the word: some of the fonts dont work without required packages")
parser$add_argument("-o", "--output", type = "character", default = "file.pdf", 
                    help = "PDF file of wordcloud plot")
args <- parser$parse_args()
isInFile <- grepl("\\.txt$", args$input)
if (!isInFile) {  
  write("Input is not a text file", stderr()) 
}
input = args$input
num = args$numword          
color = args$color
font = args$font
output = args$output
pdf('output.pdf')
wordcloudplot(input, color, num, font)
dev.off()