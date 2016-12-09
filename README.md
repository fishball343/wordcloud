# WordCloud

This program creates a wordcloud from a txt file. You can also select the number of words on the wordcloud as well as the color of the words. I used the RColorBrewer package to implement coloring of the text so the following command in R will give you a list of colors to use when running the program:

display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE)

To run my program you would need to type the following command line script. ./wordcloud.R "textfile" "number of words" "color scheme" 

For example to create a word cloud of JFK inaugural speech containing 150 words and using the color scheme Blues typing the following command script would generate a pdf file that contains the wordcloud. 

/wordcloud.R JFK.txt 150 Blues

I have placed the above example word cloud in the repository.
