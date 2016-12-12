# WordCloud

This program creates a wordcloud from a txt file. You can also select the number of words on the wordcloud as well as the color of the words. I used the RColorBrewer package to implement coloring of the text so the following command in R will give you a list of colors to use when running the program:

display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE)

To run my program you would need to type the following command line script. ./driver.R -f "input txt file" -c "colotheme" -n "number of words" -t "font" -o "output pdf file"

For example to create a word cloud of JFK inaugural speech containing 150 words and using the color scheme Blues typing the following command script would generate a pdf file that contains the wordcloud. 

./driver.R -f JFK.txt -c Blues -n 150 -t Times -o JFK_example.pdf

I have placed the above example word cloud in the repository.
