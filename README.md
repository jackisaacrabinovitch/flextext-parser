# flextext-parser

R code which I developed to convert corpus flextext files (from Fieldworks Linguistic Explorer, or FLEx) into tsv files for easier use with gathering concordance data and integrating elicited data with LaTeX for a project with Pahoturi River languages.

## How to Use

Open up FlexText-to-tsv.R for commented instructions on how to use the R code to scrape information from a flextext file to create tsv files. This program creates three kinds of tsv files: the first are "Parse" files, which include information on individual word instances in the corpus, such as their transcription, gloss, and part of speech, the second are "Trans(lation)" files which include phrase level information, such as free and literal translations, the third is the "dfil" file, which just contains information about the title of each text within the corpus.

One you have produced the tsv files, open up Compile-flex-tsv.R for commented instructions on how to combine the various tsv files into different dataframes which you can use to more easily read and manipulate corpus data.

If you have any questions, comments, or concerns, feel free to reach out to me at jackisaacrabinovitch@gmail.com!
