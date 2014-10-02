Get required packages
=====================

``` r
library(devtools)
install.packages('tm')
install.packages("SnowballC")
devtools::install_github("cran/wordcloud")
```

Setup corpus
============

-   Around here I became frustrated with the CamlCase inconsistencies. Oh well.
-   The recursive argument doesn't seem to work, either. Something to investigate.

``` r
suppressPackageStartupMessages(library(tm))
project_dir <- "~/projects/9999-99-99_master_text_miner"
setwd(project_dir)
org_dir <- "~/projects/9999-99-99_master_text_miner/data"
org_dir_source <- DirSource(org_dir, recursive = FALSE)
agenda_corpus <- VCorpus(x=org_dir_source,
                         readerControl=
                             list(reader = reader(org_dir_source),
                                  language = "en"))
```

Inspect the corpus
==================

-   Structure of the corpus object

``` r
typeof(agenda_corpus)
str(agenda_corpus[1])
```

    ## [1] "list"

    ## List of 1
    ##  $ computing.org:List of 2
    ##   ..$ content: chr [1:4509] "* Document appearance and settings" "#+LINK: google    http://www.google.com/search?q=%s" "#+TAGS: install(i) sysupdate(s) bug(b) config(c) laptop(l) desktop(d)" "#+DRAWERS: CODE HIDDEN" ...
    ##   ..$ meta   :List of 7
    ##   .. ..$ author       : chr(0) 
    ##   .. ..$ datetimestamp: POSIXlt[1:1], format: "2014-10-02 10:34:03"
    ##   .. ..$ description  : chr(0) 
    ##   .. ..$ heading      : chr(0) 
    ##   .. ..$ id           : chr "computing.org"
    ##   .. ..$ language     : chr "en"
    ##   .. ..$ origin       : chr(0) 
    ##   .. ..- attr(*, "class")= chr "TextDocumentMeta"
    ##   ..- attr(*, "class")= chr [1:2] "PlainTextDocument" "TextDocument"
    ##  - attr(*, "class")= chr [1:2] "VCorpus" "Corpus"

``` r
inspect(agenda_corpus[c(1, 4, 5)])
```

Tranformations and filtering
============================

-   It is important to remove some content, such as punctuation, letter case, certain words, etc.
-   what transformations are available?

``` r
getTransformations()
```

    ## [1] "removeNumbers"     "removePunctuation" "removeWords"      
    ## [4] "stemDocument"      "stripWhitespace"

-   Makes sense, except stem. [What's that?](http://en.wikipedia.org/wiki/Stemming). Oh, so reduce a compound or derivative words to their base. So, e.g. "computing", "computers", "computation", etc may be identified as "compute". Not a great example, since compute and computer are rather different. Digress.
-   Undigress, that highlights that one should be familiar with the stemming algorithm. I'll assume it's OK for this exercise.

``` r
suppressPackageStartupMessages(library(SnowballC))
lapply(agenda_corpus, nchar)[[1]]
agenda_corpus <- tm_map(agenda_corpus,
                        content_transformer(removeWords),
                        stopwords())
agenda_corpus <- tm_map(agenda_corpus,
                        content_transformer(removePunctuation))
agenda_corpus <- tm_map(agenda_corpus,
                        content_transformer(removeNumbers))
agenda_corpus <- tm_map(agenda_corpus,
                        content_transformer(stripWhitespace))
agenda_corpus <- tm_map(agenda_corpus,
                        content_transformer(stemDocument))
lapply(agenda_corpus, nchar)[[1]]
```

    ## content    meta 
    ##  173970     272

    ## content    meta 
    ##  103882     272

-   the transformations working, but removeWords with the org agenda terms wasn't working. A hack is implemented below for wordcloud.
-   future project, pertaining to org specifically, is add custom transformations, e.g. DONE, TODO, NEXT etc

Create document term matrix
===========================

-   Here's how to imagine a document term matrix

| doc      | word1  | word2  | ... | wordN  |
|----------|--------|--------|-----|--------|
| doc1.txt | freq11 | freq12 | ... | freq1N |
| doc2.txt | freq21 | freq22 | ... | freq2N |
| ....     | ...    | .....  | ... | .....  |
| docn.txt | freqn1 | freqn2 | ... | freqnN |

``` r
org_doc_term_matrix <- DocumentTermMatrix(agenda_corpus)
str(org_doc_term_matrix)
inspect(org_doc_term_matrix[, 1:10])
```

    ## List of 6
    ##  $ i       : int [1:13751] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ j       : int [1:13751] 9 10 14 15 24 26 27 28 40 41 ...
    ##  $ v       : num [1:13751] 6 1 1 1 2 7 2 1 2 3 ...
    ##  $ nrow    : int 7
    ##  $ ncol    : int 9126
    ##  $ dimnames:List of 2
    ##   ..$ Docs : chr [1:7] "computing.org" "fynanse.org" "personal.org" "physical.org" ...
    ##   ..$ Terms: chr [1:9126] "aabout" "aac" "aacount" "aal" ...
    ##  - attr(*, "class")= chr [1:2] "DocumentTermMatrix" "simple_triplet_matrix"
    ##  - attr(*, "weighting")= chr [1:2] "term frequency" "tf"

    ## <<DocumentTermMatrix (documents: 7, terms: 10)>>
    ## Non-/sparse entries: 12/58
    ## Sparsity           : 83%
    ## Maximal term length: 7
    ## Weighting          : term frequency (tf)
    ## 
    ##                   Terms
    ## Docs               aabout aac aacount aal aaltiv aas abbrev abcd able
    ##   computing.org         0   0       0   0      0   0      0    0    6
    ##   fynanse.org           0   0       0   0      0   0      0    0    0
    ##   personal.org          0   0       0   0      0   0      0    0    0
    ##   physical.org          0   0       0   0      0   1      0    0    1
    ##   professional.org      0   1       0  20      1   0      1    1    4
    ##   reading.org           0   0       0   0      0   0      0    0    0
    ##   website.org           1   0       1   0      0   0      0    0    0
    ##                   Terms
    ## Docs               abort
    ##   computing.org        1
    ##   fynanse.org          0
    ##   personal.org         0
    ##   physical.org         0
    ##   professional.org     0
    ##   reading.org          0
    ##   website.org          0

-   hmm, would be nice to see most frequent terms.

Examine document term matrix
============================

``` r
org_term_freq <- colSums(as.matrix(org_doc_term_matrix))
org_term_order <- order(org_term_freq)
org_term_freq[tail(org_term_order, n=25L)]
```

    ##      data       get      also      make       add       see       use 
    ##       143       143       145       145       146       164       166 
    ##     check cancelled       can       sat       sun       thu scheduled 
    ##       175       202       226       299       316       335       346 
    ##       wed       fri       mon       tue      next    closed   logbook 
    ##       399       422       537       558       653       752      1046 
    ##      todo       end     state      done 
    ##      1055      1186      1693      2143

-   the org mode todo items are common. Blimey, ive done a lot.
-   It might be intersting to remove them, but it also indicates the actionability of my work. Identify tasks and execute.
-   more examining reveals many other semi-useless words in top 50+ words

``` r
findFreqTerms(org_doc_term_matrix, lowfreq=1000)
```

    ## [1] "done"    "end"     "logbook" "state"   "todo"

``` r
findFreqTerms(org_doc_term_matrix, lowfreq=100)
```

    ##  [1] "add"       "air"       "also"      "broker"    "can"      
    ##  [6] "cancelled" "check"     "closed"    "create"    "data"     
    ## [11] "deal"      "done"      "edm"       "end"       "fri"      
    ## [16] "get"       "git"       "just"      "logbook"   "loss"     
    ## [21] "make"      "mon"       "need"      "new"       "next"     
    ## [26] "one"       "peril"     "results"   "right"     "rms"      
    ## [31] "run"       "sat"       "scheduled" "see"       "setup"    
    ## [36] "state"     "sun"       "thu"       "todo"      "tue"      
    ## [41] "use"       "waiting"   "wed"       "will"      "work"

-   can also examine word associations, plot associations, which I will omit here.

Plot wordcloud
==============

-   I had issues isntalling wordcloud. Github worked, my cran mirror (Rstudio) didn't.
-   There's a hack in here to plot only some of the words, since omitting the org bulk words via removeWords didn't work. I heard there's a bug with removeWords if they're at the start of a line, which the org state words usually are.

``` r
library("wordcloud")
set.seed(1945)
wordcloud(names(org_term_freq), org_term_freq,
          min.freq=40,
          colors=brewer.pal(6,"Dark2"))
```

![plot of chunk unnamed-chunk-10](./analysis_files/figure-markdown_github/unnamed-chunk-101.png)

``` r
org_term_freq[tail(org_term_order, n=25L)]
```

    ##      data       get      also      make       add       see       use 
    ##       143       143       145       145       146       164       166 
    ##     check cancelled       can       sat       sun       thu scheduled 
    ##       175       202       226       299       316       335       346 
    ##       wed       fri       mon       tue      next    closed   logbook 
    ##       399       422       537       558       653       752      1046 
    ##      todo       end     state      done 
    ##      1055      1186      1693      2143

``` r
all_terms <- length(org_term_freq)
omit_last <- 25
wordcloud(names(org_term_freq[head(org_term_order,
                                   n=all_terms-omit_last)]),
          org_term_freq[head(org_term_order,
                             n=all_terms-omit_last)],
          min.freq=15,
          colors=brewer.pal(5,"Dark2"))
```

![plot of chunk unnamed-chunk-10](./analysis_files/figure-markdown_github/unnamed-chunk-102.png)

Finally, make wordclouds for individual org agenda files
========================================================

-   I've omitted some of the plots for brevity.

``` r
setwd("data")
org_dirs <- dir()[!grepl(".org",dir())]
org_dirs
corp_to_wordcloud <- function(top_dir){
    dir_source <- DirSource(top_dir)
    agenda_corpus <- VCorpus(x=dir_source,
                             readerControl=
                                 list(reader = reader(dir_source),
                                      language = "en"))
    agenda_corpus <- tm_map(agenda_corpus,
                            content_transformer(removeWords),
                            stopwords())
    agenda_corpus <- tm_map(agenda_corpus,
                            content_transformer(removePunctuation))
    agenda_corpus <- tm_map(agenda_corpus,
                            content_transformer(removeNumbers))
    agenda_corpus <- tm_map(agenda_corpus,
                            content_transformer(stripWhitespace))
    agenda_corpus <- tm_map(agenda_corpus,
                            content_transformer(stemDocument))
    org_doc_term_matrix <- DocumentTermMatrix(agenda_corpus)
    org_term_freq <- colSums(as.matrix(org_doc_term_matrix))
    org_term_order <- order(org_term_freq)
    set.seed(1945)
    all_terms <- length(org_term_freq)
    omit_last <- 15
    wordcloud(names(org_term_freq[head(org_term_order,
                                       n=all_terms-omit_last)]),
              org_term_freq[head(org_term_order,
                                 n=all_terms-omit_last)],
              min.freq=15,
              colors=brewer.pal(5,"Dark2"), main=dir_source)
}
lapply(org_dirs, corp_to_wordcloud)
```

![plot of chunk unnamed-chunk-11](./analysis_files/figure-markdown_github/unnamed-chunk-111.png) ![plot of chunk unnamed-chunk-11](./analysis_files/figure-markdown_github/unnamed-chunk-112.png) ![plot of chunk unnamed-chunk-11](./analysis_files/figure-markdown_github/unnamed-chunk-113.png) ![plot of chunk unnamed-chunk-11](./analysis_files/figure-markdown_github/unnamed-chunk-114.png) ![plot of chunk unnamed-chunk-11](./analysis_files/figure-markdown_github/unnamed-chunk-115.png) ![plot of chunk unnamed-chunk-11](./analysis_files/figure-markdown_github/unnamed-chunk-116.png) ![plot of chunk unnamed-chunk-11](./analysis_files/figure-markdown_github/unnamed-chunk-117.png)

    ## [1] "computing"    "fynanse"      "personal"     "physical"    
    ## [5] "professional" "reading"      "website"

    ## [[1]]
    ## NULL
    ## 
    ## [[2]]
    ## NULL
    ## 
    ## [[3]]
    ## NULL
    ## 
    ## [[4]]
    ## NULL
    ## 
    ## [[5]]
    ## NULL
    ## 
    ## [[6]]
    ## NULL
    ## 
    ## [[7]]
    ## NULL

Next time
=========

-   Try python's nltk
-   I think some normalization of the word frequencies against a null case (some generic books, dictionary, i don't know what) might be more interesting
-   Trim terms using in inflection points on the ordered terms vs frequency relationship
-   Animate the wordclouds of the individual org agenda files

References
==========

-   <http://onepager.togaware.com/TextMiningO.pdf>
-   <http://cran.r-project.org/web/packages/tm/vignettes/tm.pdf>
