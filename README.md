phyrmeta
========



`phyrmeta`: Client to Handle Phylometa Output

## Install


```r
remotes::install_github("sckott/phyrmeta")
```


```r
library('phyrmeta')
```

## Parse Phylometa output files

### 1 group

Get an example file


```r
file <- system.file("examples/example_one_group.txt", package = "phyrmeta")
cat(readLines(file)[1:10], sep = "\n")
#> metaPhylo ANALYSIS FROM: 640_tree.txt and 640_data.txt
#> 
#> RESULTS SECTION A. Traditional meta-analysis.
#> --------------------------------------------------------------------------------
#> 
#>   TABLE 1. Summary of fit statistics.
#>   ------------------------------------------
#>     Source                Q     df    p
#>   ------------------------------------------
#>     Between groups       0.00    1  1.0000
```

Parse the file


```r
phylometa_process(file, groups = 1)
#> $traditional_summary_fit
#>           Source Q    df     P
#> 1 Between groups 0     1     1
#> 2  Within groups 0    19     1
#> 3 Within group 0 0    19     1
#> 4          Total 0 -9999 -9999
#> 5 random-effects 0     1     1
#> 
#> $traditional_summary_pooled_effects
#>         Group  k effsize  var CI_low CI_high    Z df      P
#> 1 All studies 20   0.041 0.05 -0.397   0.479 0.03  1 0.8548
#> 2     Group 0 20   0.041 0.05 -0.397   0.479 0.03  1 0.8548
#> 3 All studies 20   0.041 0.05 -0.397   0.479 0.03  1 0.8548
#> 4     Group 0 20   0.041 0.05 -0.397   0.479 0.03  1 0.8548
#> 
#> $phylogenetic_summary_fit
#>           Source    Q    df     P df_polytadj P_polytadj
#> 1 Between groups 0.00     1     1       -9999      -9999
#> 2  Within groups 0.01    19     1          19          1
#> 3 Within group 0 0.01    19     1          19          1
#> 4          Total 0.01 -9999 -9999       -9999      -9999
#> 5 random-effects 0.00     1     1       -9999      -9999
#> 
#> $phylogenetic_summary_pooled_effects
#>         Group  k effsize    var CI_low CI_high Z df      P
#> 1 All studies 20   0.041 0.4478 -1.271   1.352 0  1 0.9514
#> 2     Group 0 20   0.041 0.4478 -1.271   1.352 0  1 0.9514
#> 3 All studies 20   0.041 0.4478 -1.271   1.352 0  1 0.9514
#> 4     Group 0 20   0.041 0.4478 -1.271   1.352 0  1 0.9514
#> 
#> $traditional_vs_phylogenetic
#>                       Analysis AIC_fixed AIC_random
#> 1                  Traditional   -114.45    -114.45
#> 2 Phylogenetically-independent   -102.83    -102.83
```

### N groups

And so on for N groups. Change the `groups` parameter to the number of groups
in your output file.


## Meta

* Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). 
By participating in this project you agree to abide by its terms.
* Please [report any issues or bugs](https://github.com/sckott/phyrmeta/issues)
* License: MIT

