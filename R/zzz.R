strextract <- function(str, pattern) regmatches(str, regexpr(pattern, str))

strtrim <- function(str) gsub("^\\s+|\\s+$", "", str)

# Function to split confidence intervals, this function is required in the below functions
CI_split <- function(a){
  dd <- c(gsub("\\(","",unlist(strsplit(a[5],","))[1]),gsub("\\)","",unlist(strsplit(a[5],","))[2]))
  b22 <- a[2:8]
  b222 <- b22[-4]
  b222 <- append(b222,dd,after = 3)
  b222
}

# Function to write trees to directory for use by Phylometa
WriteTrees2 <- function(xxx) {
  for (i in 1:length(xxx)) {
    ape::write.tree(xxx[[i]], paste("tree", i, ".txt", "", sep = ""))
  }
}

# Get output from phylometa.process
phylometa.output <- function(a) {
  a
}

phylometa.output.table <- function(a,b) {
  a[[b]]
}

