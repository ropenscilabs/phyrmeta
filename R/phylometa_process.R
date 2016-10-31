#' Process phylometa output
#'
#' @export
#' @param x output from running Phylometa
#' @param groups (integer) number of groups
#' @examples
#' file <- system.file("examples/output_eg1.txt", package = "phyrmeta")
#'
phylometa_process <- function(x, groups = 1) {
  stopifnot(file.exists(x))
  txt <- readLines(x)


  # aaa <- data.frame(x)
  # if (length(subset(aaa,aaa[,1] == "poly")[,1]) == 1) {
  #   x <- x[-as.numeric(rownames(subset(aaa,aaa[,1] == "poly")))]
  # }
  # aaaa <- data.frame(x)
  # a <- as.numeric(rownames(subset(aaaa,aaaa[,1] == "RESULTS SECTION A. Traditional meta-analysis.")))
  # b <- as.numeric(rownames(subset(aaaa,aaaa[,1] == "For further details on these methods see:")))
  # bbb <- x[a:b]
  # switch(
  #   groups,
  #   `1` = maketables_1group(bbb),
  #   `2` = maketables_2group(bbb),
  #   `3` = maketables_3group(bbb),
  #   `4` = maketables_4group(bbb)
  # )
}
