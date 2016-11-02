#' Process phylometa output
#'
#' @export
#' @param x (character) a path to a file, the output from running Phylometa.
#' required.
#' @param groups (integer) number of groups. required. This is the number of
#' groups you specified in your phylometa analysis. if you look at your output
#' file, it's the number of groups listed in any table
#' @examples
#' file <- system.file("examples/example_one_group.txt", package = "phyrmeta")
#' phylometa_process(file, groups = 1)
#'
#' file <- system.file("examples/example_two_groups.txt", package = "phyrmeta")
#' phylometa_process(file, groups = 2)
#'
#' file <- system.file("examples/example_three_groups.txt", package = "phyrmeta")
#' phylometa_process(file, groups = 3)
#'
#' file <- system.file("examples/example_four_groups.txt", package = "phyrmeta")
#' phylometa_process(file, groups = 4)
phylometa_process <- function(x, groups) {
  stopifnot(file.exists(x))
  txt <- readLines(x, encoding = "latin1")

  aaa <- data.frame(txt, stringsAsFactors = FALSE)
  if (length(subset(aaa, aaa[,1] == "poly")[, 1]) == 1) {
    x <- x[-as.numeric(rownames(subset(aaa,aaa[,1] == "poly")))]
  }
  aaaa <- data.frame(txt, stringsAsFactors = FALSE)
  a <- as.numeric(
    rownames(
      subset(aaaa, aaa[,1] == "RESULTS SECTION A. Traditional meta-analysis.")))
  b <- as.numeric(
    rownames(
      subset(aaaa, aaaa[,1] == "For further details on these methods see:")))
  if (length(b) == 0) b <- length(txt)
  bbb <- txt[a:b]
  switch(
    groups,
    `1` = maketables_1group(bbb),
    `2` = maketables_2group(bbb),
    `3` = maketables_3group(bbb),
    `4` = maketables_4group(bbb)
  )
}
