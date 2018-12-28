friedmanMaxValue <- function(matrix_name, top_num){
  idx <- which( 
    matrix(matrix_name %in% head(sort(matrix_name, TRUE), top_num), 
           nr = nrow(matrix_name)), arr.ind = TRUE)
  idx
}



# test
m <- matrix(16:1, 4, 4) 
colnames(m) <- c("a", "b", "c", "d")
row.names(m) <- c("A", "B", "C", "D")
friedmanMaxValue(m,3)
