
answers = rep("A", 20)

pml_write_files = function(x){
        f <- function(i, v) {
                filename = paste0("problem_id_", i, ".txt")
                write.table(v, file=filename, quote=FALSE, row.names=FALSE, col.names=FALSE)
        }
        mapply(f, seq_along(x), x)
}

pml_write_files(answers)

