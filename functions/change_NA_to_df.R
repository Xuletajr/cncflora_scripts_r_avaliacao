
change_others_to_dataframe <- function(x) {
    # If x is a data frame, do nothing and return x
    # Otherwise, return a data frame with 1 row of NAs
    if (nrow(x) != 0) {return(x)}
    else {
        return(data.frame(om_all = NA))
        }
}


change_others_to_dataframe2 <- function(x) {
    # If x is a data frame, do nothing and return x
    # Otherwise, return a data frame with 1 row of NAs
    if (nrow(x) != 0) {return(x)}
    else {
        return(data.frame(lifeForm = NA,
                          habitat = NA,
                          vegetationType = NA))
    }
}

#####   end----