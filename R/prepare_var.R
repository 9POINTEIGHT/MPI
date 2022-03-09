prepare_var <- function(df, g, w){

  # number of indicators
  n_Ind <- if_else(is.null(g),
                  length(names(df)),
                  sum(!(names(df) %in% g)))


  # if there is not weight input
  # then default all weights to 1
  if(is.null(w)){
    w <- rep(1, n_Ind)
  }

  # sum all weight
  sum.w <- sum(w)

  if(!is.null(g)){

    # list of tibble that contains rows associated group
    grouped_df <- df %>% group_by(!!!syms(g))

    # vector of group names
    groupnames <-  group_keys(grouped_df) %>%
      unite(., g, sep =", ") %>%
      unlist(use.names = FALSE)

    # set group names vector to tibble list
    dflist <- grouped_df %>%
      group_split() %>% setNames(groupnames)

    # remove selected group column
    dflist <- purrr::map(dflist, ~(.x %>% select(-all_of(g))))
  }else{
    dflist <- list(df) %>% setNames("Null")
  }

  out <- list(n_Ind = n_Ind, weight = w ,sum.w = sum.w, dflist = dflist)
  return(out)
}
