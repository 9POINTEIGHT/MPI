#' @title Alkire-Foster (AF) method in parallel mode
#'
#' @description
#'
#' Multidimensional Poverty Index using Alkire-Foster (AF) method
#' computes in parallel mode.
#'
#' @param df A poverty data frame
#' @param g A groups(default as NULL)
#' @param w An indicator weight vectors (default as 1)
#' @param k A poverty cut-off (default as 1)
#'
#' @return returns a \code{list} of \code{list} contains
#'
#' \item{H}{Uncensored head count ratio the proportion of the population that is multidimensionally deprived calculated by divide the number of poor people with the total number of people.}
#' \item{A}{Average deprivation share among poor people, by aggregating the proportion of total deprivations each person and dividing by the total number of poor people.}
#' \item{M0}{Multidimensional Poverty Index (MPI)}
#' \item{DimentionalContribution}{Dimensional contributions denotes the magnitude of each indicators impact on MPI.}
#' \item{pov_df}{poverty data frame associated with each group.\code{Cvector} is total deprived indicators. \code{IsPoverty} is a binary variable with only 1 and 0, with 1 indicating that person does not meet the threshold(poor person) and 0 indicating the opposite. \code{Intensity}, The intensity of a deprived indication among impoverished people is computed by dividing the number of deprived indicators by the total number of indicators.}
#'
#' @export

AF_Par <- function(df, g = NULL,
                  w = NULL,
                  k = 1){

  vars_list <- prepare_var(df, g , w)
  i <- seq()
  pov_df <- data.frame()

  # Parallel
  registerDoParallel(cl <- makeCluster(3))
  out <- foreach(i = seq(length(vars_list$dflist)),
                 .packages = "dplyr") %dopar% {
    pov_df <- data.frame(mapply(`*`,
                               vars_list$dflist[[i]],
                               vars_list$weight,
                               SIMPLIFY=FALSE)) %>%
      mutate(Cvector = rowSums(.)) %>%
      mutate(IsPoverty = ifelse(.$Cvector >= k, 1 , 0)) %>%
      mutate(Intensity = .$Cvector * .$IsPoverty / vars_list$n_Ind)

    # number of row
    nrowDf <- nrow(vars_list$dflist[[i]])
    # censored headcount
    CHeadCount <- sum(pov_df$IsPoverty)

    # censored headcount ratio
    H <- CHeadCount/ nrowDf
    # average intensity among criteria data
    A <- sum(pov_df$Intensity)/CHeadCount
    # adjusted headcount ratio
    M0 <- H * A

    # censored headcount ratio of each indicator
    Hj <- ((pov_df[,1:vars_list$n_Ind] * pov_df[, "IsPoverty"]) %>%
             colSums() ) * if_else(vars_list$weight == 0, 0, 1) / nrowDf
    diCont <- (vars_list$weight/vars_list$sum.w) * (Hj / M0)

    list(groupname = names(vars_list$dflist[i]), H = H, A = A, M0 = M0,
         DimentionalContribution = as.data.frame(diCont),
         pov_df = pov_df)
  }
  stopCluster(cl)
  return(out)
}
