#' @title Alkire-Foster (AF) method in parallel mode
#'
#' @description
#'
#' Multidimensional Poverty Index using Alkire-Foster (AF) method
#' computes in parallel mode.
#'
#' @param df A poverty data frame
#' @param g A column name will be used to divide data into groups. When the value is NULL, the entire data is not separated into groups.(default as NULL)
#' @param w An indicator weight vectors (default as 1)
#' @param k A poverty cut-off. If an aggregate value of indicators of a specific person is above or equal the value of k, then this person is considered to be a poor.(default as 1)
#'
#' @return returns a \code{list} of \code{list} contains
#'
#' \item{groupname}{A Grouped value from column input `g`}
#' \item{total}{Number of population in each group}
#' \item{poors}{Number of deprived people in each group}
#' \item{H}{Head count ratio, the proportion of the population that is multidimensionally deprived calculated by dividing the number of poor people with the total number of people.}
#' \item{A}{Average deprivation share among poor people, by aggregating the proportion of total deprivations each person and dividing by the total number of poor people.}
#' \item{M0}{Multidimensional Poverty Index (MPI)}
#' \item{DimentionalContribution}{\code{indnames} is poverty indicator vectors.\code{diCont} Dimensional contributions denotes the magnitude of each indicator impacts on MPI. \code{UncensoredHCount} Uncensored head count of indicator denotes the population that are deprived in that indicator. \code{UncensoredHRatio} Uncensored head count ratio of indicator denotes the proportion of the population deprived in that indicator.\code{CensoredHCount} Censored head count of indicator denotes the population that are multidimensionally poor and deprived in that indicator at the same time. \code{CensoredHRatio} Censored head count ratio of indicator denotes the proportion that is multidimensionally poor and deprived in that indicator at the same time.}
#' \item{pov_df}{poverty data frame associated with each group.\code{Cvector} is a vector of total values of deprived indicators adjusted by weight of indicators. Each element in \code{Cvector} represents a total value of each individual. \code{IsPoverty} is a binary variable (1 and 0). 1 indicates that a person does not meet the threshold (poor person) and 0 indicates the opposite. \code{Intensity}, The intensity of a deprived indication among impoverished people is computed by dividing the number of deprived indicators by the total number of indicators.}
#'
#' @examples
#'
#' # Run this function
#' \dontrun{
#' output <- MPI::AF_Par(df = MPI::examplePovertydf, g = "Region")
#' }
#'
#' @export

AF_Par <- function(df, g = NULL,
                  w = NULL,
                  k = 1){

  vars_list <- prepare_var(df, g , w)
  i <- seq()
  pov_df <- data.frame()
  # get indicator name
  indnames <- names(vars_list$dflist[[1]])

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

    # number of indicators
    nindicator <- ncol(pov_df) - 3

    # uncensored and censored headcount of each indicator
    HcountRatio <- pov_df[1:nindicator] %>%
      colSums() %>%
      as.data.frame() %>%
      setNames("UncensoredHCounts") %>%
      mutate(UncensoredHRatio = .data$UncensoredHCounts/nrowDf) %>%
      mutate(mutate(pov_df[1:nindicator] * pov_df$IsPoverty) %>%
               colSums() %>%
               as.data.frame() %>%
               setNames("CensoredHCounts") %>%
               mutate(CensoredHRatio = .data$CensoredHCounts/nrowDf))

    # censored headcount ratio
    H <- CHeadCount/ nrowDf
    # average intensity among criteria data
    A <- sum(pov_df$Intensity)/CHeadCount
    # adjusted headcount ratio
    M0 <- H * A

    # censored headcount ratio of each indicator
    diCont <- (vars_list$weight/vars_list$sum.w) * (HcountRatio$CensoredHRatio / M0) %>%
      as.data.frame() %>%
      setNames("diCont")

    diCont <- diCont %>% mutate(HcountRatio)
    diCont <- cbind(indnames,diCont)

    list(groupname = names(vars_list$dflist[i]),
         total = nrowDf, poors = CHeadCount,H = H, A = A, M0 = M0,
         DimentionalContribution = diCont,
         pov_df = pov_df)
  }
  stopCluster(cl)
  return(out)
}
