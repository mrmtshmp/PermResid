#' The residual from linear model (mf.resid)
#'
#' @import tidyr
#' @import plyr
#' @import dplyr
#' @import tibble
#'
#' @param data_long.vs.Jnt A dataframe with column of
#' @param model A formula for lm() function to calculate residuals.
#' @param var A character vector to specify the categorical variate to differring the residuals.
#'
#' @export
#'


mf.resid <- function(
  data_long.vs.Jnt, model = my.model, var = "Visit"
){
  data_long.vs.Jnt$var <- data_long.vs.Jnt[,var]
  res.resid <-
    data_long.vs.Jnt %>%
    ddply(
      .(Meas, var),
      function(D){
        print(names(D))

        res.lm <- lm(
          my.model, data = D
        )

        sample.size <-
          length(
            res.lm$residuals
          )

        return(
          data.frame(
            "resd.coef" = sum(res.lm$residuals**2)/sample.size,
            "coeff"     = t(res.lm$coefficients)
          )
        )
      }
    )
}

#' The residual from linear model (mf.resid) and the permuted one (mf.perm.resid).
#'
#' @import tidyr
#' @import plyr
#' @import dplyr
#' @import tibble
#'
#' @param model A formula for lm() function to calculate residuals.
#' @param var A character vector to specify the categorical variate to differring the residuals.
#' @param data  A dataframe with column of (to be imput to mf.resid function as data_long.vs.Jnt argument)
#' @param n.perm The number of permuted data.
#'
#' @export
#'

mf.perm.resid <- function(
  data = data_long.vs.Jnt,
  n.perm = 500,
  model.lm = my.model,
  var = "Visit"
  ){

  df.itt <- data.frame(
    "itt" = seq(1:n.perm)
    )

  data_long.vs.Jnt.perm <- data

  data_long.vs.Jnt.perm[,"var.ori"] <-
    data_long.vs.Jnt.perm[,var]

  list.perm.resid <- df.itt %>%
    dlply(
      .(itt),
      function(i){
        if(i == 1) ADS.perm <-
            data_long.vs.Jnt.perm %>%
            dplyr::rename("var.shfl"="var.ori")
        if(i > 1){
          perm.by.SubjID <-
            data.frame(
              "SubjID" =
                unique(data_long.vs.Jnt$SubjID)
            ) %>%
            ddply(
              .(SubjID),
              function(D){
                var.shfl <- sample(
                  unique(data_long.vs.Jnt[, var])
                )
                res <- expand.grid(
                  "SubjID"  = D$SubjID,
                  "var.ori" = unique(data_long.vs.Jnt[, var])
                ) %>% data.frame()

                colnames(res) <-
                  c("Var1", "var.ori")

                res[,"var.shfl"] <-
                  var.shfl

                print(names(res))
                return(
                  res[
                    ,c("var.ori", "var.shfl")
                    ]
                )
              }
            )

          ADS.perm <-
            data_long.vs.Jnt.perm %>%
            left_join(
              perm.by.SubjID,
              by = c("SubjID", "var.ori")
            )
        }
        res <- ADS.perm %>%
          mf.resid(var="var.shfl")

        return(list(res,data_long.vs.Jnt.perm))
      }
    )
  return(list.perm.resid)
}
