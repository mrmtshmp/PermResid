#' The residual from linear model (mf.resid)
#'
#' @import tidyr
#' @import plyr
#' @import dplyr
#' @import tibble
#'
#' @param data A dataframe with column of
#' @param model.lm A formula for lm() function to calculate residuals.
#' @param var A character vector to specify the categorical variate to differring the residuals.
#'
#' @export
#'


mf.resid <- function(
  data, model.lm = my.model, var = "Visit"
){
  ads     <- data
  ads$var <- data[,var]
  res.resid <-
    ads %>%
    ddply(
      .(Meas, var),
      function(D){
        print(names(D))

        res.lm <- lm(
          model.lm,
          data = D
          )

        sample.size <-
          length(
            res.lm$residuals
            )

        return(
          data.frame(
            "resd.coef" = sum(res.lm$residuals**2)/
              sample.size,
            "coeff"     =
              t(res.lm$coefficients)
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

  .model.lm <- model.lm

  df.itt <- data.frame(
    "itt" = seq(1:n.perm)
    )

  ads <- data

  ads[,"var.ori"] <-
    ads[,var]

  list.perm.resid <- df.itt %>%
    dlply(
      .(itt),
      function(i){
        if(i == 1) ADS.perm <-
            ads %>%
            dplyr::rename("var.shfl"="var.ori")
        if(i > 1){
          perm.by.SubjID <-
            data.frame(
              "SubjID" =
                unique(data$SubjID)
              ) %>%
            ddply(
              .(SubjID),
              function(D){
                print(unique(data[, var]))
                var.shfl <- sample(
                  unique(data[, var])
                  )
                res <- expand.grid(
                  "SubjID"  =
                    D$SubjID,
                  "var.ori" =
                    unique(
                      data[, var]
                      )

                  ) %>%
                  data.frame()

                colnames(res) <-
                  c("Var1", "var.ori")

                res[,"var.shfl"] <-
                  var.shfl

                return(
                  res[
                    ,c("var.ori", "var.shfl")
                    ]
                )
              }
            )

          ADS.perm <-
            ads %>%
            left_join(
              perm.by.SubjID,
              by = c("SubjID", "var.ori")
              )
        }
        res <- try(
          ADS.perm %>%
            mf.resid(model.lm=.model.lm, var="var.shfl")
          )
        if(class(res)=="try-error") res <- NA

        return(list(res,data_long.vs.Jnt.perm))
      }
    )
  return(list.perm.resid)
}
