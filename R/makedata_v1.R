#' Make analysis data for proj. PermResid
#'
#' @import tidyr
#' @import plyr
#' @import dplyr
#' @import tibble
#'
#' @param dir.data A character string to specify the directory where .RData to be read is
#' @param fn.data A character string to specify the .RData to be read
#' @param
#'


make_data <- function(
  dir.data =  "../../Data",
  fn.data  =  "ADS_v1.RData",
  SunjID.unique_or_dupOK = "unique"
  ){

  load(sprintf("%s/%s", dir.data, fn.data))

  data.unique <-
    data[
      !duplicated(data$SubjID),
      ]

  data.inc_dup <- data %>%
    mutate(
      dupkey = ifelse(duplicated(SubjID), 1, 0),
      SubjID = sprintf("%s_%s", SubjID, dupkey)
    )

  if(SunjID.unique_or_dupOK == "unique"){
    ADS <- data.unique
  }else{
    if(SunjID.unique_or_dupOK == "dupOK"){
      ADS <- data.inc_dup
      }
    }

  raw.data_long <- ADS %>%

    dplyr::select(
      SubjID, Age, dur_months, Sex,
      starts_with("0M"),
      starts_with("6M"),
      starts_with("12M")
    ) %>%
    gather(
      var, val,
      -SubjID, -Age, -dur_months, -Sex
    )

  raw.data_long[,"Visit"] <-
    gsub(
      "([0-9]{1,2})M_(.+)[.jnt_]?([0-9]{0,})", "\\1",
      raw.data_long$var
    )
  raw.data_long[,"raw.Meas"] <-
    gsub(
      "([0-9]{1,2})M_(.+)[.jnt_]?([0-9]{0,})", "\\2",
      raw.data_long$var
    )
  raw.data_long[,"Meas"] <-
    gsub(
      ".jnt_[0-9]{1,}", "",
      raw.data_long$raw.Meas
    )
  raw.data_long[,"numb.jnt"] <-
    gsub(
      ".+jnt_([0-9]{1,})", "\\1",
      raw.data_long$raw.Meas
    )

  data_long <- raw.data_long %>%
    mutate(
      Sex = factor(Sex),
      Visit= as.numeric(Visit),
      numb.jnt = as.numeric(
        ifelse(
          !(
            numb.jnt %in%
              c(6,22)
            ),
          NA, numb.jnt
          )
        )
    ) %>%
    dplyr::select(
      -raw.Meas, -var
    ) %>%
    filter(!is.na(val))

  data_long.vs.Jnt <-
    data_long %>%
    dplyr::select(
      SubjID, Visit, Meas, numb.jnt, val
    ) %>%
    filter(
      !is.na(numb.jnt)
    ) %>%
    ddply(
      .(Visit),
      function(D){
        res <- ddply(
          D,
          .(Meas),
          function(D2){
            res2 <- D2 %>%
              spread(numb.jnt, val)
            return(res2)
          }
        )
        return(res)
      }
    ) %>%
    rename(
      "jnt.22"="22",
      "jnt.6"="6"
    )

  data_long.vs.Jnt <-
    data_long.vs.Jnt %>%
    left_join(
      data %>%
        dplyr::select(
          SubjID, Age, dur_months, Sex, fc.RF,
          fc.ACPA, cat.conc_DMARDs, fc.conc_csDMARDs,
          fc.conc_MTX, fc.conc_SASP, fc.conc.TAC,
          fc.conc.BUC, fc.conc_PSL, fc.hist_Bio,
          fc.hist_Bio_pl, fc.Bio
        ),
      by="SubjID"
    )
  return(data_long.vs.Jnt)
}


