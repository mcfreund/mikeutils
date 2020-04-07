#' Reads trial-level behavioral data and event times for DMCC2 tasks.
#'
#' @param files Character; which files to read? Must be either \\%in\\% "axcpt", "cuedts", "stern", "stroop", "subjsum"; or "all." Defaults to "all".
#' @param use.boxr Boolean; use boxr or boxdrive? When on freund's laptop, defaults to boxdrive, otherwise boxr.
#' @param boxdrive.path String indicating path to boxdrive. Defaults to path for freund's laptop.
#' @keywords read, events, behavior

#' @export
read_dmcc_behav <- function(
  files = "all",
  use.boxr = if (Sys.info()["nodename"] == "CCP-FREUND") TRUE else FALSE,
  boxdrive.path = "C:/Users/mcf/Box"
  ) {

  ## input validation

  files <- tolower(files)
  if (files == "all") files <- c("axcpt", "cuedts", "stern", "stroop", "subjsum")
  if (!any(files %in% c("axcpt", "cuedts", "stern", "stroop", "subjsum"))) stop("bad file string")

  if (!use.boxr) if (!dir.exists(boxdrive.path)) stop("boxdrive path does not exist")

  ## read

  l <- setNames(vector("list", length(files)), files)

  if (use.boxr) {

    boxr::box_auth()

    if ("axcpt" %in% files) l[["axcpt"]]     <- boxr::box_read_csv(534174973987)
    if ("cuedts" %in% files) l[["cuedts"]]   <- boxr::box_read_csv(534204743026)
    if ("stern" %in% files) l[["stern"]]     <- boxr::box_read_csv(534194820188)
    if ("stroop" %in% files) l[["stroop"]]   <- boxr::box_read_csv(534179635480)
    if ("subjsum" %in% files) l[["subjsum"]] <- boxr::box_read(file_id = 534193095106, stringsAsFactor = FALSE)


  } else {

    path <- file.path(boxdrive.path, "Preprocessed_Data", "_wrangled")

    if ("axcpt" %in% files) l[["axcpt"]]     <- data.table::fread(path, "dmcc2_behavior-and-events_axcpt.csv")
    if ("cuedts" %in% files) l[["cuedts"]]   <- data.table::fread(path, "dmcc2_behavior-and-events_cuedts.csv")
    if ("stern" %in% files) l[["stern"]]     <- data.table::fread(path, "dmcc2_behavior-and-events_sternberg.csv")
    if ("stroop" %in% files) l[["stroop"]]   <- data.table::fread(path, "dmcc2_behavior-and-events_stroop.csv")
    if ("subjsum" %in% files) l[["subjsum"]] <- data.table::fread(path, "summary", "dmcc2_all-runs-summary.csv")

  }

  l

}
