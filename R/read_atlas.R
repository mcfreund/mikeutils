#' Reads parcellation atlas and key.
#'
#'
#' @param name
#' @param space
#' @param surface
#' @param path.atlas
#' @keywords AFNI, neuroimaging

#' @export

read_atlas <- function(
  name.atlas,
  space = "hcp",
  surface = TRUE,
  path.atlas
) {
  ## TODO: lots.
  ##  - fix dependency issue; rn, it requires cifti to be loaded
  ##  - add in volumes, alterantive surfaces, etc...

  if (missing(path.atlas)) {
    nodename <- Sys.info()["nodename"]
    if (nodename == "ccplinux1") {
      path.atlas <- "/data/nil-bluearc/ccp-hcp/DMCC_ALL_BACKUPS/ATLASES/"
    } else if (nodename == "CCP-FREUND") {  ## mike freund's (i.e., ccp's) thinkpad
      path.atlas <- "C:/local/atlases"
    }
  }

  name.atlas <- tolower(name.atlas)

  if (name.atlas == "schaefer400") {

    fname.atlas <- file.path(
      path.atlas,
      "Schaefer2018_Parcellations", "HCP", "fslr32k", "cifti", "Schaefer2018_400Parcels_7Networks_order_info.txt"
    )
    if (file.exists(fname.atlas)) {
      fin <- file(fname.atlas, 'rt')
      tmp <- readLines(fin);
      close(fin); unlink(fin);
      if (length(tmp) != 800) { stop("not expected Schaefer key."); }
      tmp <- tmp[seq(from=1, to=800, by=2)];   # every-other entry is a label
      atlas.key <- gsub("7Networks_", "", tmp);
    }
    atlas <- cifti::read_cifti(
      file.path(
        path.atlas,
        "Schaefer2018_Parcellations", "HCP", "fslr32k", "cifti",
        "Schaefer2018_400Parcels_7Networks_order.dscalar.nii"
      ),
      drop_data = TRUE, trans_data = TRUE
    )
    atlas <- matrix(atlas$data)

  } else if (name.atlas == "glasser") {



  } else if (name.atlas == "gordon") {



  }

  list(atlas = atlas, key = atlas.key)

}



# if (surface.space == "hcp") {
#   n["v"] <- 32492  ## num vertices in surface (1 hemisphere)
#   surf.L <- readGIfTI("/scratch2/HCP_S1200_GroupAvg_v1/S1200.L.inflated_MSMAll.32k_fs_LR.surf.gii")
#   surf.R <- readGIfTI("/scratch2/HCP_S1200_GroupAvg_v1/S1200.R.inflated_MSMAll.32k_fs_LR.surf.gii")
# } else {
#   n["v"] <- 10242
#   surf.L <- readGIfTI("/scratch2/fsaverage_fmriprep_surfgii/fsaverage5_lh.inflated_avg.surf.gii")
#   surf.R <- readGIfTI("/scratch2/fsaverage_fmriprep_surfgii/fsaverage5_rh.inflated_avg.surf.gii")
# }
# dimnames(atlas) <- list(vertex = NULL, hemi = c("L", "R"))



# read_atlas_key <- function(
#   name.atlas,
#   surface = TRUE,
#   path.atlas = if ()
#
# ) {
#
#
# }
