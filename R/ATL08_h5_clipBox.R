#' @include class.icesat2.R
#' @importClassesFrom terra SpatExtent
setMethod(
  "clip",
  signature = c("icesat2.atl08_h5", "character", "SpatExtent"),
  function(x, output, bbox) {
    ATL08_h5_clipBox(x, output, bbox)
  }
)

setMethod(
  "clip",
  signature = c("icesat2.atl08_h5", "character", "numeric"),
  function(x, output, bbox) {
    print("clipping by bbox")
    bbox_ext <- terra::ext(bbox[c(2, 4, 3, 1)])
    ATL08_h5_clipBox(x, output, bbox_ext)
  }
)

#' Clips ICESat-2 ATL08 data
#'
#' @param atl08 [`icesat2.atl08_h5-class`] object, obtained through [`ATL08_read()`]
#' for clipping
#' @param output character. Path to the output h5 file.
#' @param bbox [`numeric-class`] or [`terra::SpatExtent`] for clipping, the
#' order of the bbox is the default from NASA's ICESat-2 CMS searching:
#' [ul_lat, ul_lon, lr_lat, lr_lon].
#'
#' @return Returns the clipped S4 object of class [`icesat2.atl08_h5-class`]
#'
#' @description This function clips the ATl08 HDF5 file. This function
#' will only clip the beam groups within hdf5, it won't change metadata
#' or ancillary data.
#'
#' @examples
#' # Specifying the path to ATL08 file (zip file)
#' outdir <- tempdir()
#' atl08_zip <- system.file("extdata",
#'   "ATL08_20220401221822_01501506_005_01.zip",
#'   package = "rICESat2Veg"
#' )
#'
#' # Unzipping ATL08 file
#' atl08_path <- unzip(atl08_zip, exdir = outdir)
#'
#' # Reading ATL08 data (h5 file)
# atl08_h5<-ATL08_read(atl08_path=atl08_path)
#'
#' # Bounding rectangle coordinates
#' xmin <- -107.7
#' xmax <- -106.5
#' ymin <- 32.75
#' ymax <- 42.75
#'
#' # Clipping ATL08 terrain and canopy attributes by boundary box
#' atl08_seg_att_dt_clip <- ATL08_h5_clipBox(atl08_h5,outdir, xmin, xmax, ymin, ymax)
#'
#' close(atl08_h5)
#' @import hdf5r
#' @export
ATL08_h5_clipBox <- function(atl08, output, bbox) {
  latitude <- longitude <- obj_type <- dataset.rank <- dataset.dims <- NA
  .I <- data.table::.I
  . <- list

  # Create a new HDF5 file
  newFile <- hdf5r::H5File$new(output, mode = "w")


  # Create all groups
  structure_dt <- data.table::as.data.table(atl08@h5$ls(recursive = T))
  groups <- structure_dt[obj_type == "H5I_GROUP"]$name


  for (group in groups) {
    grp <- newFile$create_group(group)

    # Create all atributes within group
    attributes <- hdf5r::list.attributes(atl08[[group]])
    for (attribute in attributes) {
      grp$create_attr(attribute, hdf5r::h5attr(atl08[[group]], attribute))
    }
  }

  # Create root attributes
  attributes <- hdf5r::list.attributes(atl08@h5)
  for (attribute in attributes) {
    hdf5r::h5attr(newFile, attribute) <- hdf5r::h5attr(atl08@h5, attribute)
  }

  # Get all beams
  beams <- getBeams(atl08)


  nBeam <- 0
  nBeams <- length(beams)

  # Loop the beams
  for (beamName in beams) {
    nBeam <- nBeam + 1
    message(sprintf("Clipping %s (%d/%d)", beamName, nBeam, nBeams))

    # Get the reference beam
    beam <- atl08[[beamName]]

    # Get the beam to update
    updateBeam <- newFile[[beamName]]


    # Get land segments mask
    land_segments_dt <- data.table::data.table(
      latitude = beam[["land_segments/latitude"]][],
      longitude = beam[["land_segments/longitude"]][]
    )

    landSegmentsMask <- land_segments_dt[, .(latitude, longitude, .I)][
      longitude >= bbox$xmin &
        longitude <= bbox$xmax &
        latitude >= bbox$ymin &
        latitude <= bbox$ymax, I
    ]

    # Get photons for the masked land segments
    ph_ndx_beg <- beam[["land_segments/ph_ndx_beg"]][landSegmentsMask]
    n_seg_ph <- beam[["land_segments/n_seg_ph"]][landSegmentsMask]

    # Create mask for photons
    photonsMask <- unlist(
      Vectorize(seq.default, vectorize.args = c("from", "to"))(from = ph_ndx_beg, to = ph_ndx_beg + n_seg_ph)
    )

    # Get sizes of clipping datasets
    photonsSize <- beam[["signal_photons/ph_h"]]$dims
    segmentsSize <- beam[["land_segments/segment_watermask"]]$dims

    # Get all datasets
    datasets_dt <- data.table::as.data.table(beam$ls(recursive = TRUE))[obj_type == 5]

    # Get all types of clipping photons/segment/no cut
    photonsCut <- datasets_dt[dataset.dims == photonsSize]$name

    segmentsCut <- datasets_dt[
      dataset.dims == segmentsSize
    ]$name
    segmentsCut2D <- datasets_dt[
      grepl(segmentsSize, dataset.dims) & dataset.rank == 2
    ]$name

    qtyList <- lapply(datasets_dt$dataset.dims, function(x) eval(parse(text = gsub("x", "*", x))))
    qty <- sum(unlist(qtyList))

    pb <- utils::txtProgressBar(min = 0, max = qty, style = 3)

    # Do clipping and copying


    clipByMask(beam, updateBeam, segmentsCut, landSegmentsMask, pb)
    clipByMask2D(beam, updateBeam, segmentsCut2D, landSegmentsMask, pb)
    clipByMask(beam, updateBeam, photonsCut, photonsMask, pb)
    updateBeam[["land_segments/ph_ndx_beg"]][] <- c(1, cumsum(updateBeam[["land_segments/n_seg_ph"]][])[-1])

    close(pb)
  }

  newFile$close_all()
  ATL08_read(output)
}
