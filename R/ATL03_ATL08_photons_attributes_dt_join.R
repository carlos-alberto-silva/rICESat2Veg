#' Join ATL03 and ATL08 photons attributes
#'
#' @description This function joins ATL03 and ATL08 computed photons attributes
#'
#' @usage ATL03_ATL08_photons_attributes_dt_join(atl08_h5, beam)
#'
#' @param atl03_h5 A ICESat-2 ATL03 object (output of [ATL03_read()] function).
#' An S4 object of class [`icesat2.atl03_dt-class`].
#' @param atl08_h5 A ICESat-2 ATL08 object (output of [ATL08_read()] function).
#' An S4 object of class [rICESat2Veg::icesat2.atl08_dt].
#' @param beam Character vector indicating beams to process (e.g. "gt1l", "gt1r", "gt2l", "gt2r", "gt3l", "gt3r")
#'
#' @return Returns an S4 object of class [`icesat2.atl03atl08_dt-class`]
#' containing the ATL08 computed photons attributes.
#'
#' @details These are the photons attributes extracted by default:
#' \itemize{
#' \item \emph{lon_ph} Longitude of each received photon. Computed from the ECEF Cartesian coordinates of the bounce point.
#' \item \emph{lat_ph} Latitude of each received photon. Computed from the ECEF Cartesian coordinates of the bounce point.
#' \item \emph{lat_ph} Latitude of each received photon. Computed from the ECEF Cartesian coordinates of the bounce point.
#' Height of each received photon, relative to the WGS-84 ellipsoid including the geophysical corrections noted in section 6.0. Please note that
#' neither the geoid, ocean tide nor the dynamic atmospheric corrections (DAC) are applied to the ellipsoidal heights.
#' \item \emph{quality_ph} Indicates the quality of the associated photon. 0=nominal, 1=possible_afterpulse, 2=possible_impulse_response_
#' effect, 3=possible_tep. Use this flag in conjunction with signal_conf_ph to identify those photons that are likely noise or likely signal
#' \item \emph{night_flag}  Flag indicating the data were acquired in night conditions: 0=day, 1=night. Night flag is set when solar elevation is below 0.0 degrees.
#' \item \emph{ph_segment_id}  The elevation of the sun position vector from the reference photon bounce point position in the local ENU frame.
#' The angle is measured from the East-North plane and is positive Up. ATL03g provides this value in radians; it is converted to degrees for ATL03 output.
#' \item \emph{ph_segment_id} Georeferenced	bin	number (20-m) associated	with	each photon
#' \item \emph{classed_pc_indx} Indices of photons	tracking back	to ATL03	that	surface finding	software	identified and	used	within	the
#' creation of the	data products.
#' \item \emph{classed_pc_flag} The L2B algorithm is run if this flag is set to 1 indicating data have sufficient waveform fidelity for L2B to run
#' \item \emph{ph_h} Height of photon above interpolated ground surface
#' #'\item \emph{d_flag} Flag indicating	whether DRAGANN	labeled	the photon as noise or signal
#' \item \emph{delta_time} Mid-segment	GPS	time	in seconds past	an epoch. The epoch is provided	in the metadata	at the file	level
#' }
#'
#' @seealso \url{https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/ICESat2_ATL08_ATBD_r006.pdf}
#'
#'
#' @examples
#'
#' # Specifying the path to ATL03 and ATL08 file (zip file)
#' outdir <- tempdir()
#' atl03_zip <- system.file("extdata",
#'   "ATL03_20220401221822_01501506_005_01.zip",
#'   package = "rICESat2Veg"
#' )
#'
#' atl08_zip <- system.file("extdata",
#'   "ATL08_20220401221822_01501506_005_01.zip",
#'   package = "rICESat2Veg"
#' )
#'
#' # Unzipping ATL03 file
#' atl03_path <- unzip(atl03_zip, exdir = outdir)
#'
#' # Unzipping ATL08 file
#' atl08_path <- unzip(atl08_zip, exdir = outdir)
#'
#' # Reading ATL03 data (h5 file)
# atl03_h5<-ATL03_read(atl03_path=atl03_path)
#'
#' # Reading ATL08 data (h5 file)
# atl08_h5<-ATL08_read(atl08_path=atl08_path)
#'
#' # # Extracting ATL03 and ATL08 photons and heights
#' atl03_atl08_dt <- ATL03_ATL08_photons_attributes_dt_join(atl03_h5, atl08_h5)
#' head(atl03_atl08_dt)
#'
#' close(atl03_h5)
#' close(atl08_h5)
#' @export
ATL03_ATL08_photons_attributes_dt_join <- function(atl03_h5, atl08_h5,
                                                   beam = c("gt1l", "gt1r", "gt2l", "gt2r", "gt3l", "gt3r")) {
  # Check file input
  if (!class(atl03_h5) == "icesat2.atl03_h5") {
    stop("atl03_h5 must be an object of class 'icesat2.atl03_h5' - output of [ATL03_read()] function ")
  }

  # Check file input
  if (!class(atl08_h5) == "icesat2.atl08_h5") {
    stop("atl08_h5 must be an object of class 'icesat2.atl08_h5' - output of [ATL08_read()] function ")
  }

  # Check beams to select
  groups_id_atl03 <- grep("gt[1-3][lr]", atl08_h5@h5$ls()$name, value = TRUE)
  groups_id_atl08 <- grep("gt[1-3][lr]", atl08_h5@h5$ls()$name, value = TRUE)
  #
  # groups_id_atl03 <- getBeams(atl03_h5, recursive = FALSE)
  # groups_id_atl08 <- getBeams(atl08_h5, recursive = FALSE)

  check_beams_atl03 <- groups_id_atl03 %in% beam
  check_beams_atl08 <- groups_id_atl08 %in% beam

  beam_atl03 <- groups_id_atl03[check_beams_atl03]
  beam_atl08 <- groups_id_atl08[check_beams_atl08]

  beam <- beam_atl03[beam_atl03 %in% beam_atl08]

  photon.dt <- data.table::data.table()

  pb <- utils::txtProgressBar(min = 0, max = length(beam), style = 3)


  i_s <- 0

  atl03_h5 <- atl03_h5@h5
  atl08_h5 <- atl08_h5@h5

  for (i in beam) {
    i_s <- i_s + 0.25
    utils::setTxtProgressBar(pb, i_s)
    n_segments <- atl03_h5[[paste0(i, "/geolocation/segment_length")]]$dims

    segment_ph_cnt <- atl03_h5[[paste0(i, "/geolocation/segment_ph_cnt")]][]

    segment_length <- c(0, cumsum(atl03_h5[[paste0(i, "/geolocation/segment_length")]][1:(n_segments - 1)]))
    segment_lengths <- rep(segment_length, segment_ph_cnt)

    segment_solar_elevation <- atl03_h5[[paste0(i, "/geolocation/solar_elevation")]][]
    ph_solar_elev <- rep(segment_solar_elevation, segment_ph_cnt)

    dataTableATL03Photons <- data.table::data.table(data.frame(
      lon_ph = atl03_h5[[paste0(i, "/heights/lon_ph")]][],
      lat_ph = atl03_h5[[paste0(i, "/heights/lat_ph")]][],
      h_ph = atl03_h5[[paste0(i, "/heights/h_ph")]][],
      quality_ph = atl03_h5[[paste0(i, "/heights/quality_ph")]][],
      solar_elevation = ph_solar_elev,
      dist_ph_along = atl03_h5[[paste0(i, "/heights/dist_ph_along")]][] + segment_lengths
    ))

    dataTableATL03Segs <- data.table::data.table(data.frame(
      segment_id = atl03_h5[[paste0(i, "/geolocation/segment_id")]][],
      ph_index_beg = atl03_h5[[paste0(i, "/geolocation/ph_index_beg")]][]
    ))


    dataTableATL08Photons <- data.table::data.table(data.frame(
      ph_segment_id = atl08_h5[[paste0(i, "/signal_photons/ph_segment_id")]][],
      classed_pc_indx = atl08_h5[[paste0(i, "/signal_photons/classed_pc_indx")]][],
      classed_pc_flag = atl08_h5[[paste0(i, "/signal_photons/classed_pc_flag")]][],
      ph_h = atl08_h5[[paste0(i, "/signal_photons/ph_h")]][],
      d_flag = atl08_h5[[paste0(i, "/signal_photons/d_flag")]][],
      delta_time = atl08_h5[[paste0(i, "/signal_photons/delta_time")]][],
      ph_h = atl08_h5[[paste0(i, "/signal_photons/ph_h")]][]
    ))
    data.table::setindex(dataTableATL03Segs, "segment_id")
    data.table::setindex(dataTableATL08Photons, "ph_segment_id")

    i_s <- i_s + 0.25
    utils::setTxtProgressBar(pb, i_s)

    idx <- data.table::merge.data.table(dataTableATL03Segs, dataTableATL08Photons, by.x = "segment_id", by.y = "ph_segment_id")[, ph_index_beg + classed_pc_indx - 1]

    dataTableATL03Photons[idx, c("ph_segment_id", "classed_pc_indx", "classed_pc_flag", "d_flag", "delta_time", "ph_h") := list(
      dataTableATL08Photons$ph_segment_id,
      dataTableATL08Photons$classed_pc_indx,
      dataTableATL08Photons$classed_pc_flag,
      dataTableATL08Photons$d_flag,
      dataTableATL08Photons$delta_time,
      dataTableATL08Photons$ph_h
    )]

    i_s <- i_s + 0.25
    utils::setTxtProgressBar(pb, i_s)

    dataTableATL03Photons$beam <- i
    dataTableATL03Photons$night_flag <- 0
    dataTableATL03Photons$night_flag[dataTableATL03Photons$solar_elevation > 0] <- 1

    photon_dt <- data.table::rbindlist(list(photon.dt, dataTableATL03Photons), fill = TRUE)

    i_s <- i_s + 0.25
    utils::setTxtProgressBar(pb, i_s)
  }

  photon_dt <- new("icesat2.atl03atl08_dt", dt = photon_dt)

  close(pb)

  return(photon_dt)
}
