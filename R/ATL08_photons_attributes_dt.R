#ATL08_photon.var.map
ATL08_photon.var.map=list()
ATL08_photon.var.map[["ph_segment_id"]]="ph_segment_id"
ATL08_photon.var.map[["classed_pc_indx"]]="classed_pc_indx"
ATL08_photon.var.map[["classed_pc_flag"]]="classed_pc_flag"
ATL08_photon.var.map[["ph_h"]]="ph_h"
ATL08_photon.var.map[["d_flag"]]="d_flag"
ATL08_photon.var.map[["delta_time"]]="delta_time"
#'
#'ATL08 computed photons attributes
#'
#'@description This function extracts computed photons attributes from ICESat-2 ATL08 data
#'
#'@usage ATL08_photons_attributes_dt(atl08_h5, beam)
#'
#'@param atl08_h5 A ICESat-2 ATL08 object (output of [ATL08_read()] function).
#'An S4 object of class [rICESat2Veg::icesat2.atl08_dt].
#'@param beam Character vector indicating beams to process (e.g. "gt1l", "gt1r", "gt2l", "gt2r", "gt3l", "gt3r")
#'
#'@return Returns an S4 object of class [data.table::data.table]
#'containing the ATL08 computed photons attributes.
#'
#'@details These are the photons attributes extracted by default:
#'\itemize{
#'\item \emph{ph_segment_id} Georeferenced	bin	number (20-m) associated	with	each photon
#'\item \emph{classed_pc_indx} Indices of photons	tracking back	to ATL03	that	surface finding	software	identified and	used	within	the
#'creation of the	data products.
#'\item \emph{classed_pc_flag} The L2B algorithm is run if this flag is set to 1 indicating data have sufficient waveform fidelity for L2B to run
#'\item \emph{ph_h} Height of photon above interpolated ground surface
#'#'\item \emph{d_flag} Flag indicating	whether DRAGANN	labeled	the photon as noise or signal
#'\item \emph{delta_time} Mid-segment	GPS	time	in seconds past	an epoch. The epoch is provided	in the metadata	at the file	level
#'}
#'
#'@seealso \url{https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/ICESat2_ATL08_ATBD_r006.pdf}
#'
#'
#'@examples
#'
#'# Specifying the path to ATL08 file (zip file)
#'outdir = tempdir()
#'atl08_zip <- system.file("extdata",
#'                   "ATL08_20220401221822_01501506_005_01.zip",
#'                   package="rICESat2Veg")
#'
#'# Unzipping ATL08 file
#'atl08_path <- unzip(atl08_zip,exdir = outdir)
#'
#'# Reading ATL08 data (h5 file)
#atl08_h5<-ATL08_read(ATL08_path=atl08_path)
#'
#'# Extracting ATL08 classified photons and heights
#'atl08_photons<-ATL08_photons_attributes_dt(atl08_h5=atl08_h5)
#'head(atl08_photons)
#'
#'close(atl08_h5)
#'@export
ATL08_photons_attributes_dt <- function(atl08_h5,
                       beam = c("gt1l", "gt1r", "gt2l", "gt2r", "gt3l", "gt3r"),
                       photon_attribute=c("ph_segment_id","classed_pc_indx","classed_pc_flag","ph_h", "d_flag", "delta_time")) {

  # Check file input
  if (!class(atl08_h5)=="icesat2.atl08_h5") {
    stop("atl08_h5 must be an object of class 'icesat2.atl08_h5' - output of [ATL08_read()] function ")
  }

  #h5
  atl08_h5v2<-atl08_h5@h5

  # Check beams to select
  groups_id<-hdf5r::list.groups(atl08_h5v2, recursive = F)

  check_beams<-groups_id %in% beam
  beam<-groups_id[check_beams]

  photon.dt <- data.table::data.table()

  pb <- utils::txtProgressBar(min = 0, max = length(beam), style = 3)

  i_s = 0

  if (length(photon_attribute) > 1) {

    for (i in beam) {
      i_s = i_s + 1

      atl08_h5v2_i<-atl08_h5v2[[paste0(i,"/signal_photons")]]

      m = data.table::data.table()

      for (col in photon_attribute) {
        #print(col)
        metric_address = ATL08_photon.var.map[[col]]

        if (is.null(metric_address)) {
          if (atl08_h5v2_i$exists(col)) {
            metric_address = col
          } else {
            if (i.s == 1) warning(
              sprintf(
                "The column '%s' is not available in the ATL08 product!",
                col
              )
            )
            m[, eval(col) := NA]
            next
          }
        }
        base_addr = gsub("^(.*)/.*", "\\1", metric_address)
        if (atl08_h5v2_i$exists(base_addr) && atl08_h5v2_i$exists(metric_address))

          m[, eval(col) := atl08_h5v2_i[[metric_address]][]]
          m$beam<-i

      }

      photon_dt = data.table::rbindlist(list(photon.dt, m), fill = TRUE)
      utils::setTxtProgressBar(pb, i_s)
    }
  }

  setattr(photon_dt, "class", c("icesat2.atl08_dt", "data.table", "data.frame"))

  close(pb)

  return(photon_dt)
}

