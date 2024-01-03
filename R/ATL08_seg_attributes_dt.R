# ATL08.var.map
ATL08.var.map <- list()
ATL08.var.map[["h_canopy"]] <- "/land_segments/canopy/h_canopy"
ATL08.var.map[["canopy_rh_conf"]] <- "/land_segments/canopy/canopy_rh_conf"
ATL08.var.map[["h_median_canopy_abs"]] <- "/land_segments/canopy/h_median_canopy_abs"
ATL08.var.map[["h_min_canopy"]] <- "/land_segments/canopy/h_min_canopy"
ATL08.var.map[["h_mean_canopy_abs"]] <- "/land_segments/canopy/h_mean_canopy_abs"
ATL08.var.map[["h_median_canopy"]] <- "/land_segments/canopy/h_median_canopy"
ATL08.var.map[["h_canopy_abs"]] <- "/land_segments/canopy/h_canopy_abs"
ATL08.var.map[["toc_roughness"]] <- "/land_segments/canopy/toc_roughness"
ATL08.var.map[["h_min_canopy_abs"]] <- "/land_segments/canopy/h_min_canopy_abs"
ATL08.var.map[["h_dif_canopy"]] <- "/land_segments/canopy/h_dif_canopy"
ATL08.var.map[["h_canopy_quad"]] <- "/land_segments/canopy/h_canopy_quad"
ATL08.var.map[["h_canopy_20m"]] <- "/land_segments/canopy/h_canopy_20m"
ATL08.var.map[["n_ca_photons"]] <- "/land_segments/canopy/n_ca_photons"
ATL08.var.map[["photon_rate_can"]] <- "/land_segments/canopy/photon_rate_can"
ATL08.var.map[["centroid_height"]] <- "/land_segments/canopy/centroid_height"
ATL08.var.map[["canopy_h_metrics_abs"]] <- "/land_segments/canopy/canopy_h_metrics_abs"
ATL08.var.map[["h_mean_canopy"]] <- "/land_segments/canopy/h_mean_canopy"
ATL08.var.map[["subset_can_flag"]] <- "/land_segments/canopy/subset_can_flag"
ATL08.var.map[["canopy_h_metrics"]] <- "/land_segments/canopy/canopy_h_metrics"
ATL08.var.map[["n_toc_photons"]] <- "/land_segments/canopy/n_toc_photons"
ATL08.var.map[["h_max_canopy_abs"]] <- "/land_segments/canopy/h_max_canopy_abs"
ATL08.var.map[["h_canopy_uncertainty"]] <- "/land_segments/canopy/h_canopy_uncertainty"
ATL08.var.map[["canopy_openness"]] <- "/land_segments/canopy/canopy_openness"
ATL08.var.map[["h_max_canopy"]] <- "/land_segments/canopy/h_max_canopy"
ATL08.var.map[["segment_cover"]] <- "/land_segments/canopy/segment_cover"
ATL08.var.map[["h_te_best_fit"]] <- "/land_segments/terrain/h_te_best_fit"
ATL08.var.map[["h_te_best_fit_20m"]] <- "/land_segments/terrain/h_te_best_fit_20m"
ATL08.var.map[["h_te_interp"]] <- "/land_segments/terrain/h_te_interp"
ATL08.var.map[["h_te_max"]] <- "/land_segments/terrain/h_te_max"
ATL08.var.map[["h_te_mean"]] <- "/land_segments/terrain/h_te_mean"
ATL08.var.map[["h_te_median"]] <- "/land_segments/terrain/h_te_median"
ATL08.var.map[["h_te_mode"]] <- "/land_segments/terrain/h_te_mode"
ATL08.var.map[["h_te_rh25"]] <- "/land_segments/terrain/h_te_rh25"
ATL08.var.map[["h_te_skew"]] <- "/land_segments/terrain/h_te_skew"
ATL08.var.map[["h_te_std"]] <- "/land_segments/terrain/h_te_std"
ATL08.var.map[["h_te_uncertainty"]] <- "/land_segments/terrain/h_te_uncertainty"
ATL08.var.map[["n_te_photons"]] <- "/land_segments/terrain/n_te_photons"
ATL08.var.map[["photon_rate_te"]] <- "/land_segments/terrain/photon_rate_te"
ATL08.var.map[["subset_te_flag"]] <- "/land_segments/terrain/subset_te_flag"
ATL08.var.map[["terrain_slope"]] <- "/land_segments/terrain/terrain_slope"
ATL08.var.map[["night_flag"]] <- "/land_segments/night_flag"

#' ATL08 Terrain and Canopy Attributes
#'
#' @description This function extracts terrain and canopy attributes by segments from ICESat-2 ATL08 data
#'
#' @usage ATL08_seg_attributes_dt(atl08_h5, beam)
#'
#' @param atl08_h5 A ICESat-2 ATL08 object (output of [ATL08_read()] function).
#' An S4 object of class [rICESat2Veg::icesat2.atl08_dt].
#' @param beam Character vector indicating beams to process (e.g. "gt1l", "gt1r", "gt2l", "gt2r", "gt3l", "gt3r")
#' @param attribute A character vector containing the list of terrain and canopy attributes to be extracted.
#' Default is attribute = c("h_canopy","canopy_h_metrics","canopy_openness","h_te_mean","h_te_median","terrain_slope")
#' @return Returns an S4 object of class [rICESat2Veg::icesat2.atl08_dt]
#' containing the ATL08-derived terrain and canopy attributes by segments.
#'
#' @details ATL08 canopy attributes:
#' \itemize{
#' \item \emph{"h_canopy"}
#' \item \emph{"canopy_rh_conf"}
#' \item \emph{"h_median_canopy_abs"}
#' \item \emph{"h_min_canopy"}
#' \item \emph{"h_mean_canopy_abs"}
#' \item \emph{"h_median_canopy"}
#' \item \emph{"h_canopy_abs"}
#' \item \emph{"toc_roughness"}
#' \item \emph{"h_min_canopy_abs"}
#' \item \emph{"h_dif_canopy"}
#' \item \emph{"h_canopy_quad"}
#' \item \emph{"h_canopy_20m"}
#' \item \emph{"n_ca_photons"}
#' \item \emph{"photon_rate_can"}
#' \item \emph{"centroid_height"}
#' \item \emph{"canopy_h_metrics_abs"}
#' \item \emph{"h_mean_canopy"}
#' \item \emph{"subset_can_flag"}
#' \item \emph{"canopy_h_metrics"}
#' \item \emph{"n_toc_photons"}
#' \item \emph{"h_max_canopy_abs"}
#' \item \emph{"h_canopy_uncertainty"}
#' \item \emph{"canopy_openness"}
#' \item \emph{"h_max_canopy"}
#' \item \emph{"segment_cover"}
#' }
#' @details ATL08 terrain attributes:
#' \itemize{
#' \item \emph{"h_te_best_fit"}
#' \item \emph{"h_te_best_fit_20m"}
#' \item \emph{"h_te_interp"}
#' \item \emph{"h_te_max"}
#' \item \emph{"h_te_mean"}
#' \item \emph{"h_te_median"}
#' \item \emph{"h_te_mode"}
#' \item \emph{"h_te_rh25"}
#' \item \emph{"h_te_skew"}
#' \item \emph{"h_te_std"}
#' \item \emph{"h_te_uncertainty"}
#' \item \emph{"n_te_photons"}
#' \item \emph{"photon_rate_te"}
#' \item \emph{"subset_te_flag"}
#' \item \emph{"terrain_slope"}
#' }
#'
#' @seealso \url{https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/ICESat2_ATL08_ATBD_r006.pdf}
#'
#'
#' @examples
#'
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
#' # Extracting ATL08-derived terrain and canopy attributes
#' atl08_seg_att_dt <- ATL08_seg_attributes_dt(atl08_h5 = atl08_h5)
#' head(atl08_seg_att_dt)
#'
#' close(atl08_h5)
#' @export
ATL08_seg_attributes_dt <- function(atl08_h5,
                                    beam = c("gt1l", "gt1r", "gt2l", "gt2r", "gt3l", "gt3r"),
                                    attribute = c(
                                      "h_canopy",
                                      "canopy_openness",
                                      "h_te_mean",
                                      "terrain_slope"
                                    )) {
  # Check file input
  if (!class(atl08_h5) == "icesat2.atl08_h5") {
    stop("atl08_h5 must be an object of class 'icesat2.atl08_h5' - output of [ATL08_read()] function ")
  }


  # Check beams to select
  groups <- atl08_h5@h5$ls()$name
  groups_id <- grep("gt[1-3][lr]", groups, value = TRUE)

  check_beams <- groups_id %in% beam
  beam <- groups_id[check_beams]

  attribute.dt <- data.table::data.table()

  pb <- utils::txtProgressBar(min = 0, max = length(beam), style = 3)

  i_s <- 0

  if (length(attribute) > 0) {
    for (i in beam) {
      i_s <- i_s + 1
      utils::setTxtProgressBar(pb, i_s)
      lat_i <- atl08_h5[[paste0(i, "/land_segments/latitude")]][]
      lon_i <- atl08_h5[[paste0(i, "/land_segments/longitude")]][]

      m <- data.table::data.table(latitude = lat_i, longitude = lon_i, beam = i)

      for (col in attribute) {
        # print(col)

        metric_address <- ATL08.var.map[[col]]
        base_addr <- gsub("^(.*)/.*", "\\1", paste0(i, metric_address))
        full_addr <- paste0(i, metric_address)


        if (is.null(metric_address)) {
          if (atl08_h5@h5$exists(full_addr)) {
            metric_address <- col
          } else {
            if (i.s == 1) {
              warning(
                sprintf(
                  "The column '%s' is not available in the ATL08 product!",
                  col
                )
              )
            }
            m[, eval(col) := NA]
            next
          }
        }

        # base_addr <- gsub("^(.*)/.*", "\\1", metric_address)
        # atl08_h5$exists(base_addr) &&
        if (atl08_h5@h5$exists(full_addr)) {
          if (col %in% c("h_canopy_20m", "canopy_h_metrics_abs", "subset_can_flag", "canopy_h_metrics")) {
            m <- cbind(m, t(atl08_h5[[full_addr]][, ]))

            if (col == "h_canopy_20m") {
              colnames(m)[(ncol(m) - 4):ncol(m)] <- paste0("h_canopy_20m_geo_", 1:5)
            }
            if (col == "canopy_h_metrics_abs") {
              colnames(m)[(ncol(m) - 17):ncol(m)] <- paste0("AH", seq(10, 95, 5))
            }
            if (col == "subset_can_flag") {
              colnames(m)[(ncol(m) - 4):ncol(m)] <- paste0("subset_can_flag_geo_", 1:5)
            }
            if (col == "canopy_h_metrics") {
              colnames(m)[(ncol(m) - 17):ncol(m)] <- paste0("RH", seq(10, 95, 5))
            }
          } else {
            m[, eval(col) := atl08_h5[[full_addr]][]]
          }
        }
      }
      attribute.dt <- data.table::rbindlist(list(attribute.dt, m), fill = TRUE)
    }
  }

  close(pb)

  setattr(attribute.dt, "class", c("icesat2.atl08_dt", "data.table", "data.frame"))
  return(attribute.dt)
}
