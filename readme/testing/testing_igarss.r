atl03_granules <- ICESat2_finder(
    short_name = "ATL03",
    ul_lat = 42.0,
    ul_lon = -100,
    lr_lat = 41.0,
    lr_lon = -99.0,
    version = "006",
    daterange = c("2019-08-01", "2019-08-22"),
    cloud_hosted = TRUE
)

atl08_granules <- ICESat2_finder(
    short_name = "ATL08",
    ul_lat = 42.0,
    ul_lon = -100,
    lr_lat = 41.0,
    lr_lon = -99.0,
    version = "006",
    daterange = c("2019-08-01", "2019-08-22"),
    cloud_hosted = TRUE
)

output_dir <- tempdir()
setwd(output_dir)

ICESat2data_download(atl03_granules[1], ".")
ICESat2data_download(atl08_granules[1], ".")

atl03_path <- list.files(pattern = "^ATL03")[1]
atl08_path <- list.files(pattern = "^ATL08")[1]

atl03_h5 <- ATL03_read(atl03_path)
atl08_h5 <- ATL08_read(atl08_path)

atl03_atl08_dt <- ATL03_ATL08_photons_attributes_dt_join(atl03_h5, atl08_h5)
plot(atl03_atl08_dt)