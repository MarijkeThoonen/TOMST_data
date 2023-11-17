library(myClim)
library(dplyr)
library(tidyverse)
library(ggplot2)

df <- mc_read_files("./ruwe_data/data_2023/",
                    dataformat_name = "TOMST",
                    clean = TRUE,
                    silent =T,
                    step = 300)

#inlezen met metadata, files table bevat de paden nr ruwe logger files
#(logger type en locality name)
ft <- read.table("./ruwe_data/files_table_2023.csv", sep=";", header = T)
lt <- read.csv2("./ruwe_data/lt.csv", sep=";", dec = ",")

ft$locality_id <- as.character(ft$locality_id)
lt$locality_id <- as.character(lt$locality_id)

df_1 <- mc_read_data(files_table = ft,
                     localities_table = lt,
                     silent = T)

#Crop the myClim object to defined period
#from start date and time to end date and time
#tijd veld, startuur in veld is moeilijk te bepalen.
#Geplaatst op 13/04/2023 - opgehaald op 16/10/2023
#startdag = dag na plaatsing in veld, eind = dag voor ophaling?

df_veld <- mc_prep_crop(df_1, start = as.POSIXct("2023-04-14", tz="UTC"),
                        end = as.POSIXct("2023-10-15", tz="UTC"))

#Joining in time To join fragmented time-series that are stored in separate files
#from separate downloading visits of the localities, usemc_join().

# one locality with two downloads in time

joined_data <- mc_join(df_veld,
                       comp_sensors = c("94253099", "94253109"))

# één logger is vervangen: 109 vervangen door 99 op 20/7/23
#logger 87 uit veld gehaald op 19 of 20 september en niet vervangen, staalnameplek opgeheven
# dat geeft 46 localities en 47 loggers!
mc_info_count(df_veld)

info_df_veld <- mc_info(df_veld)

write.csv2(info_df_veld, "./afgeleide_data/info_df.csv")


#Calculate the local solar time based on the longitude of the locality
#(the offset in minutes against UTC time)
mc_prep_solar_tz()

#mc_agg() -> aggregate time-series data
#into hourly, daily, weekly, monthly, seasonal, or yearly intervals

# aggregate to daily mean,
#range,
#coverage (95% van de data moet er zijn/mag geen na zijn),
#95 percentile ??
tms.day <- mc_agg(joined_data, fun = c("min", "max", "mean", "range",
                                       "coverage", "percentile"),
                  percentiles = 95, period = "day", min_coverage = 0.95)



# aggregate all time-series, return one value per sensor.
tms.all <- mc_agg(joined_data, fun = c("min", "max","mean", "range",
                                       "coverage", "percentile"),
                  percentiles = 95, period = "all", min_coverage = 0.95)

#PLOTTEN
tms.plot <- mc_filter(df_veld, localities = c("1", "2", "3", "4"))

p <- mc_plot_line(tms.plot, sensors = c("TMS_T2", "TMS_T1","TMS_T3"),
                  filename = "./afgeleide_data/plot_1234.png",
                  start_crop = as.POSIXct("2023-08-01", tz = "UTC"),
                  end_crop = as.POSIXct("2023-08-31", tz = "UTC"))

tms.plot <- mc_filter(df_veld, localities = c("9", "10", "11", "12"))

p <- mc_plot_line(tms.plot, sensors = c("TMS_T2", "TMS_T1","TMS_T3"),
                  filename = "./afgeleide_data/plot_9101112.png",
                  start_crop = as.POSIXct("2023-08-01", tz = "UTC"),
                  end_crop = as.POSIXct("2023-08-31", tz = "UTC"))

p
p <- p+ggplot2::scale_x_datetime(date_breaks = "1 day", date_labels = "%D")
p <- p+ggplot2::xlab("dag")
p <- p+ggplot2::aes(size = sensor_name)
p <- p+ggplot2::scale_size_manual(values = c(1, 1 ,2))
p <- p+ggplot2::guides(size = "none")
p <- p+ggplot2::scale_color_manual(values = c("hotpink", "pink", "darkblue"), name = NULL)
p

#Update height or rename sensors in the myClim object
mc_prep_meta_sensor()
