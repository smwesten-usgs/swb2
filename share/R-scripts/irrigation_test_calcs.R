library(ncdf4)
library(lubridate)
library(zoo)
library(Evapotranspiration)

climate_dir <- "/Users/smwesten/SMWData/COMMON_CLIMATE/Daymet_V3_2016"

mydates <- seq(mdy("01-01-2010"),mdy("12-31-2011"),by="days")

prcp <- "daymet_v3_prcp_2010_na.nc4"
tmax <- "daymet_v3_tmax_2010_na.nc4"
tmin <- "daymet_v3_tmin_2010_na.nc4"

prcp_2010 <- nc_open( paste( climate_dir, prcp, sep="/" ) )
tmin_2010 <- nc_open( paste( climate_dir, tmin, sep="/" ) )
tmax_2010 <- nc_open( paste( climate_dir, tmax, sep="/" ) )

prcp <- "daymet_v3_prcp_2011_na.nc4"
tmax <- "daymet_v3_tmax_2011_na.nc4"
tmin <- "daymet_v3_tmin_2011_na.nc4"

prcp_2011 <- nc_open( paste( climate_dir, prcp, sep="/" ) )
tmin_2011 <- nc_open( paste( climate_dir, tmin, sep="/" ) )
tmax_2011 <- nc_open( paste( climate_dir, tmax, sep="/" ) )

prcp_x <- ncvar_get( prcp_2011, varid="x" )
prcp_y <- ncvar_get( prcp_2011, varid="y" )

nx <- length( prcp_x )
ny <- length( prcp_y )

target_x <- 794192
target_y <- 226712

swb1_dir <- "/Users/smwesten/SMWData/SWB2_course_slides/swb_examples/irrigation_simple/swb"
swb2_dir <- "/Users/smwesten/SMWData/SWB2_course_slides/swb_examples/irrigation_simple/swb2"

swb1_filename <- "/Users/smwesten/SMWData/SWB2_course_slides/swb_examples/irrigation_simple/swb/SWB_variable_values__col_1__row_1.csv"

swb2_filename <- "/Users/smwesten/SMWData/SWB2_course_slides/swb_examples/irrigation_simple/swb2/SWB2_variable_values__col_1__row_1.csv"

swb1 <- read.csv( swb1_filename )
swb2 <- read.csv( swb2_filename )

swb1$date <- lubridate::mdy( paste( swb1$month, swb1$day, swb1$year,sep="-") )
swb2$date <- lubridate::mdy( paste( swb2$month, swb2$day, swb2$year,sep="-") )

swb1names <- names(swb1)
swb2names <- names(swb2)

swb1names[swb1names=="recharge"] <- "potential_recharge"
swb1names[swb1names=="actual_et"] <- "actual_ET"
swb1names[swb1names=="ref_et0"] <- "reference_ET0"
swb1names[swb1names=="kcb"] <- "crop_coefficient_kcb"

swb1 <- subset( swb1, swb1$year==2011 )
swb2 <- subset( swb2, swb2$year==2011 )

swb1$net_available_water <- swb1$net_rainfall + swb1$snowmelt + swb1$irrigation + swb1$runon
                        -swb1$runoff
#                         - swb1$outflow - swb1$flowout

latitude <- 43.5*pi/180

colfunc <- approxfun( x=prcp_x, y=seq(1, nx) )
rowfunc <- approxfun( x=prcp_y, y=seq(1, ny) )

target_col <- trunc( colfunc( target_x ) )
target_row <- trunc( rowfunc( target_y ) )

startvar <- c(target_col, target_row, 1)
lengthvar <- c(1, 1, -1)

prcpvals <- c(ncvar_get( prcp_2010, var="prcp", start=startvar, count=lengthvar),
              ncvar_get( prcp_2011, var="prcp", start=startvar, count=lengthvar) ) / 25.4
tminvals <- c(ncvar_get( tmin_2010, var="tmin", start=startvar, count=lengthvar),
              ncvar_get( tmin_2011, var="tmin", start=startvar, count=lengthvar) ) * 9. / 5. + 32.
tmaxvals <- c(ncvar_get( tmax_2010, var="tmax", start=startvar, count=lengthvar),
              ncvar_get( tmax_2011, var="tmax", start=startvar, count=lengthvar) ) * 9. / 5. + 32.

precip_5_day <- numeric(5)
PNTR <- 1

PRECIP_5_DAY <- rep(0.0, 5)

makeplot <- function(x1, y1, x2, y2, title, xlim, y1_label="R script",
                     y2_label="SWB 1.0", xlab="", units="inches",
                     ymin_diff=-3, ymax_diff=3) {
  par(mfcol=c(2,1))

  plot( x1, y1, main=title, type="l", col="blue", xlim=xlim, xlab=xlab, ylab=units )
  points( x2, y2, type="p", col="green")
  add_legend( y1_label, y2_label )

  plot( x1, (y2 - y1 ), type="p", col="black",
        main=paste("Residuals plot:",y2_label,"minus",y1_label,title),
        xlab=xlab, ylab=units, ylim=c(ymin_diff, ymax_diff))
  abline( h=0, col="red")

}


partition_precipitation <- function( precipitation, is_freezing ) {

  rainfall <- 0.0
  snowfall <- 0.0

  ifelse (is_freezing,
          snowfall <- precipitation,
          rainfall <- precipitation )

  return( list(snowfall=snowfall, rainfall=rainfall) )

}

freezing_conditions <- function( tmin, tmax, tmean ) {

  testval <- ifelse (tmean - ( tmax - tmin ) / 3.0 <= 32.0,
          TRUE,
          FALSE )

  return( testval )

}

update_s_max <- function( P, adj_curve_num ) {

  SMax <- 1.33 * ( ( 1000. / adj_curve_num - 10.0 ) ** 1.15 )

  return( SMax )
}

calc_runoff <- function (date,  P, SMax ) {

  if ( P > 0.05 * SMax ) {
    outflow <- ( P - 0.05 * SMax )**2  / (P + 0.95 * SMax)
  } else {
    outflow <- 0.0
  }

  outflow <- min( outflow, P )

  return( outflow )

}


update_snowcover <- function( snowfall, snowcover, tmin, tmax, tmean ) {

  MELT_INDEX <- 1.5   # 1.5 mm/degC

  snowcover <- snowcover + snowfall

  potential_melt <- ifelse ( tmean > 32.0,
                             MELT_INDEX * ( tmax - 32.0 ) * 5/9 / 25.4,
                             0.0 )

  snowmelt <- ifelse( potential_melt > 0.0,
                      ifelse( snowcover > potential_melt,
                              potential_melt,
                              snowcover ),
                      0.0 )

  snowcover <- snowcover - snowmelt

  return ( list( snowmelt=snowmelt, snowcover=snowcover ) )

}

update_cfgi <- function( mean_temperature_C, snow_cover, CFGI_prev ) {

  A <- 0.97

  # assuming snow depth is 10 times the water content of the snow in inches
  SnowDepthCM <- snow_cover * 10.0 * 2.54

  if ( mean_temperature_C > 0.0 ) {
    CFGI <- max(A * CFGI_prev - ( mean_temperature_C * exp (-0.4 * 0.5 * SnowDepthCM )), 0.0 )
  } else { # temperature is below freezing

    CFGI <- max(A * CFGI_prev - ( mean_temperature_C * exp (-0.4 * 0.08 * SnowDepthCM)), 0.0 )
  }

  return(CFGI)

}


prob_runoff_enhancement <- function(CFGI, LL_CFGI, UL_CFGI) {

  pRE <- ifelse( CFGI <= LL_CFGI,
            0.0,
            ifelse( CFGI >= UL_CFGI,
              1.0,
              ( CFGI - LL_CFGI ) / ( UL_CFGI - LL_CFGI ) ) )

  return( pRE )

}

update_curve_number <- function( base_curve_number, CFGI, growing_season, precip_5_day ) {

  LL_CFGI <- 55.
  UL_CFGI <- 83.

  # use probability of runoff enhancement to calculate a weighted
  # average of curve number under Type II vs Type III antecedent
  # runoff conditions

  if ( CFGI > LL_CFGI ) {

    Pf <- prob_runoff_enhancement(CFGI, LL_CFGI, UL_CFGI)

    adj_curve_number <- base_curve_number * (1 - Pf) + ( base_curve_number / (0.427 + 0.00573 *  base_curve_number ) * Pf )

  } else {

    if ( growing_season ) {   # growing season AMC conditions

      WET <- 2.1
      DRY <- 1.4

    } else {                  # dormant AMC conditions

      WET <- 1.1
      DRY <- 0.5

    }

    if ( precip_5_day < DRY ) {                                         # AMC I

      # The following comes from page 192, eq. 3.145 of "SCS Curve Number Methodology"
      adj_curve_number <- base_curve_number / (2.281 - 0.01281 * base_curve_number )

    } else if ( (precip_5_day >= DRY ) & (precip_5_day < WET ) ) {    # AMC II

      adj_curve_number <- base_curve_number

    } else {                                                           # AMC III

      adj_curve_number <- base_curve_number / (0.427 + 0.00573 * base_curve_number )
    }
    # ensure that whatever modification have been made to the curve number
    # remain within reasonable bounds
    adj_curve_number = min( adj_curve_number, 100.0 )
    adj_curve_number = max( adj_curve_number, 30.0 )

  }

  return( adj_curve_number )

}

solar_declination <- function(DayOfYear, NumDaysInYear) {

  Delta <- 0.409 * sin ( 2.0 * pi * DayOfYear / NumDaysInYear - 1.39)

  return( Delta )
}

ET_ext <- function (data, constants, ts = "daily", ...)
{
  if (is.null(data$Tmax) | is.null(data$Tmin)) {
    stop("Required data missing for 'Tmax.daily' and 'Tmin.daily', or 'Temp.subdaily'")
  }
  Ta <- (data$Tmax + data$Tmin)/2
  #P <- 101.3 * ((293 - 0.0065 * constants$Elev)/293)^5.26

  # similar to eqn 11 and 12 in Allen (1998)? sat vapor pressure?
#  delta <- 4098 * (0.6108 * exp((17.27 * Ta)/(Ta + 237.3)))/((Ta + 237.3)^2)

#  gamma <- 0.00163 * P/constants$lambda

  # earth-sun distance ratio
  d_r2 <- 1 + 0.033 * cos(2 * pi/365 * data$J)

  # same as SWB 'Delta'
  delta2 <- 0.409 * sin(2 * pi/365 * data$J - 1.39)
  w_s <- acos(-tan(constants$lat_rad) * tan(delta2))
  N <- 24/pi * w_s
  R_a <- (1440/pi) * d_r2 * constants$Gsc * (w_s * sin(constants$lat_rad) *
                                               sin(delta2) + cos(constants$lat_rad) * cos(delta2) *
                                               sin(w_s))

  C_HS <- 0.00185 * (data$Tmax - data$Tmin)^2 - 0.0433 * (data$Tmax -
                                                            data$Tmin) + 0.4023
  ET_HS.Daily <- 0.0135 * C_HS * R_a/constants$lambda * (data$Tmax -
                                                           data$Tmin)^0.5 * (Ta + 17.8)

  # the preceding two equations come from a paper by Samani: C_HS is equation 3 in his paper;
  # ET_HS.Daily is eqn 2 in that paper
  # source: http://www.zohrabsamani.com/research_material/files/Hargreaves-samani.pdf

    ET.Daily <- ET_HS.Daily
  ET.Monthly <- aggregate(ET.Daily, as.yearmon(data$Date.daily,
                                               "%m/%y"), FUN = sum)
  ET.Annual <- aggregate(ET.Daily, floor(as.numeric(as.yearmon(data$Date.daily,
                                                               "%m/%y"))), FUN = sum)
  ET.MonthlyAve <- ET.AnnualAve <- NULL
  for (mon in min(as.POSIXlt(data$Date.daily)$mon):max(as.POSIXlt(data$Date.daily)$mon)) {
    i = mon - min(as.POSIXlt(data$Date.daily)$mon) + 1
    ET.MonthlyAve[i] <- mean(ET.Daily[as.POSIXlt(data$Date.daily)$mon ==
                                        mon])
  }
  for (year in min(as.POSIXlt(data$Date.daily)$year):max(as.POSIXlt(data$Date.daily)$year)) {
    i = year - min(as.POSIXlt(data$Date.daily)$year) + 1
    ET.AnnualAve[i] <- mean(ET.Daily[as.POSIXlt(data$Date.daily)$year ==
                                       year])
  }
  ET_formulation <- "Hargreaves-Samani"
  ET_type <- "Reference Crop ET"
  message(ET_formulation, " ", ET_type)
  message("Evaporative surface: reference crop")
  results <- list(ET.Daily = ET.Daily, ET.Monthly = ET.Monthly,
                  ET.Annual = ET.Annual, ET.MonthlyAve = ET.MonthlyAve,
                  ET.AnnualAve = ET.AnnualAve, ET_formulation = ET_formulation,
                  ET_type = ET_type,
                  delta2=delta2, d_r2=d_r2, w_s=w_s, N=N, R_a=R_a,C_HS=C_HS)
  if (ts == "daily") {
    res_ts <- ET.Daily
  }
  else if (ts == "monthly") {
    res_ts <- ET.Monthly
  }
  else if (ts == "annual") {
    res_ts <- ET.Annual
  }
  message("Timestep: ", ts)
  message("Units: mm")
  message("Time duration: ", time(res_ts[1]), " to ", time(res_ts[length(res_ts)]))
  if (NA %in% res_ts) {
    message(length(res_ts), " ET estimates obtained; ", length(which(is.na(res_ts))),
            " NA output entries due to missing data")
    message("Basic stats (NA excluded)")
    message("Mean: ", round(mean(res_ts, na.rm = T), digits = 2))
    message("Max: ", round(max(res_ts, na.rm = T), digits = 2))
    message("Min: ", round(min(res_ts, na.rm = T), digits = 2))
  }
  else {
    message(length(res_ts), " ET estimates obtained")
    message("Basic stats")
    message("Mean: ", round(mean(res_ts), digits = 2))
    message("Max: ", round(max(res_ts), digits = 2))
    message("Min: ", round(min(res_ts), digits = 2))
  }

  return(results)

}

rel_earth_sun_dist <- function(DayOfYear, NumDaysInYear) {

  DsubR <- 1 + 0.033 * cos ( 2 * pi * DayOfYear / NumDaysInYear )

  return( DsubR )

}

sunset_angle <- function( latitude, Delta ) {

  Omega_s <- acos( - tan(latitude) * tan(Delta) )

  return( Omega_s )
}

extraterrestrial_radiation_Ra <- function(latitude,Delta,Omega_s,DsubR) {

  Gsc <- 0.0820  # MJ / m**2 / min

  PartA <- Omega_s * sin(latitude) * sin(Delta)
  PartB <- cos(latitude) * cos(Delta) * sin(Omega_s)

  Ra <- 24 * 60 * Gsc * DsubR * (PartA + PartB) / pi

  return( Ra )

}

equivalent_evaporation <- function(R) {

 R_equiv <- R * 0.408

 return( R_equiv )

}


FtoK <- function( temp_F ) {

  temp_K <- (temp_F - 32.0 ) * 5/9 + 273.15

  return( temp_K )

}

FtoC <- function( temp_F ) {

  temp_C <- (temp_F - 32.0 ) * 5/9

  return( temp_C )

}

solar_radiation_Hargreaves_Rs <- function(Ra, TMIN, TMAX) {

  KRs <- 0.17

  Rs <- KRs * sqrt( FtoK( TMAX ) - FtoK( TMIN) ) * Ra

  return( Rs )

}

ET0_hargreaves <- function( TMIN, TMAX, Ra ) {

  TMEAN <- ( TMAX + TMIN ) / 2
  TDELTA <- FtoK(TMAX) - FtoK(TMIN)

  ET_0 <- 0.0023 * equivalent_evaporation( Ra ) *( FtoC(TMEAN) +17.8 ) * (TDELTA**0.5) / 25.4

  ET_0[ET_0 < 0.] <- 0.0

  return(ET_0)
}


CalcEvaporationReductionCoefficient <- function(TEW, REW, Deficit) {

  Kr <- ifelse (( Deficit > REW & Deficit < TEW),
    (TEW - Deficit) / (TEW - REW),
      ifelse( Deficit <= REW,
              1,
              0 ) )

  return( Kr )

}


CalcFractionExposedAndWettedSoil <- function( Kcb_min, Kcb_max, plant_height, Kcb ) {

  Numerator <-  Kcb - Kcb_min
  Denominator <- rep(Kcb_max - Kcb_min,length(Kcb))
  Exponent <- 1.0 + 0.5 * plant_height * 0.3048

  r_fc <- ifelse (Denominator > 0.0,
    (Numerator / Denominator) ** Exponent,
    1.0 )

  r_few <- 1.0 - r_fc

  r_few <- pmax(0.0, r_few)
  r_few <- pmin(1.0, r_few)

  return(r_few)

}


CalcSurfaceEvaporationCoefficient <- function(Kcb_max, Kcb, Kr, r_few ) {

  Ke_interim <- Kr * ( Kcb_max - Kcb )

  Ke <- ifelse( Ke_interim <= r_few * Kcb_max,
                Ke_interim,
                r_few * Kcb_max )

  return( Ke )

}



CalcEffectiveRootDepth <- function(Kcb_max, Kcb_ini, L_plant, L_dev, Zr_max, L ) {

  # 0.328 feet equals 0.1 meters, which is seems to be the standard
  # initial rooting depth in the FAO-56 methodology
  Zr_min <- 0.328

  Zr_i <- ifelse( L < L_plant,
                  Zr_min,
                  Zr_min + (Zr_max - Zr_min) * ( L - L_plant) / ( as.numeric(L_dev- L_plant) ) )

  Zr_i <- ifelse( Zr_i > Zr_max, Zr_max, Zr_i )

  if (Kcb_max - Kcb_ini < 0.1) {
    # this is needed because for areas like forests, where the
    # Kcb_ini and Kcb_mid are nearly the same, we assume that root depths are
    # constant
    Zr_i <- rep(Zr_max, length(L) )

  }

  return(Zr_i)

}


CalcTotalAvailableWater <- function( rooting_depth, soil_awc_in_ft, p ) {

  TotalAvailableWater = rooting_depth * soil_awc_in_ft
  ReadilyAvailableWater = TotalAvailableWater * p

  return( list(TAW=TotalAvailableWater,RAW=ReadilyAvailableWater) )

}



CalcWaterStressCoefficient <- function(TAW, RAW, Deficit) {

  Ks <- ifelse (( Deficit > RAW & Deficit < TAW),
                (TAW - Deficit) / (TAW - RAW),
                ifelse( Deficit <= RAW,
                        1,
                        0 ) )

  return( Ks )

}

add_legend <- function( y1_label, y2_label) {

  legend("topright", c(y1_label, y2_label),
         lty=c(1,NA), pch=c(NA,21),
         col=c("blue","green"),
         text.width=70)

}



Delta <- solar_declination( seq(1,365), 365 )
DsubR <- rel_earth_sun_dist( seq(1,365), 365 )
Omega_s <- sunset_angle( latitude, Delta )
Ra <- extraterrestrial_radiation_Ra(latitude, Delta, Omega_s, DsubR)
ReferenceET0 <- ET0_hargreaves( tminvals, tmaxvals, Ra )

Date.daily <- mydates

mydata <- list(Date.daily=Date.daily, Tmin=zoo(FtoC(tminvals), mydates), Tmax=zoo(FtoC(tmaxvals), mydates),
               J=c(seq(1,365),seq(1,365) ) )

etvals <- ET.HargreavesSamani(data=mydata, constants=list(Elev=2000.,lambda=2.45, lat_rad=43.5*pi/180, Gsc=0.0820) )


#--------------------------

Smax <- 2.6
Sprev <- Smax
CFGI_prev <- 100.
TEW <- 0.669
REW <- 0.295
AWC <- 1.3   # inches per foot
MAX_INFIL_RATE <- 0.6

Kcb_min <- 0.2
Kcb_late <- 0.6
Kcb_max <- 1.2
plant_height <- 4.0
Zr_max <- 2.0

L_plant <- mdy("05-01-2010")
L_init <- L_plant + 20
L_dev <- L_init + 50
L_mid <- L_dev + 40
L_late <- L_mid + 30

# Kcb curve definition
Kcb_vals <- c(Kcb_min, Kcb_min, Kcb_min, Kcb_max, Kcb_max, Kcb_late, Kcb_min, Kcb_min )
Kcb_vals <- c(Kcb_vals, Kcb_vals)
Kcb_dates_2010 <- c(mdy("01-01-2010"), L_plant, L_init, L_dev, L_mid, L_late, L_late+0.001, mdy("12-31-2010"))

L_plant <- mdy("05-01-2011")
L_init <- L_plant + 20
L_dev <- L_init + 50
L_mid <- L_dev + 40
L_late <- L_mid + 30

Kcb_dates_2011 <- c(mdy("01-01-2011"), L_plant, L_init, L_dev, L_mid, L_late, L_late+0.001, mdy("12-31-2011"))

Kcb_dates <- c( Kcb_dates_2010, Kcb_dates_2011 )

Kcb_func <- approxfun( Kcb_dates, Kcb_vals )

ndays <- 365*2

BareSoilEvap <- numeric(ndays)
CropETc <- numeric(ndays)
r_few <- numeric(ndays)
Kr <- numeric(ndays)
Ke <- numeric(ndays)
Zr <- numeric(ndays)
Ks <- numeric(ndays)
Deficit <- numeric(ndays)
Depth_evap <- numeric(ndays)
S <- numeric(ndays)
TAW <- numeric(ndays)
RAW <- numeric(ndays)
is_freezing <- numeric(ndays)
ActualET <- numeric(ndays)
rainfall <- numeric(ndays)
snowfall <- numeric(ndays)
snowcover <- numeric(ndays)
snowmelt <- numeric(ndays)
runoff <- numeric(ndays)
net_infil <- numeric(ndays)
net_available_water <- numeric(ndays)
rejected_infil <- numeric(ndays)
curve_num <- numeric(ndays)
CFGI <- numeric(ndays)
precip_5_day_sum <- numeric(ndays)
precip_5_day_sum__shift_1d <- numeric(ndays)
GROW_SEASON <- numeric(ndays)
S_Max <- numeric(ndays)
p <- 0.55

snowcover_prev <- 2.0
GROW_SEASON <- rep(FALSE,365)
GROW_SEASON[ 133:268 ] <- TRUE
GROW_SEASON <- c(GROW_SEASON,GROW_SEASON)
BASE_CN <- 75.

tmeanvals <- ( tminvals + tmaxvals ) / 2.0

is_freezing <- freezing_conditions( tmax=tmaxvals, tmin=tminvals, tmean=tmeanvals )

for (d in seq(1,365*2)) {

  S[d] <- Sprev

  # partition precip into rain and snow
  prcp_partition <- partition_precipitation( prcpvals[d], is_freezing[d] )

  snowfall[d] <- prcp_partition$snowfall
  rainfall[d] <- prcp_partition$rainfall

  tmean_deg_C <- FtoC( tmeanvals[d] )

  # in SWB 1.0, call to update CFGI is made *BEFORE* the snowcover is updated
  CFGI[d] <- update_cfgi( tmean_deg_C, snowcover_prev, CFGI_prev )
  CFGI_prev <- CFGI[d]

  # update snowcover, snowmelt
  snow_list <- update_snowcover(snowfall=snowfall[d], snowcover=snowcover_prev,
                                tmin=tminvals[d], tmax=tmaxvals[d], tmean=tmeanvals[d] )
  snowmelt[d] <- snow_list$snowmelt
  snowcover[d] <- snow_list$snowcover
  snowcover_prev <- snowcover[d]

  precip_5_day_sum[d] <- sum( PRECIP_5_DAY )

  curve_num[d] <- update_curve_number( base_curve_number=BASE_CN,
                                       CFGI=CFGI[d],
                                       growing_season=GROW_SEASON[d],
                                       precip_5_day=precip_5_day_sum[d] )

  S_Max[d] <- update_s_max( P=(rainfall[d] + snowmelt[d]), adj_curve_num=curve_num[d] )

  runoff[d] <- calc_runoff( date=mydates[d], P=(rainfall[d] + snowmelt[d]),
                            SMax=S_Max[d] )

  net_available_water[d] <- rainfall[d] + snowmelt[d] - runoff[d]

  # now update the antecedant runoff condition, consistent with the order of operations
  # present in the SWB code.
  PNTR <- PNTR + 1
  if ( PNTR > 5 )  { PNTR <- 1 }
  PRECIP_5_DAY[ PNTR ] <- rainfall[d] + snowmelt[d]

  # this kludge is needed because of the order of operations between R script and
  # swb is reversed; this value will be used in the plots
  precip_5_day_sum__shift_1d[d] <- sum(PRECIP_5_DAY)

  # calculate interim soil moisture value
#  S[d] <- Sprev + rainfall[d] + snowmelt[d] - runoff[d]

  # pluck Kcb off of Kcb curve based on current date
  Kcb <- Kcb_func( mydates[d])

  # update rooting depth and storage capacity within current root zone
  Zr[d] <- CalcEffectiveRootDepth(Kcb_max, Kcb_min, L_plant, L_dev, Zr_max, mydates[d] )
  TAW[d] <- Zr[d] * AWC
  RAW[d] <- p * TAW[d]

  # update current depth of evaporation
  Depth_evap[d] <- max( ( Smax - S[d] ), 0.0 )

  # calculate FAO-56 reduction factors
  r_few[d] <- CalcFractionExposedAndWettedSoil( Kcb_min, Kcb_max, plant_height, Kcb )
  Kr[d] <- CalcEvaporationReductionCoefficient(TEW, REW, Depth_evap[d])
  Ke[d] <- CalcSurfaceEvaporationCoefficient(Kcb_max, Kcb, Kr[d], r_few[d] )

  # update soil moisture deficit
  Deficit[d] <- max((TAW[d] - S[d]), 0.0)

  Ks[d] <- CalcWaterStressCoefficient(TAW[d], RAW[d], Deficit[d])

  # now calculate and apply evap and transpiration terms; update soil moisture
  BareSoilEvap[d] <- ReferenceET0[d] * Ke[d]
  CropETc[d] <- ReferenceET0[d] * Kcb * Ks[d]
  ActualET[d] <- CropETc[d] + BareSoilEvap[d]
  S[d] <- Sprev - CropETc[d] - BareSoilEvap[d] + rainfall[d] + snowmelt[d] - runoff[d]

  cat( month(mydates[d]), day(mydates[d]),S[d],Sprev, curve_num[d], S_Max[d], rainfall[d],runoff[d], "\n")

  # only positive when updated soil moisture exceeds maximum
  net_infil[d] <- max((S[d] - Smax),0.0)

  # enforce max net infiltration rate
  if ( net_infil[d] > MAX_INFIL_RATE ) {
    rejected_infil[d] <- net_infil[d]
    net_infil[d] <- MAX_INFIL_RATE
  } else {
    rejected_infil[d] <- 0.0
  }

  # last, update the soil moisture value
  S[d] <- min( Smax, S[d] )
  Sprev <- S[d]

}

makeplot( x1=mydates[366:730], y1=prcpvals[366:730],
          x2=swb1$date, y2=swb1$gross_precip,
          title="Gross precipitation",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

makeplot( x1=mydates[366:730], y1=precip_5_day_sum__shift_1d [366:730],
          x2=swb1$date, y2=(swb1$inflowbuf1+swb1$inflowbuf2+swb1$inflowbuf3+swb1$inflowbuf4+swb1$inflowbuf5 ),
          title="5-day sum precipitation",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

makeplot( x1=mydates[366:730], y1=rainfall[366:730],
          x2=swb1$date, y2=swb1$net_rainfall,
          title="Rainfall",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

makeplot( x1=mydates[366:730], y1=snowmelt[366:730],
          x2=swb1$date, y2=swb1$snowmelt,
          title="Snowmelt",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

makeplot( x1=mydates[366:730], y1=S_Max[366:730],
          x2=swb1$date, y2=swb1$s_max,
          title="S_Max, curve number max soil storage term",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

makeplot( x1=mydates[366:730], y1=net_available_water[366:730],
          x2=swb1$date, y2=swb1$net_available_water,
          title="Net available water, Healy's 'potential recharge'",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

makeplot( x1=mydates[366:730], y1=curve_num[366:730],
          x2=swb1$date, y2=swb1$curve_num_adj,
          title="SCS curve number",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

makeplot( x1=mydates[366:730], y1=snowcover[366:730],
          x2=swb1$date, y2=swb1$snow_cover,
          title="Snow cover",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )


swb1_deficit <- pmax(swb1$total_available_water - swb1$soil_storage, 0.)
makeplot( x1=mydates[366:730], y1=Deficit[366:730],
          x2=swb1$date, y2=,swb1_deficit,
          title="Deficit relative to total available water",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

makeplot( x1=mydates[366:730], y1=TAW[366:730],
          x2=swb1$date, y2=swb1$total_available_water,
          title="Total available water (TAW)",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

makeplot( x1=mydates[366:730], y1=RAW[366:730],
          x2=swb1$date, y2=swb1$readily_available_water,
          title="Readily available water (RAW)",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

makeplot( x1=mydates[366:730], y1=Kr[366:730],
          x2=swb1$date, y2=swb1$evap_reduction_coef_kr,
          title="Evaporation reduction coefficient, Kr",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

makeplot( x1=mydates[366:730], y1=Ke[366:730],
          x2=swb1$date, y2=swb1$surf_evap_coef_ke,
          title="Surface evaporation coefficient, Ke",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

makeplot( x1=mydates[366:730], y1=Zr[366:730],
          x2=swb1$date, y2=swb1$current_rooting_depth,
          title="Effective plant rooting depth",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ),
          ymin_diff=-0.1, ymax_diff=0.1 )

makeplot( x1=mydates[366:730], y1=Ks[366:730],
          x2=swb1$date, y2=swb1$plant_stress_coef_ks,
          title="Plant stress coefficient, Ks",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

makeplot( x1=mydates[366:730], y1=net_infil[366:730],
          x2=swb1$date, y2=swb1$potential_recharge,
          title="Net infiltration (formerly called 'potential recharge')",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ),
          ymin_diff=-0.1, ymax_diff=0.1 )

makeplot( x1=mydates[366:730], y1=S[366:730],
          x2=swb1$date, y2=swb1$soil_storage,
          title="Soil moisture",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

makeplot( x1=mydates[366:730], y1=Kcb_func(mydates[366:730]),
          x2=swb1$date, y2=swb1$kcb,
          title="Crop coefficient, Kcb",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

makeplot( x1=mydates[366:730], y1=CropETc[366:730],
          x2=swb1$date, y2=swb1$crop_etc,
          title="Crop ETc",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

makeplot( x1=mydates[366:730], y1=BareSoilEvap[366:730],
          x2=swb1$date, y2=swb1$bare_soil_evap,
          title="Bare soil evaporation",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

makeplot( x1=mydates[366:730], y1=ActualET[366:730],
          x2=swb1$date, y2=swb1$ref_etc0_adj,
          title="Actual ET",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

makeplot( x1=mydates[366:730], y1=S[366:730],
          x2=swb1$date, y2=swb1$soil_storage,
          title="Soil moisture",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

makeplot( x1=mydates[366:730], y1=runoff[366:730],
          x2=swb1$date, y2=swb1$runoff,
          title="Runoff",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

makeplot( x1=mydates[366:730], y1=CFGI[366:730],
          x2=swb1$date, y2=swb1$cfgi,
          title="Continuous frozen ground index (CFGI)",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )


makeplot( x1=mydates[366:730], y1=tmaxvals[366:730],
          x2=swb1$date, y2=swb1$tmax,
          title="Maximum air temperature",
          units="degrees Fahrenheit",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

makeplot( x1=mydates[366:730], y1=tminvals[366:730],
          x2=swb1$date, y2=swb1$tmin,
          title="Minimum air temperature",
          units="degrees Fahrenheit",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

makeplot( x1=mydates[366:730], y1=tmeanvals[366:730],
          x2=swb1$date, y2=swb1$tmean,
          title="Mean air temperature",
          units="degrees Fahrenheit",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ) )

###

makeplot( x1=mydates[366:730], y1=rainfall[366:730],
          x2=swb2$date, y2=swb2$rainfall,
          title="Rainfall",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ),
          y2_label="SWB 2.0")

makeplot( x1=mydates[366:730], y1=snowmelt[366:730],
          x2=swb2$date, y2=swb2$snowmelt,
          title="Snowmelt",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ),
          y2_label="SWB 2.0")

makeplot( x1=mydates[366:730], y1=CFGI[366:730],
          x2=swb2$date, y2=swb2$cfgi,
          title="Continuous frozen ground index (CFGI)",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ),
          y2_label="SWB 2.0")

makeplot( x1=mydates[366:730], y1=runoff[366:730],
          x2=swb2$date, y2=swb2$runoff,
          title="Runoff",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ),
          y2_label="SWB 2.0")

makeplot( x1=mydates[366:730], y1=S[366:730],
          x2=swb2$date, y2=swb2$soil_storage,
          title="Soil moisture",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ),
          y2_label="SWB 2.0")


makeplot( x1=mydates[366:730], y1=net_infil[366:730],
          x2=swb2$date, y2=swb2$potential_recharge,
          title="Net infiltration (formerly called 'potential recharge')",
          xlim=c(ymd("2011-01-01"),ymd("2011-12-31") ),
          ymin_diff=-0.1, ymax_diff=0.1,
          y2_label="SWB 2.0")



# reset for a single plot per page
par( mfrow=c(1,1) )

plot(mydates[366:730], etvals$ET.Daily[366:730]/25.4, cex=0.7, col="grey", type="l",
     xlim=c(ymd("2011-01-01"),ymd("2011-12-31")))
lines(mydates[366:730],ReferenceET0[366:730], type="l", col="blue")
points( swb1$date, swb1$ref_et0, col="green", pch=21 )
points( swb1$date, swb1$ref_etc0_adj, col="purple", pch=22, cex=.5 )
legend("topright", c("R package'ET'","R script","SWB ref_ET0","SWB ref_ET0_adj"),
       lty=c(1,1,NA,NA), pch=c(NA,NA,21,22),
       col=c("grey","blue","green","purple"))

plot( swb1$date, swb1$irrigation, col="green", type="p",
      xlim=c(ymd("2011-01-01"),ymd("2011-12-31")))

plot( mydates[366:730], r_few[366:730], main="Fraction exposed and wetted surface", type="l", col="blue",
      xlim=c(ymd("2011-01-01"),ymd("2011-12-31")))

plot( mydates[366:730], Depth_evap[366:730], main="Depth of evaporation", type="l", col="blue",
      xlim=c(ymd("2011-01-01"),ymd("2011-12-31")))

