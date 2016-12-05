library(ncdf4)
library(lubridate)
library(zoo)

climate_dir <- "/Users/smwesten/SMWData/COMMON_CLIMATE/Daymet_V3_2016"

prcp <- "daymet_v3_prcp_2012_na.nc4"
tmax <- "daymet_v3_tmax_2012_na.nc4"
tmin <- "daymet_v3_tmin_2012_na.nc4"

mydates <- seq(mdy("01-01-2012"),mdy("12-30-2012"),by="days")

prcp_nc <- nc_open( paste( climate_dir, prcp, sep="/" ) )
tmin_nc <- nc_open( paste( climate_dir, tmin, sep="/" ) )
tmax_nc <- nc_open( paste( climate_dir, tmax, sep="/" ) )

prcp_x <- ncvar_get( prcp_nc, varid="x" )
prcp_y <- ncvar_get( prcp_nc, varid="y" )

nx <- length( prcp_x )
ny <- length( prcp_y )

target_x <- 794192
target_y <- 226712

latitude <- 43.5*pi/180

colfunc <- approxfun( x=prcp_x, y=seq(1, nx) )
rowfunc <- approxfun( x=prcp_y, y=seq(1, ny) )

target_col <- trunc( colfunc( target_x ) )
target_row <- trunc( rowfunc( target_y ) )

startvar <- c(target_col, target_row, 1)
lengthvar <- c(1, 1, -1)

prcpvals <- ncvar_get( prcp_nc, var="prcp", start=startvar, count=lengthvar) / 25.4
tminvals <- ncvar_get( tmin_nc, var="tmin", start=startvar, count=lengthvar) * 9 / 5 + 32
tmaxvals <- ncvar_get( tmax_nc, var="tmax", start=startvar, count=lengthvar) * 9 / 5 + 32

precip_5_day <- numeric(5)
PNTR <- 1

precip_5_day <- 0.0

rolling_sum_5_day_precip <- function( precip ) {

  precip_5_day[ PNTR ] <- precip
  PNTR <- PNTR + 1
  return( sum( precip_5_day ) )

}

partition_precipitation <- function( precipitation, tmin, tmax ) {

  tmean <- ( tmin + tmax )  / 2.0

  rainfall <- 0.0
  snowfall <- 0.0

  ifelse (tmean - ( tmax - tmin ) / 3.0 <= 32.0,
          snowfall <- precipitation,
          rainfall <- precipitation )

  return( list(snowfall=snowfall, rainfall=rainfall) )

}

update_snowcover <- function( snowfall, snowcover, tmin, tmax ) {

  MELT_INDEX <- 1.5   # 1.5 mm/degC

  tmean <- (tmax + tmin) / 2.0

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

update_cfgi <- function( mean_temperature_C, snow_cover ) {

  A <- 0.97

  # assuming snow depth is 10 times the water content of the snow in inches
  SnowDepthCM = snow_cover * 10.0 * 2.54

  if ( mean_temperature_C > 0.0 ) {
    CFGI <- max(A * CFGI
                - ( mean_temperature_C * exp (-0.4 * 0.5 * SnowDepthCM )), 0.0 )
  } else { # temperature is below freezing

    CFGI = max(A * CFGI
               - ( mean_temperature_C * exp (-0.4 * 0.08 * SnowDepthCM)), 0.0 )
  }

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

  Pf <- prob_runoff_enhancement(CFGI, LL_CFGI, UL_CFGI)

  # use probability of runoff enhancement to calculate a weighted
  # average of curve number under Type II vs Type III antecedent
  # runoff conditions

  if ( CFGI > 0.0 ) {

    adj_curve_number <- base_curve_number * (1 - Pf)
    + ( base_curve_number / (0.427 + 0.00573 *  base_curve_number ) * Pf )

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

    } else if ( (precip_5_day >= DRY ) & (precip_5_day <= WET ) ) {    # AMC II

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



Delta <- solar_declination( seq(1,365), 365 )
DsubR <- rel_earth_sun_dist( seq(1,365), 365 )
Omega_s <- sunset_angle( latitude, Delta )
Ra <- extraterrestrial_radiation_Ra(latitude, Delta, Omega_s, DsubR)
ReferenceET0 <- ET0_hargreaves( tminvals, tmaxvals, Ra )

Date.daily <- mydates

mydata <- list(Date.daily=Date.daily, Tmin=zoo(FtoC(tminvals), mydates), Tmax=zoo(FtoC(tmaxvals), mydates),
               J=seq(1,365))

etvals <- ET.HargreavesSamani(data=mydata, constants=list(Elev=2000.,lambda=2.45, lat_rad=43.5*pi/180, Gsc=0.0820) )


#--------------------------

Smax <- 4.5
Sprev <- Smax * 0.85
TEW <- 3/25.4
REW <- 1.5/25.4
AWC <- 2.6   # inches per foot

Kcb_min <- 0.2
Kcb_max <- 1.2
plant_height <- 4.0
Zr_max <- 2.0

L_plant <- mdy("04-15-2012")
L_dev <- mdy("07-15-2012")

# Kcb curve definition
Kcb_vals <- c(Kcb_min, Kcb_min, Kcb_max, Kcb_max, Kcb_min, Kcb_min )
Kcb_dates <- c(mdy("01-01-2012"), L_plant, L_dev, mdy("08-28-2012"), mdy("09-20-2012"), mdy("12-30-2012"))

Kcb_func <- approxfun( Kcb_dates, Kcb_vals )

BareSoilEvap <- numeric(365)
CropETc <- numeric(365)
r_few <- numeric(365)
Kr <- numeric(365)
Ke <- numeric(365)
Zr <- numeric(365)
Ks <- numeric(365)
Deficit <- numeric(365)
Depth_evap <- numeric(365)
S <- numeric(365)
TAW <- numeric(365)
RAW <- numeric(365)
ActualET <- numeric(365)
rainfall <- numeric(365)
snowfall <- numeric(365)
snowcover <- numeric(365)
snowmelt <- numeric(365)
net_infil <- numeric(365)
five_day_sum_prcp <- numeric(365)
p <- 0.0

SNOW_COVER <- 2.0

for (d in seq(1,365)) {

  five_day_sum_prcp[d] <- rolling_sum_5_day_precip( prcpvals[d] )

  prcp_partition <- partition_precipitation( prcpvals[d], tminvals[d], tmaxvals[d] )

  snowfall[d] <- prcp_partition$snowfall
  rainfall[d] <- prcp_partition$rainfall

  snow_list <- update_snowcover(snowfall[d], SNOW_COVER, tminvals[d], tmaxvals[d] )

  SNOW_COVER <- snow_list$snowcover
  snowmelt[d] <- snow_list$snowmelt
  snowcover[d] <- snow_list$snowcover

  S[d] <- Sprev + rainfall[d] + snowmelt[d]

  # pluck Kcb off of Kcb curve based on current date
  Kcb <- Kcb_func( mydates[d])

  Zr[d] <- CalcEffectiveRootDepth(Kcb_max, Kcb_min, L_plant, L_dev, Zr_max, mydates[d] )
  TAW[d] <- Zr[d] * AWC
  RAW[d] <- p * TAW[d]

  Depth_evap[d] <- max( ( Smax - S[d] ), 0.0 )

  r_few[d] <- CalcFractionExposedAndWettedSoil( Kcb_min, Kcb_max, plant_height, Kcb )
  Kr[d] <- CalcEvaporationReductionCoefficient(TEW, REW, Depth_evap[d])
  Ke[d] <- CalcSurfaceEvaporationCoefficient(Kcb_max, Kcb, Kr[d], r_few[d] )

  Deficit[d] <- max((TAW[d] - S[d]), 0.0)

  Ks[d] <- CalcWaterStressCoefficient(TAW[d], RAW[d], Deficit[d])

  BareSoilEvap[d] <- ReferenceET0[d] * Ke[d]
  CropETc[d] <- ReferenceET0[d] * Kcb * Ks[d]
  ActualET[d] <- CropETc[d] + BareSoilEvap[d]
  S[d] <- S[d] - CropETc[d] - BareSoilEvap[d]

  net_infil[d] <- max((S[d] - Smax),0.0)
  S[d] <- min( Smax, S[d] )
  Sprev <- S[d]

}

plot( mydates, Depth_evap, main="Depth of evaporation", type="l", col="blue")
plot( mydates, Deficit, main="Deficit relative to total available water", type="l", col="blue")
plot( mydates, r_few, main="Fraction exposed and wetted surface", type="l", col="blue")
plot( mydates, TAW, main="Total available water", type="l", col="blue")
plot( mydates, RAW, main="Readily available water", type="l", col="blue")
plot( mydates, Kr, main="Evaporation reduction coefficient", col="blue")
plot( mydates, Ke, main="Surface evap coefficient", type="l", col="blue")
plot( mydates, Zr, main="Effective plant rooting depth", type="l", col="blue")
plot( mydates, Ks, main="Plant stress coefficient", type="l", col="blue")
plot( mydates, net_infil, main="Net infiltration", type="l", col="blue")
plot( mydates, S, main="Soil moisture", type="l", col="blue")
plot( mydates, Kcb_func(mydates), main="Kcb, crop coefficient", type="l", col="blue")

plot(mydates,ReferenceET0, type="l")
points(mydates, etvals$ET.Daily/25.4, cex=0.7, col="blue", pch=21)
lines(mydates, BareSoilEvap, col="cyan")
lines(mydates, CropETc, col="green")
lines(mydates, ActualET, col="orange")
points(mydates, ActualET, col="orange",cex=0.5, pch=22)
legend("topright", c("SWB","R package ET","Bare soil evap","Crop ETc","ActualET"),
       lty=c(1,NA,1,1,1), pch=c(NA,21,NA,NA,22),
       col=c("black","blue","cyan","green","orange"))








