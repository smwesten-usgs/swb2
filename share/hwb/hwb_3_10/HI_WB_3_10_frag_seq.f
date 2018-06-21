      program HI_WB

c  Recharge Computation Program for the Hawaiian Islands
c  version 3.9, 2/6/14
c  version 3.9r, 4/6/2018 (fixed "goto 440" line in smccalc subroutine)
c  version 3.9r2, 5/9/2018 (see changes listed below)
c     1. replaced tabs with spaces
c     2. increased size of yrlyrech array from 30 to nyrs using ALLOCATE statement
c     3. corrected no. days in month for output to unit 38
c     4. modified conversion factors using exact values
c     5. modified FAO_post subroutine to avoid having to read in information from hi_wb2.out
c     6. corrected the header in hi_wb2.out (IRR and RO headers were switched)
c
c  version 3.10, 5/11/2018
c     1. adjust fragments such that daily values for a month sum to 1
c  version 3.10_frag_seq 6/21/2018
c     1. outputs fragments

c  authors: J.A. Engott and D.S. Oki
c  
c  Modified from Recharge Computation Program for Lihue Basin, Kauai
c  program lihuewb02 by D.S. Oki
c
c  Computes daily water budget using 4 AE/PE methods:
c    1.  Veihmeyer 
c    2.  FAO56
c    3.  Giambelluca 
c    4.  Thornthwaite 
c
c  Some differences from previous versions of this program:
c
c  (1) crop and urban irrigation is computed using a ratio of irrigation to 
c      potential evapotranspiration and is applied daily.
c  (2) historical monthly rainfall, in inches, from the period 1920-2007 (Frazier and Giambelluca,2012) is used
c  (3) water mains, cesspools, and disposal wells can be included as sources of
c      direct recharge
c  (4) discharge from septic systems can be added to soil moisture
c  (5) effect of storm drains in urban areas can be toggled on/off
c  (6) fog-to-rainfall ratios vary by month, elevation, and zone
c  (7) fog-catch efficiency factor, based on land cover, is used
c  (8) three canopy-interception options are offered for forested areas
c  (9) pervious ratio is input for every polygon instead of by land cover
c (10) crop-irrigation parameters can be tailored to a wider range of crops
c (11) temporally varibale monthly runoff:rainfall ratios can be used 
c (12) recharge beneath taro lo'i is estimated using a constant rate
c (13) near-coastal or estuarine water bodies can be set to zero recharge 
c
c  Assumptions:
c
c  leap years not considered (February has only 28 days for all years)
c  daily runoff-to-rainfall ratio same as monthly ratio
c  daily fog drip is linearly related to daily rainfall
c
c
c  List of input files:
c
c  fpoly           file with polygon information
c  fland           file with land-cover information
c  frunoff         file with monthly runoff-rainfall ratios
c  fawc            file with available water capacity data
c  ffrag           file with daily rainfall fragments
c  fpan            file with monthly-to-annual pan ratios by ET zone
c  ffog            file with monthly fog parameters
c  firr            file with irrigation parameters
c  fsug            file with sugarcane information, if sugarcane is present
c  frainnames      file with names of monthly rainfall files
c
c
c
c  List of hardwired input/output files
c
c   hi_wb.in       main input file
c   hi_wb1.out     main output file that relists input information and
c                  summarizes recharge (Mgal/d) over the entire modeled area
c                  for each of the simulations (1-nsim) using 4 AE/PE models
c   hi_wb2.out     summary output file that lists GIS poly-ID, land cover,
c                  field no., crop-stage group, area (m^2), rainfall,
c                  fog, irrigation, runoff, actual evapotranspiration,
c                  and recharge (all values in inches/yr and averaged over
c                  nsim simulations)
c   hi_wb3.out     summary output file that lists GIS poly-ID, land cover,
c                  field no., crop-stage group, area (m^2), rainfall,
c                  fog, irrigation, runoff, actual evapotranspiration,
c                  and recharge (all values in inches/yr and averaged over
c                  nsim simulations)--CONTAINS ONLY SUGARCANE POLYGONS
c   hi_wb4.out     polygon water-budget components for each polygon and
c                  simulation (water-budget components are in total inches
c                  accumulated over the nyrs of each simulation)
c   
c
c  List of subroutines:
c
c  input      reads input data from biwb01.in
c  irrsched   determines periods with potential sugarcane irrigation
c  smccalc    calculates soil-moisture storage capacity for all soil types
c             and root depths from 1-100 inches
c  fraguse    selects fragment sets to be used for a simulation
c  fields     selects initial sugarcane-field configuration for each plantation
c  pcoef      determines daily pan coefficient for current sugarcane polygon
c  rech       computes daily water budget using 4 different methods
c  FAO_post   summarizes results calculated using the FAO method, creating files 
c             more easily imported into spreadsheet and Arc products than 
c             the .ou2 file.
c
c  List of selected variables (includes variables from alternate versions):
c
c  ae1mgd(i)     cumulative yearly average evapotranspiration for simulation i
c                   (i=1,nsim) using the Veihmeyer AE/PE model, in Mgal/d
c  ae2mgd(i)     cumulative yearly average evapotranspiration for simulation i
c                   (i=1,nsim) using the Giambelluca AE/PE model, in Mgal/d
c  ae3mgd(i)     cumulative yearly average evapotranspiration for simulation i
c                   (i=1,nsim) using the FAO 56 AE/PE model, in Mgal/d
c  ae4mgd(i)     cumulative yearly average evapotranspiration for simulation i
c                   (i=1,nsim) using the Thornthwaite AE/PE model, in Mgal/d
c  afld(i,j)     area of sugarcane type i and field j
c                   i=1,7; j=1,nfld(i)
c  agirr         irrigation depth (in.) for current polygon and day
c  agirrate      ratio of daily irrigation to potential evapotranspiration
c  agmgd(i)      cumulative yearly average total irrigation for simulation i
c                   (i=1,nsim), in Mgal/d
c  annrfwt(i)    annual rainfall weight for year i (i=1,nannrfwt)
c  aplant(i)     irrigated sugarcane area for each plantation i (i=1,7)
c  area(i)       area (m^2) of GIS polygon i (i=1,npoly)
c  av1ae(i,m)    average evapotranspiration in polygon i (i=1,npoly) for month m 
c                over nsim simulations using the Veihmeyer AE/PE model, in/mo
c  av1rc(i,m)    average recharge in polygon i (i=1,npoly) for month m over nsim
c                   simulations using the Veihmeyer AE/PE model, in/mo
c  av2ae(i,m)    average evapotranspiration in polygon i (i=1,npoly) for month m
c                    over nsim simulations using the Giambelluca AE/PE model, in/mo
c  av2rc(i,m)    average recharge in polygon i (i=1,npoly) for month m over nsim
c                   simulations using the Giambelluca AE/PE model, in/mo
c  av3ae(i,m)    average evapotranspiration in polygon i (i=1,npoly) for month m 
c                   over nsim simulations using the FAO 56 AE/PE model, in/mo
c  av3rc(i,m)    average recharge in polygon i (i=1,npoly) for month m over nsim
c                   simulations using the FAO 56 AE/PE model, in/mo
c  av4ae(i,m)    average evapotranspiration in polygon i (i=1,npoly) for month m 
c                   over nsim  simulations using the Thornthwaite AE/PE model, in/mo
c  av4rc(i,m)    average recharge in polygon i (i=1,npoly) for month m over nsim
c                   simulations using the Thornthwaite AE/PE model, in/mo
c  avir(i,m)     average irrigation in polygon i (i=1,npoly) for month m over nsim
c                   simulations, in/mo
c  avfd(i,m)     average fog drip in polygon i (i=1,npoly) for month m over nsim
c                   simulations, in/mo
c  avrf(i,m)     average rainfall in polygon i (i=1,npoly) for month m over nsim
c                   simulations, in/mo
c  avro(i,m)     average runoff in polygon i (i=1,npoly) for month m over nsim
c                   simulations, in/mo
c  avpnet(i,m)   average runoff in polygon i (i=1,npoly) for month m over nsim
c                   simulations, in/mo
c  avcanint(i,m) average runoff in polygon i (i=1,npoly) for month m over nsim
c                   simulations, in/mo
c  avrfadj(i,m)  average adjustment to rainfall in polygon i (i=1,npoly) for month m 
c                   over nsim simulations, in/mo
c  avseptic(i,m) average septic discharge to polygon i (i=1,npoly) for month m over 
c                   nsim simulations, in/mo
c  avstrmdrn(i,m) average flow to storm drains from polygon i (i=1,npoly) for month 
c                   m over nsim simulations, in/mo
c  awc(i,j,l)    available water capacity (in/in) for soil code i,
c                   sequence number j, and layer l
c                   i=1,nsoil; j=1,nseq(i); l=1,nlay(i,j)
c                   (note that in NRCS soil coverage, each soil code may
c                   actually contain more than one soil type or
c                   sequence number)
c  cesspool      total cesspool discharge for polygon accumulated during nyrs
c  cropirr(i)    monthly irrigation goals for each crop i (i=1,3) (1-sugar,2-pineapple,
c                3-macadamia nuts)
c  df5mm(i)      FAO 56 depletion fraction at 5 mm/d ET for land cover i (i=1,nlu)
c                   (see FAO 56, table 22)
c  dispwell      total disposal-well discharge for polygon accumulated during nyrs
c  dripeff       drip irrigation efficiency (0-1)
c  etmwf(i,m)    ratio of monthly-to-annual pan evaporation rate for
c                   ET zone i and month m (m=1,12)
c  fdmgd(i)      cumulative yearly average total fog drip for simulation i
c                   (i=1,nsim), in Mgal/d
c  fogeff(i)     fog-catch efficiency of vegetation; the ratio of the amount of fog 
c                deposition on vegetation to the amount of fog deposition on a standard 
c                cylindrical fog gage (i=1,nlu)
c  frg(m,i,j,k)  fragment for month m, rainfall zone i, fragment set j, and day k
c                   m=1,12; i=1,nrfz(m); j=1,nmbrrfz; k=1,31
c  furreff       furrow irrigation efficiency (0-1) (changed to micro-spray for Molokai WB)
c  golfirr(m)    monthly average golf course (and div ag) irrigation depths (in.);m=1,12
c  icd(i,j)      starting day for each pan coefficient stage j and land cover i
c                   i=1,nlu; j=1,5
c  icrst(i)      sugarcane crop stage for polygon i (i=1,npoly)
c                   note that this variable is ignored for non-sugarcane polys
c                   1 = crop stage 1
c                   2 = crop stage 2
c  ietzone(i)    pan evaporation variability zone for polygon i (i=1,npoly)
c  id1(i)        initial day of planting and irrigation for each of 2
c                   field groups (i=1,2)
c  idaypan(i,j)  number of days for land cover i in each of the j stages of
c                   pan coefficients
c                   i=1,nlu; j=1,5
c  idays(m)      number of days in month m (m=1,12)
c  iddrip(i)     specified list drip-irrigation days in a month (i=1,nddrip)
c  idfurrow(i)   specified list furrow-irrigation days in a month (i=1,ndfurrow)
c  ifld(i)       sugarcane field number for polygon i (i=1,npoly)
c                   (note: ifld(i)=0 if not sugar)
c  ifog(i)       fog-drip zone number for polygon i (i=1,npoly)
c  ilu(i)        land-cover code for polygon i (i=1,npoly)
c  imacirr       macadamia nut irrigation code (0-unirrigated,1-irrigated)
c  iplant(i)     sugar plantation code for each polygon i (i=1,npoly)
c  ipoly         polygon identification number from GIS (i=1,npoly)
c  ipineirr      pineapple irrigation code (0-unirrigated,1-irrigated)
c  iro(i)        runoff zone number for polygon i (i=1,npoly)
c  irfzone(i)    rainfall gage zone for polygon i (i=i,npoly)
c  irrday(i)     sugarcane irrigation code for day i (i=1,ndsug)
c                   -1 = fallow (not planted)
c                    0 = unirrigated sugarcane
c                    1 = drip- or furrow-irrigated sugarcane
c  iseed         specified seed value for random number generator
c  isoil(i)      soil code for polygon i (i=1,npoly)
c  istartyr      starting year of simulation
c  istormdrain   code indicating presence of storm drains in urbanized areas (0-no storm drains,
c                1-storm drains)
c  isugarirr     sugarcane irrigation code (0-unirrigated,1-irrigated)
c  jfr(m,j,i)    selected fragment set for month m, band j, and year i
c                   m=1,12; j=1,nbands(m); i=1,nyrs
c  nannrfwt      number of years of annual rainfall weights
c  nddrip        specified number of drip application days per month
c  ndfallow      specified length of sugarcane fallow period (no. days)
c  ndfurrow      specified number of furrow application days per month
c  ndirr         specified length of sugarcane irrigation period (no. days)
c  ndsug         calculated number of days in sugarcane crop cycle
c                   ndsug = ndirr + ndunirr + ndfallow
c  ndunirr       specified length of sugarcane unirrigated period (no. days)
c  nfld(i)       number of irrigated sugarcane fields for each plantation i (i=1,7)
c  nfr(m,j)      number of monthly fragment sets for month m and band j
c                   m=1,12; j=1,nmbrrfz
c  nlay(i,j)     number of layers for soil code i and sequence number j
c                   i=1,nsoil; j=1,nseq(i)
c  nlu           number of different land-cover types
c  nmbrrfz       number of rainfall gage zones in study area
c  nplant        number of sugar plantations
c  npoly         total number of polygons
c  nrfzcro(m,j,k) monthly rainfall zone correlation rankings for month,m, and rainfall zone,j
c  nrrr          number of runoff zone
c  nseq(i)       number of sequence numbers (soil types) within
c                  soil code i (i=1,nsoil)
c  nsim          specified total number of simulations desired
c  nsoil         number of different soil codes
c  nyrs          specified number of years in each simulation
c  panann(i)     annual pan evaporation rate (in.) for polygon i (i=1,npoly)
c  pancoef(i,j)  pan coefficients for land cover i and different growth stage j
c                  i=1,nlu; j=1,4
c  pavint        specified interception capacity (in.) of paved surfaces
c  pc            pan coefficient for current polygon and day
c  pct(i,j,l)    fraction of sequence number (soil type) j with
c                   soil code i and layer l
c                   i=1,nsoil; j=1,nseq(i); l=1,nlay(i,j)
c                   (also see awc variable)
c  perv(i)       fraction of area (0-1) for land cover i that is
c                   pervious (i=1,nlu)
c  rc1           recharge for current polygon and simulation
c                   using the Veihmeyer AE/PE model, in/yr
c  rc2           recharge for current polygon and simulation
c                   using the Giambelluca AE/PE model, in/yr
c  rc3           recharge for current polygon and simulation
c                   using the FAO 56 AE/PE model, in/yr
c  rc4           recharge for current polygon and simulation
c                   using the Thornthwaite AE/PE model, in/yr
c  rc1mgd(i)     cumulative yearly average total recharge for simulation i
c                   (i=1,nsim) using the Veihmeyer AE/PE model, in Mgal/d
c  rc2mgd(i)     cumulative yearly average total recharge for simulation i
c                   (i=1,nsim) using the Giambelluca AE/PE model, in Mgal/d
c  rc3mgd(i)     cumulative yearly average total recharge for simulation i
c                   (i=1,nsim) using the FAO 56 AE/PE model, in Mgal/d
c  rc4mgd(i)     cumulative yearly average total recharge for simulation i
c                   (i=1,nsim) using the Thornthwaite AE/PE model, in Mgal/d
c  rd(i)         root depth (in.) for land cover i (i,1,nlu)
c  rd0           specified pseudo-root depth (in.) for unvegetated surfaces
c  resrc         specified recharge rate beneath reservoirs (in/yr)
c  rfmgd(i)      cumulative yearly average total rainfall for simulation i
c                   (i=1,nsim), in Mgal/d
c  rfmon(i,m)    monthly rainfall for polygon i and month m
c                   i=1,npoly; m=1,12
c                   (monthly rainfall input in mm.)
c  rn            random number
c  romgd(i)      cumulative yearly average total runoff for simulation i
c                   (i=1,nsim), in Mgal/d
c  rrr(i,m)      runoff-rainfall ratio for zone i and month m
c                   m=1,12
c  sm0           specified initial fraction of soil-moiture storage
c                   capacity for all polygons
c  smca(i,j)     soil-moisture storage capacity for soil i and root depth j (in.)
c                   i=1,nsoil; j=1,100
c  tol1          specified initial tolerance error for selecting 50% of
c                   fields for crop state 1 (0-0.5)
c  urbirr(m)     monthly average urban irrigation depths (in.);m=1,12
c  wateravail    availability of surface water to meet sugarcane irrigation goals (0-1)
c  watermain     total water-main leakage for polygon accumulated during nyrs
c  wbrc          specified recharge rate beneath water-bodies (not including reservoirs) (in/yr)
c  zb(i,j,l)     bottom of layer l (in.) for soil code i and sequence number j
c                   i=1,nsoil; j=1,nseq(i); l=1,nlay(i,j)
c  zt(i,j,l)     top of layer l (in.) for soil code i and sequence number j
c                   i=1,nsoil; j=1,nseq(i); l=1,nlay(i,j)


      implicit real*8 (a-h,o-z)

c.....dso20180509 add following declaration statement for yrlyrech array
      ALLOCATABLE :: yrlyrech(:,:)

      dimension avrf(590000,12),avfd(590000,12),avir(590000,12),
     1   avro(590000,12),av1ae(590000,12),av2ae(590000,12),
     2   av3ae(590000,12),av4ae(590000,12),av1rc(590000,12),
     3   av2rc(590000,12),av3rc(590000,12),av4rc(590000,12),
     4   avdirect(590000,12),avpnet(590000,12),avcanint(590000,12),
     5   avrfadj(590000,12),avseptic(590000,12),avstrmdrn(590000,12)
      dimension rc1mgd(500),rc2mgd(500),rc3mgd(500),rc4mgd(500),
     1   ae1mgd(500),ae2mgd(500),ae3mgd(500),ae4mgd(500),
     2   rfmgd(500),fdmgd(500),agmgd(500),romgd(500)
      dimension zrf(12),zfd(12),zir(12),zro(12),ae1(12),rc1(12),
     1   ae2(12),rc2(12),ae3(12),rc3(12),ae4(12),rc4(12),direct(12),
     2   zpnet(12),zcanint(12),zrfadj(12),zseptic(12),zsd(12)
      dimension id1(2),iddrip(28),idfurrow(28),ilusug(3)
      dimension ipoly(590000),area(590000),rfmon(50000,1200),
     1   ilu(590000),ifld(590000),smc(590000),perv(590000),
     2   iro(590000),isoil(590000),panann(590000),ifog(590000),
     3   ietzone(590000),irfzone(590000),iplant(590000),
     4   ifogelev(590000),iwatmain(590000),idispwell(590000),
     5   icpsepsewer(590000),iasys(590000),cprate(590000),
     6   irfcell(590000),ilup(590000,12),irrcode(50),rfnorm(590000,12),
     7   isdrain(590000),canfrac(590000),tfrac(590000),cerf(590000),
     8   romon(1000,1200),fdrfratio(500,100,12)
      dimension rd(50),df5mm(50),pancoef(50,12),idaypan(50,5),isd(50),
     1   icd(50,5),rrr(50,12),awc(500,5,8),pct(500,5,8),zt(500,5,8),
     2   zb(500,5,8),nlay(500,5),nseq(500),smca(500,100),fogeff(50),
     3   rfw(10,25),spancoef(50,4),cancap(50),tcap(50),rfwt(50000,12)
      dimension nfr(12,100),frg(12,100,500,31),jfr(12,100,500)
      dimension rfwtmon(50,100,12),etmwf(50000,12)
      dimension icrst(590000)
      dimension idays(12),nrfzcro(12,50,50)
      dimension idemsup(20),supirr(20),rmltirr(20),effirr(20),
     1          irrday(20,31),nirrdays(20,12),irrsug(50000)

      data idays/31,28,31,30,31,30,31,31,30,31,30,31/

      common /consti/nsim,nyrs,ndirr,ndunirr,ndfallow,id1,irfweights,
     1    nddrip,iddrip,ndfurrow,idfurrow,ndsug,istartyr,iendyr,nilusug,
     2    isugarirr,ipineirr,imacirr,istormdrain,iluper,icim,irfnorm,
     3    ilusug,ilupine,ilumac,iludag,ilucoff,ilucorn,iluwater,ilures,
     4    irechyrly,irotype,ilutaro,iluzrwb
      common /constr/sm0,rd0,tol1,pavint,wbrc,dripeff,furreff,
     1   wateravail,resrc,wmleak,dwrate,sprayeff,tarorc,
     2   watermain,canstore,dcif,constAC,constBD,tarofrac
      common /poly/ipoly,area,rfmon,ilu,ifld,smc,iasys,
     1   iro,isoil,panann,ifog,ietzone,irfzone,iplant,npoly,nplant,
     2   ifogelev,iwatmain,idispwell,icpsepsewer,irfcell,ilup,
     3   perv,cprate,isdrain,canfrac,tfrac,cerf
      common /landi/nlu,idaypan,icd,irrcode,isd
      common /landr/rd,df5mm,pancoef,fogeff,spancoef,cancap,tcap
      common /runoff/rrr,romon,nrrr
      common /soili/nlay,nseq,nsoil
      common /soilr/awc,pct,zt,zb,smca
      common /fragi/nmbrrfz,nfr,jfr,isimulation
      common /fragr/frg
      common /rain/rfwtmon,nrfzcro,rfw,rfwt,rfnorm,irfstyr
      common /evap/etmwf
      common /fogdrip/fdrfratio
      common /irrig/idemsup,supirr,rmltirr,effirr,irrday,nirrdays,irrsug
      common /seeder/iseed
      common /field/icrst
      common /nprint/jprint,iprint
c.....dso20180511 add next common statement for FAO output
      common /avgmo/avrf,avfd,avir,avro,av1ae,av2ae,av3ae,av4ae,
     1   av1rc,av2rc,av3rc,av4rc,avdirect,avpnet,avcanint,
     2   avrfadj,avseptic,avstrmdrn


      open(1,file='output\hi_wb1.out')
      open(2,file='output\hi_wb2.out')
      open(3,file='output\hi_wb3.out')
      open(4,file='output\hi_wb4.out')
      open(5,file='output\fields_selected.out')
      open(7,file='output\frag_sequence.out')

c.....read input data
      call input
      write(*,*)'Input data read'

c.....determine irrigated/unirrigated periods for sugarcane growing cycle
      if(nilusug.ne.0)call irrsched

c.....compute soil-moisture storage capacity for root depths of 1-100 in.
      call smccalc

c.....initialize polygon recharge arrays
      do 100 j=1,npoly
      do 50 m=1,12
         avrf(j,m) =0.
         avfd(j,m) =0.
         avir(j,m) =0.
         avro(j,m) =0.
         av1ae(j,m)=0.
         av2ae(j,m)=0.
         av3ae(j,m)=0.
         av4ae(j,m)=0.
         av1rc(j,m)=0.
         av2rc(j,m)=0.
         av3rc(j,m)=0.
         av4rc(j,m)=0.
         avdirect(j,m)=0.
         avpnet(j,m)=0.
         avcanint(j,m)=0.
         avseptic(j,m)=0.
         avstrmdrn(j,m)=0.
         avrfadj(j,m)=0.
         zrf(m)=0.
         zfd(m)=0.
         zir(m)=0.
         zro(m)=0.
         ae1(m)=0.
         ae2(m)=0.
         ae3(m)=0.
         ae4(m)=0.
         rc1(m)=0.
         rc2(m)=0.
         rc3(m)=0.
         rc4(m)=0.
         direct(m)=0.
         zpnet(m)=0.
         zcanint(m)=0.
         zrfadj(m)=0.
         zseptic(m)=0.
         zsd(m)=0.
 50   continue
 100  continue

c.....dso20180509 add following ALLOCATE statement
      ALLOCATE(yrlyrech(npoly,nyrs))

      write(1,1000)
      write(2,1000)
      write(3,1000)
      write(4,1000)

      write(1,1100)
      write(2,2000)
      write(3,3000)
      write(4,4000)
      write(7,9300)

c.....compute desired number of water-budget simulations
      do 300 i=1,nsim
         isimulation=i
c........get new seed value for random number generator
         iseed=iseed+1
         if(mod(iseed,2).eq.0)iseed=iseed+1

c........determine field configuration (furrow, drip, nonirrigated)
        if(nilusug.ne.0)then
            call fields
            write(*,9000)i
        endif
        
c........select fragments to use for current simulation
         call fraguse
         write(*,9100)i

c........compute water budget by polygon for desired number of years

         do 200 j=1,npoly
            call rech(i,j,zrf,zfd,zir,zro,ae1,rc1,
     1        ae2,rc2,ae3,rc3,ae4,rc4,direct,zpnet,zcanint,zrfadj,
     2        zseptic,zsd,yrlyrech)

            
c...........keep running yearly averages
            do m=1,12
               rfmgd(i)=rfmgd(i)+zrf(m)*area(j)/
     1            (0.0254d0*0.0254d0*231*365*1000000)
               fdmgd(i)=fdmgd(i)+zfd(m)*area(j)/
     1            (0.0254d0*0.0254d0*231*365*1000000)
               agmgd(i)=agmgd(i)+zir(m)*area(j)/
     1            (0.0254d0*0.0254d0*231*365*1000000)
               romgd(i)=romgd(i)+zro(m)*area(j)/
     1            (0.0254d0*0.0254d0*231*365*1000000)
               ae1mgd(i)=ae1mgd(i)+ae1(m)*area(j)/
     1            (0.0254d0*0.0254d0*231*365*1000000)
               ae2mgd(i)=ae2mgd(i)+ae2(m)*area(j)/
     1            (0.0254d0*0.0254d0*231*365*1000000)
               ae3mgd(i)=ae3mgd(i)+ae3(m)*area(j)/
     1            (0.0254d0*0.0254d0*231*365*1000000)
               ae4mgd(i)=ae4mgd(i)+ae4(m)*area(j)/
     1            (0.0254d0*0.0254d0*231*365*1000000)
               rc1mgd(i)=rc1mgd(i)+rc1(m)*area(j)/
     1            (0.0254d0*0.0254d0*231*365*1000000)
               rc2mgd(i)=rc2mgd(i)+rc2(m)*area(j)/
     1            (0.0254d0*0.0254d0*231*365*1000000)
               rc3mgd(i)=rc3mgd(i)+rc3(m)*area(j)/
     1            (0.0254d0*0.0254d0*231*365*1000000)
               rc4mgd(i)=rc4mgd(i)+rc4(m)*area(j)/
     1            (0.0254d0*0.0254d0*231*365*1000000)

c...........keep average polygon values (inches/mo)
            avrf(j,m)=avrf(j,m)+zrf(m)/dfloat(nsim)
            avfd(j,m)=avfd(j,m)+zfd(m)/dfloat(nsim)
            avir(j,m)=avir(j,m)+zir(m)/dfloat(nsim)
            avro(j,m)=avro(j,m)+zro(m)/dfloat(nsim)
            av1ae(j,m)=av1ae(j,m)+ae1(m)/dfloat(nsim)
            av2ae(j,m)=av2ae(j,m)+ae2(m)/dfloat(nsim)
            av3ae(j,m)=av3ae(j,m)+ae3(m)/dfloat(nsim)
            av4ae(j,m)=av4ae(j,m)+ae4(m)/dfloat(nsim)
            av1rc(j,m)=av1rc(j,m)+rc1(m)/dfloat(nsim)
            av2rc(j,m)=av2rc(j,m)+rc2(m)/dfloat(nsim)
            av3rc(j,m)=av3rc(j,m)+rc3(m)/dfloat(nsim)
            av4rc(j,m)=av4rc(j,m)+rc4(m)/dfloat(nsim)
            avdirect(j,m)=avdirect(j,m)+direct(m)/dfloat(nsim)
            avpnet(j,m)=avpnet(j,m)+zpnet(m)/dfloat(nsim)
            avcanint(j,m)=avcanint(j,m)+zcanint(m)/dfloat(nsim)
            avrfadj(j,m)=avrfadj(j,m)+zrfadj(m)/dfloat(nsim)
            avseptic(j,m)=avseptic(j,m)+zseptic(m)/dfloat(nsim)
            avstrmdrn(j,m)=avstrmdrn(j,m)+zsd(m)/dfloat(nsim)

            end do

c...........output status to screen every 1000 polygons completed
            if(mod(j,1000).eq.0)write(*,9200)i,j,npoly

 200     continue

         write(*,'(i5,2x,4f11.3)')i,rc1mgd(i),rc2mgd(i),rc3mgd(i),
     1      rc4mgd(i)

c........divide running yearly averages by number of simulations completed

         trfmgd=(trfmgd*(i-1)+rfmgd(i))/dfloat(i)
         tfdmgd=(tfdmgd*(i-1)+fdmgd(i))/dfloat(i)
         tagmgd=(tagmgd*(i-1)+agmgd(i))/dfloat(i)
         tromgd=(tromgd*(i-1)+romgd(i))/dfloat(i)
         tae1mgd=(tae1mgd*(i-1)+ae1mgd(i))/dfloat(i)
         tae2mgd=(tae2mgd*(i-1)+ae2mgd(i))/dfloat(i)
         tae3mgd=(tae3mgd*(i-1)+ae3mgd(i))/dfloat(i)
         tae4mgd=(tae4mgd*(i-1)+ae4mgd(i))/dfloat(i)
         trc1mgd=(trc1mgd*(i-1)+rc1mgd(i))/dfloat(i)
         trc2mgd=(trc2mgd*(i-1)+rc2mgd(i))/dfloat(i)
         trc3mgd=(trc3mgd*(i-1)+rc3mgd(i))/dfloat(i)
         trc4mgd=(trc4mgd*(i-1)+rc4mgd(i))/dfloat(i)

c........write yearly averages to hi_wb1.out

         write(1,1200)i,trfmgd,tfdmgd,tagmgd,tromgd,
     1   tae1mgd,trc1mgd,tae2mgd,trc2mgd,
     2   tae3mgd,trc3mgd,tae4mgd,trc4mgd

 300  continue

c.....output rainfall, fog drip, irrigation, runoff, AE, and recharge
c     by polygon (average of nsim runs in inches)
      do 400 j=1,npoly
       do m=1,12
          write(2,2100)m,ipoly(j),ilu(j),irfcell(j),iasys(j),area(j),
     1    avrf(j,m),avfd(j,m),avir(j,m),avro(j,m),avdirect(j,m),
     2    av1ae(j,m),av1rc(j,m),av2ae(j,m),av2rc(j,m),av3ae(j,m),
     3    av3rc(j,m),av4ae(j,m),av4rc(j,m),avpnet(j,m),avcanint(j,m),
     4    avrfadj(j,m),avseptic(j,m),avstrmdrn(j,m)

       do n=1,nilusug
         if(ilu(j).eq.ilusug(n))then
            write(3,3100)m,ipoly(j),ilu(j),ifld(j),icrst(j),area(j),
     1      avrf(j,m),avfd(j,m),avir(j,m),avro(j,m),av1ae(j,m),
     2      av1rc(j,m),av2ae(j,m),av2rc(j,m),av3ae(j,m),av3rc(j,m),
     3      av4ae(j,m),av4rc(j,m)
         endif
       end do
       end do
 400  continue

      call FAO_post

 1000 format('Output file generated by HI_WB.f version 3.10',/)
 
 1100 format(/,
     1       'Water-budget summary (Mgal/d) for 4 AE/PE models',//,
     2       137('-'),/,
     2       t56,'----Veihmeyer---      -----FAO 56-----      '
     3       '---Giambelluca--      --Thornthwaite--',/,
     4       'Sim.    Rainfall       Fog      Irrig.     Runoff      ',
     5       'AE      Recharge      AE      Recharge      AE      ',
     6       'Recharge      AE      Recharge',/,137('-'))
 
 1200 format(i4,1x,12(2x,f9.3))

 2000 format('MO     POLY-ID  LC    IRC    AQSYS    AREA, m^2     ',
     1       '   RF',
c    2       '     FOG      RO     IRR       DIR_RC     AE1     RC1',
     2       '     FOG     IRR      RO     DIR_RC     AE1     RC1',
     3       '     AE2     RC2     AE3     RC3     AE4     RC4',     
     4       '    PNET    CINT   RFADJ     SEP    SDRN')
 
 2100 format(i2,2x,i10,2x,i2,2x,i6,2x,i6,2x,f11.2,2x,4(1x,f7.3),
     1       1x,f10.3,13(1x,f7.3))
 3000 format('MO     POLY-ID  LC FIELD STAGE    AREA, m^2     ',
     1       '  RF',
     2       '    FOG     IRR      RO     AE1     RC1',
     3       '    AE2     RC2     AE3     RC3     AE4     RC4')
 
 3100 format(i2,2x,i10,2x,i2,2x,i4,2x,i4,2x,f11.2,2x,12(1x,f7.3))
 
 4000 format('  SIM YRS     POLY    AREA,m^2  LU SOIL    SMC',
     1    '  PERV     RAINFALL     FOG DRIP       IRRIG.       SEPTIC',
     2    '     DIR RECH       RUN-ON       RUNOFF      CAN INT',
     3    '     PAN EVAP  STORM DRAIN       PE/PAN',
     4    '   SM1i          AE1       RECH 1  SM1f    ERR.1  SM2i    ',
     5    '      AE2       RECH 2  SM2f    ERR.2  SM3i          AE3   ',
     6    '    RECH 3  SM3f    ERR.3  SM4i          AE4       RECH 4',
     7    '  SM4f    ERR.4')

 9000 format(' Fields selected for simulation no. ',i3)
 9100 format(' Fragments selected for simulation no. ',i3)
 9200 format(' Simulation no. ',i3,', ',i6,' of ',i6,
     1       ' polygons complete')
 9300 format('simulation  month  frag_zone  year  random_number  '
     1       'selected_set')      

      stop
      end

c----------------------------------------------------------------------------

      subroutine input

      implicit real*8 (a-h,o-z)

      character*50 fpoly,fland,frunoff,fawc,ffrag,fmonrf,fpan,fronames,
     1   ffog,firr,furb,frainnames,frfw,fsug,frfweights,frfnorm,fgash
      character*60 c,txt,fmt
      character*120 rainfile,rofile

      dimension id1(2),iddrip(28),idfurrow(28),ilusug(3)
      dimension ipoly(590000),area(590000),rfmon(50000,1200),
     1   ilu(590000),ifld(590000),smc(590000),perv(590000),
     2   iro(590000),isoil(590000),panann(590000),ifog(590000),
     3   ietzone(590000),irfzone(590000),iplant(590000),
     4   ifogelev(590000),iwatmain(590000),idispwell(590000),
     5   icpsepsewer(590000),iasys(590000),cprate(590000),
     6   irfcell(590000),ilup(590000,12),irrcode(50),rfnorm(590000,12),
     7   isdrain(590000),canfrac(590000),tfrac(590000),cerf(590000),
     8   romon(1000,1200),fdrfratio(500,100,12)
      dimension rd(50),df5mm(50),pancoef(50,12),idaypan(50,5),isd(50),
     1   icd(50,5),rrr(50,12),awc(500,5,8),pct(500,5,8),zt(500,5,8),
     2   zb(500,5,8),nlay(500,5),nseq(500),smca(500,100),fogeff(50),
     3   rfw(10,25),spancoef(50,4),cancap(50),tcap(50),rfwt(50000,12)
      dimension nfr(12,100),frg(12,100,500,31),jfr(12,100,500)
      dimension rfwtmon(50,100,12),etmwf(50000,12)
      dimension idemsup(20),supirr(20),rmltirr(20),effirr(20),
     1          irrday(20,31),nirrdays(20,12),irrsug(50000)
      dimension idays(12),f(32),wf(25),etwt(12),nrfzcro(12,50,50)
      dimension ceint(6)
    
      data idays/31,28,31,30,31,30,31,31,30,31,30,31/
      data ilusug/3*0/
 
      common /consti/nsim,nyrs,ndirr,ndunirr,ndfallow,id1,irfweights,
     1    nddrip,iddrip,ndfurrow,idfurrow,ndsug,istartyr,iendyr,nilusug,
     2    isugarirr,ipineirr,imacirr,istormdrain,iluper,icim,irfnorm,
     3    ilusug,ilupine,ilumac,iludag,ilucoff,ilucorn,iluwater,ilures,
     4    irechyrly,irotype,ilutaro,iluzrwb
      common /constr/sm0,rd0,tol1,pavint,wbrc,dripeff,furreff,
     1   wateravail,resrc,wmleak,dwrate,sprayeff,tarorc,
     2   watermain,canstore,dcif,constAC,constBD,tarofrac
      common /poly/ipoly,area,rfmon,ilu,ifld,smc,iasys,
     1   iro,isoil,panann,ifog,ietzone,irfzone,iplant,npoly,nplant,
     2   ifogelev,iwatmain,idispwell,icpsepsewer,irfcell,ilup,
     3   perv,cprate,isdrain,canfrac,tfrac,cerf
      common /landi/nlu,idaypan,icd,irrcode,isd
      common /landr/rd,df5mm,pancoef,fogeff,spancoef,cancap,tcap
      common /runoff/rrr,romon,nrrr
      common /soili/nlay,nseq,nsoil
      common /soilr/awc,pct,zt,zb,smca
      common /fragi/nmbrrfz,nfr,jfr,isimulation
      common /fragr/frg
      common /rain/rfwtmon,nrfzcro,rfw,rfwt,rfnorm,irfstyr
      common /evap/etmwf
      common /fogdrip/fdrfratio
      common /irrig/idemsup,supirr,rmltirr,effirr,irrday,nirrdays,irrsug
      common /seeder/iseed
      common /npfog/slpnpfog,yintnpfog
      common /canint/gashcal,ceint,ifm
      common /nprint/jprint,iprint
      
c.....open main input file
      open(10,file='HI_wb.in')

c.....open error message file
      open(8,file='error.out')


c.....read information from main input file

c.....read in number of simulations desired
      read(10,*)nsim
      write(1,1000)nsim
      
c.....read in number of years desired for each simulation
      read(10,*)nyrs
      write(1,1010)nyrs

c.....read in start year
      read(10,*)istartyr
      iendyr=istartyr+nyrs-1
      write(1,1011)istartyr,iendyr

c.....read in land-cover period code    
      read(10,*)iluper
      write(1,1012)iluper

c.....read no. of sugarcane land-cover codes
      read(10,*)nilusug
      if(nilusug.gt.3)then
        write(8,*)'ABORTING--NUMBER OF SUGARCANE LAND-COVER CODES', 
     1            ' CANNOT EXCEED 3'
        stop
      endif  
      write(1,1013)nilusug

c.....read in sugarcane land-cover codes, if any
      if(nilusug.ne.o)then
        read(10,*)(ilusug(i),i=1,nilusug)
        do j=1,nilusug
            write(1,1014)ilusug(j)
        end do
      else
        read(10,*)junk1
      endif       

c.....read in land-cover code for corn
      read(10,*)ilucorn
      write(1,1018)ilucorn

c.....read in land-cover code for water body
      read(10,*)iluwater
      write(1,1019)iluwater

c.....read in land-cover code for reservoir
      read(10,*)ilures
      write(1,1020)ilures

c.....read in land-cover code for zero-recharge water body
      read(10,*)iluzrwb
      write(1,1024)iluzrwb

c.....read in land-cover code for taro
      read(10,*)ilutaro
      write(1,1021)ilutaro
      
c.....read in fraction of taro land cover in production
      read(10,*)tarofrac
      write(1,1022)tarofrac

c.....read in seed value for random number generator
      read(10,*)iseed
      write(1,1023)iseed

c.....read in initial soil-moisture storage (in fraction of capacity, 0-1)
      read(10,*)sm0
      write(1,1030)sm0

c.....read in "root depth" to use for unvegetated surfaces
      read(10,*)rd0
      write(1,1040)rd0

c.....read in storm drain active code (0-drains inactive, 1-drains active)
      read(10,*)istormdrain
      write(1,1158)istormdrain

c.....read in paved surface interception capacity (in)
      read(10,*)pavint
      write(1,1160)pavint

c.....read in constant water-body recharge rate (in/yr)
      read(10,*)wbrc
      write(1,1170)wbrc

c.....read in constant reservoir recharge rate (in/yr)
      read(10,*)resrc
      write(1,1180)resrc

c.....read in constant taro recharge rate (in/yr)
      read(10,*)tarorc
      write(1,1190)tarorc

c.....read in water main leakage rate (in/day)
      read(10,*)wmleak
      write(1,1200)wmleak

c.....read in disposal well discharge rate (MGD)
      read(10,*)dwrate
      write(1,1210)dwrate

c.....read in canopy-interception method      
      read(10,*)icim
      write(1,1220)icim

c.....read in constant A or C in canopy-interception simulating equation
      read(10,*)constAC
      write(1,1230)constAC
      
c.....read in constant B or D in canopy-interception simulating equation
      read(10,*)constBD
      write(1,1240)constBD
         
c.....read in rainfall data type code (0-month-year rainfall grids, 1-polygon-based normals)
      read(10,*)irfnorm
      write(1,1241)irfnorm
      
c.....read in number of rainfall grid files
      read(10,*)irfnf
      write(1,1242)irfnf

c.....read in number of months represented in rainfall grid files
      read(10,*)irfnm
      write(1,1243)irfnm

c.....read in start year of rainfall grid files
      read(10,*)irfstyr
      write(1,1244)irfstyr
            
c.....read in monthly rainfall coefficients indicator (0-no coefficients used, 1-coefficients used)
      read(10,*)irfweights
      write(1,1250)irfweights
      
c.....read in runoff data type (0-variable monthly RO:RF ratios, 1-constant monthly RO:RF ratios)
      read(10,*)irotype
      write(1,1251)irotype
      
      if(irotype.eq.0.and.irfnorm.ne.0)then
        write(8,*)'ABORTING--variable monthly rainfall must be used', 
     1            ' with variable monthly runoff-to rainfall ratios'
        stop
      endif        

c.....read in polygon print-out interval for every-simulation output file (hi_wb4.out)
      read(10,*)iprint
      write(1,1260)iprint
      
c.....read in yearly recharge output code (1-yearly output,0-no yearly output)(30 years max; modified to 200 max dso20180509)
      read(10,*)irechyrly
      write(1,1270)irechyrly

c.....read in names of input files********************************************************************

c.....read in name of main polygon input file 
      read(10,'(a50)')fpoly
      write(1,2010)fpoly

c.....read in name of land-cover input file 
      read(10,'(a50)')fland
      write(1,2020)fland

c.....read in name file with available-water-capacity values by soil code
      read(10,'(a50)')fawc
      write(1,2040)fawc

c.....read in name of rainfall fragment file
      read(10,'(a50)')ffrag
      write(1,2050)ffrag

c.....read in name of monthly:annual pan evaporation ratio file
      read(10,'(a50)')fpan
      write(1,2070)fpan

c.....read in name of fog parameter file
      read(10,'(a50)')ffog
      write(1,2080)ffog

c.....read in name of monthly crop irrigation file
      read(10,'(a50)')firr
      write(1,2090)firr

c.....read in name of monthly rainfall normals file if being used
      if(irfnorm.eq.0)then
c.......read in name of file with complete month-year rainfall grid filenames
        read(10,'(a50)')frainnames
        write(1,2097)frainnames
c.......read in name of monthly rainfall coefficients for month-year grids
        read(10,'(a50)')frfweights
        write(1,2098)frfweights
      elseif(irfnorm.eq.1)then
        read(10,'(a50)')frfnorm
        write(1,2099)frfnorm
c.....read in name of file with monthly observed-to-normal rainfall ratios
        read(10,'(a50)')frfw
        write(1,2098)frfw
      else
        write(8,*)'ABORTING--RAINFALL DATA TYPE CODE MUST BE 1 OR 0'
        stop
      endif
      
c.....read in name(s) of runoff files
      if(irotype.eq.0)then
c.....read in the name of file variable monthly runoff filenames
        read(10,'(a50)')fronames
        write(1,2101)fronames 
      elseif(irotype.eq.1)then
c.....read in name of constant monthly rainfall-runoff ratio file 
        read(10,'(a50)')frunoff
        write(1,2030)frunoff
      endif          

c.....read in name of sugarcane file if sugarcane is present
      if(nilusug.ne.0)then
         read(10,'(a50)')fsug
         write(1,2096)fsug
      endif

c.....read in name of modified Gash model file
      if(icim.eq.2)then
         read(10,'(a50)')fgash
         write(1,2091)fgash
      endif  
 
  
      open(6,file='output\rainfiles.out')

      write(1,*)
      write(1,*)
      write(1,*)
      write(1,*)

c.....open input files
      open(11,file=fpoly)
      open(12,file=fland)
c     open(13,file=frunoff)
      open(14,file=fawc)
      open(15,file=ffrag)
      open(17,file=fpan)
      open(18,file=ffog)
      open(19,file=firr)
      if(nilusug.ne.0)open(21,file=fsug)
      if(irfnorm.eq.1)then
        open(24,file=frfnorm)
        open(16,file=frfw)
      else
        open(22,file=frainnames)
        open(23,file=frfweights)
      endif    
      if(icim.eq.2)open(20,file=fgash)
      if(irotype.eq.0)then
        open(25,file=fronames)
        open(26,file='output\rofiles.out')
      endif
        
c.....read polygon information
      i=1
      nplant=0
 7    read(11,*,end=910)ipoly(i),area(i),irfcell(i),(ilup(i,j),j=1,12),
     1   iro(i),isoil(i),panann(i),ietzone(i),ifog(i),
     2   ifogelev(i),irfzone(i),iwatmain(i),idispwell(i),icpsepsewer(i),
     3   cprate(i),iasys(i),iplant(i),ifld(i),perv(i),isdrain(i),
     4   canfrac(i),tfrac(i),cerf(i)
     
      if(iplant(i).gt.nplant)nplant=iplant(i)
      ilu(i)=ilup(i,iluper)
      i=i+1
      goto 7
 910  npoly=i-1

c.....initialize monthly rainfall weighting factor arrays
      do 259 k=1,50000
      do 259 m=1,12
        rfwt(k,m)=1.
 259  continue    
      
        do 260 i=1,50
        do 260 j=1,100
        do 260 k=1,12
           rfwtmon(i,j,k)=-9999.
 260    continue

c.....read in rainfall data based on format of data
      if(irfnorm.eq.0)then
c.....read in rainfall month-year grid data
        icount=0
  8     read(22,'(a)',end=911)rainfile
        write(6,*)rainfile
        open(35,file=rainfile)
        icount=icount+1
        k=36*icount
  9     if(icount.eq.irfnf) then
            read(35,*,end=10)j,(rfmon(j,m),m=k-35,irfnm)
        else    
            read(35,*,end=10)j,(rfmon(j,m),m=k-35,k)
        endif
        goto 9      
10      continue       
        close(35)  
        goto 8
911     continue      
        nrfcells=j
        if(irfweights.eq.1)then
c.......read monthly rainfall weighting factors
  20        read(23,*,end=320)irc,(rfwt(irc,j),j=1,12)
            goto 20
 320        continue
            write(*,*)'mwf read'        
        endif 
      elseif(irfnorm.eq.1)then
c.....read in polygon-based rainfall normal data....................................................................................................................................
  21    read(24,*,end=321)ipol,(rfnorm(ipol,k),k=1,12)    
        goto 21
 321    continue
        if(irfweights.eq.1)then
c.......read monthly rainfall weighting factors
  16        maxrfyr=0
            read(23,*,end=916)irc,nrfyr,(wf(m),m=1,12)
               if(nrfyr.ge.istartyr.and.nrfyr.le.iendyr)then
                  iseqyr=nrfyr-istartyr+1
                  m=1
                  do m=1,12
                     rfwtmon(irc,iseqyr,m)=wf(m)
                  end do
              endif
              if(nrfyr.gt.maxrfyr)maxrfyr=nrfyr
              goto 16
 916        continue
            if(iendyr.gt.maxrfyr)then
               write(8,*)'ABORTING--NOT ENOUGH MONTHLY RAINFALL WEIGHTS'
               stop
            endif   
            write(*,*)'mwf read'        
           endif           
       endif    

c.....read in land-cover codes and associated information
      i=1
 12   read(12,*,end=912)j,rd(i),fogeff(i),irrcode(i),isd(i),cancap(i),
     1     tcap(i),df5mm(i),(pancoef(i,k),k=1,12),(spancoef(i,k),k=1,4),
     2     (idaypan(i,k),k=1,5)

      if(i.ne.j)then
         write(8,*)'ABORTING--SORT LANDCOVER FILE BY LANDCOVER CODE'
         stop
      endif

      icd(i,1)=idaypan(i,1)
      icd(i,2)=icd(i,1)+idaypan(i,2)
      icd(i,3)=icd(i,2)+idaypan(i,3)
      icd(i,4)=icd(i,3)+idaypan(i,4)
      icd(i,5)=icd(i,4)+idaypan(i,5)

      i=i+1
      goto 12
      
 912  nlu=i-1
      
      if(irotype.eq.0)then
c.....read in variable runoff month-year grid data
        icount=0
108     read(25,'(a)',end=1911)rofile
        write(26,*)rofile
        open(99,file=rofile)
        icount=icount+1
        k=36*icount
109     if(icount.eq.irfnf) then
            read(99,*,end=110)j,(romon(j,m),m=k-35,irfnm)
        else    
            read(99,*,end=110)j,(romon(j,m),m=k-35,k)
        endif
        goto 109      
110     continue       
        close(26)  
        goto 108
1911    continue      
        nrocells=j      
      elseif(irotype.eq.1)then
c.....read in constant monthly runoff-to-rainfall ratios by runoff-zone code
        i=1
 13     read(13,*,end=913)j,(rrr(j,m),m=1,12)
        if(i.ne.j)then
           write(8,*)'ABORTING--SORT RUNOFF FILE BY RO:RF-ZONE CODE'
           stop
        endif
        i=i+1
        goto 13
 913    nrrr=i-1
      endif

c.....read in AWC values by soil code
c     CAUTION:  AWC file must be sorted by soil-id (i), seqnum (j), and layer number (l)
      do 100 i=1,500
      do 100 j=1,5
      do 100 l=1,8
         awc(i,j,l)=0.
         pct(i,j,l)=0.
         zt(i,j,l)=0.
         zb(i,j,l)=0.
         nlay(i,j)=0
         nseq(i)=0
 100  continue

 14   read(14,*,end=914)i,j,p,l,z1,z2,xawc,ihsg

      awc(i,j,l)=xawc
      pct(i,j,l)=p
      zt(i,j,l)=z1
      zb(i,j,l)=z2
      nlay(i,j)=nlay(i,j)+1
      nseq(i)=j
      goto 14
 914  nsoil=i


c.....initialize fragment arrays and then read in fragments
      do 200 i=1,12
      do 200 j=1,100
         nfr(i,j)=0
 200  continue

      do 220 m=1,12
      do 220 i=1,100
      do 220 j=1,500
      do 220 k=1,31
         frg(m,i,j,k)=9999.
 220  continue
 15   read(15,*,end=915)mo,irfz,iseq,(f(k),k=1,31)
      
      if(iseq.gt.nfr(mo,irfz))nfr(mo,irfz)=iseq
c.....dso20180509 modify section for fragment adjustment
      frsum=0.d0
      nda=idays(mo)
      do 240 k=1,nda
         frg(mo,irfz,iseq,k)=f(k)
         frsum=frsum+f(k)
 240  continue
      if(frsum.lt.0.99999d0.or.frsum.gt.1.00001d0)then
         if(frsum.lt.0.1d0)then
            write(*,*)'ABORTING--check fragment set with zero values'
            write(*,*)mo,irfz,iseq
            stop
         endif
c........adjust fragments so sum equals to 1
         do 245 k=1,nda
            frg(mo,irfz,iseq,k)=frg(mo,irfz,iseq,k)/frsum
 245     continue
      endif
c.....dso20180509 end of fragment adjustment section
      goto 15
 915  continue

      nmbrrfz=irfz


c.....read monthly:annual pan evaporation ratios for each ET zone
 17   read(17,*,end=917)netz,(etwt(m),m=1,12)
      do m=1,12
         etmwf(netz,m)=etwt(m)
      end do 
      goto 17
 917  continue


c.....read 12 monthly fog drip parameters
c.....daily fog=fdrfratio*fogeff*daily rainfall, in/d per in/d

      m=1
 18   read(18,*,end=318)ifz,ifelev,(fdrfratio(ifz,ifelev,m),m=1,12)
      goto 18
 318  continue

c.....read irrigation file 
 19   read(19,*,end=319)ic,idemsup(ic),supirr(ic),rmltirr(ic),
     1   effirr(ic),(irrday(ic,id),id=1,31)
      if(idemsup(ic).eq.1.and.supirr(ic).ne.0)then
         write(8,*)'ABORTING--CHECK IRRIGATION FILE'
         stop
      endif
      goto 19
 319  continue 
        
        j=1
        k=1
        m=1
        do j=1,20 
            do m=1,12
                ndays=idays(m)
                do k=1,ndays
                     nirrdays(j,m)=nirrdays(j,m)+irrday(j,k)
                end do
            end do   
        end do     
        
c.....read sugarcane file if sugarcane is present
      if(nilusug.ne.0)then
      do n=1,3
      if(ilusug(n).ne.0)then
        read(21,*)ndirr,ndunirr,ndfallow,id1(1),id1(2),tol1
        ndsug=ndirr+ndunirr+ndfallow
        if(id1(1).gt.ndsug)then
            write(8,*)'ABORTING--PLANTING DAY FOR GROUP 1 TOO LARGE'
            stop
        elseif(id1(2).gt.ndsug)then
            write(8,*)'ABORTING--PLANTING DAY FOR GROUP 2 TOO LARGE'
            stop
        endif
        if(idaypan(ilusug(n),5).ne.ndfallow)then
            write(8,*)'ABORTING--INCONSISTENT NO. FALLOW DAYS',ilusug(n)
            stop
        endif
        if(icd(ilusug(n),5).ne.ndsug)then
            write(8,*)'ABORTING--INCONSISTENT NO. SUGAR DAYS',ilusug(n)
            stop
        endif
        write(1,1050)tol1
        write(1,1060)ndirr
        write(1,1070)ndunirr
        write(1,1080)ndfallow
      endif
      end do
      endif  

      i=1
c.... read modified Gash-model file if Gash method is selcted
      if(icim.eq.2)then
        read(20,*)ifm,(ceint(i),i=1,6),gashcal
      endif
      
c.....close files
c     do i=11,64
c        close(i)
c     end do
 
 1000 format(i10,t60,  ': Number of simulations')
 1010 format(i10,t60,  ': Number of years in each simulation')
 1011 format(i10,t60,  ': Starting year of simulation',/,
     1       i10,t60,  ': Ending year of simulation')
 1012 format(i10,t60,  ': land-cover period')
 1013 format(i10,t60,  ': Number of land-cover codes for sugarcane')
 1014 format(i10,t60,  ': land-cover code for sugarcane')
 1018 format(i10,t60,  ': land-cover code for corn')
 1019 format(i10,t60,  ': land-cover code for water bodies')
 1020 format(i10,t60,  ': land-cover code for reservoirs')
 1021 format(i10,t60,  ': land-cover code for taro')
 1022 format(f10.2,t60,
     1   ': fraction of taro land-cover in production')
 1023 format(i10,t60,
     1   ': Initial seed (integer) for random number generator')
 1024 format(i10,t60,
     1   ': land-cover code for zero-recharge water body')
 1030 format(f10.3,t60,
     1   ': Initial soil moisture (fraction of capacity, 0-1)')
 1040 format(f10.3,t60,
     1   ': pseudo root depth for unvegetated surfaces, in.')
 1050 format(f10.3,t60,
     1   ': Initial allowable error (0.0-0.5) for field selection')
 1060 format(i10,t60,  ': Length of sugarcane irrig. period (d)')
 1070 format(i10,t60,  ': Length of sugarcane unirr. period (d)')
 1080 format(i10,t60,  ': Length of sugarcane fallow period (d)',/)
 1090 format(i10,t60,  ': Initial plant day for field set 1')
 1100 format(i10,t60,  ': Initial plant day for field set 2')
 1110 format(i10,t60,  ': Number of drip irrigation days per month',
     1   '( list on next line)')
 1120 format(19i3,t60, ': Drip irrigation days in each month')
 1130 format(28i3)
 1140 format(i10,t60,  ': Number of furrow irrigation days per month',
     1   '( list on next line)') 
 1150 format(19i3,t60, ': Furrow irrigation days in each month')
 1151 format(f10.3,t60, ': Drip irrigation efficiency')
 1152 format(f10.3,t60, ': Furrow irrigation efficiency')
 1153 format(i10,t60, ': Sugarcane irrigation status (i-irr,0-unirr)')
 1154 format(i10,t60, ': Pineapple irrigation status (1-irr,0-unirr)')
 1155 format(i10,t60, ': Mac nut irrigation status (1-irr,0-unirr)')
 1156 format(i10,t60, ': Maalaea fields status (1-WAg,2-HCS)')
 1157 format(f10.3,t60,
     1 ': Water availability for non-groundwater-supplied fields as
     2fraction of demand (0-1)')
 1158 format(i10,t60, ': Storm drain code (0-no storm drains)')
 1160 format(f10.3,t60,
     1   ': Interception capacity of paved surfaces, in.')
 1170 format(f10.3,t60,
     1   ': Recharge from water bodies, in/yr')
 1180 format(f10.3,t60,
     1   ': Recharge from reservoirs, in/yr')
 1190 format(f10.3,t60,
     1   ': Recharge from beneath taro')
 1195 format(f10.3,t60,
     1   ': Urban irrigation factor (irrigation/PE)')
 1200 format(f10.3,t60,
     1   ': Water-main leakage, in/day')
 1210 format(f10.3,t60,
     1   ': Kealakehe disposal well discharge rate, MGD')
 1220 format(i10,t60,
     1   ': canopy-interception method')    
 1230 format(f10.5,t60,
     1   ': constant A or C in canopy-interception simulation equation')
 1240 format(f10.5,t60,
     1   ': constant B or D in canopy-interception simulation equation')
 1241 format(i10,t60,
     1   ': rainfall data type code (0-gridded month-year, ',
     2   '1-polygon-based normals')
 1242 format(i10,t60,
     1   ': number of month-year rainfall grid files')
 1243 format(i10,t60,
     1   ': number of months represented in rainfall grid files')
 1244 format(i10,t60,
     1   ': start year of rainfall grid files') 
 1250 format(i10,t60,
     1   ': monthly RF weighting factor code (0-no weights,1-weights)')
 1251 format(i10,t60,
     1   ': runoff data type (0-variable monthly RO:RF ratios,',
     2   ' 1-constant monthly RO:RF ratios)')
 1260 format(i10,t60,
     1   ': polygon print-out interval for hi_wb4.out')
 1270 format(i10,t60,
     1   ': yearly recharge print-out code (0,no print-1-print)')
 2010 format(a50,t60,  ': File with polygon information')
 2020 format(a50,t60,  ': File with land-cover information')
 2030 format(a50,t60,  ': File with monthly runoff-to-rainfall ratios')
 2040 format(a50,t60,  ': File with available water capacity data')
 2050 format(a50,t60,  ': File with fragments')
 2070 format(a50,t60,  ': File with monthly-to-annual pan ratios')
 2080 format(a50,t60,
     1   ': File with monthly fog parameters for linear relation w/rf')
 2090 format(a50,t60,
     2   ': File with irrigation information')
 2091 format(a50,t60,  ': File with modified Gash-model parameters')
 2095 format(a50,t60,  ': File with rainfall zone correlation rankings')
 2096 format(a50,t60,  ': File with sugarcane information')
 2097 format(a50,t60,  ': File with monthly rainfall filenames')
 2098 format(a50,t60,  ': File with monthly rainfall coefficients')
 2099 format(a50,t60,  ': File with monthly rainfall normals')
 2101 format(a50,t60,  ': File with variable monthly RO:RF filenames')

      return
      end

c----------------------------------------------------------------------------

      subroutine irrsched

c  subroutine to determine periods with potential sugarcane irrigation
c
c  day 1 of the cycle is the first day of sugarcane planting and irrigation
c
c  this subroutine assigns the following irrigation codes
c
c     -1 = fallow (not planted)
c      0 = unirrigated sugarcane
c      1 = drip- or furrow-irrigated sugarcane


      implicit real*8 (a-h,o-z)

      dimension id1(2),iddrip(28),idfurrow(28),ilusug(3)
      dimension idemsup(20),supirr(20),rmltirr(20),effirr(20),
     1          irrday(20,31),nirrdays(20,12),irrsug(50000)

      common /consti/nsim,nyrs,ndirr,ndunirr,ndfallow,id1,irfweights,
     1    nddrip,iddrip,ndfurrow,idfurrow,ndsug,istartyr,iendyr,nilusug,
     2    isugarirr,ipineirr,imacirr,istormdrain,iluper,icim,irfnorm,
     3    ilusug,ilupine,ilumac,iludag,ilucoff,ilucorn,iluwater,ilures,
     4    irechyrly,irotype,ilutaro,iluzrwb
      common /constr/sm0,rd0,tol1,pavint,wbrc,dripeff,furreff,
     1   wateravail,resrc,wmleak,dwrate,sprayeff,tarorc,
     2   watermain,canstore,dcif,constAC,constBD,tarofrac
      common /irrig/idemsup,supirr,rmltirr,effirr,irrday,nirrdays,irrsug

      write(*,*)'irrsched open'
c.....assign "ndirr" irrigated days
      do 200 j=1,ndirr
         irrsug(j)=1
 200  continue
c.....assign "ndunirr" unirrigated days
      do 220 j=ndirr+1,ndirr+ndunirr
         irrsug(j)=0
 220  continue
c.....assign "ndfallow" fallow days
      do 240 j=ndirr+ndunirr+1,ndsug
         irrsug(j)=-1
 240  continue
      write(*,*)'irrsched ran'

      return
      end

c----------------------------------------------------------------------------
      subroutine smccalc

c  subroutine to compute soil-moisture storage capacity for all soil types
c  (with potentially depth-varying available water capacity, AWC) and
c  root depths of 1-100 inches.  The computed soil-moisture storage
c  capacity values will be stored in an array that will be used to
c  determine the soil-moisture storage capacity for each polygon
c  if AWC information is not available for greater root depths, then the
c  deepest AWC value is extrapolated to the desired root depth


      implicit real*8 (a-h,o-z)

      dimension ipoly(590000),area(590000),rfmon(50000,1200),
     1   ilu(590000),ifld(590000),smc(590000),perv(590000),
     2   iro(590000),isoil(590000),panann(590000),ifog(590000),
     3   ietzone(590000),irfzone(590000),iplant(590000),
     4   ifogelev(590000),iwatmain(590000),idispwell(590000),
     5   icpsepsewer(590000),iasys(590000),cprate(590000),
     6   irfcell(590000),ilup(590000,12),irrcode(50),rfnorm(590000,12),
     7   isdrain(590000),canfrac(590000),tfrac(590000),cerf(590000),
     8   romon(1000,1200),fdrfratio(500,100,12)
      dimension rd(50),df5mm(50),pancoef(50,12),
     1   awc(500,5,8),pct(500,5,8),zt(500,5,8),
     2   zb(500,5,8),nlay(500,5),nseq(500),smca(500,100),fogeff(50),
     3   rfw(10,25),spancoef(50,4),cancap(50),tcap(50),rfwt(50000,12)

      common /poly/ipoly,area,rfmon,ilu,ifld,smc,iasys,
     1   iro,isoil,panann,ifog,ietzone,irfzone,iplant,npoly,nplant,
     2   ifogelev,iwatmain,idispwell,icpsepsewer,irfcell,ilup,
     3   perv,cprate,isdrain,canfrac,tfrac,cerf
      common /landr/rd,df5mm,pancoef,fogeff,spancoef,cancap,tcap
      common /soili/nlay,nseq,nsoil
      common /soilr/awc,pct,zt,zb,smca

      write(*,*)'smccalc open'

      do 520 i=1,nsoil
      do 500 k=1,100
c........root depth is defined by k (1-100 inches)
         z=dfloat(k)

c........compute area-weighted soil-moisture storage capacity and account for
c        depth-varying AWC
         smca(i,k)=0.
         do 420 j=1,nseq(i)
         do 400 l=1,nlay(i,j)
            if(z.ge.zt(i,j,l).and.z.le.zb(i,j,l))then
c...........bottom of root zone is within current layer
               smca(i,k)=smca(i,k)+pct(i,j,l)*awc(i,j,l)*(z-zt(i,j,l))
            elseif(z.gt.zb(i,j,l).and.l.eq.nlay(i,j))then
c...........bottom of root zone is below current layer, which is the
c           bottom layer; extrapolate bottom layer down to root depth
               smca(i,k)=smca(i,k)+pct(i,j,l)*awc(i,j,l)*(z-zt(i,j,l))
            elseif(z.gt.zb(i,j,l).and.l.lt.nlay(i,j))then
c...........bottom of root zone is below current layer, which is not the
c           bottom layer; include entire current layer
               smca(i,k)=smca(i,k)+pct(i,j,l)*awc(i,j,l)*
     1         (zb(i,j,l)-zt(i,j,l))
c...........exit if bottom of root zone is above current layer
            elseif(z.lt.zt(i,j,l))then
               goto 420
            endif
 400     continue
 420     continue
 440     continue

 500  continue
 520  continue
      write(*,*)'depth-varying AWC routine complete'
c.....compute soil-moisture storage capacity by polygon
      do 600 ip=1,npoly
         root=rd(ilu(ip))
         smc(ip)=smca(isoil(ip),nint(root))
 600  continue
 
      write(*,*)'smccalc ran'

      open (50,file='output\smc.out')
      ip=0

      do 700 ip=1,npoly
         write(50,1650)ip,smc(ip)
 700  continue 

 1650 format(i6,2x,f7.2)

      return
      end

c----------------------------------------------------------------------------
      subroutine fields

c  subroutine to select initial sugarcane-field configuration for each plantation
c
c  assumes approximately half of field area in each plantation is in one 
c  stage of growth and the other half are in another stage of growth:
c
c  must know how many fields of each type, area of each field


      implicit real*8 (a-h,o-z)

      real*4 rn,rnuse(10000)

      dimension id1(2),iddrip(28),idfurrow(28),ilusug(3)
      dimension ipoly(590000),area(590000),rfmon(50000,1200),
     1   ilu(590000),ifld(590000),smc(590000),perv(590000),
     2   iro(590000),isoil(590000),panann(590000),ifog(590000),
     3   ietzone(590000),irfzone(590000),iplant(590000),
     4   ifogelev(590000),iwatmain(590000),idispwell(590000),
     5   icpsepsewer(590000),iasys(590000),cprate(590000),
     6   irfcell(590000),ilup(590000,12),irrcode(50),rfnorm(590000,12),
     7   isdrain(590000),canfrac(590000),tfrac(590000),cerf(590000),
     8   romon(1000,1200),fdrfratio(500,100,12)
      dimension aplant(7),nfld(7),afld(7,500),ileft(500),icrop1(7,500)
      dimension icrst(590000),nrfzcro(12,50,50)
      dimension idemsup(20),supirr(20),rmltirr(20),effirr(20),
     1          irrday(20,31),nirrdays(20,12),irrsug(50000)

      common /consti/nsim,nyrs,ndirr,ndunirr,ndfallow,id1,irfweights,
     1    nddrip,iddrip,ndfurrow,idfurrow,ndsug,istartyr,iendyr,nilusug,
     2    isugarirr,ipineirr,imacirr,istormdrain,iluper,icim,irfnorm,
     3    ilusug,ilupine,ilumac,iludag,ilucoff,ilucorn,iluwater,ilures,
     4    irechyrly,irotype,ilutaro,iluzrwb
      common /constr/sm0,rd0,tol1,pavint,wbrc,dripeff,furreff
     1   wateravail,resrc,wmleak,dwrate,sprayeff,tarorc,
     2   watermain,canstore,dcif,constAC,constBD,tarofrac
      common /poly/ipoly,area,rfmon,ilu,ifld,smc,iasys,
     1   iro,isoil,panann,ifog,ietzone,irfzone,iplant,npoly,nplant,
     2   ifogelev,iwatmain,idispwell,icpsepsewer,irfcell,ilup,
     3   perv,cprate,isdrain,canfrac,tfrac,cerf
      common /seeder/iseed
      common /field/icrst
      common /irrig/idemsup,supirr,rmltirr,effirr,irrday,nirrdays,irrsug


c.....determine number and area of fields of each sugarcane plantation
      do 100 i=1,nplant
         aplant(i)=0.
         nfld(i)=0
 100  continue
 
      do 120 j=1,nilusug
      do 120 i=1,npoly
         if(ilu(i).eq.ilusug(j).and.iplant(i).ne.0)then
            aplant(iplant(i))=aplant(iplant(i))+area(i)
            afld(iplant(i),ifld(i))=afld(iplant(i),ifld(i))+area(i)
            if(ifld(i).gt.nfld(iplant(i)))nfld(iplant(i))=ifld(i)
         endif
 120  continue

      do 125 i=1,nplant
      do 125 j=1,nfld(i)
         write(5,'(2i6,2x,f12.2)')i,j,afld(i,j)
 125  continue
      write(5,*)'field number and area calc complete'

c.....select ~50% (by area) of fields (within prescribed tolerance) of
c     each irrigation type.
c     selected fields will be started in crop stage 1
c     must not select a field more than once (sample without replacement)

c.....set initial tolerance (allowable error) for selecting 50% area
      ino=iseed-1
      do 400 i=1,nplant
         tol=tol1
c........if only one field of type i, exit loop
         if(nfld(i).eq.1)then
            icrop1(i,1)=1
            goto 400
         endif
c........initialize variables
 1       nretry1=0
 2       nleft=nfld(i)
         ahalf=0.
         do 340 j=1,nfld(i)
            ileft(j)=j
            icrop1(i,j)=0
            ahalf=0
 340     continue
 
         do 380 j=1,nfld(i)
c...........randomly select a field
 3          ino=ino+9999
            rn=ran(ino)
            if(rn.gt.0.99999999)rn=0.9999
            iuse=int(nleft*rn)+1
            icrop1(i,j)=ileft(iuse)
            nleft=nleft-1
            do 360 jj=iuse,nleft
               ileft(jj)=ileft(jj+1)
 360        continue
c...........check if ~50% of field area selected
            ahalf=ahalf+afld(i,icrop1(i,j))
            if(ahalf.gt.(0.5-tol)*aplant(i).and.
     1         ahalf.lt.(0.5+tol)*aplant(i))then
c..............exit loop, about 50% of area (within tolerance) selected
               write(5,'(i1,2x,2f12.1,2x,f5.3)')i,ahalf,aplant(i),tol
               goto 385
            elseif(ahalf.ge.(0.5+tol)*aplant(i))then
c..............more than 50% (plus tolerance) of area selected
               if(nretry1.le.100)then
c..............reselect fields, less than 100 tries so far
                  nretry1=nretry1+1
                  goto 2
            elseif(nretry1.gt.100)then
c.................reset tolerance (double) and restart field selection
c                 note that if tol>0.5, then loop will be exited after only
c                 one field is selected
                  tol=tol*2.
                  goto 1
              endif
            elseif(ahalf.le.(0.5-tol)*aplant(i))then
c..............continue selecting fields, less than 50% (minus tolerance)
c              of area selected
            endif
 380     continue
 385     continue

         do 386 j=1,nfld(i)
            write(5,'(3i6)')i,j,icrop1(i,j)
 386     continue


 400  continue
      write(5,*)
      write(5,*)

c.....determine crop stage by polygon
      do 540 i=1,npoly
         icrst(i)=2
            if(iplant(i).ne.0)then
            do 500 j=1,nfld(iplant(i))
               if(ifld(i).eq.icrop1(iplant(i),j))then
                  icrst(i)=1
                  goto 520
               endif
 500        continue
            endif
 520  continue
 540  continue



      return
      end

c----------------------------------------------------------------------------

      subroutine fraguse

c  subroutine to select fragments to be used in water-budget simulation
c
c  assume that the same fragment sets are to be used for a particular
c  fragment zone and month
    
      implicit real*8 (a-h,o-z)

      real*4 rn

      dimension id1(2),iddrip(28),idfurrow(28),ilusug(3)
      dimension nfr(12,100),frg(12,100,500,31),jfr(12,100,500)

      common /consti/nsim,nyrs,ndirr,ndunirr,ndfallow,id1,irfweights,
     1    nddrip,iddrip,ndfurrow,idfurrow,ndsug,istartyr,iendyr,nilusug,
     2    isugarirr,ipineirr,imacirr,istormdrain,iluper,icim,irfnorm,
     3    ilusug,ilupine,ilumac,iludag,ilucoff,ilucorn,iluwater,ilures,
     4    irechyrly,irotype,ilutaro,iluzrwb
      common /constr/sm0,rd0,tol1,pavint,wbrc,dripeff,furreff,
     1   wateravail,resrc,wmleak,dwrate,sprayeff,tarorc,
     2   watermain,canstore,dcif,constAC,constBD,tarofrac
      common /fragi/nmbrrfz,nfr,jfr,isimulation
      common /fragr/frg
      common /seeder/iseed

c.....select fragment set for each month, fragment zone, and year of simulation.
c     for a given month and fragment zone, a particular fragment set can
c     be selected more than once
      ino=iseed-1

      do 100 m=1,12
      do 100 j=1,nmbrrfz
      do 100 i=1,nyrs
         ino=ino+1
         rn=ran(ino)
         if(rn.gt.0.99999999)rn=0.9999
         jfr(m,j,i)=int(nfr(m,j)*rn)+1
         write(7,901)isimulation,m,j,i,rn,jfr(m,j,i)
 100  continue
 901  format(i2,1x,i2,1x,i3,1x,i2,1x,f10.8,1x,i4) 
         
      return
      end

c----------------------------------------------------------------------------
     
      subroutine pcoef(ip,ndc,pc)

c  subroutine to determine daily pan coefficient for current polygon
c  land cover and day of cycle

      implicit real*8 (a-h,o-z)

      dimension ipoly(590000),area(590000),rfmon(50000,1200),
     1   ilu(590000),ifld(590000),smc(590000),perv(590000),
     2   iro(590000),isoil(590000),panann(590000),ifog(590000),
     3   ietzone(590000),irfzone(590000),iplant(590000),
     4   ifogelev(590000),iwatmain(590000),idispwell(590000),
     5   icpsepsewer(590000),iasys(590000),cprate(590000),
     6   irfcell(590000),ilup(590000,12),irrcode(50),rfnorm(590000,12),
     7   isdrain(590000),canfrac(590000),tfrac(590000),cerf(590000),
     8   romon(1000,1200),fdrfratio(500,100,12)
      dimension rd(50),df5mm(50),pancoef(50,12),idaypan(50,5),isd(50),
     1   icd(50,5),rrr(50,12),awc(500,5,8),pct(500,5,8),zt(500,5,8),
     2   zb(500,5,8),nlay(500,5),nseq(500),smca(500,100),fogeff(50),
     3   rfw(10,25),spancoef(50,4),cancap(50),tcap(50),rfwt(50000,12)

      common /poly/ipoly,area,rfmon,ilu,ifld,smc,iasys,
     1   iro,isoil,panann,ifog,ietzone,irfzone,iplant,npoly,nplant,
     2   ifogelev,iwatmain,idispwell,icpsepsewer,irfcell,ilup,
     3   perv,cprate,isdrain,canfrac,tfrac,cerf
      common /landi/nlu,idaypan,icd,irrcode,isd
      common /landr/rd,df5mm,pancoef,fogeff,spancoef,cancap,tcap


      if(ndc.le.icd(ilu(ip),1))then
c........initial growing stage
         pc=spancoef(ilu(ip),1)
      elseif(ndc.le.icd(ilu(ip),2))then
c........developmental stage (linearly interpolate)
         pc=spancoef(ilu(ip),1) +
     1      (spancoef(ilu(ip),2)-spancoef(ilu(ip),1))*
     2      (ndc-icd(ilu(ip),1))/dfloat(icd(ilu(ip),2)-icd(ilu(ip),1))
      elseif(ndc.le.icd(ilu(ip),3))then
c........middle stage
         pc=spancoef(ilu(ip),2)
      elseif(ndc.le.icd(ilu(ip),4))then
c........late stage (linearly interpolate)
         pc=spancoef(ilu(ip),2) +
     1      (spancoef(ilu(ip),3)-spancoef(ilu(ip),2))*
     2      (ndc-icd(ilu(ip),3))/dfloat(icd(ilu(ip),4)-icd(ilu(ip),3))
      else
c........fallow stage
         pc=spancoef(ilu(ip),4)
      endif

      return
      end

c----------------------------------------------------------------------------

      subroutine rech(isim,ip,totrf,totfog,totirr,totro,
     1   totae1,totrc1,totae2,totrc2,totae3,totrc3,totae4,totrc4,
     2   totdirect,totpnet,totcanint,totrfadj,totseptic,totsd,
     3   yrlyrech)

c  subroutine to compute daily water budgets using 4 different AE/PE models
c  (step function of Veihmeyer, root constant model of Giambelluca, root
c  constant model from FAO 56, and linear function of Thornthwaite and Mather)

c  assumptions:
c    precipitation (including fog, but less average runoff) on paved
c    areas contributes to a concentrated water input on pervious areas
c    agricultural irrigation does not run off
c    recharge occurs from the bottom of the plant root depth
c    evaporation from paved areas occurs at pan rate
c    pan coefficient is not temporally variable


      implicit real*8 (a-h,o-z)

      real*8 mrf,mrfa,Psat

      dimension id1(2),iddrip(28),idfurrow(28),ilusug(3)
      dimension ipoly(590000),area(590000),rfmon(50000,1200),
     1   ilu(590000),ifld(590000),smc(590000),perv(590000),
     2   iro(590000),isoil(590000),panann(590000),ifog(590000),
     3   ietzone(590000),irfzone(590000),iplant(590000),
     4   ifogelev(590000),iwatmain(590000),idispwell(590000),
     5   icpsepsewer(590000),iasys(590000),cprate(590000),
     6   irfcell(590000),ilup(590000,12),irrcode(50),rfnorm(590000,12),
     7   isdrain(590000),canfrac(590000),tfrac(590000),cerf(590000),
     8   romon(1000,1200),fdrfratio(500,100,12)
      dimension rd(50),df5mm(50),pancoef(50,12),idaypan(50,5),isd(50),
     1   icd(50,5),rrr(50,12),awc(500,5,8),pct(500,5,8),zt(500,5,8),
     2   zb(500,5,8),nlay(500,5),nseq(500),smca(500,100),fogeff(50),
     3   rfw(10,25),spancoef(50,4),cancap(50),tcap(50),rfwt(50000,12)
      dimension nfr(12,100),frg(12,100,500,31),jfr(12,100,500)
      dimension rfwtmon(50,100,12),etmwf(50000,12),af(12),bf(12)
      dimension idays(12),f(32)
      dimension icrst(590000),nrfzcro(12,50,50)
      dimension totrf(12),totfog(12),totirr(12),totro(12),
     1   totae1(12),totrc1(12),totae2(12),totrc2(12),totae3(12),
     2   totrc3(12),totae4(12),totrc4(12),totpe(12),totdirect(12),
     3   totpnet(12),totcanint(12),totrfadj(12),totae1a(12),totrc1a(12),
     4   totae2a(12),totrc2a(12),totae3a(12),totrc3a(12),totae4a(12),
     5   totrc4a(12),totseptic(12),totsd(12)
c.....dso20180509 add line above and comment out line below
c     5   totrc4a(12),totseptic(12),totsd(12),yrlyrech(590000,30)
      dimension idemsup(20),supirr(20),rmltirr(20),effirr(20),
     1          irrday(20,31),nirrdays(20,12),irrsug(50000)
      dimension stormdrain(590000),ceint(6)
c.....dso20180509 add following dimension statement for lblyr array
      dimension lblyr(200)
c.....dso20180509 add following dimension statement for yrlyrech array
      dimension yrlyrech(npoly,nsim)

      common /consti/nsim,nyrs,ndirr,ndunirr,ndfallow,id1,irfweights,
     1    nddrip,iddrip,ndfurrow,idfurrow,ndsug,istartyr,iendyr,nilusug,
     2    isugarirr,ipineirr,imacirr,istormdrain,iluper,icim,irfnorm,
     3    ilusug,ilupine,ilumac,iludag,ilucoff,ilucorn,iluwater,ilures,
     4    irechyrly,irotype,ilutaro,iluzrwb
      common /constr/sm0,rd0,tol1,pavint,wbrc,dripeff,furreff,
     1   wateravail,resrc,wmleak,dwrate,sprayeff,tarorc,
     2   watermain,canstore,dcif,constAC,constBD,tarofrac
      common /poly/ipoly,area,rfmon,ilu,ifld,smc,iasys,
     1   iro,isoil,panann,ifog,ietzone,irfzone,iplant,npoly,nplant,
     2   ifogelev,iwatmain,idispwell,icpsepsewer,irfcell,ilup,
     3   perv,cprate,isdrain,canfrac,tfrac,cerf
      common /landi/nlu,idaypan,icd,irrcode,isd
      common /landr/rd,df5mm,pancoef,fogeff,spancoef,cancap,tcap
      common /runoff/rrr,romon,nrrr
      common /soili/nlay,nseq,nsoil
      common /soilr/awc,pct,zt,zb,smca
      common /fragi/nmbrrfz,nfr,jfr,isimulation
      common /fragr/frg
      common /rain/rfwtmon,nrfzcro,rfw,rfwt,rfnorm,irfstyr
      common /evap/etmwf
      common /fogdrip/fdrfratio
      common /irrig/idemsup,supirr,rmltirr,effirr,irrday,nirrdays,irrsug
      common /seeder/iseed
      common /field/icrst
      common /npfog/slpnpfog,yintnpfog
      common /canint/gashcal,ceint,ifm
      common /nprint/jprint,iprint

      data idays/31,28,31,30,31,30,31,31,30,31,30,31/      

c.....dso20180509 add following if-endif section
      if(irechyrly.eq.1.and.ip.eq.1.and.isim.eq.1)then
         open(43,file='output\FAO_rech_yr_in.csv')
         do 15 i=1,npoly
         do 15 j=1,nyrs
            yrlyrech(i,j)=0.d0
 15      continue
         do 17 iyrs=istartyr,iendyr
            lblyr(iyrs)=iyrs
 17      continue
         write(43,4200)(lblyr(iyrs),iyrs=istartyr,iendyr)
      endif
      
      icorncheck=1
      
c.....initialize total water budget arrays for each polygon
10    do 20 m=1,12
         totrf(m)=0.
         totfog(m)=0.
         totirr(m)=0.
         totro(m)=0.
         totpe(m)=0.
         totae1(m)=0.
         totae2(m)=0.
         totae3(m)=0.
         totae4(m)=0.
         totrc1(m)=0.
         totrc2(m)=0.
         totrc3(m)=0.
         totrc4(m)=0.
         totdirect(m)=0.
         totpnet(m)=0.
         totcanint(m)=0.
         totrfadj(m)=0.
         totseptic(m)=0.
         totsd(m)=0.
20    continue

      polypan=0.
      rechann=0.
      
c.....initialize direct-recharge totals
      watermain=0.
      dispwell=0.
      cesspool=0.

c.....initialize storm-drain capture
      wstormdrain=0.

c.....initialize root depth
      root=rd(ilu(ip))

c.....set previous day's rain and fog to zero (can affect canopy interception)
      drfy=0.d0
      dfogy=0.d0
      
c.....determine if polygon land cover is fog forest and adjust pan coefficient by changing land-cover code
!      if(ilu(ip).eq.10.and.ifog(ip).eq.1)ilu(ip)=18
!      if(ilu(ip).eq.11.and.ifog(ip).eq.1)ilu(ip)=19
!      if(ilu(ip).eq.17.and.ifog(ip).eq.1)ilu(ip)=20
      
c.....determine soil-moisture storage capacity values
      smctb=smca(isoil(ip),nint(root))
      smct=smca(isoil(ip),nint(rd0))
      smcb=smctb-smct
      smp=pavint*sm0

c.....initialize values for Veihmeyer model
      sm1=smctb*sm0
      sm1t=smct*sm0
      sm1b=sm1-sm1t
      sm1begin=sm1
      

c.....initialize values for FAO model
      sm2=smctb*sm0
      sm2t=smct*sm0
      sm2b=sm2-sm2t
      sm2begin=sm2
      
c.....determine FAO unadjusted root constant (see FAO 56, table 22)
c     program adjusts below for crop PE (p. 162)
c     user may consider adjustments for soil texture (p. 167)
      p22=df5mm(ilu(ip))

c.....initialize values for Giambelluca model
      sm3=smctb*sm0
      sm3t=smct*sm0
      sm3b=sm3-sm3t
      sm3begin=sm3
      
c.....initialize values for Thornthwaite model
      sm4=smctb*sm0
      sm4t=smct*sm0
      sm4b=sm4-sm4t
      sm4begin=sm4
 
c.....determine initial day in the initial sugarcane irrigation cycle
      do n=1,3
       if(ilu(ip).eq.ilusug(n))then
          ndicyc=id1(icrst(ip))
          nicyc=1
       endif
      end do

c.....assume pan coefficient cycle starts on initial day, except if sugar
      ndpcyc=1
      
      do n=1,3
       if(ilu(ip).eq.ilusug(n))ndpcyc=ndicyc
      end do

c.....begin water-budget computations--run for "nyrs" years
      do 500 i=1,nyrs

      do 400 m=1,12

c........determine number of days in the month (ignore leap year)
         nd=idays(m)
c........to include leap year uncomment line below AND modify program
c        if(m.eq.2.and.mod(i,4).eq.0)nd=nd+1
          
          if(irfnorm.eq.0)then
c........determine rainfall for current month from month-year rainfall grid data
          monthrf=m+(istartyr+(i-1)-irfstyr)*12
          mrf=rfmon(irfcell(ip),monthrf)

c.........apply monthly rainfall weighting factor if indicated
            if(irfweights.eq.1)then
                mrf=mrf*rfwt(irfcell(ip),m)
            endif

          elseif(irfnorm.eq.1)then
c........determine rainfall for current month from rainfall normal data
            mrf=rfnorm(ip,m)
          
c.........apply monthly rainfall weighting factor if indicated
            if(irfweights.eq.1)then
                mrf=mrf*rfwtmon(irfcell(ip),i,m)
            endif
          endif  
          
c........determine runoff for current month
          if(irotype.eq.0)then
            ro=romon(iro(ip),monthrf)*mrf
          else
            ro=rrr(iro(ip),m)*mrf
          endif

c...........determine monthly fog drip for current polygon (ratio varies by elevation)
          fog=0.d0
          if(ifog(ip).gt.0)then
             fog=fdrfratio(ifog(ip),ifogelev(ip),m)*fogeff(ilu(ip))*mrf
          endif

c...........determine monthly net precipitation if canopy-interception method 1 is selected
          pnet=0.d0
          if(icim.eq.1.and.fogeff(ilu(ip)).gt.0.5)then
            pnet=mrf*(constBD+constAC*fog)
          endif

!c...........check if runoff greater than net precipitation
!          if(ro.gt.pnet)then
!             pnet=ro
!
!c...........adjust rainfall/fog for forested areas inside fog zone
!              if(ilu(ip).ge.18)then
!                mrfa=(-yintnpfog+(yintnpfog**2-4*slpnpfog*
!     1              fdrfratio(ifog(ip),ifogelev(ip),m)*-ro)**0.5)/
!     2              (2*slpnpfog*fdrfratio(ifog(ip),ifogelev(ip),m))
!                fog=fdrfratio(ifog(ip),ifogelev(ip),m)*
!     1              fogeff(ilu(ip))*mrfa
!
!c...........adjust rainfall/fog for shrub areas inside fog zone
!              elseif(ilu(ip).eq.13.and.ifog(ip).gt.0)then
!                mrfa=ro/(1+fogeff(ilu(ip))*
!     1              fdrfratio(ifog(ip),ifogelev(ip),m))
!                fog=fdrfratio(ifog(ip),ifogelev(ip),m)*
!     1              fogeff(ilu(ip))*mrfa
!                
!c...........adjust rainfall for forested areas outside fog zone
!              elseif(ilu(ip).eq.10.or.ilu(ip).eq.11
!     1               .or.ilu(ip).eq.17)then
!                mrfa=ro/yintnpfog
!                
!c...........adjust rainfall for all other circumstances                
!              else
!                mrfa=ro
!              endif
!              totrfadj(m)=totrfadj(m)+(mrfa-mrf)
!              mrf=mrfa
!          endif

c...........compute canopy interception for month
!          canint=mrf+fog-pnet

c...........check if rainfall is above limit for canopy evaporation to occur
!          if(ifog(ip).gt.0)then  
!            raincheck=(1+fdrfratio(ifog(ip),ifogelev(ip),m)-yintnpfog)/
!     1                (slpnpfog*fdrfratio(ifog(ip),ifogelev(ip),m))
!
!            if(fogeff(ilu(ip)).gt.0.and.ifog(ip).gt.0.and.
!     1         mrf.gt.raincheck)then
!                canint=0.d0
!                pnet=mrf+fog
!            endif
!          endif     

c........determine daily pan evaporation rate for current month

         pan=panann(ip)*etmwf(ietzone(ip),m)/dfloat(nd)

c........PERFORM DAILY BUDGETS
         do 300 k=1,nd

c...........synthesize daily rainfall
            drf=0.d0
            drf=frg(m,irfzone(ip),jfr(m,irfzone(ip),i),k)*mrf
c...........determine fog drip for current polygon
            dfog=0.d0
            if(ifog(ip).gt.0)then
               dfog=frg(m,irfzone(ip),jfr(m,irfzone(ip),i),k)*fog
            endif

c...........determine daily net precipitation and canopy interception..........................................................................................................................
            dpnet=0.d0
            dcanint=0.d0
            if(fogeff(ilu(ip)).gt.0.5.and.drf.gt.0.0001)then
              if(icim.eq.2)then
                    if(ifm.eq.1)then
                        ceint2=ceint(1)
                    elseif(ifm.eq.2)then
                        if(drfy+dfogy.ge.ceint(2))then
                            ceint2=ceint(3)
                        elseif(drfy+dfogy.ge.ceint(4))then
                            ceint2=ceint(5)
                        else
                            ceint2=1.
                        endif
                    elseif(ifm.eq.3)then
                        ceint2=exp(ceint(6)*(drfy+dfogy))               
                    else
                        ceint2=1.
                    endif
                Psat=-(cancap(ilu(ip))/(canfrac(ip)*cerf(ip)))*
     1               log((1-cerf(ip))/(1-(1-ceint2)*cerf(ip)))
                if(drf+dfog.lt.Psat)then
                   dcanint=canfrac(ip)*(drf+dfog)
                elseif(drf+dfog.gt.tcap(ilu(ip))/tfrac(ip))then
                   dcanint=canfrac(ip)*Psat+canfrac(ip)*cerf(ip)*
     1                     (drf+dfog-Psat)+tcap(ilu(ip))
                else
                   dcanint=canfrac(ip)*Psat+canfrac(ip)*cerf(ip)*
     1                     (drf+dfog-Psat)+tfrac(ip)*(drf+dfog)
                endif
                dcanint=gashcal*dcanint
                dpnet=drf+dfog-dcanint
              elseif(icim.eq.3)then
                dpnet=(drf+dfog)*(constAC*exp(constBD/(drf+dfog)))
                dcanint=drf+dfog+dpnet
              else
                dpnet=frg(m,irfzone(ip),jfr(m,irfzone(ip),i),k)*pnet
              endif  
            else
              dpnet=drf+dfog
            endif

c...........determine daily runoff
            dro=frg(m,irfzone(ip),jfr(m,irfzone(ip),i),k)*ro

c...........compute daily canopy interception
            dcanint=0.d0
c            dcanint=frg(m,irfzone(ip),jfr(m,irfzone(ip),i),k)*canint
            dcanint=drf+dfog-dpnet

c...........determine mean daily potential et
            if(ilu(ip).eq.ilucorn.and.icorncheck.eq.1)then
                pe=0.25*pan
            elseif(ilu(ip).eq.ilusug(1).or.ilu(ip).eq.ilusug(2).or.
     1             ilu(ip).eq.ilusug(3))then   
                call pcoef(ip,ndpcyc,pc)
                pe=pc*pan
            else
                pe=pancoef(ilu(ip),m)*pan   
            endif

c...........determine irrigation and root depth for current polygon
            agirr=0.d0

            if(irrcode(ilu(ip)).gt.0)then
                if(ilu(ip).eq.ilusug(1).or.ilu(ip).eq.ilusug(2).or.
     1             ilu(ip).eq.ilusug(3))then   
                    if(irrsug(ndicyc).eq.-1)then
                            root=rd0
                    elseif(irrsug(ndicyc).eq.1.and.
     1                     irrday(irrcode(ilu(ip)),k).eq.1)then
                        if(idemsup(irrcode(ilu(ip))).eq.1)then
                            agirr=rmltirr(irrcode(ilu(ip)))*
     1                      (pe*nd-mrf+ro)/(effirr(irrcode(ilu(ip)))*
     2                      dfloat(nirrdays(irrcode(ilu(ip)),m)))
                        elseif(idemsup(irrcode(ilu(ip))).eq.2)then
                            agirr=rmltirr(irrcode(ilu(ip)))*
     1                      supirr(irrcode(ilu(ip)))/
     2                      (effirr(irrcode(ilu(ip)))* 
     3                      dfloat(nirrdays(irrcode(ilu(ip)),m)))
                        endif
                    endif    
                else
                    if(irrday(irrcode(ilu(ip)),k).eq.1)then
                        if(idemsup(irrcode(ilu(ip))).eq.1)then
                            agirr=rmltirr(irrcode(ilu(ip)))*
     1                      (pe*nd-mrf+ro)/(effirr(irrcode(ilu(ip)))* 
     2                      dfloat(nirrdays(irrcode(ilu(ip)),m)))
                        elseif(idemsup(irrcode(ilu(ip))).eq.2)then
                            agirr=rmltirr(irrcode(ilu(ip)))*
     1                      supirr(irrcode(ilu(ip)))/
     2                      (effirr(irrcode(ilu(ip)))* 
     3                      dfloat(nirrdays(irrcode(ilu(ip)),m)))
                        endif
                    endif    
                endif
            endif

            if(agirr.lt.0.)agirr=0.d0

c...........determine next day in irrigation and pan coefficient cycles
c...........for sugarcane
            do n=1,3
            if(ilu(ip).eq.ilusug(n))then
               ndicyc=ndicyc+1
               if(ndicyc.gt.ndsug)then
                  ndicyc=1
                  nicyc=nicyc+1
               endif
               ndpcyc=ndicyc
            endif
            end do

c...........update cumulative water-budget component totals
            totrf(m)=totrf(m)+drf
            totfog(m)=totfog(m)+dfog
            totirr(m)=totirr(m)+agirr*perv(ip)
            totro(m)=totro(m)+dro
            totpe(m)=totpe(m)+pe
            totpnet(m)=totpnet(m)+dpnet
            totcanint(m)=totcanint(m)+dcanint
            polypan=polypan+pan

c...........check if current polygon is a water body or reservoir
            if(ilu(ip).eq.iluwater.or.ilu(ip).eq.ilures
     1         .or.ilu(ip).eq.iluzrwb)then
    
c..............initialize yearly totals    
               if(i.eq.nyrs.and.k.eq.nd)then
                  sm1begin=0.
                  sm2begin=0.
                  sm3begin=0.
                  sm4begin=0.
                  sm1=0.
                  sm2=0.
                  sm3=0.
                  sm4=0.
                  totro(m)=0.
                  if(ilu(ip).eq.iluwater)totrc1(m)=wbrc*nyrs/12.0
                  if(ilu(ip).eq.ilures)totrc1(m)=resrc*nyrs/12.0
                  if(ilu(ip).eq.iluzrwb)totrc1(m)=0.0
                  totae1(m)=totpe(m)
                  totrc2(m)=totrc1(m)
                  totrc3(m)=totrc1(m)
                  totrc4(m)=totrc1(m)
                  totae2(m)=totae1(m)
                  totae3(m)=totae1(m)
                  totae4(m)=totae1(m)
                  totdirect(m)=totrc1(m)
                  if(m.eq.12) goto 600
                  goto 400
               endif
               goto 300
            endif
        
c...........account for recharge from leaking water mains
            if(iwatmain(ip).eq.1)then
               watermain=watermain+wmleak
               totrc1(m)=totrc1(m)+wmleak
               totrc2(m)=totrc2(m)+wmleak
               totrc3(m)=totrc3(m)+wmleak
               totrc4(m)=totrc4(m)+wmleak
               totdirect(m)=totdirect(m)+wmleak
            endif

c...........account for recharge from disposal wells
            if(idispwell(ip).eq.1)then
c..............dso20180510 use exact conversion, and assume dwrate is in Mgal/d
c               dw=(dwrate*0.00379/area(ip))*39.37
               dw=(dwrate/area(ip))*231*0.0254d0*0.0254d0*365*1000000
               dispwell=dispwell+dw
               totrc1(m)=totrc1(m)+dw
               totrc2(m)=totrc2(m)+dw
               totrc3(m)=totrc3(m)+dw
               totrc4(m)=totrc4(m)+dw
               totdirect(m)=totdirect(m)+dw
            endif

c...........account for recharge from cesspools
            if(icpsepsewer(ip).eq.1)then
            cp=cprate(ip)
            cesspool=cesspool+cp
            totrc1(m)=totrc1(m)+cp
            totrc2(m)=totrc2(m)+cp
            totrc3(m)=totrc3(m)+cp
            totrc4(m)=totrc4(m)+cp
            totdirect(m)=totdirect(m)+cp
            endif

c...........account for additional soil moisture from septic systems
          if(icpsepsewer(ip).eq.2)then
            septic=cprate(ip)
          else
            septic=0.
          endif

          totseptic(m)=totseptic(m)+septic*perv(ip)

c...........account for water on paved areas (no irrigation on paved areas)
            wadd=0.
            aepaved=0.
            wstormdrain=0.

            if(perv(ip).lt.0.999)then
               x1=smp+dpnet-dro
               if(x1.lt.0)x1=0
               if(x1.gt.pavint)then
c.................water contribution from paved to unpaved areas
                  smp=pavint
                  wadd=(x1-pavint)*(1-perv(ip))
c.....................account for presence of storm drains in urban areas
                      if(istormdrain.eq.1.and.isdrain(ip).eq.1.and.
     1                isd(ilu(ip)).eq.1)then
                         wstormdrain=wadd
                         wadd=0.  
                         totsd(m)=totsd(m)+wstormdrain
                      endif
               else
c.................precipitation on paved areas fully intercepted
                  smp=x1
               endif
c..............update intercepted moisture depth
               if(smp.gt.pan)then
                  aepaved=pan
                  smp=smp-pan
               else
                  aepaved=smp
                  smp=0.
               endif
            endif

c...........update run-on total over entire area
            polyron=polyron+wadd

c...........special irrigation rate for taro
            if(ilu(ip).eq.ilutaro)then
                totirr(m)=totirr(m)+tarorc*tarofrac/365.0
            endif    

c...........VEIHMEYER'S MODEL

            sm1hold=sm1
c...........compute budget for pervious area
            if(root.lt.0.99*rd(ilu(ip)).or.
     1         root.gt.1.01*rd(ilu(ip)))then
c..............fallow period, use 2-layer model
               x1=sm1t+agirr+ septic+((dpnet-dro)*perv(ip)
     1            + wadd ) / perv(ip)
               if(x1.lt.0)x1=0
               if(pe.le.x1)then
                  ae=pe
                  x2=x1-ae
               else
                  ae=x1
                  x2=0.
               endif
               if(x2.gt.smct)then
                  rct=x2-smct
                  sm1t=smct
               else
                  rct=0.
                  sm1t=x2
               endif
               x2=sm1b+rct
               if(x2.gt.smcb)then
                  rc=x2-smcb
                  sm1b=smcb
               else
                  rc=0.
                  sm1b=x2
               endif
               sm1=sm1t+sm1b
            else
c..............planted period, use 1-layer model
               x1=sm1+agirr+septic+((dpnet-dro)*perv(ip)
     1            + wadd ) / perv(ip)
               if(x1.lt.0)x1=0
               if(pe.le.x1)then
                  ae=pe
                  x2=x1-ae
               else
                  ae=x1
                  x2=0.
               endif
               if(x2.gt.smctb)then
                  rc=x2-smctb
                  sm1=smctb
               else
                  rc=0.
                  sm1=x2
               endif
               if(smctb.ne.0)then
                  sm1t=sm1*(smct/smctb)
                  sm1b=sm1*(smcb/smctb)
               else
                  sm1t=0.
                  sm1b=0.
               endif
            endif

c...........adjust budget components for total area
            ae=ae*perv(ip)
            rc=rc*perv(ip)

c...........special water-budget calculations for taro..................................................................................
            if(ilu(ip).eq.ilutaro)then
                ae=ae*(1.0-tarofrac)+pan*tarofrac
                rc=rc*(1.0-tarofrac)+tarorc*tarofrac/365.0
            endif    

c...........update totals over entire area
            totae1(m)=totae1(m)+ae+aepaved*(1-perv(ip))
            totrc1(m)=totrc1(m)+rc

c...........ignore following 10 lines that are for debugging purposes only
c            if(ip.eq.6498)then
c      wb=sm1hold+drf+dfog+agirr-dro-ae-rc-sm1
c      write(4,4100)isim,ip,area(ip),perv(ip),ilu(ip),isoil(ip),
c     1   i,m,k,icrst(ip),nicyc,ndicyc-1,root,rd(ilu(ip)),ndpcyc-1,pc,
c     2   pan,pe,sm1hold,drf,dfog,agirr*perv(ip),
c     3   wadd*(1-perv(ip)),
c     4   dro,ae+aepaved*(1-perv(ip)),rc,sm1,wb
c 4100 format(i3,1x,i5,1x,f10.2,1x,f5.2,1x,i2,1x,i3,1x,5i3,1x,i4,1x,
c     1   2f5.1,i4,1x,f4.2,1x,2f6.2,4x,9f6.2,2x,f6.3)
c            endif
        

c...........FAO 56 ROOT CONSTANT MODEL

c...........compute FAO 56 p-value (see p. 162 and table 22 of FAO 56)
            pa=p22+(0.04*(5.-(pe*25.4)))
c...........check upper and lower bounds
            if(pa.gt.0.8)pa=0.8
            if(pa.lt.0.1)pa=0.1
            if(root.lt.0.99*rd(ilu(ip)).or.
     1         root.gt.1.01*rd(ilu(ip)))then
c..............fallow period, use 2-layer model
               x1=sm2t+agirr+septic+((dpnet-dro)*perv(ip)
     1            + wadd ) / perv(ip)
               if(x1.lt.0)x1=0
c..............compute root constant
               ci=(1-pa)*smct
               if(ci.le.0.)then
c.................full pe until moisture exhausted
                  if(pe.le.x1)then
                     ae=pe
                  else
                     ae=x1
                  endif
               elseif(x1.gt.ci)then
c.................compute time (d) for full pe
                  if(pe.gt.0.)then
                     t=(x1-ci)/pe
                  else
                     t=99.
                  endif
                  if(t.ge.1.)then
c....................entire day at full pe
                     ae=pe
                  else
c....................part of day at full pe
                     ae=pe*t + ci*(1-dexp(-pe*(1-t)/ci))
                  endif
               else
c.................entire day at less than full pe
                  ae=x1*(1-dexp(-pe*1.0/ci))
               endif
               x2=x1-ae
               if(x2.le.0)then
                  ae=x1
                  sm2t=0.
                  rct=0.
               else
                  if(x2.le.smct)then
                     sm2t=x2
                     rct=0.
                  else
                     sm2t=smct
                     rct=x2-smct
                  endif
               endif
               x2=sm2b+rct
               if(x2.gt.smcb)then
                  rc=x2-smcb
                  sm2b=smcb
               else
                  rc=0.
                  sm2b=x2
               endif
               sm2=sm2t+sm2b
            else
c..............planted period, use 1-layer model
               x1=sm2+agirr+septic+((dpnet-dro)*perv(ip)
     1            + wadd ) / perv(ip)
               if(x1.lt.0)x1=0
c..............compute root constant
               ci=(1-pa)*smctb
               if(ci.le.0.)then
c.................full pe until moisture exhausted
                  if(pe.le.x1)then
                     ae=pe
                  else
                     ae=x1
                  endif
               elseif(x1.gt.ci)then
c.................compute time (d) for full pe
                  if(pe.gt.0.)then
                     t=(x1-ci)/pe
                  else
                     t=99.
                  endif
                  if(t.ge.1.)then
c....................entire day at full pe
                     ae=pe
                  else
c....................part of day at full pe
                     ae=pe*t + ci*(1-dexp(-pe*(1-t)/ci))
                  endif
               else
c.................entire day at less than full pe
                  ae=x1*(1-dexp(-pe*1.0/ci))
               endif
               x2=x1-ae
               if(x2.le.0)then
                  ae=x1
                  sm2=0.
                  rc=0.
               else
                  if(x2.le.smctb)then
                     sm2=x2
                     rc=0.
                  else
                     sm2=smctb
                     rc=x2-smctb
                  endif
               endif
               if(smctb.ne.0)then
                  sm2t=sm2*(smct/smctb)
                  sm2b=sm2*(smcb/smctb)
               else
                  sm2t=0.
                  sm2b=0.
               endif
            endif
            
c...........adjust budget components for total area
            ae=ae*perv(ip)
            rc=rc*perv(ip)

c...........special water-budget calculations for taro..................................................................................
            if(ilu(ip).eq.ilutaro)then
                ae=ae*(1.0-tarofrac)+pan*tarofrac
                rc=rc*(1.0-tarofrac)+tarorc*tarofrac/365.0
            endif    

c...........update totals over entire area
            totae2(m)=totae2(m)+ae+aepaved*(1-perv(ip))
            totrc2(m)=totrc2(m)+rc
            rechann=rechann+rc


c...........GIAMBELLUCA'S (1983) ROOT CONSTANT MODEL
            if(root.lt.0.99*rd(ilu(ip)).or.
     1         root.gt.1.01*rd(ilu(ip)))then
c..............fallow period, use 2-layer model
               x1=sm3t+agirr+septic+((dpnet-dro)*perv(ip)
     1            + wadd ) / perv(ip)
               if(x1.lt.0)x1=0
c..............compute root constant
               if(pe.le.(6/25.4))then
                  ci=(1.25 - (0.00187*root*25.4) + (0.052*pe*25.4))*
     1               smct
               else
                  ci=(1.41 - (0.00187*root*25.4) + (0.022*pe*25.4))*
     1               smct
               endif
               if(ci.gt.smct)ci=smct
               if(ci.le.0.)then
c.................full pe until moisture exhausted
                  if(pe.le.x1)then
                     ae=pe
                  else
                     ae=x1
                  endif
               elseif(x1.gt.ci)then
c.................compute time (d) for full pe
                  if(pe.gt.0.)then
                     t=(x1-ci)/pe
                  else
                     t=99.
                  endif
                  if(t.ge.1.)then
c....................entire day at full pe
                     ae=pe
                  else
c....................part of day at full pe
                     ae=pe*t + ci*(1-dexp(-pe*(1-t)/ci))
                  endif
               else
c.................entire day at less than full pe
                  ae=x1*(1-dexp(-pe*1.0/ci))
               endif
               x2=x1-ae
               if(x2.le.0)then
                  ae=x1
                  sm3t=0.
                  rct=0.
               else
                  if(x2.le.smct)then
                     sm3t=x2
                     rct=0.
                  else
                     sm3t=smct
                     rct=x2-smct
                  endif
               endif
               x2=sm3b+rct
               if(x2.gt.smcb)then
                  rc=x2-smcb
                  sm3b=smcb
               else
                  rc=0.
                  sm3b=x2
               endif
               sm3=sm3t+sm3b
            else
c..............planted period, use 1-layer model
               x1=sm3+agirr+septic+((dpnet-dro)*perv(ip)
     1            + wadd ) / perv(ip)
               if(x1.lt.0)x1=0
c..............compute root constant
               if(pe.le.(6/25.4))then
                  ci=(1.25 - (0.00187*root*25.4) + (0.052*pe*25.4))*
     1               smctb
               else
                  ci=(1.41 - (0.00187*root*25.4) + (0.022*pe*25.4))*
     1               smctb
               endif
               if(ci.gt.smctb)ci=smctb
               if(ci.le.0.)then
c.................full pe until moisture exhausted
                  if(pe.le.x1)then
                     ae=pe
                  else
                     ae=x1
                  endif
               elseif(x1.gt.ci)then
c.................compute time (d) for full pe
                  if(pe.gt.0.)then
                     t=(x1-ci)/pe
                  else
                     t=99.
                  endif
                  if(t.ge.1.)then
c....................entire day at full pe
                     ae=pe
                  else
c....................part of day at full pe
                     ae=pe*t + ci*(1-dexp(-pe*(1-t)/ci))
                  endif
               else
c.................entire day at less than full pe
                  ae=x1*(1-dexp(-pe*1.0/ci))
               endif
               x2=x1-ae
               if(x2.le.0)then
                  ae=x1
                  sm3=0.
                  rc=0.
               else
                  if(x2.le.smctb)then
                     sm3=x2
                     rc=0.
                  else
                     sm3=smctb
                     rc=x2-smctb
                  endif
               endif
               if(smctb.ne.0)then
                  sm3t=sm3*(smct/smctb)
                  sm3b=sm3*(smcb/smctb)
               else
                  sm3t=0.
                  sm3b=0.
               endif
            endif
c...........adjust budget components for total area
            ae=ae*perv(ip)
            rc=rc*perv(ip)

c...........special water-budget calculations for taro..................................................................................
            if(ilu(ip).eq.ilutaro)then
                ae=ae*(1.0-tarofrac)+pan*tarofrac
                rc=rc*(1.0-tarofrac)+tarorc*tarofrac/365.0
            endif    

c...........update totals over entire area
            totae3(m)=totae3(m)+ae+aepaved*(1-perv(ip))
            totrc3(m)=totrc3(m)+rc



c...........THORNTHWAITE'S MODEL

c...........compute budget for pervious area
            if(root.lt.0.99*rd(ilu(ip)).or.
     1         root.gt.1.01*rd(ilu(ip)))then
c..............fallow period, use 2-layer model
               x1=sm4t+agirr+septic+((dpnet-dro)*perv(ip)
     1            + wadd ) / perv(ip)
               if(x1.lt.0)x1=0
               if(smct.eq.0.0)then
                  ae=0.0
               elseif(x1.gt.smct)then
c.................compute time (d) for full pe
                  if(pe.gt.0.)then
                     t=(x1-smct)/pe
                  else
                     t=99.
                  endif
                  if(t.ge.1.)then
                     ae=pe
                  else
                     ae=pe*t + smct*(1.-dexp(-pe*(1-t)/smct))
                  endif
               else
c.................entire day at less than full pe
                  ae=x1*(1-dexp(-pe*1./smct))
               endif
               x2=x1-ae
               if(x2.le.0.)then
                  ae=x1
                  sm4t=0.
                  rct=0.
               else
                  if(x2.le.smct)then
                     sm4t=x2
                     rct=0.
                  else
                     sm4t=smct
                     rct=x2-smct
                  endif
               endif
               x2=sm4b+rct
               if(x2.gt.smcb)then
                  rc=x2-smcb
                  sm4b=smcb
               else
                  rc=0.
                  sm4b=x2
               endif
               sm4=sm4t+sm4b
            else
c..............planted period, use 1-layer model
               x1=sm4+agirr+septic+((dpnet-dro)*perv(ip)
     1            + wadd ) / perv(ip)
               if(x1.lt.0)x1=0
               if(smctb.eq.0.0)then
                  ae=0.0 
               elseif(x1.gt.smctb)then
c.................compute time (d) for full pe
                  if(pe.gt.0.)then
                     t=(x1-smctb)/pe
                  else
                     t=99.
                  endif
                  if(t.ge.1.)then
                     ae=pe
                  else
                     ae=pe*t + smctb*(1.-dexp(-pe*(1-t)/smctb))
                  endif
               else
c.................entire day at less than full pe
                  ae=x1*(1-dexp(-pe*1./smctb))
               endif
               x2=x1-ae
               if(x2.le.0.)then
                  ae=x1
                  sm4=0.
                  rc=0.
               else
                  if(x2.le.smctb)then
                     sm4=x2
                     rc=0.
                  else
                     sm4=smctb
                     rc=x2-smctb
                  endif
               endif
               if(smctb.ne.0)then
                  sm4t=sm4*(smct/smctb)
                  sm4b=sm4*(smcb/smctb)
               else
                  sm4t=0.
                  sm4b=0.
               endif
            endif
c...........adjust budget components for total area
            ae=ae*perv(ip)
            rc=rc*perv(ip)

c...........special water-budget calculations for taro..................................................................................
            if(ilu(ip).eq.ilutaro)then
                ae=ae*(1.0-tarofrac)+pan*tarofrac
                rc=rc*(1.0-tarofrac)+tarorc*tarofrac/365.0
            endif    
                
c...........update totals over entire area
            totae4(m)=totae4(m)+ae+aepaved*(1-perv(ip))
            totrc4(m)=totrc4(m)+rc

c...........reset root depth
            root=rd(ilu(ip)) 
 
c...........save today's rain and fog as yesterday's rain and fog
            drfy=drf
            dfogy=dfog 
 
 300     continue

c.....keep yearly FAO recharge estimates
          yrlyrech(ip,i)=yrlyrech(ip,i)+rechann/dfloat(nsim)
          rechann=0.
 
 400  continue
 
c.....Check if land cover is seed corn and run simulation again for same polygon with fallow land properties.
c.....Recharge and AE are then computed for 25% of polygon area seed corn and 75% fallow
      if(ilu(ip).eq.ilucorn.and.i.eq.nyrs)then
        if(icorncheck.eq.2)then
            do j=1,12
                totae1(j)=0.75*totae1a(j)+0.25*totae1(j)
                totrc1(j)=0.75*totrc1a(j)+0.25*totrc1(j)
                totae2(j)=0.75*totae2a(j)+0.25*totae2(j)
                totrc2(j)=0.75*totrc2a(j)+0.25*totrc2(j)
                totae3(j)=0.75*totae3a(j)+0.25*totae3(j)
                totrc3(j)=0.75*totrc3a(j)+0.25*totrc3(j)
                totae4(j)=0.75*totae4a(j)+0.25*totae4(j)
                totrc4(j)=0.75*totrc4a(j)+0.25*totrc4(j)
                totirr(j)=0.25*totirr(j)
            end do
        else       
            do n=1,12
                totae1a(n)=totae1(n)
                totrc1a(n)=totrc1(n)
                totae2a(n)=totae2(n)
                totrc2a(n)=totrc2(n)
                totae3a(n)=totae3(n)
                totrc3a(n)=totrc3(n)
                totae4a(n)=totae4(n)
                totrc4a(n)=totrc4(n)
            end do
            icorncheck=2
            goto 10
        endif
      endif

      if(irechyrly.eq.1)then
c.....write yearly recharge during first year of last simulation if water body
        if(ilu(ip).eq.iluwater.and.i.eq.1.and.isim.eq.nsim)then
c            if(ip.eq.1)write(43,4200)
            do j=1,nyrs
                yrlyrech(ip,j)=wbrc
            end do
            write(43,4300)ip,(yrlyrech(ip,j),j=1,nyrs),wbrc
        elseif(ilu(ip).eq.ilures.and.i.eq.1.and.isim.eq.nsim)then
c            if(ip.eq.1)write(43,4200)
            do j=1,nyrs
                yrlyrech(ip,j)=resrc
            end do
            write(43,4300)ip,(yrlyrech(ip,j),j=1,nyrs),resrc  
        elseif(ilu(ip).eq.ilutaro.and.i.eq.1.and.isim.eq.nsim)then
c            if(ip.eq.1)write(43,4200)
            do j=1,nyrs
                yrlyrech(ip,j)=tarorc
            end do
            write(43,4300)ip,(yrlyrech(ip,j),j=1,nyrs),tarorc     

c.....write yearly FAO recharge estimates during last simulation if not water body
        elseif(isim.eq.nsim.and.i.eq.nyrs)then
c            if(ip.eq.1)write(43,4200)
            avgrech=0.d0
            do 420 j=1,nyrs
               avgrech=avgrech+yrlyrech(ip,j)/dfloat(nyrs)
 420        continue
            write(43,4300)ip,(yrlyrech(ip,j),j=1,nyrs),avgrech
        endif      
      endif
      
 500  continue
 
 600  sm1begin=sm0*pavint*(1-perv(ip))+sm1begin*perv(ip)
      sm2begin=sm0*pavint*(1-perv(ip))+sm2begin*perv(ip)
      sm3begin=sm0*pavint*(1-perv(ip))+sm3begin*perv(ip)
      sm4begin=sm0*pavint*(1-perv(ip))+sm4begin*perv(ip)

      sm1end=smp*(1-perv(ip))+sm1*perv(ip)
      sm2end=smp*(1-perv(ip))+sm2*perv(ip)
      sm3end=smp*(1-perv(ip))+sm3*perv(ip)
      sm4end=smp*(1-perv(ip))+sm4*perv(ip)

c.....initialize polygon totals
      polyrf=0
      polyfog=0
      polyirr=0
      polysep=0
      polyro=0
      polycint=0
      polysd=0
      polyae1=0
      polyrc1=0
      polyae2=0
      polyrc2=0
      polyae3=0
      polyrc3=0
      polyae4=0
      polyrc4=0
      polype=0

c.....sum direct recharge sources
      directrc=watermain+dispwell+cesspool

c.....total monthly values for current polygon and simulation      
      do 700 m=1,12
         polyrf=polyrf+totrf(m)
         polyfog=polyfog+totfog(m)
         polyirr=polyirr+totirr(m)
         polysep=polysep+totseptic(m)
         polyro=polyro+totro(m)
         polycint=polycint+totcanint(m)
         polysd=polysd+totsd(m)
         polyae1=polyae1+totae1(m)
         polyrc1=polyrc1+totrc1(m)
         polyae2=polyae2+totae2(m)
         polyrc2=polyrc2+totrc2(m)
         polyae3=polyae3+totae3(m)
         polyrc3=polyrc3+totrc3(m)
         polyae4=polyae4+totae4(m)
         polyrc4=polyrc4+totrc4(m)
         polype=polype+totpe(m)
 700  continue

c.....compute water balance on current simulation totals for each method 
      wb1=sm1begin+polyrf+polyfog+polyirr-polyro-polyae1-polyrc1-sm1end
     1-polysd-polycint+directrc+polysep

      wb2=sm2begin+polyrf+polyfog+polyirr-polyro-polyae2-polyrc2-sm2end
     1-polysd-polycint+directrc+polysep

      wb3=sm3begin+polyrf+polyfog+polyirr-polyro-polyae3-polyrc3-sm3end
     1-polysd-polycint+directrc+polysep

      wb4=sm4begin+polyrf+polyfog+polyirr-polyro-polyae4-polyrc4-sm4end
     1-polysd-polycint+directrc+polysep

c.....water balance does not apply to water bodies, reservoirs, or taro
      if(ilu(ip).eq.iluwater.or.ilu(ip).eq.ilures.or.
     1   ilu(ip).eq.ilutaro)then
        wb1=0.
        wb2=0.
        wb3=0.
        wb4=0.
      endif 

c.....output polygon information for current simulation
      if(ip.eq.1)jprint=1
      if(ip.eq.jprint)then
         write(4,4100)isim,nyrs,ip,area(ip),ilu(ip),isoil(ip), 
     1      smc(ip),perv(ip),polyrf,polyfog,polyirr,polysep,directrc,
     2      polyron,polyro,polycint,polypan,polysd,polype/polypan,
     3      sm1begin,polyae1,polyrc1,sm1end,wb1,
     4      sm2begin,polyae2,polyrc2,sm2end,wb2,
     5      sm3begin,polyae3,polyrc3,sm3end,wb3,
     6      sm4begin,polyae4,polyrc4,sm4end,wb4
         jprint=jprint+iprint
      endif

c.....determine monthly average values for current polygon and simulation
      do 800 m=1,12
         totrf(m)=totrf(m)/dfloat(nyrs)
         totfog(m)=totfog(m)/dfloat(nyrs)
         totirr(m)=totirr(m)/dfloat(nyrs)
         totro(m)=totro(m)/dfloat(nyrs)
         totae1(m)=totae1(m)/dfloat(nyrs)
         totrc1(m)=totrc1(m)/dfloat(nyrs)
         totae2(m)=totae2(m)/dfloat(nyrs)
         totrc2(m)=totrc2(m)/dfloat(nyrs)
         totae3(m)=totae3(m)/dfloat(nyrs)
         totrc3(m)=totrc3(m)/dfloat(nyrs)
         totae4(m)=totae4(m)/dfloat(nyrs)
         totrc4(m)=totrc4(m)/dfloat(nyrs)
         totdirect(m)=totdirect(m)/dfloat(nyrs)
         totpnet(m)=totpnet(m)/dfloat(nyrs)
         totcanint(m)=totcanint(m)/dfloat(nyrs)
         totrfadj(m)=totrfadj(m)/dfloat(nyrs)
         totseptic(m)=totseptic(m)/dfloat(nyrs)
         totsd(m)=totsd(m)/dfloat(nyrs)
800   continue

      polyron=polyron/dfloat(nyrs)
      polypan=polypan/dfloat(nyrs)
      polype=polype/dfloat(nyrs)

 4100 format(i5,1x,i3,1x,i8,1x,f11.2,2x,i2,2x,i3,1x,f6.3,2x,f4.2,
     1   11(1x,f12.2),1x,4(1x,f5.2,1x,f12.2,1x,f12.2,1x,f5.2,1x,f8.3))
c dso20180509 modified 4200 format below
 4200 format('Poly_ID',300(',',i4))
c 4200 format('Poly_ID,Rech1,Rech2,Rech3,Rech4,Rech5,Rech6,Rech7,Rech8,',
c     1       'Rech9,Rech10,Rech11,Rech12,Rech13,Rech14,Rech15,Rech16,',
c     2       'Rech17,Rech18,Rech19,Rech20,Rech21,Rech22,Rech23,Rech24,',
c     3       'Rech25,Rech26,Rech27,Rech28,Rech29,Rech30,')
 4300 format(i7,300(',',f8.3))   

      return
      end

c----------------------------------------------------------------------------

      subroutine FAO_post

c.....This subroutine will take one output file from the Hawaii Water Budget Model,
c.....hi_wb.ou2, which consists of monthly water budget values, and create files 
c.....more easily imported into spreadsheet and Arc products than the .ou2 file. Results
c.....using the FAO method summarized.

c.....dso20180511 modified to avoid having to read information from hi_wb2.out and
c     instead rely on internal values. Also used exact conversion factors.

      implicit real*8 (a-h,o-z)

      real*8 rf(12),fog(12),wirr(12),ro(12),ae1(12),rc1(12),
     1     ae2(12),rc2(12),ae3(12),rc3(12),ae4(12),rc4(12),pnet(12),
     1     cint(12),rfadj(12),septic(12),sdrn(12),
     1     janrf,janfog,janirr,janro,janae,janrc,janpnet,jancint,
     1     febrf,febfog,febirr,febro,febae,febrc,febpnet,febcint,
     1     marrf,marfog,marirr,marro,marae,marrc,marpnet,marcint,
     1     aprrf,aprfog,aprirr,aprro,aprae,aprrc,aprpnet,aprcint,
     1     mayrf,mayfog,mayirr,mayro,mayae,mayrc,maypnet,maycint,
     1     junrf,junfog,junirr,junro,junae,junrc,junpnet,juncint,
     1     julrf,julfog,julirr,julro,julae,julrc,julpnet,julcint,
     1     augrf,augfog,augirr,augro,augae,augrc,augpnet,augcint,
     1     seprf,sepfog,sepirr,sepro,sepae,seprc,seppnet,sepcint,
     1     octrf,octfog,octirr,octro,octae,octrc,octpnet,octcint,
     1     novrf,novfog,novirr,novro,novae,novrc,novpnet,novcint,
     1     decrf,decfog,decirr,decro,decae,decrc,decpnet,deccint,
     1     jansep,jansd,febsep,febsd,marsep,marsd,aprsep,aprsd,
     1     maysep,maysd,junsep,junsd,julsep,julsd,augsep,augsd,
     1     sepsep,sepsd,octsep,octsd,novsep,novsd,decsep,decsd
 
      real*8 direct(12),totrf,totfog,totirr,totro,totae,totrc,
     1       totdirect,totpnet,totcint,totrfadj,totseptic,totsdrn,
     1       vtotrf,vtotfog,vtotirr,vtotro,vtotae,vtotrc,vtotdirect,
     1       vtotpnet,vtotcint,vtotrfadj,vtotseptic,vtotsdrn

      integer mo,poly,itest

      character*120 junk

      data janrf,janfog,janirr,janro,janae,janrc,janpnet,jancint,
     1     febrf,febfog,febirr,febro,febae,febrc,febpnet,febcint,
     1     marrf,marfog,marirr,marro,marae,marrc,marpnet,marcint,
     1     aprrf,aprfog,aprirr,aprro,aprae,aprrc,aprpnet,aprcint,
     1     mayrf,mayfog,mayirr,mayro,mayae,mayrc,maypnet,maycint,
     1     junrf,junfog,junirr,junro,junae,junrc,junpnet,juncint,
     1     julrf,julfog,julirr,julro,julae,julrc,julpnet,julcint,
     1     augrf,augfog,augirr,augro,augae,augrc,augpnet,augcint,
     1     seprf,sepfog,sepirr,sepro,sepae,seprc,seppnet,sepcint,
     1     octrf,octfog,octirr,octro,octae,octrc,octpnet,octcint,
     1     novrf,novfog,novirr,novro,novae,novrc,novpnet,novcint,
     1     decrf,decfog,decirr,decro,decae,decrc,decpnet,deccint,
     1     vtotrf,vtotfog,vtotirr,vtotro,vtotae,vtotrc,
     1     vtotdirect,vtotpnet,vtotcint,vtotrfadj,vtotseptic,vtotsdrn,
     1     jansep,jansd,febsep,febsd,marsep,marsd,aprsep,aprsd,
     1     maysep,maysd,junsep,junsd,julsep,julsd,augsep,augsd,
     1     sepsep,sepsd,octsep,octsd,novsep,novsd,decsep,decsd
     1     /132*0.0/

c.....dso20180511 added following dimension and common statements     
      dimension avrf(590000,12),avfd(590000,12),avir(590000,12),
     1   avro(590000,12),av1ae(590000,12),av2ae(590000,12),
     2   av3ae(590000,12),av4ae(590000,12),av1rc(590000,12),
     3   av2rc(590000,12),av3rc(590000,12),av4rc(590000,12),
     4   avdirect(590000,12),avpnet(590000,12),avcanint(590000,12),
     5   avrfadj(590000,12),avseptic(590000,12),avstrmdrn(590000,12)
      dimension ipoly(590000),area(590000),rfmon(50000,1200),
     1   ilu(590000),ifld(590000),smc(590000),perv(590000),
     2   iro(590000),isoil(590000),panann(590000),ifog(590000),
     3   ietzone(590000),irfzone(590000),iplant(590000),
     4   ifogelev(590000),iwatmain(590000),idispwell(590000),
     5   icpsepsewer(590000),iasys(590000),cprate(590000),
     6   irfcell(590000),ilup(590000,12),irrcode(50),rfnorm(590000,12),
     7   isdrain(590000),canfrac(590000),tfrac(590000),cerf(590000),
     8   romon(1000,1200),fdrfratio(500,100,12)
     
      common /avgmo/avrf,avfd,avir,avro,av1ae,av2ae,av3ae,av4ae,
     1   av1rc,av2rc,av3rc,av4rc,avdirect,avpnet,avcanint,
     2   avrfadj,avseptic,avstrmdrn
      common /poly/ipoly,area,rfmon,ilu,ifld,smc,iasys,
     1   iro,isoil,panann,ifog,ietzone,irfzone,iplant,npoly,nplant,
     2   ifogelev,iwatmain,idispwell,icpsepsewer,irfcell,ilup,
     3   perv,cprate,isdrain,canfrac,tfrac,cerf

      open(36,file='output\FAO_subarea_mo_in.csv')
      open(37,file='output\FAO_subarea_ann_in.csv')
      open(38,file='output\FAO_tot_area_mo_MGD.out')
      open(40,file='output\FAO_tot_area_ann_MGD.out')
      open(41,file='output\postcalc_error.out')
c      open(42,file='output\hi_wb2.out')

      write(36,3100)
      write(37,3200)
      write(38,3300)
      write(40,3400)
      write(*,9500)

c      read(42,*)junk
c      read(42,*)junk
  
      itest=1

      convfac1=(0.0254d0*0.0254d0*231*1000000)
      convfac2=(0.0254d0*0.0254d0*231*365*1000000)

      do 100 j=1,npoly

      do m=1,12
c         read(42,*,end=200)mo,poly,ilu(mo),irfcell(mo),iasys(mo),
c     1    area(mo),rf(mo),fog(mo),wirr(mo),ro(mo),direct(mo),ae1(mo),
c     2    rc1(mo),ae2(mo),rc2(mo),ae3(mo),rc3(mo),ae4(mo),rc4(mo),
c     3    pnet(mo),cint(mo),rfadj(mo),septic(mo),sdrn(mo)

c         if(m.ne.mo)then
c            write(41,*)'ABORTING--organize by month'
c            stop
c         endif
         if(m.eq.1)then
            totrf=0.
            totfog=0.
            totirr=0.
            totro=0.
            totae=0.
            totrc=0.
            totdirect=0.
            totpnet=0.
            totcint=0.
            totrfadj=0.
            totseptic=0.
            totsdrn=0.

            janrf=janrf+avrf(j,m)*area(j)/(convfac1*31)
            janfog=janfog+avfd(j,m)*area(j)/(convfac1*31)
            janirr=janirr+avir(j,m)*area(j)/(convfac1*31)
            janro=janro+avro(j,m)*area(j)/(convfac1*31)
            janae=janae+av2ae(j,m)*area(j)/(convfac1*31)
            janrc=janrc+av2rc(j,m)*area(j)/(convfac1*31)
            janpnet=janpnet+avpnet(j,m)*area(j)/(convfac1*31)
            jancint=jancint+avcanint(j,m)*area(j)/(convfac1*31)
            jansep=jansep+avseptic(j,m)*area(j)/(convfac1*31)
            jansd=jansd+avstrmdrn(j,m)*area(j)/(convfac1*31)
 
         elseif(m.eq.2)then
            febrf=febrf+avrf(j,m)*area(j)/(convfac1*28)
            febfog=febfog+avfd(j,m)*area(j)/(convfac1*28)
            febirr=febirr+avir(j,m)*area(j)/(convfac1*28)
            febro=febro+avro(j,m)*area(j)/(convfac1*28)
            febae=febae+av2ae(j,m)*area(j)/(convfac1*28)
            febrc=febrc+av2rc(j,m)*area(j)/(convfac1*28)
            febpnet=febpnet+avpnet(j,m)*area(j)/(convfac1*28)
            febcint=febcint+avcanint(j,m)*area(j)/(convfac1*28)
c            dso20180509
c            febsep=febsep+avseptic(j,m)*area(j)*6.71E-6/31
c            febsd=febsd+avstrmdrn(j,m)*area(j)*6.71E-6/31
            febsep=febsep+avseptic(j,m)*area(j)/(convfac1*28)
            febsd=febsd+avstrmdrn(j,m)*area(j)/(convfac1*28)

         elseif(m.eq.3)then
            marrf=marrf+avrf(j,m)*area(j)/(convfac1*31)
            marfog=marfog+avfd(j,m)*area(j)/(convfac1*31)
            marirr=marirr+avir(j,m)*area(j)/(convfac1*31)
            marro=marro+avro(j,m)*area(j)/(convfac1*31)
            marae=marae+av2ae(j,m)*area(j)/(convfac1*31)
            marrc=marrc+av2rc(j,m)*area(j)/(convfac1*31)
            marpnet=marpnet+avpnet(j,m)*area(j)/(convfac1*31)
            marcint=marcint+avcanint(j,m)*area(j)/(convfac1*31)
            marsep=marsep+avseptic(j,m)*area(j)/(convfac1*31)
            marsd=marsd+avstrmdrn(j,m)*area(j)/(convfac1*31)

         elseif(m.eq.4)then
            aprrf=aprrf+avrf(j,m)*area(j)/(convfac1*30)
            aprfog=aprfog+avfd(j,m)*area(j)/(convfac1*30)
            aprirr=aprirr+avir(j,m)*area(j)/(convfac1*30)
            aprro=aprro+avro(j,m)*area(j)/(convfac1*30)
            aprae=aprae+av2ae(j,m)*area(j)/(convfac1*30)
            aprrc=aprrc+av2rc(j,m)*area(j)/(convfac1*30)
            aprpnet=aprpnet+avpnet(j,m)*area(j)/(convfac1*30)
            aprcint=aprcint+avcanint(j,m)*area(j)/(convfac1*30)
c            dso20180509
c            aprsep=aprsep+avseptic(j,m)*area(j)*6.71E-6/31
c            aprsd=aprsd+avstrmdrn(j,m)*area(j)*6.71E-6/31
            aprsep=aprsep+avseptic(j,m)*area(j)/(convfac1*30)
            aprsd=aprsd+avstrmdrn(j,m)*area(j)/(convfac1*30)

         elseif(m.eq.5)then
            mayrf=mayrf+avrf(j,m)*area(j)/(convfac1*31)
            mayfog=mayfog+avfd(j,m)*area(j)/(convfac1*31)
            mayirr=mayirr+avir(j,m)*area(j)/(convfac1*31)
            mayro=mayro+avro(j,m)*area(j)/(convfac1*31)
            mayae=mayae+av2ae(j,m)*area(j)/(convfac1*31)
            mayrc=mayrc+av2rc(j,m)*area(j)/(convfac1*31)
            maypnet=maypnet+avpnet(j,m)*area(j)/(convfac1*31)
            maycint=maycint+avcanint(j,m)*area(j)/(convfac1*31)
            maysep=maysep+avseptic(j,m)*area(j)/(convfac1*31)
            maysd=maysd+avstrmdrn(j,m)*area(j)/(convfac1*31)

         elseif(m.eq.6)then
            junrf=junrf+avrf(j,m)*area(j)/(convfac1*30)
            junfog=junfog+avfd(j,m)*area(j)/(convfac1*30)
            junirr=junirr+avir(j,m)*area(j)/(convfac1*30)
            junro=junro+avro(j,m)*area(j)/(convfac1*30)
            junae=junae+av2ae(j,m)*area(j)/(convfac1*30)
            junrc=junrc+av2rc(j,m)*area(j)/(convfac1*30)
            junpnet=junpnet+avpnet(j,m)*area(j)/(convfac1*30)
            juncint=juncint+avcanint(j,m)*area(j)/(convfac1*30)
c            dso20180509
c            junsep=junsep+avseptic(j,m)*area(j)*6.71E-6/31
c            junsd=junsd+avstrmdrn(j,m)*area(j)*6.71E-6/31
            junsep=junsep+avseptic(j,m)*area(j)/(convfac1*30)
            junsd=junsd+avstrmdrn(j,m)*area(j)/(convfac1*30)

         elseif(m.eq.7)then
            julrf=julrf+avrf(j,m)*area(j)/(convfac1*31)
            julfog=julfog+avfd(j,m)*area(j)/(convfac1*31)
            julirr=julirr+avir(j,m)*area(j)/(convfac1*31)
            julro=julro+avro(j,m)*area(j)/(convfac1*31)
            julae=julae+av2ae(j,m)*area(j)/(convfac1*31)
            julrc=julrc+av2rc(j,m)*area(j)/(convfac1*31)
            julpnet=julpnet+avpnet(j,m)*area(j)/(convfac1*31)
            julcint=julcint+avcanint(j,m)*area(j)/(convfac1*31)
            julsep=julsep+avseptic(j,m)*area(j)/(convfac1*31)
            julsd=julsd+avstrmdrn(j,m)*area(j)/(convfac1*31)
      
         elseif(m.eq.8)then
            augrf=augrf+avrf(j,m)*area(j)/(convfac1*31)
            augfog=augfog+avfd(j,m)*area(j)/(convfac1*31)
            augirr=augirr+avir(j,m)*area(j)/(convfac1*31)
            augro=augro+avro(j,m)*area(j)/(convfac1*31)
            augae=augae+av2ae(j,m)*area(j)/(convfac1*31)
            augrc=augrc+av2rc(j,m)*area(j)/(convfac1*31)
            augpnet=augpnet+avpnet(j,m)*area(j)/(convfac1*31)
            augcint=augcint+avcanint(j,m)*area(j)/(convfac1*31)
            augsep=augsep+avseptic(j,m)*area(j)/(convfac1*31)
            augsd=augsd+avstrmdrn(j,m)*area(j)/(convfac1*31)

         elseif(m.eq.9)then
            seprf=seprf+avrf(j,m)*area(j)/(convfac1*30)
            sepfog=sepfog+avfd(j,m)*area(j)/(convfac1*30)
            sepirr=sepirr+avir(j,m)*area(j)/(convfac1*30)
            sepro=sepro+avro(j,m)*area(j)/(convfac1*30)
            sepae=sepae+av2ae(j,m)*area(j)/(convfac1*30)
            seprc=seprc+av2rc(j,m)*area(j)/(convfac1*30)
            seppnet=seppnet+avpnet(j,m)*area(j)/(convfac1*30)
            sepcint=sepcint+avcanint(j,m)*area(j)/(convfac1*30)
c            dso20180509
c            sepsep=sepsep+avseptic(j,m)*area(j)*6.71E-6/31
c            sepsd=sepsd+avstrmdrn(j,m)*area(j)*6.71E-6/31
            sepsep=sepsep+avseptic(j,m)*area(j)/(convfac1*30)
            sepsd=sepsd+avstrmdrn(j,m)*area(j)/(convfac1*30)

         elseif(m.eq.10)then
            octrf=octrf+avrf(j,m)*area(j)/(convfac1*31)
            octfog=octfog+avfd(j,m)*area(j)/(convfac1*31)
            octirr=octirr+avir(j,m)*area(j)/(convfac1*31)
            octro=octro+avro(j,m)*area(j)/(convfac1*31)
            octae=octae+av2ae(j,m)*area(j)/(convfac1*31)
            octrc=octrc+av2rc(j,m)*area(j)/(convfac1*31)
            octpnet=octpnet+avpnet(j,m)*area(j)/(convfac1*31)
            octcint=octcint+avcanint(j,m)*area(j)/(convfac1*31)
            octsep=octsep+avseptic(j,m)*area(j)/(convfac1*31)
            octsd=octsd+avstrmdrn(j,m)*area(j)/(convfac1*31)

         elseif(m.eq.11)then
            novrf=novrf+avrf(j,m)*area(j)/(convfac1*30)
            novfog=novfog+avfd(j,m)*area(j)/(convfac1*30)
            novirr=novirr+avir(j,m)*area(j)/(convfac1*30)
            novro=novro+avro(j,m)*area(j)/(convfac1*30)
            novae=novae+av2ae(j,m)*area(j)/(convfac1*30)
            novrc=novrc+av2rc(j,m)*area(j)/(convfac1*30)
            novpnet=novpnet+avpnet(j,m)*area(j)/(convfac1*30)
            novcint=novcint+avcanint(j,m)*area(j)/(convfac1*30)
c            dso20180509
c            novsep=novsep+avseptic(j,m)*area(j)*6.71E-6/31
c            novsd=novsd+avstrmdrn(j,m)*area(j)*6.71E-6/31
            novsep=novsep+avseptic(j,m)*area(j)/(convfac1*30)
            novsd=novsd+avstrmdrn(j,m)*area(j)/(convfac1*30)

         elseif(m.eq.12)then
            decrf=decrf+avrf(j,m)*area(j)/(convfac1*31)
            decfog=decfog+avfd(j,m)*area(j)/(convfac1*31)
            decirr=decirr+avir(j,m)*area(j)/(convfac1*31)
            decro=decro+avro(j,m)*area(j)/(convfac1*31)
            decae=decae+av2ae(j,m)*area(j)/(convfac1*31)
            decrc=decrc+av2rc(j,m)*area(j)/(convfac1*31)
            decpnet=decpnet+avpnet(j,m)*area(j)/(convfac1*31)
            deccint=deccint+avcanint(j,m)*area(j)/(convfac1*31)
            decsep=decsep+avseptic(j,m)*area(j)/(convfac1*31)
            decsd=decsd+avstrmdrn(j,m)*area(j)/(convfac1*31)
         endif


c.....totalizing in in/yr by polygon
      totrf=totrf+avrf(j,m)
      totfog=totfog+avfd(j,m)
      totirr=totirr+avir(j,m)
      totro=totro+avro(j,m)
      totae=totae+av2ae(j,m)
      totrc=totrc+av2rc(j,m)
      totdirect=totdirect+avdirect(j,m)
      totpnet=totpnet+avpnet(j,m)
      totcint=totcint+avcanint(j,m)
      totrfadj=totrfadj+avrfadj(j,m)
      totseptic=totseptic+avseptic(j,m)
      totsdrn=totsdrn+avstrmdrn(j,m)
      iasysz=iasys(j)


c.....totalizing in MGD for entire model area
      vtotrf=vtotrf+avrf(j,m)*area(j)/convfac2
      vtotfog=vtotfog+avfd(j,m)*area(j)/convfac2
      vtotirr=vtotirr+avir(j,m)*area(j)/convfac2
      vtotro=vtotro+avro(j,m)*area(j)/convfac2
      vtotae=vtotae+av2ae(j,m)*area(j)/convfac2
      vtotrc=vtotrc+av2rc(j,m)*area(j)/convfac2
      vtotdirect=vtotdirect+avdirect(j,m)*area(j)/convfac2
      vtotpnet=vtotpnet+avpnet(j,m)*area(j)/convfac2
      vtotcint=vtotcint+avcanint(j,m)*area(j)/convfac2
      vtotrfadj=vtotrfadj+avrfadj(j,m)*area(j)/convfac2    
      vtotseptic=vtotseptic+avseptic(j,m)*area(j)/convfac2
      vtotsdrn=vtotsdrn+avstrmdrn(j,m)*area(j)/convfac2    

      if(m.eq.12)then
c.........write all 12 months for all 4 methods to one file
c         write(35,4000)poly,(rf(k),fog(k),wirr(k),ro(k),ae1(k),
c    1    rc1(k),ae2(k),rc2(k),ae3(k),rc3(k),ae4(k),rc4(k),k=1,12)
c.........write all 12 months for FAO method only to one file
c          write(36,5000)poly,(rf(k),fog(k),wirr(k),ro(k),
c     1    ae2(k),rc2(k),pnet(k),cint(k),septic(k),sdrn(k),k=1,12)
          write(36,5000)ipoly(j),(avrf(j,k),avfd(j,k),avir(j,k),
     1    avro(j,k),av2ae(j,k),av2rc(j,k),avpnet(j,k),avcanint(j,k),
     2    avseptic(j,k),avstrmdrn(j,k),k=1,12)

c.........write annual mean values for each polygon to one file (FAO method)
          write(37,6000)ipoly(j),totrf,totfog,totirr,totdirect,totro,
     1    totae,totrc,totpnet,totcint,totrfadj,totseptic,totsdrn
      endif
      end do

        itest=2
c        goto 100
 100     continue

      write(38,7000)janrf,janfog,janirr,janro,janae,janrc,janpnet,
     1     jancint,jansep,jansd,
     1     febrf,febfog,febirr,febro,febae,febrc,febpnet,febcint, 
     1     febsep,febsd,
     1     marrf,marfog,marirr,marro,marae,marrc,marpnet,marcint,
     1     marsep,marsd,
     1     aprrf,aprfog,aprirr,aprro,aprae,aprrc,aprpnet,aprcint,
     1     aprsep,aprsd,
     1     mayrf,mayfog,mayirr,mayro,mayae,mayrc,maypnet,maycint,
     1     maysep,maysd,
     1     junrf,junfog,junirr,junro,junae,junrc,junpnet,juncint,
     1     junsep,junsd,
     1     julrf,julfog,julirr,julro,julae,julrc,julpnet,julcint,
     1     julsep,julsd,
     1     augrf,augfog,augirr,augro,augae,augrc,augpnet,augcint,
     1     augsep,augsd,
     1     seprf,sepfog,sepirr,sepro,sepae,seprc,seppnet,sepcint,
     1     sepsep,sepsd,
     1     octrf,octfog,octirr,octro,octae,octrc,octpnet,octcint,
     1     octsep,octsd,
     1     novrf,novfog,novirr,novro,novae,novrc,novpnet,novcint,
     1     novsep,novsd,
     1     decrf,decfog,decirr,decro,decae,decrc,decpnet,deccint,
     1     decsep,decsd
 
      write(40,9000)vtotrf,vtotfog,vtotirr,vtotro,vtotae,vtotrc,
     1     vtotdirect,vtotpnet,vtotcint,vtotrfadj,vtotseptic,vtotsdrn

1000  format('Poly_ID   Rain     Fog     Irr   Runoff    AE1     RC1',
     1'     AE2     RC2     AE3     RC3     AE4     RC4')
2000  format(i6,2x,12(f7.3,1x))

3000  format('Poly ID  1Rain    1Fog    1Irr  1Runoff   1AE1    1RC1',
     1'    1AE2    1RC2    1AE3    1RC3    1AE4    1RC4',
     2'   2Rain    2Fog    2Irr  2Runoff   2AE1    2RC1',
     3'    2AE2    2RC2    2AE3    2RC3    2AE4    2RC4',
     2'   3Rain    3Fog    3Irr  3Runoff   3AE1    3RC1',
     3'    3AE2    3RC2    3AE3    3RC3    3AE4    3RC4',
     2'   4Rain    4Fog    4Irr  4Runoff   4AE1    4RC1',
     3'    4AE2    4RC2    4AE3    4RC3    4AE4    4RC4',
     2'   5Rain    5Fog    5Irr  5Runoff   5AE1    5RC1',
     3'    5AE2    5RC2    5AE3    5RC3    5AE4    5RC4',
     2'   6Rain    6Fog    6Irr  6Runoff   6AE1    6RC1',
     3'    6AE2    6RC2    6AE3    6RC3    6AE4    6RC4',
     2'   7Rain    7Fog    7Irr  7Runoff   7AE1    7RC1',
     3'    7AE2	   7RC2    7AE3    7RC3    7AE4    7RC4',
     2'   8Rain    8Fog    8Irr  8Runoff   8AE1    8RC1',
     3'    8AE2    8RC2    8AE3    8RC3    8AE4    8RC4',
     2'   9Rain    9Fog    9Irr  9Runoff   9AE1    9RC1',
     3'    9AE2    9RC2    9AE3    9RC3    9AE4    9RC4',
     2'  10Rain   10Fog   10Irr 10Runoff  10AE1   10RC1',
     3'   10AE2   10RC2   10AE3   10RC3   10AE4   10RC4',
     2'  11Rain   11Fog   11Irr 11Runoff  11AE1   11RC1',
     3'   11AE2   11RC2   11AE3   11RC3   11AE4   11RC4',
     2'  12Rain   12Fog   12Irr 12Runoff  12AE1   12RC1',
     3'   12AE2   12RC2   12AE3   12RC3   12AE4   12RC4')

3100  format('Poly_ID,1Rain,1Fog,1Irr,1Runoff,1AE,1Rech,',
     1'1PNet,1Can_Int,1Septic,1SDrn,',
     2'2Rain,2Fog,2Irr,2Runoff,2AE,2Rech,2PNet,2Can_Int,2Septic,2SDrn,',
     3'3Rain,3Fog,3Irr,3Runoff,3AE,3Rech,3PNet,3Can_Int,3Septic,3SDrn,',
     4'4Rain,4Fog,4Irr,4Runoff,4AE,4Rech,4PNet,4Can_Int,4Septic,4SDrn,',
     5'5Rain,5Fog,5Irr,5Runoff,5AE,5Rech,5PNet,5Can_Int,5Septic,5SDrn,',
     6'6Rain,6Fog,6Irr,6Runoff,6AE,6Rech,6PNet,6Can_Int,6Septic,6SDrn,',
     7'7Rain,7Fog,7Irr,7Runoff,7AE,7Rech,7PNet,7Can_Int,7Septic,7SDrn,',
     8'8Rain,8Fog,8Irr,8Runoff,8AE,8Rech,8PNet,8Can_Int,8Septic,8SDrn,',
     9'9Rain,9Fog,9Irr,9Runoff,9AE,9Rech,9PNet,9Can_Int,9Septic,9SDrn,',
     1'10Rain,10Fog,10Irr,10Runoff,10AE,10Rech,10PNet,10Can_Int,',
     2'10Septic,10SDrn,',
     3'11Rain,11Fog,11Irr,11Runoff,11AE,11Rech,11PNet,11Can_Int,',
     4'11Septic,11SDrn,',
     5'12Rain,12Fog,12Irr,12Runoff,12AE,12Rech,12PNet,12Can_Int,',
     6'12Septic,12SDrn')

3200  format('Poly_ID,Rain,Fog,Irr,Dir_RC,Runoff,AE,Rech,PNet,Can_Int,',
     1       'RF_Adj,Septic,SDrain')     

3300  format('     Rain       Fog       Irr    Runoff        AE      ',
     1       'Rech      PNet   Can_Int    Septic    SDrain  ')         

3400  format('    Rain      Fog      Irr       RO       AE',
     1       '     Rech    DirRC     PNet   CanInt    RFAdj   Septic',
     2       '   SDrain')

4000  format(i6,2x,144(f7.3,1x))	
5000  format(i6,',',120(f7.3,','))
6000  format(i6,',',12(f10.3,','))
7000  format(12(10(f9.3,1x),/))
9000  format(12(f8.2,1x))
9500  format('writing FAO-method output files...') 

      return
      end