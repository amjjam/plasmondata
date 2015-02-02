C  =====================================================================

C  PROGRAM GEO-CGM             Version 2001                 January 2001
   
C  This version is significantly redesigned and now it consist of two 
C  major parts:
C 
C  1. MAIN PROGRAM GEO-CGM which provides a dialog to key-entry the data
C
C  2. SUBROUTINE GEOCGM01 which does all necessary calculations 
C
C  The user can write a new main program and run GEOCGM01 independently

C  RECENT UPDATES:
C  Jan 22, 2001  The code was extended to 2005 (subs IGRF and RECALC)
C  Feb  8, 1999  The code was significantly modularized (v. 9.9)
C  Jan 22, 1999  The function OVL_ANG is added (v.9.1)
C  Jan 18, 1999  Main program is modified to call SUBR GEOCGM (v.9.0)
C  Jan 15, 1999  "Geographic" is replaced by "Geocentric" and positive 
C                direction of the meridian_angle is corrected in the 
C                Southern Hemisphere
C  Aug 26, 1997  IGRF is updated with IGRF-1995, SV 1995-2000, and 
C                GEOPACK-1996
C  Dec 22, 1995  Output 132 characters and correction of BF=180-BF
C  Aug 15, 1995  GEOPACK-1994 was incorporated in the version 7.4

C  =====================================================================

      DIMENSION DAT(11,4),PLA(4),PLO(4),IUH(4),IUM(4)
      CHARACTER FOUT*20,FINP*20,HEAD*25,HST*5,HCJ*5
      CHARACTER YS*1,COD*3,DIV*42,DVV*15
      DATA DIV/'------------------------------------------'/
      DATA DVV/'---------------'/

  100 FORMAT(
     *'                                                               '/
     *' ----------------------- GEO <--> CGM -------------------------'/
     *' |                                                            |'/
     *' | The GEO-CGM code provides transformation between CORRECTED |'/
     *' | GEOMAGNETIC (CGM) and geographic  (GEOCENTRIC) coordinates |'/
     *' | of a given point utilizing the 1945-2000 DGRF/IGRF models. |'/
     *' | The B-min approach is applied to calculate CGM coordinates |'/
     *' | through the near-equator  area where the definition of CGM |'/
     *' | coordinates is invalid. However, GEO<->GGM transformations |'/
     *' | are not performed at certain regions where the CGM equator |'/
     *' | cannot be defined at all (see Gustafsson et al. [1992] and |'/
     *' | http://nssdc.gsfc.nasa.gov/space/cgm/ for details).        |'/
     *' |                                                            |'/
     *' | Main GEO-CGM.FOR & subroutine GEOCGM01.FOR    January 2001 |'/
     *' --------------------------------------------------------------'/
     */' Type <H/h> if you need information on input and output or'/
     *' <Enter> to proceed')

      WRITE (*,100)
      READ  (*,'(A1)') YS
        IF (YS.eq.'H'.or.YS.eq.'h') WRITE (*,110)

  110 FORMAT(
     *'                                                               '/
     *' --------------------------------------------------------------'/
     *' | INPUT:                                                     |'/
     *' | - Geocentric or Corrected GeoMagnetic Latitude / Longitude |'/
     *' | - Altitude above 1-Re (6371.2 km) surface: 40,000 km limit |'/
     *' | - Year for the DGRF/IGRF model epochs from 1945 to 2000    |'/
     *' |                                                            |'/
     *' | OUTPUT:                                                    |'/
     *' | - GEOCENTRIC, CGM, and AACGM coordinates of a given point  |'/
     *' |   (see http://sd-www.jhuapl.edu/RADAR/AACGM for definition |'/
     *' |    of the Altitude Adjusted CGM coordinates)               |'/
     *' | - DGRF/IGRF magnetic field components at this point        |'/
     *' | - Geocentric and CGM coordinates of the magnetically       |'/
     *' |    conjugate point and the magnetic field line footprint   |'/
     *' | - Apex of the magnetic field line (in Re)                  |'/
     *' | - MLT midnight in UT (hr:mm) at the given point            |'/
     *' | - Meridian_angle: the azimuth along a great-circle arc to  |'/
     *' |    the North (South) CGM pole measured from the geographic |'/
     *' |    North (South) meridian; positive to East (West)         |'/
     *' | - Oval_angle: the angle between local tangents to the CGM  |'/
     *' |    and geographic (geocentric) latitudes; this angle is    |'/
     *' |    presented as the azimuth to the local "magnetic north"  |'/
     *' |    ("magnetic south") if the eastward (westward) tangent   |'/
     *' |    to the CGM latitude points southward (northward) from   |'/
     *' |    local East (West); measured positive to East (West)     |'/
     *' --------------------------------------------------------------'/
     *)
         
C  IRD is an index to key-enter the input parameters or read the file

          IRD = 0

      WRITE (*,*)
     * 'Enter <Y/y> to read the file or <Enter> for keyboard:'
      READ  (*,'(A1)') YS     
      IF (YS.eq.'Y'.or.YS.eq.'y') THEN
          IRD = 1
        WRITE (*,*) 'Sample of the input_file (A3,2F7.2,F8.1):'
        WRITE (*,*) '-------------------------'
        WRITE (*,*) 'COD   Lat.  Long.   H, km'
        WRITE (*,*) 'MOS  56.34 276.89      0.'
        WRITE (*,*) 'SAT   1.23 150.00 36600.5'
        WRITE (*,*) 'VOS -78.46 106.83     3.2'
        WRITE (*,*) 

   11   WRITE (*,*) 'Enter input_file name:'  
        READ  (*,'(A20)/') FINP
          OPEN(11,FILE=FINP,STATUS='OLD',IOSTAT=IOS)
            if(IOS.NE.0) then
              write(*,*) 'File ',FINP,' does not exist!'
              goto 11
            endif
            READ(11,'(A25)') HEAD

   12   WRITE (*,*)'Enter name of the output file to write results'
        READ  (*,'(A20)') FOUT
          OPEN(12,FILE=FOUT,STATUS='NEW',IOSTAT=IOS)
            if(IOS.NE.0) then
              write(*,*) 'File ',FOUT,' already exists!'
              goto 12
              endif
      ENDIF

C  Read the input data from a keyboard

   15   WRITE (*,*) 'Enter year <1945 to 2005> (enter 0 to quit)'
        WRITE (*,*) '   or  /   to use previous value'
        WRITE (*,*) '****'
        READ  (*,*) iyear
          if (iyear.lt.1) goto 111
            if (iyear.lt.1945.or.iyear.gt.2005) then
              write (*,*) '*** WARNING: Year is out of range! ***'
              goto 15
            endif

        WRITE (*,*)'Enter  1  to compute GEO  --> CGM'
        WRITE (*,*)'  or  -1  to compute GEO <--  CGM'
        WRITE (*,*)'  or   /  to use previous value'
        READ  (*,*) ICOR

      IF(IRD.EQ.1) WRITE (12,250) IYEAR,DVV,DIV,DIV

   20 CONTINUE

C  Nullify the parameter array before getting to the next point

        DO I = 1,11
          DO J = 1,4
            DAT(I,J) = 0.
          ENDDO
        ENDDO

      IF(IRD.EQ.1) THEN

C  Read data from file

      IF (ICOR.EQ. 1) THEN
              READ(11,240,END=111) COD,SLAR,SLOR,HI
          DAT(1,1) = SLAR
          DAT(2,1) = SLOR
                            ELSE  
          READ(11,240,END=111) COD,CLAR,CLOR,HI
              DAT(3,1) = CLAR
          DAT(4,1) = CLOR
            ENDIF          
          GOTO 50
                     ELSE

   30   WRITE(*,*)'Enter North/South Latitude and East/West Longitude'
        WRITE(*,*)'<76.54 -123.48> or <-76.54 123.48>'
        WRITE(*,*)'or enter  /  to use previous values'
          IF (ICOR.EQ. 1) THEN
          READ (*,*) SLAR,SLOR
            DAT(1,1) = SLAR
            DAT(2,1) = SLOR
                          ELSE 
            READ (*,*) CLAR,CLOR
            DAT(3,1) = CLAR
            DAT(4,1) = CLOR
          ENDIF

   40   WRITE (*,*)'Enter altitude above 1-Re (6371.2 km) surface'
        WRITE (*,*)'<0 to 40,000> km (or / to use previous value)'
        READ  (*,*) HI
          IF(HI.GT.40000.) THEN
            WRITE(*,*) 'Altitude is too high...'
            GOTO 40
          ENDIF     
          WRITE (*,*)

      ENDIF      

   50 CONTINUE  

C  Call the subroutine GEOCGM01 where DAT - 11 input/output parameters
C  (slar,slor,clar,clor,rbm,btr,bfr,brr,ovl,azm,utm) for the start point
C  (*,1), for the conjugate point (*,2), and then for their footprints 
C  at 1-Re surface - (*,3) and (*,4), respectively

      CALL GEOCGM01(ICOR,IYEAR,HI,DAT,PLA,PLO)

C  Headers for the CGM pole coordinates
   
        if(DAT(3,1).lt.0.) then 
          HST = 'South'
                           else
          HST = 'North'
        endif
        if(DAT(3,2).lt.0.) then 
          HCJ = 'South'
                           else
          HCJ = 'North'
        endif

C  Convert UT to HHH:MM for the start and conjugate points

        CALL UTHM(DAT(11,1),IUH(1),IUM(1))
        CALL UTHM(DAT(11,2),IUH(2),IUM(2))

C  Convert UT to HHH:MM for footprints of the start and conjugate points

        IF(HI.GT.0.) THEN
          CALL UTHM(DAT(11,3),IUH(3),IUM(3))
          CALL UTHM(DAT(11,4),IUH(4),IUM(4))
        ENDIF

      IF (IRD.EQ.1) THEN

C  Write out the results into the file

        WRITE (*,*)
        WRITE (*,'(A1,A3,A17/)') '   ',COD,' is processing...'
        WRITE (12,260) COD,HI,(DAT(i,1),i=1,10),IUH(1),IUM(1)
            IF(HI.GT.0.) THEN
          WRITE (12,265) 0.,(DAT(i,3),i=1,10),IUH(1),IUM(1)
          WRITE (12,270) 0.,(DAT(i,4),i=1,10),IUH(1),IUM(1)
            ENDIF
        WRITE (12,280) HI,(DAT(i,2),i=1,10),IUH(1),IUM(1)

C  Go to get a new point

          GOTO 20
        
                    ELSE 

C  Write out the results on the display

        WRITE (*,*)

C  Write parameters of the CGM pole(s) for the start point

        WRITE (*,130) DIV,DIV
        WRITE (*,140) IYEAR,HST,HI,PLA(1),PLO(1)
          IF(HI.GT.0.) WRITE (*,145) 0.,PLA(3),PLO(3)
        WRITE (*,130) DIV,DIV

C  Write parameters for the start point

        WRITE (*,150) DIV,DIV,HI
        WRITE (*,155) (DAT(i,1),i=1,10),IUH(1),IUM(1)

C  Write parameters for the footprints

          IF(HI.GT.0.) THEN
            WRITE (*,160) 
            WRITE (*,155) (DAT(i,3),i=1,10),IUH(3),IUM(3)
            WRITE (*,170) 
            WRITE (*,155) (DAT(i,4),i=1,10),IUH(4),IUM(4)
          ENDIF

C  Write parameters of the CGM pole(s) for the conjugate point

        WRITE (*,180) HI
        WRITE (*,155) (DAT(i,2),i=1,10),IUH(2),IUM(2)
        WRITE (*,*)

C  Write parameters of the CGM pole(s) for the conjugate point

        WRITE (*,130) DIV,DIV
        WRITE (*,140) IYEAR,HCJ,HI,PLA(2),PLO(2)
          IF(HI.GT.0.) WRITE (*,145) 0.,PLA(4),PLO(4)
        WRITE (*,130) DIV,DIV
        WRITE (*,*)

C  Go to get a new point
        GOTO 15

C  Ending to write data
      ENDIF

  130 FORMAT(2X,2A42)
  140 FORMAT('       Year: ',I4,4X,A5,' CGM pole at ',F7.1,
     +' km:   Lat.=',F7.2,'   Long.=',F7.2)
  145 FORMAT('                                    at ',F7.1,
     +' km:        ',F7.2,'         ',F7.2)
  150 FORMAT(
     +'     Geocentric         CGM       L-value  IGRF Magnetic Field',
     +'   Oval & Azimuth  MLTMN'/
     +'    Lat.   Long.    Lat.   Long.    Re     H,nT   D,deg   Z,nT',
     +'  angles N/S:+E/W  in UT'/2X,2A42//'  Starting point at ',
     +F7.1,' km:'/)
  155 FORMAT(4F8.2,F7.2,F8.0,F8.2,F8.0,2F8.2,2X,I2,':',I2)
  160 FORMAT(/2X,
     +'Footprint at 1-Re & AACGM coords of the starting point:'/)
  170 FORMAT(/2X,
     +'Footprint at 1-Re & AACGM coords of the conjugate point:'/)
  180 FORMAT(/2X,'Conjugate point at ',F7.1,' km:'/)

  240 FORMAT(A3,2F7.2,F8.1)
  250 FORMAT(' Year  Altitude   Geocentric        CGM       L-value  ',
     +'IGRF Magnetic Field    Oval & Azimuth  MLTMN'/
     +I5,'    (km)    Lat.   Long.    Lat.   Long.   Re     H,nT    ',
     +'D,deg   Z,nT  angles N/S:+E/W  in UT'/A15,2A42)
  260 FORMAT(/
     +      1X,A3,1X,F8.1,4F8.2,F7.2,F8.0,F8.2,F8.0,2F8.2,2X,I2,':',I2)
  265 FORMAT('  FTP',F8.1,4F8.2,F7.2,F8.0,F8.2,F8.0,2F8.2,2X,I2,':',I2)
  270 FORMAT('  CFT',F8.1,4F8.2,F7.2,F8.0,F8.2,F8.0,2F8.2,2X,I2,':',I2)
  280 FORMAT(' CONJ',F8.1,4F8.2,F7.2,F8.0,F8.2,F8.0,2F8.2,2X,I2,':',I2)

  111   STOP
        END

C  *********************************************************************

      SUBROUTINE UTHM(UTM,IUH,IUM)

C  Converts UTM from the hour and fraction to HH:MM

        IUH = IFIX(UTM)
        IF(IUH.EQ.99) THEN
            IUM = 99
                    ELSE
            IUM = NINT((UTM-IUH)*60)         
        ENDIF
       
        RETURN
        END

C  *********************************************************************
C  =====================================================================

      SUBROUTINE GEOCGM01(ICOR,IYEAR,HI,DAT,PLA,PLO)

C  Version 2001 (2508 lines) for GEO-CGM.FOR                January 2001

C  AUTHORS:                                                  
C  Natalia E. Papitashvili (WDC-B2, Moscow, Russia, now at NSSDC,
C    NASA/Goddard Space Flight Center, Greenbelt, Maryland)
C  Vladimir O. Papitashvili (IZMIRAN, Moscow, Russia, now at SPRL,
C    The University of Michigan, Ann Arbor)
C  Conributions from Boris A. Belov and Vladimir A. Popov (both at 
C    IZMIRAN), as well as from Therese Moretto (DMI, DSRI, now at
C    NASA/GSFC). 

C  The original version of this code is described in the brochure by
C  N.A. Tsyganenko, A.V. Usmanov, V.O. Papitashvili, N.E. Papitashvili,
C  and V.A. Popov, Software for computations of geomagnetic field and 
C  related coordinate systems, Soviet Geophys. Committ., Moscow, 58 pp.,
C  1987. A number of subroutines from the revised GEOPACK-96 software
C  package developed by Nikolai A. Tsyganenko and Mauricio Peredo are
C  utilized in this code with some modifications (see full version of
C  GEOPACK-96 on http://www-spof.gsfc.nasa.gov/Modeling/geopack.html). 
  
C  This code consists of the main subroutine GEOCGM99, five functions
C  (OVL_ANG, CGMGLA, CGMGLO, DFRIDR, and AZM_ANG), eigth new and revised
C  subroutines from the above-mentioned brochure (MLTUT, MFC, FTPRNT,
C  GEOLOW, CORGEO, GEOCOR, SHAG, and RIGHT), and 9 subroutines from
C  GEOPACK-96 (IGRF, SPHCAR, BSPCAR, GEOMAG, MAGSM, SMGSM, RECALC, SUN)
 
C  =====================================================================

C  Input parameters:
C     icor = +1    geo to cgm
C            -1    cgm to geo
C     iyr  = year 
C     hi   = altitude

C     slar = geocentric latitude
C     slor = geocentric longitude (east +)                                  
C   These two pairs can be either input or output parameters
C     clar = cgm latitude
C     clor = cgm longitude (east +)

C  Output parameters:
C     Array DAT(11,4) consists of 11 parameters (slar, slor, clar, clor,
C     rbm, btr, bfr, brr, ovl, azm, utm) organized for the start point 
C     (*,1), its conjugate point (*,2), then for the footprints at 1-Re
C     of the start (*,3) and conjugate (*,4) points

C  Description of parameters used in the subroutine: 
C     slac = conjugate geocentric latitude
C     sloc = conjugate geocentric longitude
C     slaf = footprint geocentric latitude
C     slof = footprint geocentric longitude
C     rbm  = apex of the magnetic field line in Re (Re=6371.2 km)
C            (this parameter approximately equals the McIlwain L-value) 
C     btr  = IGRF Magnetic field H (nT)  
C     bfr  = IGRF Magnetic field D (deg)
C     brr  = IGRF Magnetic field Z (nT)
C     ovl  = oval_angle as the azimuth to "magnetic north": 
C                + east in Northern Hemisphere 
C                + west in Southern Hemisphere
C     azm  = meridian_angle as the azimuth to the CGM pole:              
C                + east in Northern Hemisphere 
C                + west in Southern Hemisphere
C     utm  = magnetic local time (MLT) midnight in UT hours
C     pla  = array of geocentric latitude and
C     plo  = array of geocentric longitudes for the CGM poles 
C            in the Northern and Southern hemispheres at a given 
C            altitude (indices 1 and 2) and then at the Earth's 
C            surface - 1-Re or zero altitude - (indices 3 and 4)
C     dla  = dipole latitude
C     dlo  = dipole longitude

C  =====================================================================

      COMMON /C1/ AA(27),II(2),BB(8)
      COMMON /IYR/ IYR
      COMMON /NM/ NM
      COMMON /RZ/ RH

      DIMENSION DAT(11,4),PLA(4),PLO(4)
      CHARACTER STR*12

C  Year (for example, as for Epoch 1995.0 - no fraction of the year)

       IYR = iyear

C  Earth's radius (km)

        RE = 6371.2

C  NM is the number of harmonics

        NM = 10

C  The radius of the sphere to compute the coordinates (in Re)

        RH = (RE + HI)/RE

C  Correction of latitudes and longitudes if they are entered beyond of
C  the limits (this actually does not affect coordinate calculations
C  but the oval/meridian angles and MLT midnight cannot be computed)

          IF (DAT(1,1).GT. 90.) DAT(1,1) =  180. - DAT(1,1)
          IF (DAT(1,1).LT.-90.) DAT(1,1) = -180. - DAT(1,1)
          IF (DAT(3,1).GT. 90.) DAT(3,1) =  180. - DAT(3,1)
          IF (DAT(3,1).LT.-90.) DAT(3,1) = -180. - DAT(3,1)

          IF (DAT(2,1).GT. 360.) DAT(2,1) = DAT(2,1) - 360.
          IF (DAT(2,1).LT.-360.) DAT(2,1) = DAT(2,1) + 360.
          IF (DAT(4,1).GT. 360.) DAT(4,1) = DAT(4,1) - 360.
          IF (DAT(4,1).LT.-360.) DAT(4,1) = DAT(4,1) + 360.

C  Computation of CGM coordinates from geocentric ones at high- and 
C  middle latitudes

      IF (ICOR.EQ. 1) THEN

                SLAR = DAT(1,1)
                SLOR = DAT(2,1)
                IF (ABS(SLAR).EQ.90.) SLOR = 360.         
          CALL GEOCOR(SLAR,SLOR,RH,DLA,DLO,CLAR,CLOR,PMR)
            DAT(3,1) = CLAR
            DAT(4,1) = CLOR

	                ELSE

C  Computation of geocentric coordinates from CGM ones at high- and 
C  middle latitudes 

                CLAR = DAT(3,1)
                CLOR = DAT(4,1)
        IF (ABS(CLAR).EQ.90.) CLOR = 360.
          CALL CORGEO(SLAR,SLOR,RH,DLA,DLO,CLAR,CLOR,PMR)
            DAT(1,1) = SLAR
            DAT(2,1) = SLOR

	ENDIF

C  PMI is L-shell parameter for the magnetic field line; limit to 16 Re

        IF(PMR.GE.16.) PMR = 999.99
            DAT(5,1) = PMR         

C  Check if CGM_Lat has been calculated, then go for the conjugate point

        IF(CLAR.GT.999.) THEN

C  CGM_Lat has NOT been calculated, call GEOLOW for computation of the
C  CGM coordinates at low latitudes using the CBM approach (see the
C  reference in GEOLOW) 

        CALL GEOLOW(SLAR,SLOR,RH,CLAR,CLOR,RBM,SLAC,SLOC)
            DAT(3,1) = CLAR
            DAT(4,1) = CLOR
        IF(RBM.GE.16.) RBM = 999.99
            DAT(5,1) = RBM

C  Conjugate point coordinates at low latitudes

          WRITE(STR,'(2F6.2)') SLAC,SLOC
          READ (STR,'(2F6.2)') SLAC,SLOC
            DAT(1,2) = SLAC
            DAT(2,2) = SLOC
                CALL GEOCOR(SLAC,SLOC,RH,DAA,DOO,CLAC,CLOC,RBM)
          IF(CLAC.GT.999.)
     +    CALL GEOLOW(SLAC,SLOC,RH,CLAC,CLOC,RBM,SLAL,SLOL)
            DAT(3,2) = CLAC
            DAT(4,2) = CLOC
            DAT(5,2) = RBM

                         ELSE

C  Computation of the magnetically conjugated point at high- and 
C  middle latitudes   

                CLAC = -CLAR 
                CLOC =  CLOR
            DAT(3,2) = CLAC
            DAT(4,2) = CLOC
        CALL CORGEO(SLAC,SLOC,RH,DAA,DOO,CLAC,CLOC,PMC)
            DAT(1,2) = SLAC
            DAT(2,2) = SLOC
        IF(PMC.GE.16.) PMC = 999.99
            DAT(5,2) = PMC

      ENDIF

C  Same RBM for footprints as for the starting and conjugate points

            DAT(5,3) = DAT(5,1)
            DAT(5,4) = DAT(5,2)

C  Calculation of the magnetic field line footprint at the 
C  Earth's surface for the starting point   

      IF(RH.GT.1..and.CLAR.LT.999..and.CLAR.LT.999.) THEN
        CALL FTPRNT(RH,SLAR,SLOR,CLAR,CLOR,ACLAR,ACLOR,SLARF,SLORF,1.)
            DAT(1,3) = SLARF
            DAT(2,3) = SLORF
            DAT(3,3) = ACLAR
            DAT(4,3) = ACLOR
C  and for the conjugate point
        CALL FTPRNT(RH,SLAC,SLOC,CLAC,CLOC,ACLAC,ACLOC,SLACF,SLOCF,1.)
            DAT(1,4) = SLACF
            DAT(2,4) = SLOCF
            DAT(3,4) = ACLAC
            DAT(4,4) = ACLOC
                                                     ELSE
        do i = 1,4
          do j = 3,4
            DAT(i,j) = 999.99
          enddo
        enddo
          
      ENDIF

C  Computation of geocentric coordinates of the North or South CGM
C  poles for a given year at the altitude RH and Earth's surface (1-Re)

        CALL CORGEO(PLAN,PLON,RH,DAA,DOO, 90.,360.,PMP)
            PLAN1 = PLAN
            PLON1 = PLON

        CALL CORGEO(PLAS,PLOS,RH,DAA,DOO,-90.,360.,PMP)
            PLAS1 = PLAS
            PLOS1 = PLOS

        IF(RH.GT.1.) THEN
          CALL CORGEO(PLAN1,PLON1,1.,DAA,DOO, 90.,360.,PMP)
          CALL CORGEO(PLAS1,PLOS1,1.,DAA,DOO,-90.,360.,PMM)
        ENDIF

         IF(CLAR.LT.0.) THEN
           PLA(1) = PLAS
           PLO(1) = PLOS
                        ELSE
           PLA(1) = PLAN
           PLO(1) = PLON
         ENDIF
         IF(ACLAR.LT.0.) THEN
           PLA(3) = PLAS1
           PLO(3) = PLOS1
                        ELSE
           PLA(3) = PLAN1
           PLO(3) = PLON1
         ENDIF
         IF(CLAC.LT.0.) THEN
           PLA(2) = PLAS
           PLO(2) = PLOS
                        ELSE
           PLA(2) = PLAN
           PLO(2) = PLON
         ENDIF
         IF(ACLAC.LT.0.) THEN
           PLA(4) = PLAS1
           PLO(4) = PLOS1
                         ELSE
           PLA(4) = PLAN1
           PLO(4) = PLON1
         ENDIF

      do j = 1,4
        DAT( 6,j) = 99999.
        DAT( 7,j) = 999.99
        DAT( 8,j) = 99999.
        DAT( 9,j) = 999.99
        DAT(10,j) = 999.99
        DAT(11,j) =  99.99
      enddo

      icount = 2
      if(RH.gt.1.) icount = 4
          RJ = RH
      do j = 1,icount
        if(j.gt.2) RJ = 1.

        PLAJ = PLA(j)
        PLOJ = PLO(j)
 
        SLAJ = DAT(1,j)
        SLOJ = DAT(2,j)
        CLAJ = DAT(3,j)
        CLOJ = DAT(4,j)
            
C  Computation of the IGRF components
        CALL MFC(SLAJ,SLOJ,RJ,BTR,BFR,BRR)
          DAT(6,j) = BTR
          DAT(7,j) = BFR
          DAT(8,j) = BRR

C  Computation of the oval_angle (OVL) between the tangents to  
C  geographic and CGM latitudes at a given point (the code is slightly
C  modified from the source provided by Therese Morreto in 1994). Note 
C  that rotation of OVL on 90 deg anticlockwise provides the azimuth 
C  to the local "magnetic" north (south) measured from the local 
C  geographic meridian. The OVL_ANG can be calculated only at middle 
C  and high latitudes where CGM --> GEO is permitted.

        OVL = OVL_ANG(SLAJ,SLOJ,CLAJ,CLOJ,RJ)
          DAT(9,j) = OVL

C  Computation of the meridian_angle (AZM) between the geographic 
C  meridian and direction (azimuth along the great-circle arc) to 
C  the North (South) CGM pole

        AZM = AZM_ANG(SLAJ,SLOJ,CLAJ,PLAJ,PLOJ)
          DAT(10,j) = AZM

C  Computation of the MLT midnight (in UT) 
        CALL MLTUT(SLAJ,SLOJ,CLAJ,PLAJ,PLOJ,UT)
          DAT(11,j) = UT

C  End of loop j = 1,icount
      enddo

      RETURN
      END

C  *********************************************************************

      real function OVL_ANG(sla,slo,cla,clo,rr)

C  This function returns an estimate at the given location of the angle
C  (oval_angle) between the directions (tangents) along the constant 
C  CGM and geographic latitudes by utilizing the function DFRIDR from 
C  Numerical Recipes for FORTRAN. 

C  This angle can be taken as the azimuth to the local "magnetic" north
C  (south) if the eastward (westward) tangent to the local CGM latitude
C  points south (north) from the local geographic latitude.

C  Written by Therese Moretto in August 1994 (revised by V. Papitashvili
C  in January 1999).
   
      real cgmgla,cgmglo,dfridr
      logical cr360,cr0

      external cgmgla,cgmglo,dfridr

      common/cgmgeo/clat,cr360,cr0,rh

C  Ignore points which nearly coincide with the geographic or CGM poles
C  within 0.01 degree in latitudes; this also takes care if SLA or CLA 
C  are dummy values (e.g., 999.99)

      if(abs(sla).ge.89.99.or.abs(cla).ge.89.99.or.
     +   abs(sla).lt.30.) then
        OVL_ANG = 999.99
        return
      endif

C  Initialize values for the cgmglo and cgmgla functions
         
	    rh = rr
        clat = cla
       cr360 = .false.
         cr0 = .false.

C  Judge if SLO may be crossing the 360-0 limit. If geocentric 
C  longitude of the location is larger than 270 deg, then cr360 is 
C  set "true"; if it is less than 90 deg, then cr0 is set "true".

       if(slo.ge.270.) cr360 = .true.
       if(slo.le. 90.)   cr0 = .true.

C  An initial stepsize (in degrees)  

       step = 10.

C  Note that in the near-pole region the functions CGMGLA and CGMGLO
C  could be called from DFRIDR with the CGM latitudes exceeded 90 or 
C  -90 degrees (e.g., 98 or -98) when STEP is added or subtracted to a 
C  given CGM latitude (CLA). This does not produce discontinuities in 
C  the functions because GEOCOR calculates GEOLAT smoothly for the 
C  points lying behind the pole (e.g., as for 82 or - 82 deg. in the
C  above-mentioned example). However, it could be discontinuity in
C  GEOLON if |GEOLAT| = 90 deg. - see CGMGLO for details.

           hom = dfridr(cgmgla,clo,step,err1)

         denom = dfridr(cgmglo,clo,step,err2)
    
         denom = denom*cos(sla*0.017453293)

       OVL_ANG = -atan2(hom,denom)

       OVL_ANG = OVL_ANG*57.2957751

      return
      end  

C  *********************************************************************

      real function cgmgla(clon)

C  This function returns the geocentric latitude as a function of CGM 
C  longitude with the CGM latitude held in common block CGMGEO. 
C  Essentially this function just calls the subroutine CORGEO. 

      logical cr360,cr0
      common/cgmgeo/cclat,cr360,cr0,rh
      
	    rr = rh
       if(clon.gt.360.) clon = clon - 360.
         if(clon.lt.0.) clon = clon + 360.
       call CORGEO(geolat,geolon,rr,dla,dlo,cclat,clon,pmi)
         cgmgla = geolat         

      return
      end
      
C *********************************************************************

      real function cgmglo(clon)

C  Same as the function CGMGLA but this returns the geocentric
C  longitude. If cr360 is true, geolon+360 deg is returned when geolon
C  is less than 90 deg. If cr0 is true, geolon-360 deg is returned 
C  when geolon is larger than 270 degrees.

      logical cr360,cr0

      common/cgmgeo/cclat,cr360,cr0,rh

          rr = rh
       if(clon.gt.360.) clon = clon - 360.
         if(clon.lt.0.) clon = clon + 360.
   1   continue
       call CORGEO(geolat,geolon,rr,dla,dlo,cclat,clon,pmi)

C  Geographic longitude geolon could be any number (e.g., discontinued)
C  when geolat is the geographic pole

	 if(abs(geolat).ge.89.99) then
	       clon = clon - 0.01
	       goto 1
	 endif

       if(cr360.and.(geolon.le.90.)) then
           cgmglo = geolon + 360.
                                     else 
	   if (cr0.and.(geolon.ge.270.)) then
           cgmglo = geolon - 360.
                                       else
           cgmglo = geolon
         endif
	 endif

      return
      end

C **********************************************************************

      FUNCTION DFRIDR(func,x,h,err)

C  Numerical Recipes Fortran 77 Version 2.07
C  Copyright (c) 1986-1995 by Numerical Recipes Software

      INTEGER NTAB
      REAL dfridr,err,h,x,func,CON,CON2,BIG,SAFE
      PARAMETER (CON=1.4,CON2=CON*CON,BIG=1.E30,NTAB=10,SAFE=2.)
      EXTERNAL func

      INTEGER i,j
      REAL errt,fac,hh,a(NTAB,NTAB)
       if(h.eq.0.) stop 'h must be nonzero in dfridr'
       hh = h
       a(1,1) = (func(x+hh)-func(x-hh))/(2.0*hh)
       err = BIG
      do 12 i=2,NTAB
        hh = hh/CON
        a(1,i) = (func(x+hh)-func(x-hh))/(2.0*hh)
        fac = CON2
        do 11 j=2,i
          a(j,i) = (a(j-1,i)*fac-a(j-1,i-1))/(fac-1.)
          fac = CON2*fac
          errt = max(abs(a(j,i)-a(j-1,i)),abs(a(j,i)-a(j-1,i-1)))
          if (errt.le.err) then
            err = errt
            dfridr = a(j,i)
          endif
  11    continue
         if(abs(a(i,i)-a(i-1,i-1)).ge.SAFE*err) return
  12  continue

      return
      END

C  *********************************************************************

      real function AZM_ANG(sla,slo,cla,pla,plo)

C  Computation of an angle between the north geographic meridian and 
C  direction to the North (South) CGM pole: positive azimuth is 
C  measured East (West) from geographic meridian, i.e., the angle is 
C  measured between the great-circle arc directions to the geographic 
C  and CGM poles. In this case the geomagnetic field components in 
C  XYZ (NEV) system can be converted into the CGM system in both 
C  hemispheres as:
C                           XM = X cos(alf) + Y sin(alf) 
C                           YM =-X sin(alf) + Y cos(alf) 

C  Written by V. O. Papitashvili in mid-1980s; revised in February 1999

C  Ignore points which nearly coincide with the geographic or CGM poles
C  within 0.01 degree in latitudes; this also takes care if SLA or CLA 
C  are dummy values (e.g., 999.99)

      if(abs(sla).ge.89.99.or.abs(cla).ge.89.99) then
        AZM_ANG = 999.99
        return
      endif
          sp = 1.
          ss = 1.
      if(sign(sp,pla).ne.sign(ss,cla)) then
        write(*,2) pla,cla 
   2    format(/
     +  'WARNING - The CGM pole PLA = ',f6.2,' and station CLAT = ',
     +  f6.2,' are not in the same hemisphere: AZM_ANG is incorrect!')
      endif

      RAD = 0.017453293

           am = (90. - abs(pla))*rad
        if(sign(sp,pla).eq.sign(ss,sla)) then
           cm = (90. - abs(sla))*rad
                                          else
           cm = (90. + abs(sla))*rad
        endif
        if(sla.ge.0.) then
          bet = (plo - slo)*rad
                     else
          bet = (slo - plo)*rad
        endif
           sb = sin(bet)
           st = sin(cm)/tan(am) - cos(cm)*cos(bet)
         alfa = atan2(sb,st)
         AZM_ANG = alfa/rad    

      RETURN
      END

C  *********************************************************************

      SUBROUTINE MLTUT(SLA,SLO,CLA,PLA,PLO,UT)

C  Calculates the MLT midnight in UT hours 

C  Definition of the MLT midnight (MLTMN) here is different from the 
C  approach described elsewhere. This definition does not take into 
C  account the geomagnetic meridian of the subsolar point which causes
C  seasonal variations of the MLTMN in UT time. The latter approach is
C  perfectly applicable to the dipole or eccentric dipole magnetic 
C  coordinates but it fails with the CGM coordinates because there are 
C  forbidden areas near the geomagnetic equator where CGM coordinates 
C  cannot be calculated by definition [e.g., Gustafsson et al., JATP, 
C  54, 1609, 1992].

C  In this code the MLT midnight is defined as location of a given point
C  on (or above) the Earth's surface strictly behind the North (South) 
C  CGM pole in such the Sun, the pole, and the point are lined up.

C  This approach was originally proposed and coded by Boris Belov 
C  sometime in the beginning of 1980s; here it is slightly edited by 
C  Vladimir Papitashvili in February 1999.

C  Ignore points which nearly coincide with the geographic or CGM poles
C  within 0.01 degree in latitudes; this also takes care if SLA or CLA 
C  are dummy values (e.g., 999.99)

      if(abs(sla).ge.89.99.or.abs(cla).ge.89.99) then
        UT = 99.99
        return
      endif

      TPI = 6.283185307
      RAD = 0.017453293
       sp = 1.
       ss = 1.
      if(sign(sp,pla).ne.sign(ss,cla)) then
        write(*,2) pla,cla 
   2    format(/
     +  'WARNING - The CGM pole PLA = ',f6.2,' and station CLAT = ',
     +  f6.2,' are not in the same hemisphere: MLTMN is incorrect!')
      endif

C  Solve the spherical triangle

         QQ = PLO*RAD
        CFF = 90. - abs(PLA)
        CFF = CFF*RAD
      IF(CFF.LT.0.0000001) CFF=0.0000001

      if(sign(sp,pla).eq.sign(ss,sla)) then
        CFT = 90. - abs(SLA)
                                       else
        CFT = 90. + abs(SLA)
      endif
      
          CFT = CFT*RAD
      IF(CFT.LT.0.0000001) CFT=0.0000001
      
        QT = SLO*RAD
         A = SIN(CFF)/SIN(CFT)
         Y = A*SIN(QQ) - SIN(QT)
         X = COS(QT) - A*COS(QQ)
        UT = ATAN2(Y,X)
      
        IF(UT.LT.0.) UT = UT + TPI
       QQU = QQ + UT
       QTU = QT + UT
        BP = SIN(CFF)*COS(QQU)
        BT = SIN(CFT)*COS(QTU)
        UT = UT/RAD
        UT = UT/15.
      IF(BP.LT.BT) GOTO 10
      
        IF(UT.LT.12.) UT = UT + 12.
      IF(UT.GT.12.) UT = UT - 12.
   
  10  CONTINUE 

      RETURN
      END

C  *********************************************************************

        SUBROUTINE MFC(SLA,SLO,R,H,D,Z)

C  Computation of the IGRF magnetic field components  

C  Extracted as a subroutine from the earlier version of GEO-CGM.FOR
C  V. Papitashvili, February 1999

      COMMON /NM/NM
      COMMON /IYR/IYR
    
C  This takes care if SLA or CLA are dummy values (e.g., 999.99)

      if(sla.ge.999.) then
          X = 99999.
          Y = 99999.
          Z = 99999.
          H = 99999.
          D = 999.99
          I = 999.99
          F = 99999.
        return
      endif

C  Computation of all geomagnetic field components
        RLA = (90.-SLA)*0.017453293
        RLO = SLO*0.017453293
       CALL IGRF(IYR,NM,R,RLA,RLO,BR,BT,BF)
          X = -BT
          Y =  BF
          Z = -BR
          H = SQRT(X**2+Y**2)
          D = 57.2957751*ATAN2(Y,X)
          I = 57.2957751*ATAN2(Z,H)
          F = SQRT(H**2+Z**2)
        RETURN
        END

C  *********************************************************************

      SUBROUTINE FTPRNT(RH,SLA,SLO,CLA,CLO,ACLA,ACLO,SLAF,SLOF,RF)

C  Calculation of the magnetic field line footprint at the Earth's 
C  (or any higher) surface. 

C  Extracted as a subroutine from the earlier version of GEO-CGM.FOR by
C  V. Papitashvili in February 1999 but then the subroutine was revised
C  to obtain the Altitude Adjusted CGM coordinates. The AACGM approach 
C  is proposed by Kile Baker of the JHU/APL, see their World Wide Web 
C  site http://sd-www.jhuapl.edu/RADAR/AACGM/ for details.

C  If RF = 1-Re (i.e., at the Earth's surface), then the footprint 
C  location is defined as the Altitude Adjusted (AA) CGM coordinates
C  for a given point (ACLA, ACLO).

C  If RF = 1.xx Re (i.e., at any altitude above or below the starting
C  point), then the conjunction between these two points can be found 
C  along the field line.

      COMMON /NM/NM
      COMMON /IYR/IYR

C  This takes care if SLA or CLA are dummy values (e.g., 999.99)

      if(sla.gt.999..or.cla.gt.999.or.RF.eq.RH) then
        ACLA = 999.99
        ACLO = 999.99
        SLAF = 999.99
        SLOF = 999.99
        return
      endif

C  Defining the Altitude Adjusted CGM coordinates for a given point

         COL = (90. - CLA)*0.017453293
         SN2 = (SIN(COL))**2
        ACOL = ASIN(SQRT((SN2*RF)/RH))
        ACLA = 90. - ACOL*57.29577951
        IF(CLA.LT.0.) ACLA = -ACLA
        ACLO = CLO

        CALL CORGEO(SLAF,SLOF,RF,DLAF,DLOF,ACLA,ACLO,PMIF)

        IF(SLAF.LT.999.) RETURN

C  Tracing the magnetic field line down to the Earth's surface at low
C  latitudes if CORGEO failed to calculate geocentric coordinates SLAF
C  and SLOF

        IF(SN2.LT.0.0000001) SN2 = 0.0000001
          RL = RH/SN2
        FRAC = 0.03/(1.+3./(RL-0.6))

C  Checking direction of the magnetic field-line, so the step along
C  the field-line will go down, to the Earth surface

        IF(CLA.GE.0.) FRAC = -FRAC
          DS = RH*FRAC
  
  250   CONTINUE

C  Start from an initial point

           R = RH
        RSLA = (90. - SLA)*0.0174533
        RSLO = SLO*0.0174533
        CALL SPHCAR(R,RSLA,RSLO,XF,YF,ZF,1)
         RF1 = R
         XF1 = XF
         YF1 = YF
         ZF1 = ZF

  255   CALL SHAG(XF,YF,ZF,DS)
          RR = SQRT(XF**2+YF**2+ZF**2)
          IF (RR.GT.RH) THEN
            DS = -DS
            XF = XF1
            YF = YF1
            ZF = ZF1
              GOTO 250
            ENDIF
         IF (RR.GT.RF) THEN
                 RF1 = RR
                 XF1 = XF
                 YF1 = YF
                 ZF1 = ZF
                 GOTO 255
                       ELSE
            DR1 = ABS(RF1 - RF)
            DR0 = ABS( RF - RR)
           DR10 = DR1 + DR0
              IF(DR10.NE.0.) THEN
                 DS = DS*(DR1/DR10)
                 CALL SHAG(XF1,YF1,ZF1,DS)
              ENDIF
           CALL SPHCAR(RR,SLAF,SLOF,XF1,YF1,ZF1,-1)
            SLAF = 90. - SLAF*57.29578
            SLOF = SLOF*57.29578
         ENDIF

      RETURN
      END

C  *********************************************************************

      SUBROUTINE GEOLOW(SLAR,SLOR,RH,CLAR,CLOR,RBM,SLAC,SLOC)

C  Calculates CGM coordinates from geocentric ones at low latitudes 
C  where the DGRF/IGRF magnetic field lines may never cross the dipole
C  equatorial plane and, therefore, the definition of CGM coordinates
C  becomes invalid.

C  The code is written by Natalia and Vladimir Papitashvili as a part
C  of the earlier versions of GEO-CGM.FOR; extracted as a subroutine by
C  V. Papitashvili in February 1999.

C  See the paper by  Gustafsson, G., N. E. Papitashvili, and V. O. 
C  Papitashvili, A revised corrected geomagnetic coordinate system for
C  Epochs 1985 and 1990 [J. Atmos. Terr. Phys., 54, 1609-1631, 1992] 
C  for detailed description of the B-min approach utilized here.

      COMMON /NM/NM
      COMMON /IYR/IYR

      DIMENSION BC(2),ARLAT(181),ARLON(181)
      REAL*8 BM,B2,B3

C  This takes care if SLA is a dummy value (e.g., 999.99)

      if(slar.gt.999.) then
        CLAR = 999.99
        CLOR = 999.99                                   
        SLAC = 999.99
        SLOC = 999.99                                   
         RBM = 999.99
        return
      endif

C  HH is an error (nT) to determine B-min along the magnetic field line

       DHH = 0.5

C  Filling the work arrays of CGM latitudes and longitudes with 999.99 
C  Note that at certain geocentric longitudes in the very near-equator
C  region no "geomagnetic equator" can be defined at all.

          DO J=61,121
            ARLAT(J) = 999.99
            ARLON(J) = 999.99
          ENDDO

        SLO = SLOR

           NDIR=0

C  Finding the geomagnetic equator as a projection of the B-min point 
C  found for the field lines started from the last latitude in each 
C  hemisphere where the CGM coordinates were obtained from geocentric 
C  ones (GEO --> CGM). First the CGM coordinates are calculated in the
C  Northern (NDIR=0) and then in the Southern hemispheres (NDIR=1) 

  53     IF(NDIR.EQ.0) THEN

C  Program works from 30 deg. latitude down to the geographic equator 
C  in the Northern Hemisphere

             DO JC = 61,91
               SLA = 90.-(JC-1)
               CALL GEOCOR(SLA,SLO,RH,DAA,DOO,CLA,CLO,PMM)   
               IF(CLA.GT.999.) THEN
                 NDIR=1
                 GOTO 53
               ENDIF
               ARLAT(JC) = CLA
               ARLON(JC) = CLO    
             ENDDO
           NDIR=1
         GOTO 53

                       ELSE 

C  Program works from -30 deg. latitude down to the geographic equator
C  in the Southern Hemisphere

             DO JC = 121,92,-1
               SLA = 90.-(JC-1)
               CALL GEOCOR(SLA,SLO,RH,DAA,DOO,CLA,CLO,PMM)     
               IF(CLA.GT.999.) THEN
                 NDIR=0
                 GOTO 57
               ENDIF
               ARLAT(JC) = CLA
               ARLON(JC) = CLO
             ENDDO
           NDIR=0
         ENDIF

  57   CONTINUE

C  Finding last geographic latitudes along SLO where CGM coordinates
C  can be calculated

         n999=0
         ndir=0
         do jc = 61,121
           if(arlat(jc).gt.999.) then
             if(ndir.eq.0) then
                 jcn = jc - 1
               rnlat = arlat(jcn)
               rnlon = arlon(jcn)
                ndir = 1
                n999 = 1
             endif
           endif           
           if(arlat(jc).lt.999.) then
             if(ndir.eq.1) then
                 jcs = jc
               rslat = arlat(jc)
               rslon = arlon(jc)
                ndir = 0
                goto 59
             endif
           endif      
         enddo
 59     continue

C  If there is no points with 999.99 found along the SLO meridian, 
C  then the IHEM loop will start from 3; otherwise it starts from 1

            if(n999.eq.0) then
              ih = 3
              goto 31
                          else
              ih = 1
            endif

C  Interpolation of the appropriate CGM longitudes between last
C  geocentric latitudes along SLO where CGM coordinates were defined

          rdel = jcs - jcn
          if(rdel.eq.0.) then
             delat = 0.
                         else
             delon = (rslon - rnlon)/rdel
           endif
               jdel = 0
              do jc = jcn+1,jcs-1
               jdel = jdel + 1
               arlon(jc) = rnlon + delon*jdel
              enddo
   31   continue

C  Finding the CGM equator at SLO on the sphere with radius RH 

            NOBM = 0      
         do ihem = ih,3
              RM = RH

C  Defining the real equator point from the Northern Hemisphere

         if(ihem.eq.1) then
             CLA = rnlat
             SLA = 90. - (jcn - 1.)
            SLAN = SLA
         endif

C  Defining the real equator point from the Southern Hemisphere

         if(ihem.eq.2) then
             CLA = rslat
             SLA = 90. - (jcs - 1)
            SLAS = SLA
         endif

C  Defining the apex of the current magnetic field line

         if(ihem.eq.3) then
               CLA = 0.
               SLA = SLAR
           endif

C  Here CLA is used only to calculate FRAC 

        COL = (90. - CLA)*0.017453293
        SLM = (90. - SLA)*0.017453293
        SLL = SLO*0.017453293
        CALL IGRF(IYR,NM,RM,SLM,SLL,BR,BT,BF)
          SZ = -BR
        CALL SPHCAR(RM,SLM,SLL,XGEO,YGEO,ZGEO,1)
          BM = SQRT(BR*BR + BT*BT + BF*BF)
         XBM = XGEO
         YBM = YGEO
         ZBM = ZGEO
      
          RL = 1./(SIN(COL))**2
        FRAC = 0.03/(1. + 3./(RL - 0.6))
        IF(SZ.LE.0.) FRAC = -FRAC
         DSD = RL*FRAC
          DS = DSD

    5   CONTINUE

C  Keep two consequently computed points to define B-min

        DO 7 I = 1,2
            DD = DS
          CALL SHAG(XGEO,YGEO,ZGEO,DD)
   11     IF(I.NE.1) GOTO 9
            XBM1 = XGEO
            YBM1 = YGEO
            ZBM1 = ZGEO
            RBM1 = SQRT(XBM1**2 + YBM1**2 + ZBM1**2)
    9     CONTINUE
      
        CALL SPHCAR(RM,SLM,SLL,XGEO,YGEO,ZGEO,-1)
        CALL IGRF(IYR,NM,RM,SLM,SLL,BR,BT,BF)

C  Go and compute the conjugate point if no B-min was found at this
C  magnetic field line (could happen at very near geomagnetic equator)

          if(RM.LT.RH) then
            NOBM = 1
            GOTO 77
          endif

         BC(I) = SQRT(BR*BR + BT*BT + BF*BF)
    7  CONTINUE

         B2 = BC(1)
         B3 = BC(2)
        IF(BM.GT.B2.AND.B2.LT.B3) GO TO 15
        IF(BM.GE.B2.AND.B2.LT.B3) GO TO 17
        IF(BM.GT.B2.AND.B2.LE.B3) GO TO 17
         BM = BC(1)
       XGEO = XBM1
       YGEO = YBM1
       ZGEO = ZBM1
        XBM = XBM1
        YBM = YBM1
        ZBM = ZBM1
        GOTO 5
   15   BB3 = ABS(B3 - B2)
        BB2 = ABS(BM - B2)
        IF(BB2.LT.DHH.AND.BB3.LT.DHH) GO TO 21
   17    BM = BM
       XGEO = XBM
       YGEO = YBM
       ZGEO = ZBM
         DS = DS/2.
        GOTO 5

   21  CONTINUE

        CALL SPHCAR(RBM1,RLA,RLO,XBM1,YBM1,ZBM1,-1)
         RLA = 90. - RLA*57.2957751
         RLO = RLO*57.2957751

        if(ihem.eq.1) rlan = rla
        if(ihem.eq.2) rlas = rla

C  Computation of the magnetically conjugate point at low latitudes
   
   54  continue
        if(ihem.eq.3) then
           RBM = RBM1
            RM = RBM
            DS = DSD
   55  continue    
           CALL SHAG(XBM1,YBM1,ZBM1,DS)
           RR = SQRT(XBM1**2 + YBM1**2 + ZBM1**2)
           IF (RR.GT.RH) THEN
                R1 = RR
                X1 = XBM1
                Y1 = YBM1
                Z1 = ZBM1
                GOTO 55
                         ELSE
            DR1 = ABS(RH - R1)
            DR0 = ABS(RH - RR)
           DR10 = DR1 + DR0
              IF(DR10.NE.0.) THEN
                DS = DS*(DR1/DR10)
                RM = R1
                CALL SHAG(X1,Y1,Z1,DS)
              ENDIF

         CALL SPHCAR(RR,SLAC,SLOC,X1,Y1,Z1,-1)
         SLAC = 90. - SLAC*57.2957751
         SLOC = SLOC*57.2957751
           ENDIF
        endif

C  End of loop IHEM     
   77 continue
       enddo  

         if (n999.eq.0) goto 91
         
           IF (NOBM.EQ.1) THEN

C  Interpolation of CGM latitudes if there is no B-min at this 
C  magnetic field line
       
	     rdel = jcs - jcn
           if(rdel.eq.0.) then
               delat = 0.
                          else
               delat = (rslat - rnlat)/rdel
           endif
                jdel = 0
             do jc=jcn+1,jcs-1
                   jdel = jdel + 1
                   arlat(jc) = rnlat + delat*jdel
             enddo
                 RBM = 999.99
                SLAC = 999.99
                SLOC = 999.99
                      
                                        ELSE

C  Geocentric latitude of the CGM equator
       
	     rla = (rlan + rlas)/2.

C  Interpolation of the CGM latitudes in the Northern hemisphere
       
	    rdel = SLAN - rla
           if(rdel.eq.0.) then
                delat = 0.
                          else
                delat = rnlat/rdel
           endif
          jdn = abs(rdel)
                   jdel = 0
             do jc = jcn+1,jcn+jdn
                   jdel = jdel + 1
                   arlat(jc) = rnlat - delat*jdel
             enddo

C  Interpolation of the CGM latitudes in the Southern hemisphere
         
	    rdel = SLAS - rla
           if(rdel.eq.0.) then
                delat = 0.
                          else
                delat = rslat/rdel
           endif
          jds = abs(rdel)
                   jdel = 0
             do jc = jcs-1,jcs-jds,-1
                   jdel = jdel + 1
                   arlat(jc) = rslat + delat*jdel
             enddo
      ENDIF

   91 continue

C  Defining by interpolation the exact values of the CGM latitude 
C  and longitude between two adjacent values
         
	         L1 = 90. - SLAR + 1.
         IF(SLAR.LT.0.) THEN
               L2 = L1-1
                       ELSE
               L2 = L1+1
          ENDIF
             DSLA =  ABS(SLAR - INT(SLAR))
           DELCLA = ARLAT(L2) - ARLAT(L1)
           DELCLO = ARLON(L2) - ARLON(L1)
             CLAR = ARLAT(L1) + DELCLA*DSLA
             CLOR = ARLON(L1) + DELCLO*DSLA
      
      RETURN
      END

C  *********************************************************************

      SUBROUTINE CORGEO(SLA,SLO,RH,DLA,DLO,CLA,CLO,PMI)

C  Calculates geocentric coordinates from corrected geomagnetic ones.

C  The code is written by Vladimir Popov and Vladimir Papitashvili
C  in mid-1980s; revised by V. Papitashvili in February 1999

      COMMON /NM/NM
      COMMON /IYR/IYR

C  This takes care if CLA is a dummy value (e.g., 999.99)
      
	    jc = 0
      if(abs(cla).lt.1.) then
          write(*,*)
     +'WARNING - No calculations within +/-1 degree near CGM equator'
          jc = 1
      endif
      if(cla.gt.999..or.jc.eq.1) then
        SLA = 999.99
        SLO = 999.99
        DLA = 999.99
        DLO = 999.99
        PMI = 999.99
        return
      endif

        NG = NM

       COL = 90. - CLA
         R = 10.
        R1 = R
        R0 = R
       COL = COL*0.017453293
       RLO = CLO*0.017453293
        SN = SIN(COL)
       SN2 = SN*SN

C  The CGM latitude should be at least 0.01 deg. away of the CGM pole

      IF(SN2.LT.0.000000003) SN2 = 0.000000003
C      RFI = 1./SN2
       RFI = RH/SN2
       PMI = RFI
      IF(PMI.GT.99.999) PMI = 999.99      
         AA10 = R/RFI

C  RFI = R if COL = 90 deg.

        IF(RFI.LE.R) GOTO 1
        SAA = AA10/(1.-AA10)
        SAQ = SQRT(SAA)
       SCLA = ATAN(SAQ)
      IF(CLA.LT.0) SCLA = 3.14159265359 - SCLA

      GOTO 3

    1   SCLA = 1.57079632679
          R0 = RFI

    3 CALL SPHCAR(R0,SCLA,RLO,XM,YM,ZM,1)
      CALL GEOMAG(X,Y,Z,XM,YM,ZM,-1,IYR)
         RL = R0
       FRAC = -0.03/(1. + 3./(RL - 0.6))
      IF(CLA.LT.0.) FRAC = -FRAC
          R = R0

    5    DS = R*FRAC
         NM = (1. + 9./R) + 0.5
      CALL SHAG(X,Y,Z,DS)
          R = SQRT(X**2+Y**2+Z**2)
      IF(R.LE.RH) GOTO 7
         R1 = R
         X1 = X
         Y1 = Y
         Z1 = Z
         GOTO 5

C  Define intersection with the start surface

    7   DR1 = ABS(RH - R1)
        DR0 = ABS(RH - R)
       DR10 = DR1 + DR0
       IF(DR10.NE.0.) THEN
         DS = DS*(DR1/DR10)
         CALL SHAG(X1,Y1,Z1,DS)
       ENDIF

      CALL SPHCAR(R,GTET,GXLA,X1,Y1,Z1,-1)
        GTH = GTET*57.2957751
        SLO = GXLA*57.2957751
        SLA = 90. - GTH
      CALL GEOMAG(X1,Y1,Z1,XM,YM,ZM,1,IYR)
      CALL SPHCAR(RM,TH,PF,XM,YM,ZM,-1)
        DLO = PF*57.2957751
        DLA = 90. - TH*57.2957751

        NM = NG

C  Because CORGEO cannot check if the CGM --> GEO transformation is
C  performed correctly in the equatorial area (that is, where the IGRF
C  field line may never cross the dipole equatorial plane). Therefore,
C  the backward check is required for geocentric latitudes lower than 
C  30 degrees (see the paper referenced in GEOLOW)

      IF(ABS(SLA).LT.30..OR.ABS(CLA).LT.30.) THEN
          CALL GEOCOR(SLA,SLO,RH,DLS,DLS,CLAS,CLOS,PMS)

      IF(CLAS.GT.999.) CALL GEOLOW(SLA,SLO,RH,CLAS,CLOS,RBM,SLAC,SLOC)
        IF(ABS(ABS(CLA)-ABS(CLAS)).GE.1.) THEN
          write(*,*)
     +'WARNING - Selected CGM_Lat.=',CLA,' is located in the ',
     +'near CGM equator area where the latter cannot be defined'
           SLA = 999.99
           SLO = 999.99
           PMI = 999.99
        ENDIF
      ENDIF

      RETURN
      END

C  *********************************************************************

      SUBROUTINE GEOCOR(SLA,SLO,RH,DLA,DLO,CLA,CLO,PMI)

C  Calculates corrected geomagnetic coordinates from geocentric ones

C  The code is written by Vladimir Popov and Vladimir Papitashvili
C  in mid-1980s; revised by V. Papitashvili in February 1999

      COMMON /NM/NM
      COMMON /IYR/IYR

C  This takes care if SLA is a dummy value (e.g., 999.99)

      if(sla.gt.999.) then
        CLA = 999.99
        CLO = 999.99
        DLA = 999.99
        DLO = 999.99
        PMI = 999.99
        return
      endif

         NG = NM

        COL = 90. - SLA
          R = RH
         R1 = R
        COL = COL*0.017453293
        RLO = SLO*0.017453293
      CALL SPHCAR(R,COL,RLO,X,Y,Z,1)
      CALL GEOMAG(X,Y,Z,XM,YM,ZM,1,IYR)
      CALL SPHCAR(RM,TH,PF,XM,YM,ZM,-1)
        SZM = ZM
        DLO = PF*57.2957751
        DCO = TH*57.2957751
        DLA = 90. - DCO
         RL = R/(SIN(TH))**2
       FRAC = 0.03/(1. + 3./(RL - 0.6))

      IF(SZM.LT.0.) FRAC = -FRAC

C  Error to determine the dipole equtorial plane: aprox. 0.5 arc min

        HHH = 0.0001571

C  Trace the IGRF magnetic field line to the dipole equatorial plane
   
   1     DS = R*FRAC
   3     NM = (1. + 9./R) + 0.5
         R1 = R
         X1 = X
         Y1 = Y
         Z1 = Z
      CALL SHAG(X,Y,Z,DS)
      CALL GEOMAG(X,Y,Z,XM,YM,ZM,1,IYR)
      CALL SPHCAR(R,C,S,XM,YM,ZM,-1)

C  As tracing goes above (RH+10_Re), use the dipole field line  

        IF(R.GT.10.+RH) GOTO 9

C  If the field line returns to the start surface without crossing the
C  dipole equatorial plane, no CGM coordinates can be calculated

        IF(R.LE.RH) GOTO 11

        DCL = C - 1.5707963268
        IF(ABS(DCL).LE.HHH) GOTO 9
        RZM = ZM
        IF(SZM.GT.0..AND.RZM.GT.0.) GOTO 1
        IF(SZM.LT.0..AND.RZM.LT.0.) GOTO 1
          R = R1
          X = X1   
          Y = Y1                 
          Z = Z1   
         DS = DS/2.         
          GOTO 3

   9  CALL GEOMAG(X,Y,Z,XM,YM,ZM,1,IYR)
      CALL SPHCAR(R,GTET,GXLA,XM,YM,ZM,-1)
         ST = ABS(SIN(GTET))
        RRH = ABS(RH/(R - RH*ST**2))
        CLA = 1.5707963 - ATAN(ST*SQRT(RRH))
        CLA = CLA*57.2957751
        CLO = GXLA*57.2957751
      IF(SZM.LT.0.) CLA = -CLA
       SSLA = 90. - CLA
       SSLA = SSLA*0.017453293
         SN = SIN(SSLA)
C       PMI = 1/(SN*SN)
        PMI = RH/(SN*SN)
        GOTO 13

   11   CLA = 999.99
        CLO = 999.99
        PMI = 999.99

   13    NM = NG

      RETURN
      END

C  *********************************************************************

      SUBROUTINE SHAG(X,Y,Z,DS)

C  Similar to SUBR STEP from GEOPACK-1996 but SHAG takes into account
C  only internal sources

C  The code is re-written from Tsyganenko's subroutine STEP by 
C  Natalia and Vladimir Papitashvili in mid-1980s

      COMMON/A5/DS3
      
          DS3 = -DS/3.
      CALL RIGHT(X,Y,Z,R11,R12,R13)
      CALL RIGHT(X+R11,Y+R12,Z+R13,R21,R22,R23)
      CALL RIGHT(X+.5*(R11+R21),Y+.5*(R12+R22),Z+.5*(R13+R23),
     *R31,R32,R33)
      CALL RIGHT(X+.375*(R11+3.*R31),Y+.375*(R12+3.*R32),
     *Z+.375*(R13+3.*R33),R41,R42,R43)
      CALL RIGHT(X+1.5*(R11-3.*R31+4.*R41),
     *Y+1.5*(R12-3.*R32+4.*R42),Z+1.5*(R13-3.*R33+4.*R43),
     *R51,R52,R53)
        X = X+.5*(R11+4.*R41+R51)
        Y = Y+.5*(R12+4.*R42+R52)
        Z = Z+.5*(R13+4.*R43+R53)

      RETURN
      END

C  *********************************************************************

      SUBROUTINE RIGHT(X,Y,Z,R1,R2,R3)

C  Similar to SUBR RHAND from GEOPACK-1996 but RIGHT takes into account
C  only internal sources

C  The code is re-written from Tsyganenko's subroutine RHAND
C  by Natalia and Vladimir Papitashvili in mid-1980s

      COMMON /A5/DS3
      COMMON /NM/NM
      COMMON /IYR/IYR

      CALL SPHCAR(R,T,F,X,Y,Z,-1)
      CALL IGRF(IYR,NM,R,T,F,BR,BT,BF)
      CALL BSPCAR(T,F,BR,BT,BF,BX,BY,BZ)
        B = DS3/SQRT(BX**2+BY**2+BZ**2)
       R1 = BX*B
       R2 = BY*B
       R3 = BZ*B

      RETURN
      END

C  *********************************************************************
        SUBROUTINE IGRF(IY,NM,R,T,F,BR,BT,BF)

c  Jan 20, 2001: Subroutine IGRF is modified by V. Papitashvili - SHA
c    coefficients for IGRF-2000, and SV 2000-2005 are added (note that
c    IGRF-1995 has not been changed to DGRF-1995 this time
c    (see http://www.ngdc.noaa.gov/IAGA/wg8/igrf2000.html)

c  Aug 26, 1997: Subroutine IGRF is modified by V. Papitashvili - SHA
c    coefficients for DGRF-1990, IGRF-1995, and SV 1995-2000 are added
c    (EOS, v.77, No.16, p.153, April 16, 1996)

c  Feb 03, 1995: Modified by Vladimir Papitashvili (SPRL, University of 
c    Michigan) to accept dates between 1945 and 2000 
       
C  MODIFIED TO ACCEPT DATES BETWEEN 1965 AND 2000; COEFFICIENTS FOR IGRF
C  1985 HAVE BEEN REPLACED WITH DGRF1985 COEFFICIENTS [EOS TRANS. AGU 
C  APRIL 21, 1992, C  P. 182]. ALSO, THE CODE IS MODIFIED TO ACCEPT 
C  DATES BEYOND 1990, AND TO USE LINEAR EXTRAPOLATION BETWEEN 1990 AND 
C  2000 BASED ON THE IGRF COEFFICIENTS FROM THE SAME EOS ARTICLE

C  Modified by Mauricio Peredo, Hughes STX at NASA/GSFC, September 1992

C  CALCULATES COMPONENTS OF MAIN GEOMAGNETIC FIELD IN SPHERICAL 
C  GEOCENTRIC COORDINATE SYSTEM BY USING THIRD GENERATION IGRF MODEL 
C  (J. GEOMAG. GEOELECTR. V.34, P.313-315, 1982; GEOMAGNETISM AND 
C  AERONOMY V.26, P.523-525, 1986).

C  UPDATING OF COEFFICIENTS TO A GIVEN EPOCH IS MADE DURING THE FIRST 
C  CALL AND AFTER EVERY CHANGE OF PARAMETER IY

C---INPUT PARAMETERS:
C  IY - YEAR NUMBER (FROM 1945 UP TO 1990)
C  NM - MAXIMAL ORDER OF HARMONICS TAKEN INTO ACCOUNT (NOT MORE THAN 10)
C  R,T,F - SPHERICAL COORDINATES OF THE POINT (R IN UNITS RE=6371.2 KM,
C    COLATITUDE T AND LONGITUDE F IN RADIANS)
C---OUTPUT PARAMETERS:
C  BR,BT,BF - SPHERICAL COMPONENTS OF MAIN GEOMAGNETIC FIELD (in nT)

C  AUTHOR: NIKOLAI A. TSYGANENKO, INSTITUTE OF PHYSICS, ST.-PETERSBURG
C      STATE UNIVERSITY, STARY PETERGOF 198904, ST.-PETERSBURG, RUSSIA
C      (now the NASA Goddard Space Fligth Center, Greenbelt, Maryland)

      IMPLICIT NONE

C  G0, G1, and H1 are used in SUBROUTINE DIP to calculate geodipole's 
C  moment for a given year
  
      COMMON /DMOM/ G0,G1,H1
      REAL A(11),B(11),G(66),H(66),REC(66),
     *G1945(66),H1945(66),
     *G1950(66),H1950(66),G1955(66),H1955(66),
     *G1960(66),H1960(66),G1965(66),H1965(66),
     *G1970(66),H1970(66),G1975(66),H1975(66),
     *G1980(66),H1980(66),G1985(66),H1985(66),
     *G1990(66),H1990(66),G1995(66),H1995(66),
     *G2000(66),H2000(66),
     *DG2000(45),DH2000(45)

      REAL R,T,F,BR,BT,BF,DT,F2,F1,S,P,AA,PP,D,BBR,BBF,U,CF,SF,
     *     C,W,X,Y,Z,Q,BI,P2,D2,AN,E,HH,BBT,QQ,XK,DP,PM,G0,G1,H1

      INTEGER IY,NM,MA,IPR,IYR,KNM,N,N2,M,MNN,MN,K,MM

      LOGICAL BK,BM

      DATA G1945/0.,-30594.,-2285.,-1244., 2990., 1578., 1282.,-1834.,
     * 1255.,  913.,   944.,  776.,  544., -421.,  304., -253.,  346.,
     *  194.,  -20.,  -142.,  -82.,   59.,   57.,    6., -246.,  -25.,
     *   21., -104.,    70.,  -40.,    0.,    0.,  -29.,  -10.,   15.,
     *   29.,   13.,     7.,   -8.,   -5.,    9.,    7.,  -10.,    7.,
     *    2.,    5.,   -21.,    1.,  -11.,    3.,   16.,   -3.,   -4.,
     *   -3.,   -4.,    -3.,   11.,    1.,    2.,   -5.,   -1.,    8.,
     *   -1.,   -3.,     5.,   -2./

      DATA H1945/0.,     0., 5810.,    0.,-1702.,  477.,    0., -499.,
     *  186.,  -11.,     0.,  144., -276.,  -55., -178.,    0.,  -12.,
     *   95.,  -67.,  -119.,   82.,    0.,    6.,  100.,   16.,   -9.,
     *  -16.,  -39.,     0.,  -45.,  -18.,    2.,    6.,   28.,  -17.,
     *  -22.,    0.,    12.,  -21.,  -12.,   -7.,    2.,   18.,    3.,
     *  -11.,    0.,   -27.,   17.,   29.,   -9.,    4.,    9.,    6.,
     *    1.,    8.,     0.,    5.,    1.,  -20.,   -1.,   -6.,    6.,
     *   -4.,   -2.,     0.,   -2./

      DATA G1950/0.,-30554.,-2250.,-1341., 2998., 1576., 1297.,-1889.,
     * 1274.,  896.,   954.,  792.,  528., -408.,  303., -240.,  349.,
     *  211.,  -20.,  -147.,  -76.,   54.,   57.,    4., -247.,  -16.,
     *   12., -105.,    65.,  -55.,    2.,    1.,  -40.,   -7.,    5.,
     *   19.,   22.,    15.,   -4.,   -1.,   11.,   15.,  -13.,    5.,
     *   -1.,    3.,    -7.,   -1.,  -25.,   10.,    5.,   -5.,   -2.,
     *    3.,    8.,    -8.,    4.,   -1.,   13.,   -4.,    4.,   12.,
     *    3.,    2.,    10.,    3./

      DATA H1950/0.,     0., 5815.,    0.,-1810.,  381.,    0., -476.,
     *  206.,  -46.,     0.,  136., -278.,  -37., -210.,    0.,    3.,
     *  103.,  -87.,  -122.,   80.,    0.,   -1.,   99.,   33.,  -12.,
     *  -12.,  -30.,     0.,  -35.,  -17.,    0.,   10.,   36.,  -18.,
     *  -16.,    0.,     5.,  -22.,    0.,  -21.,   -8.,   17.,   -4.,
     *  -17.,    0.,   -24.,   19.,   12.,    2.,    2.,    8.,    8.,
     *  -11.,   -7.,     0.,   13.,   -2.,  -10.,    2.,   -3.,    6.,
     *   -3.,    6.,    11.,    8./

      DATA G1955/0.,-30500.,-2215.,-1440., 3003., 1581., 1302.,-1944.,
     * 1288.,  882.,   958.,  796.,  510., -397.,  290., -229.,  360.,
     *  230.,  -23.,  -152.,  -69.,   47.,   57.,    3., -247.,   -8.,
     *    7., -107.,    65.,  -56.,    2.,   10.,  -32.,  -11.,    9.,
     *   18.,   11.,     9.,   -6.,  -14.,    6.,   10.,   -7.,    6.,
     *    9.,    4.,     9.,   -4.,   -5.,    2.,    4.,    1.,    2.,
     *    2.,    5.,    -3.,   -5.,   -1.,    2.,   -3.,    7.,    4.,
     *   -2.,    6.,    -2.,    0./

      DATA H1955/0.,     0., 5820.,    0.,-1898.,  291.,    0., -462.,
     *  216.,  -83.,     0.,  133., -274.,  -23., -230.,    0.,   15.,
     *  110.,  -98.,  -121.,   78.,    0.,   -9.,   96.,   48.,  -16.,
     *  -12.,  -24.,     0.,  -50.,  -24.,   -4.,    8.,   28.,  -20.,
     *  -18.,    0.,    10.,  -15.,    5.,  -23.,    3.,   23.,   -4.,
     *  -13.,    0.,   -11.,   12.,    7.,    6.,   -2.,   10.,    7.,
     *   -6.,    5.,     0.,   -4.,    0.,   -8.,   -2.,   -4.,    1.,
     *   -3.,    7.,    -1.,   -3./

      DATA G1960/0.,-30421.,-2169.,-1555., 3002., 1590., 1302.,-1992.,
     * 1289.,  878.,   957.,  800.,  504., -394.,  269., -222.,  362.,
     *  242.,  -26.,  -156.,  -63.,   46.,   58.,    1., -237.,   -1.,
     *   -2., -113.,    67.,  -56.,    5.,   15.,  -32.,   -7.,   17.,
     *    8.,   15.,     6.,   -4.,  -11.,    2.,   10.,   -5.,   10.,
     *    8.,    4.,     6.,    0.,   -9.,    1.,    4.,   -1.,   -2.,
     *    3.,   -1.,     1.,   -3.,    4.,    0.,   -1.,    4.,    6.,
     *    1.,   -1.,     2.,    0./

      DATA H1960/0.,     0., 5791.,    0.,-1967.,  206.,    0., -414.,
     *  224., -130.,     0.,  135., -278.,    3., -255.,    0.,   16.,
     *  125., -117.,  -114.,   81.,    0.,  -10.,   99.,   60.,  -20.,
     *  -11.,  -17.,     0.,  -55.,  -28.,   -6.,    7.,   23.,  -18.,
     *  -17.,    0.,    11.,  -14.,    7.,  -18.,    4.,   23.,    1.,
     *  -20.,    0.,   -18.,   12.,    2.,    0.,   -3.,    9.,    8.,
     *    0.,    5.,     0.,    4.,    1.,    0.,    2.,   -5.,    1.,
     *   -1.,    6.,     0.,   -7./

      DATA G1965/0.,-30334.,-2119.,-1662., 2997., 1594., 1297.,-2038.,
     * 1292.,  856.,   957.,  804.,  479., -390.,  252., -219.,  358.,
     *  254.,  -31.,  -157.,  -62.,   45.,   61.,    8., -228.,    4.,
     *    1., -111.,    75.,  -57.,    4.,   13.,  -26.,   -6.,   13.,
     *    1.,   13.,     5.,   -4.,  -14.,    0.,    8.,   -1.,   11.,
     *    4.,    8.,    10.,    2.,  -13.,   10.,   -1.,   -1.,    5.,
     *    1.,   -2.,    -2.,   -3.,    2.,   -5.,   -2.,    4.,    4.,
     *    0.,    2.,     2.,    0./

      DATA H1965/0.,     0., 5776.,    0.,-2016.,  114.,    0., -404.,
     *  240., -165.,     0.,  148., -269.,   13., -269.,    0.,   19.,
     *  128., -126.,   -97.,   81.,    0.,  -11.,  100.,   68.,  -32.,
     *   -8.,   -7.,     0.,  -61.,  -27.,   -2.,    6.,   26.,  -23.,
     *  -12.,    0.,     7.,  -12.,    9.,  -16.,    4.,   24.,   -3.,
     *  -17.,    0.,   -22.,   15.,    7.,   -4.,   -5.,   10.,   10.,
     *   -4.,    1.,     0.,    2.,    1.,    2.,    6.,   -4.,    0.,
     *   -2.,    3.,     0.,   -6./

      DATA G1970/0.,-30220.,-2068.,-1781., 3000., 1611., 1287.,-2091.,
     * 1278.,  838.,   952.,  800.,  461., -395.,  234., -216.,  359.,
     *  262.,  -42.,  -160.,  -56.,   43.,   64.,   15., -212.,    2.,
     *    3., -112.,    72.,  -57.,    1.,   14.,  -22.,   -2.,   13.,
     *   -2.,   14.,     6.,   -2.,  -13.,   -3.,    5.,    0.,   11.,
     *    3.,    8.,    10.,    2.,  -12.,   10.,   -1.,    0.,    3.,
     *    1.,   -1.,    -3.,   -3.,    2.,   -5.,   -1.,    6.,    4.,
     *    1.,    0.,     3.,   -1./

      DATA H1970/0.,     0., 5737.,    0.,-2047.,   25.,    0., -366.,
     *  251., -196.,     0.,  167., -266.,   26., -279.,    0.,   26.,
     *  139., -139.,   -91.,   83.,    0.,  -12.,  100.,   72.,  -37.,
     *   -6.,    1.,     0.,  -70.,  -27.,   -4.,    8.,   23.,  -23.,
     *  -11.,    0.,     7.,  -15.,    6.,  -17.,    6.,   21.,   -6.,
     *  -16.,    0.,   -21.,   16.,    6.,   -4.,   -5.,   10.,   11.,
     *   -2.,    1.,     0.,    1.,    1.,    3.,    4.,   -4.,    0.,
     *   -1.,    3.,     1.,   -4./

      DATA G1975/0.,-30100.,-2013.,-1902., 3010., 1632., 1276.,-2144.,
     * 1260.,  830.,   946.,  791.,  438., -405.,  216., -218.,  356.,
     *  264.,  -59.,  -159.,  -49.,   45.,   66.,   28., -198.,    1.,
     *    6., -111.,    71.,  -56.,    1.,   16.,  -14.,    0.,   12.,
     *   -5.,   14.,     6.,   -1.,  -12.,   -8.,    4.,    0.,   10.,
     *    1.,    7.,    10.,    2.,  -12.,   10.,   -1.,   -1.,    4.,
     *    1.,   -2.,    -3.,   -3.,    2.,   -5.,   -2.,    5.,    4.,
     *    1.,    0.,     3.,   -1./

      DATA H1975/0.,     0., 5675.,    0.,-2067.,  -68.,    0., -333.,
     *  262., -223.,     0.,  191., -265.,   39., -288.,    0.,   31.,
     *  148., -152.,   -83.,   88.,    0.,  -13.,   99.,   75.,  -41.,
     *   -4.,   11.,     0.,  -77.,  -26.,   -5.,   10.,   22.,  -23.,
     *  -12.,    0.,     6.,  -16.,    4.,  -19.,    6.,   18.,  -10.,
     *  -17.,    0.,   -21.,   16.,    7.,   -4.,   -5.,   10.,   11.,
     *   -3.,    1.,     0.,    1.,    1.,    3.,    4.,   -4.,   -1.,
     *   -1.,    3.,     1.,   -5./

      DATA G1980/0.,-29992.,-1956.,-1997., 3027., 1663., 1281.,-2180.,
     * 1251.,  833.,   938.,  782.,  398., -419.,  199., -218.,  357.,
     *  261.,  -74.,  -162.,  -48.,   48.,   66.,   42., -192.,    4.,
     *   14., -108.,    72.,  -59.,    2.,   21.,  -12.,    1.,   11.,
     *   -2.,   18.,     6.,    0.,  -11.,   -7.,    4.,    3.,    6.,
     *   -1.,    5.,    10.,    1.,  -12.,    9.,   -3.,   -1.,    7.,
     *    2.,   -5.,    -4.,   -4.,    2.,   -5.,   -2.,    5.,    3.,
     *    1.,    2.,     3.,    0./

      DATA H1980/0.,     0., 5604.,    0.,-2129., -200.,    0., -336.,
     *  271., -252.,     0.,  212., -257.,   53., -297.,    0.,   46.,
     *  150., -151.,   -78.,   92.,    0.,  -15.,   93.,   71.,  -43.,
     *   -2.,   17.,     0.,  -82.,  -27.,   -5.,   16.,   18.,  -23.,
     *  -10.,    0.,     7.,  -18.,    4.,  -22.,    9.,   16.,  -13.,
     *  -15.,    0.,   -21.,   16.,    9.,   -5.,   -6.,    9.,   10.,
     *   -6.,    2.,     0.,    1.,    0.,    3.,    6.,   -4.,    0.,
     *   -1.,    4.,     0.,   -6./

      DATA G1985/0.,-29873.,-1905.,-2072., 3044., 1687., 1296.,-2208.,
     * 1247.,  829.,   936.,  780.,  361., -424.,  170., -214.,  355.,
     *  253.,  -93.,  -164.,  -46.,   53.,   65.,   51., -185.,    4.,
     *   16., -102.,    74.,  -62.,    3.,   24.,   -6.,    4.,   10.,
     *    0.,   21.,     6.,    0.,  -11.,   -9.,    4.,    4.,    4.,
     *   -4.,    5.,    10.,    1.,  -12.,    9.,   -3.,   -1.,    7.,
     *    1.,   -5.,    -4.,   -4.,    3.,   -5.,   -2.,    5.,    3.,
     *    1.,    2.,     3.,    0./

      DATA H1985/0.,     0., 5500.,    0.,-2197., -306.,    0., -310.,
     *  284., -297.,     0.,  232., -249.,   69., -297.,    0.,   47.,
     *  150., -154.,   -75.,   95.,    0.,  -16.,   88.,   69.,  -48.,
     *   -1.,   21.,     0.,  -83.,  -27.,   -2.,   20.,   17.,  -23.,
     *   -7.,    0.,     8.,  -19.,    5.,  -23.,   11.,   14.,  -15.,
     *  -11.,    0.,   -21.,   15.,    9.,   -6.,   -6.,    9.,    9.,
     *   -7.,    2.,     0.,    1.,    0.,    3.,    6.,   -4.,    0.,
     *   -1.,    4.,     0.,   -6./

      DATA G1990/0.,-29775.,-1848.,-2131., 3059., 1686., 1314.,-2239.,
     * 1248.,  802.,   939.,  780.,  325., -423.,  141., -214.,  353.,
     *  245., -109.,  -165.,  -36.,   61.,   65.,   59., -178.,    3.,
     *   18.,  -96.,    77.,  -64.,    2.,   26.,   -1.,    5.,    9.,
     *    0.,   23.,     5.,   -1.,  -10.,  -12.,    3.,    4.,    2.,
     *   -6.,    4.,     9.,    1.,  -12.,    9.,   -4.,   -2.,    7.,
     *    1.,   -6.,    -3.,   -4.,    2.,   -5.,   -2.,    4.,    3.,
     *    1.,    3.,     3.,    0./

      DATA H1990/0.,     0., 5406.,    0.,-2279., -373.,    0., -284.,
     *  293., -352.,     0.,  247., -240.,   84., -299.,    0.,   46.,
     *  154., -153.,   -69.,   97.,    0.,  -16.,   82.,   69.,  -52.,
     *    1.,   24.,     0.,  -80.,  -26.,    0.,   21.,   17.,  -23.,
     *   -4.,    0.,    10.,  -19.,    6.,  -22.,   12.,   12.,  -16.,
     *  -10.,    0.,   -20.,   15.,   11.,   -7.,   -7.,    9.,    8.,
     *   -7.,    2.,     0.,    2.,    1.,    3.,    6.,   -4.,    0.,
     *   -2.,    3.,    -1.,   -6./

      DATA G1995/0.,-29682.,-1789.,-2197., 3074., 1685., 1329.,-2268.,
     * 1249.,  769.,   941.,  782.,  291., -421.,  116., -210.,  352.,
     *  237., -122.,  -167.,  -26.,   66.,   64.,   65., -172.,    2.,
     *   17.,  -94.,    78.,  -67.,    1.,   29.,    4.,    8.,   10.,
     *   -2.,   24.,     4.,   -1.,   -9.,  -14.,    4.,    5.,    0.,
     *   -7.,    4.,     9.,    1.,  -12.,    9.,   -4.,   -2.,    7.,
     *    0.,   -6.,    -3.,   -4.,    2.,   -5.,   -2.,    4.,    3.,
     *    1.,    3.,     3.,    0./

      DATA H1995/0.,     0., 5318.,    0.,-2356., -425.,    0., -263.,
     *  302., -406.,     0.,  262., -232.,   98., -301.,    0.,   44.,
     *  157., -152.,   -64.,   99.,    0.,  -16.,   77.,   67.,  -57.,
     *    4.,   28.,     0.,  -77.,  -25.,    3.,   22.,   16.,  -23.,
     *   -3.,    0.,    12.,  -20.,    7.,  -21.,   12.,   10.,  -17.,
     *  -10.,    0.,   -19.,   15.,   11.,   -7.,   -7.,    9.,    7.,
     *   -8.,    1.,     0.,    2.,    1.,    3.,    6.,   -4.,    0.,
     *   -2.,    3.,    -1.,   -6./

      DATA G2000/0.,-29615.,-1728.,-2167., 3072., 1672., 1341.,-2290.,
     * 1253.,  715.,   935.,  787.,  251., -405.,  110., -217.,  351.,
     *  222., -131.,  -169.,  -12.,   72.,   68.,   74., -161.,   -5.,
     *   17.,  -91.,    79.,  -74.,    0.,   33.,    9.,    7.,    8.,
     *   -2.,   25.,     6.,   -9.,   -8.,  -17.,    9.,    7.,   -8.,
     *   -7.,    5.,     9.,    3.,   -8.,    6.,   -9.,   -2.,    9.,
     *   -4.,   -8.,    -2.,   -6.,    2.,   -3.,   -0.,    4.,    1.,
     *    2.,    4.,     0.,   -1./

      DATA H2000/0.,     0., 5186.,    0.,-2478., -458.,    0., -227.,
     *  296., -492.,     0.,  272., -232.,  119., -304.,    0.,   44.,
     *  172., -134.,   -40.,  107.,    0.,  -17.,   64.,   65.,  -61.,
     *    1.,   44.,     0.,  -65.,  -24.,    6.,   24.,   15.,  -25.,
     *   -6.,    0.,    12.,  -22.,    8.,  -21.,   15.,    9.,  -16.,
     *   -3.,    0.,   -20.,   13.,   12.,   -6.,   -8.,    9.,    4.,
     *   -8.,    5.,     0.,    1.,    0.,    4.,    5.,   -6.,   -1.,
     *   -3.,    0.,    -2.,   -8./

      DATA DG2000/0.,  14.6,  10.7, -12.4,   1.1,  -1.1,   0.7,  -5.4,
     *   0.9,   -7.7,  -1.3,   1.6,  -7.3,   2.9,  -3.2,   0.0,  -0.7,
     *  -2.1,   -2.8,  -0.8,   2.5,   1.0,  -0.4,   0.9,   2.0,  -0.6,
     *  -0.3,    1.2,  -0.4,  -0.4,  -0.3,   1.1,   1.1,  -0.2,   0.6, 
     *  -0.9,   -0.3,   0.2,  -0.3,   0.4,  -1.0,   0.3,  -0.5,  -0.7,
     *  -0.4/

      DATA DH2000/0.,   0.0, -22.5, -20.6,  -9.6,   6.0,  -0.1, -14.2,
     *  2.1,     1.3,   5.0,   0.3,  -0.1,   0.6,   1.7,   1.9,   0.1,
     * -0.2,    -1.4,   0.0,  -0.8,   0.0,   0.9,   1.1,   0.0,   0.3,
     * -0.1,    -0.6,  -0.7,   0.2,   0.1,   0.0,   0.0,   0.3,   0.6,
     * -0.4,     0.3,   0.7,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
     *  0.0/

      DATA MA,IYR,IPR/0,0,0/

      IF(MA.NE.1) GOTO 10
      IF(IY.NE.IYR) GOTO 30
      GOTO 130

10     MA = 1
      KNM = 15

      DO 20 N=1,11
         N2=2*N-1
         N2=N2*(N2-2)
         DO 20 M=1,N
            MN=N*(N-1)/2+M
20    REC(MN)=FLOAT((N-M)*(N+M-2))/FLOAT(N2)

30    IYR=IY
      IF (IYR.LT.1945) IYR=1945
      IF (IYR.GT.2005) IYR=2005
      IF (IY.NE.IYR.AND.IPR.EQ.0) write(*,999)IY,IYR
999   FORMAT(//1X,
     * '*** IGRF WARNS: YEAR IS OUT OF INTERVAL 1945-2005: IY =',I5/,
     *',         CALCULATIONS WILL BE DONE FOR IYR =',I5,' ****'//)

      IF (IYR.NE.IY) IPR=1
      IF (IYR.LT.1950) GOTO 1945      !INTERPOLATE BETWEEN 1945 - 1950
      IF (IYR.LT.1955) GOTO 1950      !INTERPOLATE BETWEEN 1950 - 1955
      IF (IYR.LT.1960) GOTO 1955      !INTERPOLATE BETWEEN 1955 - 1960
      IF (IYR.LT.1965) GOTO 1960      !INTERPOLATE BETWEEN 1960 - 1965
      IF (IYR.LT.1970) GOTO 1965      !INTERPOLATE BETWEEN 1965 - 1970
      IF (IYR.LT.1975) GOTO 1970      !INTERPOLATE BETWEEN 1970 - 1975
      IF (IYR.LT.1980) GOTO 1975      !INTERPOLATE BETWEEN 1975 - 1980
      IF (IYR.LT.1985) GOTO 1980      !INTERPOLATE BETWEEN 1980 - 1985
      IF (IYR.LT.1990) GOTO 1985      !INTERPOLATE BETWEEN 1985 - 1990
      IF (IYR.LT.1995) GOTO 1990      !INTERPOLATE BETWEEN 1990 - 1995
      IF (IYR.LT.2000) GOTO 1995      !INTERPOLATE BETWEEN 1995 - 2000

C  EXTRAPOLATE BETWEEN 2000 - 2005

      DT=FLOAT(IYR)-2000.
      DO N=1,45
        G(N)=G2000(N)+DG2000(N)*DT
        H(N)=H2000(N)+DH2000(N)*DT
      ENDDO
	GOTO 300

C  INTERPOLATE BETWEEEN 1945 - 1950

1945  F2=(IYR-1945)/5.
      F1=1.-F2
      DO N=1,66
        G(N)=G1945(N)*F1+G1950(N)*F2
        H(N)=H1945(N)*F1+H1950(N)*F2
      ENDDO
      GOTO 300

C  INTERPOLATE BETWEEEN 1950 - 1955

1950  F2=(IYR-1950)/5.
      F1=1.-F2
      DO N=1,66
        G(N)=G1950(N)*F1+G1955(N)*F2
        H(N)=H1950(N)*F1+H1955(N)*F2
      ENDDO
      GOTO 300

C  INTERPOLATE BETWEEEN 1955 - 1960

1955  F2=(IYR-1955)/5.
      F1=1.-F2
      DO N=1,66
        G(N)=G1955(N)*F1+G1960(N)*F2
        H(N)=H1955(N)*F1+H1960(N)*F2
      ENDDO
      GOTO 300

C  INTERPOLATE BETWEEEN 1960 - 1965

1960  F2=(IYR-1960)/5.
      F1=1.-F2
      DO N=1,66
        G(N)=G1960(N)*F1+G1965(N)*F2
        H(N)=H1960(N)*F1+H1965(N)*F2
      ENDDO
      GOTO 300

C  INTERPOLATE BETWEEEN 1965 - 1970

1965  F2=(IYR-1965)/5.
      F1=1.-F2
      DO N=1,66
        G(N)=G1965(N)*F1+G1970(N)*F2
        H(N)=H1965(N)*F1+H1970(N)*F2
      ENDDO
      GOTO 300

C  INTERPOLATE BETWEEN 1970 - 1975

1970  F2=(IYR-1970)/5.
      F1=1.-F2
      DO N=1,66
        G(N)=G1970(N)*F1+G1975(N)*F2
        H(N)=H1970(N)*F1+H1975(N)*F2
      ENDDO
      GOTO 300

C  INTERPOLATE BETWEEN 1975 - 1980

1975  F2=(IYR-1975)/5.
      F1=1.-F2
      DO N=1,66
        G(N)=G1975(N)*F1+G1980(N)*F2
        H(N)=H1975(N)*F1+H1980(N)*F2
      ENDDO
      GOTO 300

C  INTERPOLATE BETWEEN 1980 - 1985

1980  F2=(IYR-1980)/5.
      F1=1.-F2
      DO N=1,66
        G(N)=G1980(N)*F1+G1985(N)*F2
        H(N)=H1980(N)*F1+H1985(N)*F2
      ENDDO
      GOTO 300

C  INTERPOLATE BETWEEN 1985 - 1990

1985  F2=(IYR-1985)/5.
      F1=1.-F2
      DO N=1,66
        G(N)=G1985(N)*F1+G1990(N)*F2
        H(N)=H1985(N)*F1+H1990(N)*F2
      ENDDO
      GOTO 300

C  INTERPOLATE BETWEEN 1990 - 1995

1990  F2=(IYR-1990)/5.
      F1=1.-F2
      DO N=1,66
        G(N)=G1990(N)*F1+G1995(N)*F2
        H(N)=H1990(N)*F1+H1995(N)*F2
      ENDDO
      GOTO 300

C  INTERPOLATE BETWEEN 1995 - 2000

1995  F2=(IYR-1995)/5.
      F1=1.-F2
      DO N=1,66
        G(N)=G1995(N)*F1+G2000(N)*F2
        H(N)=H1995(N)*F1+H2000(N)*F2
      ENDDO
      GOTO 300

C  GET HERE WHEN COEFFICIENTS FOR APPROPRIATE IGRF MODEL HAVE BEEN 
C  ASSIGNED

300    S = 1.

      G0 = G(2)
      G1 = G(3)
      H1 = H(3)

      DO 120 N=2,11
         MN=N*(N-1)/2+1
         S=S*FLOAT(2*N-3)/FLOAT(N-1)
         G(MN)=G(MN)*S
         H(MN)=H(MN)*S
         P=S
         DO 120 M=2,N
            AA=1.
            IF (M.EQ.2) AA=2.
            P=P*SQRT(AA*FLOAT(N-M+1)/FLOAT(N+M-2))
            MNN=MN+M-1
            G(MNN)=G(MNN)*P
120         H(MNN)=H(MNN)*P

130   IF(KNM.EQ.NM) GO TO 140
      KNM=NM
      K=KNM+1
140   PP=1./R
      P=PP
      DO 150 N=1,K
         P=P*PP
         A(N)=P
150      B(N)=P*N
      P=1.
      D=0.
      BBR=0.
      BBT=0.
      BBF=0.
      U=T
      CF=COS(F)
      SF=SIN(F)
      C=COS(U)
      S=SIN(U)
      BK=(S.LT.1.E-5)
      DO 200 M=1,K
         BM=(M.EQ.1)
         IF(BM) GOTO 160
         MM=M-1
         W=X
         X=W*CF+Y*SF
         Y=Y*CF-W*SF
         GOTO 170
160      X=0.
         Y=1.
170      Q=P
         Z=D
         BI=0.
         P2=0.
         D2=0.
         DO 190 N=M,K
            AN=A(N)
            MN=N*(N-1)/2+M
            E=G(MN)
            HH=H(MN)
            W=E*Y+HH*X
            BBR=BBR+B(N)*W*Q
            BBT=BBT-AN*W*Z
            IF(BM) GOTO 180
            QQ=Q
            IF(BK) QQ=Z
            BI=BI+AN*(E*X-HH*Y)*QQ
180         XK=REC(MN)
            DP=C*Z-S*Q-XK*D2
            PM=C*Q-XK*P2
            D2=Z
            P2=Q
            Z=DP
190        Q=PM
         D=S*D+C*P
         P=S*P
         IF(BM) GOTO 200
         BI=BI*MM
         BBF=BBF+BI
200   CONTINUE

      BR=BBR
      BT=BBT
      IF(BK) GOTO 210
      BF=BBF/S
      GOTO 220

210   IF(C.LT.0.) BBF=-BBF
      BF=BBF

220   CONTINUE

      RETURN
      END

C  *********************************************************************

      SUBROUTINE RECALC(IYR,IDAY,IHOUR,MIN,ISEC)

C  THIS IS A MODIFIED VERSION OF THE SUBROUTINE RECOMP WRITTEN BY 
C  N. A. TSYGANENKO. SINCE I WANT TO USE IT IN PLACE OF SUBROUTINE 
C  RECALC, I HAVE RENAMED THIS ROUTINE RECALC AND ELIMINATED THE 
C  ORIGINAL RECALC FROM THIS VERSION OF THE <GEOPACK.FOR> PACKAGE. 
C  THIS WAY ALL ORIGINAL CALLS TO RECALC WILL CONTINUE TO WORK WITHOUT
C  HAVING TO CHANGE THEM TO CALLS TO RECOMP.

C  AN ALTERNATIVE VERSION OF THE SUBROUTINE RECALC FROM THE GEOPACK 
C  PACKAGE BASED ON A DIFFERENT APPROACH TO DERIVATION OF ROTATION 
C  MATRIX ELEMENTS

C  THIS SUBROUTINE WORKS BY 20% FASTER THAN RECALC AND IS EASIER TO 
C  UNDERSTAND
C  #####################################################
C  #  WRITTEN BY  N.A. TSYGANENKO ON DECEMBER 1, 1991  #
C  #####################################################
C  Modified by Mauricio Peredo, Hughes STX at NASA/GSFC Code 695, 
C  September 1992

c  Modified to accept years up to 2005 (V. Papitashvili, January 2001)

c  Modified to accept dates up to year 2000 and updated IGRF coeficients
c  from 1945 (updated by V. Papitashvili, February 1995)

C   OTHER SUBROUTINES CALLED BY THIS ONE: SUN

C     IYR = YEAR NUMBER (FOUR DIGITS)
C     IDAY = DAY OF YEAR (DAY 1 = JAN 1)
C     IHOUR = HOUR OF DAY (00 TO 23)
C     MIN = MINUTE OF HOUR (00 TO 59)
C     ISEC = SECONDS OF DAY(00 TO 59)

        IMPLICIT NONE

        REAL ST0,CT0,SL0,CL0,CTCL,STCL,CTSL,STSL,SFI,CFI,SPS,CPS,
     1       SHI,CHI,HI,PSI,XMUT,A11,A21,A31,A12,A22,A32,A13,A23,
     2       A33,DS3,F2,F1,G10,G11,H11,DT,SQ,SQQ,SQR,S1,S2,
     3       S3,CGST,SGST,DIP1,DIP2,DIP3,Y1,Y2,Y3,Y,Z1,Z2,Z3,DJ,
     4       T,OBLIQ,DZ1,DZ2,DZ3,DY1,DY2,DY3,EXMAGX,EXMAGY,EXMAGZ,
     5       EYMAGX,EYMAGY,GST,SLONG,SRASN,SDEC,BA(8)

        INTEGER IYR,IDAY,IHOUR,MIN,ISEC,K,IY,IDE,IYE,IPR

       COMMON/C1/ ST0,CT0,SL0,CL0,CTCL,STCL,CTSL,STSL,SFI,CFI,SPS,CPS,
     * SHI,CHI,HI,PSI,XMUT,A11,A21,A31,A12,A22,A32,A13,A23,A33,DS3,
     * K,IY,BA

      DATA IYE,IDE,IPR/3*0/
      IF (IYR.EQ.IYE.AND.IDAY.EQ.IDE) GOTO 5

C  IYE AND IDE ARE THE CURRENT VALUES OF YEAR AND DAY NUMBER

      IY=IYR
      IDE=IDAY
      IF(IY.LT.1945) IY=1945
      IF(IY.GT.2005) IY=2005

C  WE ARE RESTRICTED BY THE INTERVAL 1945-2005, FOR WHICH THE IGRF 
C  COEFFICIENTS ARE KNOWN; IF IYR IS OUTSIDE THIS INTERVAL, THE 
C  SUBROUTINE GIVES A WARNING (BUT DOES NOT REPEAT IT AT THE NEXT CALLS)

      IF(IY.NE.IYR.AND.IPR.EQ.0) PRINT 10,IYR,IY
      IF(IY.NE.IYR) IPR=1
      IYE=IY

C  LINEAR INTERPOLATION OF THE GEODIPOLE MOMENT COMPONENTS BETWEEN THE
C  VALUES FOR THE NEAREST EPOCHS:

        IF (IY.LT.1950) THEN                            !1945-1950
           F2=(FLOAT(IY)+FLOAT(IDAY)/365.-1945.)/5.
           F1=1.D0-F2
           G10=30594.*F1+30554.*F2
           G11=-2285.*F1-2250.*F2
           H11=5810.*F1+5815.*F2
        ELSEIF (IY.LT.1955) THEN                        !1950-1955
           F2=(FLOAT(IY)+FLOAT(IDAY)/365.-1950.)/5.
           F1=1.D0-F2
           G10=30554.*F1+30500.*F2
           G11=-2250.*F1-2215.*F2
           H11=5815.*F1+5820.*F2
        ELSEIF (IY.LT.1960) THEN                        !1955-1960
           F2=(FLOAT(IY)+FLOAT(IDAY)/365.-1955.)/5.
           F1=1.D0-F2
           G10=30500.*F1+30421.*F2
           G11=-2215.*F1-2169.*F2
           H11=5820.*F1+5791.*F2
        ELSEIF (IY.LT.1965) THEN                        !1960-1965
           F2=(FLOAT(IY)+FLOAT(IDAY)/365.-1960.)/5.
           F1=1.D0-F2
           G10=30421.*F1+30334.*F2
           G11=-2169.*F1-2119.*F2
           H11=5791.*F1+5776.*F2
        ELSEIF (IY.LT.1970) THEN                        !1965-1970
           F2=(FLOAT(IY)+FLOAT(IDAY)/365.-1965.)/5.
           F1=1.D0-F2
           G10=30334.*F1+30220.*F2
           G11=-2119.*F1-2068.*F2
           H11=5776.*F1+5737.*F2
        ELSEIF (IY.LT.1975) THEN                        !1970-1975
           F2=(FLOAT(IY)+FLOAT(IDAY)/365.-1970.)/5.
           F1=1.D0-F2
           G10=30220.*F1+30100.*F2
           G11=-2068.*F1-2013.*F2
           H11=5737.*F1+5675.*F2
        ELSEIF (IY.LT.1980) THEN                        !1975-1980
           F2=(DFLOAT(IY)+DFLOAT(IDAY)/365.-1975.)/5.
           F1=1.D0-F2
           G10=30100.*F1+29992.*F2
           G11=-2013.*F1-1956.*F2
           H11=5675.*F1+5604.*F2
        ELSEIF (IY.LT.1985) THEN                        !1980-1985
           F2=(FLOAT(IY)+FLOAT(IDAY)/365.-1980.)/5.
           F1=1.D0-F2
           G10=29992.*F1+29873.*F2
           G11=-1956.*F1-1905.*F2
           H11=5604.*F1+5500.*F2
        ELSEIF (IY.LT.1990) THEN                        !1985-1990
           F2=(FLOAT(IY)+FLOAT(IDAY)/365.-1985.)/5.
           F1=1.D0-F2
           G10=29873.*F1+29775.*F2
           G11=-1905.*F1-1848.*F2
           H11=5500.*F1+5406.*F2
        ELSEIF (IY.LT.1995) THEN                        !1990-1995
           F2=(FLOAT(IY)+FLOAT(IDAY)/365.-1990.)/5.
           F1=1.D0-F2
           G10=29775.*F1+29682.*F2
           G11=-1848.*F1-1789.*F2
           H11=5406.*F1+5318.*F2
        ELSEIF (IY.LT.2000) THEN                        !1995-2000
           F2=(FLOAT(IY)+FLOAT(IDAY)/365.-1995.)/5.
           F1=1.D0-F2
           G10=29682.*F1+29615.*F2
           G11=-1789.*F1-1728.*F2
           H11=5318.*F1+5186.*F2
        ELSE                                            !2000-2005
           DT=FLOAT(IY)+FLOAT(IDAY)/365.-2000.
           G10=29615.-14.6*DT
           G11=-1728.+10.7*DT
           H11=5186.-22.5*DT
        ENDIF

C  NOW CALCULATE THE COMPONENTS OF THE UNIT VECTOR EzMAG IN GEO COORD
C  SYSTEM: 
C  SIN(TETA0)*COS(LAMBDA0), SIN(TETA0)*SIN(LAMBDA0), AND COS(TETA0)
C         ST0 * CL0                ST0 * SL0                CT0

      SQ=G11**2+H11**2
      SQQ=SQRT(SQ)
      SQR=SQRT(G10**2+SQ)
      SL0=-H11/SQQ
      CL0=-G11/SQQ
      ST0=SQQ/SQR
      CT0=G10/SQR
      STCL=ST0*CL0
      STSL=ST0*SL0
      CTSL=CT0*SL0
      CTCL=CT0*CL0

C  THE CALCULATIONS ARE TERMINATED IF ONLY GEO-MAG TRANSFORMATION
C  IS TO BE DONE  (IHOUR>24 IS THE AGREED CONDITION FOR THIS CASE):

   5   IF (IHOUR.GT.24) RETURN

      CALL SUN(IY,IDAY,IHOUR,MIN,ISEC,GST,SLONG,SRASN,SDEC)

C  S1,S2, AND S3 ARE THE COMPONENTS OF THE UNIT VECTOR EXGSM=EXGSE 
C  IN THE SYSTEM GEI POINTING FROM THE EARTH'S CENTER TO THE SUN:

      S1=COS(SRASN)*COS(SDEC)
      S2=SIN(SRASN)*COS(SDEC)
      S3=SIN(SDEC)
      CGST=COS(GST)
      SGST=SIN(GST)

C  DIP1, DIP2, AND DIP3 ARE THE COMPONENTS OF THE UNIT VECTOR 
C  EZSM=EZMAG IN THE SYSTEM GEI:

      DIP1=STCL*CGST-STSL*SGST
      DIP2=STCL*SGST+STSL*CGST
      DIP3=CT0

C  NOW CALCULATE THE COMPONENTS OF THE UNIT VECTOR EYGSM IN THE SYSTEM
C  GEI BY TAKING THE VECTOR PRODUCT D x S AND NORMALIZING IT TO UNIT 
C  LENGTH:

      Y1=DIP2*S3-DIP3*S2
      Y2=DIP3*S1-DIP1*S3
      Y3=DIP1*S2-DIP2*S1
      Y=SQRT(Y1*Y1+Y2*Y2+Y3*Y3)
      Y1=Y1/Y
      Y2=Y2/Y
      Y3=Y3/Y

C  THEN IN THE GEI SYSTEM THE UNIT VECTOR Z=EZGSM=EXGSM x EYGSM=S x Y
C  HAS THE COMPONENTS:

      Z1=S2*Y3-S3*Y2
      Z2=S3*Y1-S1*Y3
      Z3=S1*Y2-S2*Y1

C  THE VECTOR EZGSE (HERE DZ) IN GEI HAS THE COMPONENTS (0,-SIN(DELTA),
C  COS(DELTA)) = (0.,-0.397823,0.917462); HERE DELTA = 23.44214 DEG FOR
C  THE EPOCH 1978 (SEE THE BOOK BY GUREVICH OR OTHER ASTRONOMICAL 
C  HANDBOOKS). HERE THE MOST ACCURATE TIME-DEPENDENT FORMULA IS USED:

      DJ=FLOAT(365*(IY-1900)+(IY-1901)/4 +IDAY)-0.5+FLOAT(ISEC)/86400.
      T=DJ/36525.
      OBLIQ=(23.45229-0.0130125*T)/57.2957795
      DZ1=0.
      DZ2=-SIN(OBLIQ)
      DZ3=COS(OBLIQ)

C  THEN THE UNIT VECTOR EYGSE IN GEI SYSTEM IS THE VECTOR PRODUCT DZ x S

      DY1=DZ2*S3-DZ3*S2
      DY2=DZ3*S1-DZ1*S3
      DY3=DZ1*S2-DZ2*S1

C  THE ELEMENTS OF THE MATRIX GSE TO GSM ARE THE SCALAR PRODUCTS:
C  CHI=EM22=(EYGSM,EYGSE), SHI=EM23=(EYGSM,EZGSE), 
C  EM32=(EZGSM,EYGSE)=-EM23, AND EM33=(EZGSM,EZGSE)=EM22

      CHI=Y1*DY1+Y2*DY2+Y3*DY3
      SHI=Y1*DZ1+Y2*DZ2+Y3*DZ3
      HI=ASIN(SHI)

C  TILT ANGLE: PSI=ARCSIN(DIP,EXGSM)

      SPS=DIP1*S1+DIP2*S2+DIP3*S3
      CPS=SQRT(1.-SPS**2)
      PSI=ASIN(SPS)

C  THE ELEMENTS OF THE MATRIX MAG TO SM ARE THE SCALAR PRODUCTS:
C  CFI=GM22=(EYSM,EYMAG), SFI=GM23=(EYSM,EXMAG); THEY CAN BE DERIVED 
C  AS FOLLOWS:

C  IN GEO THE VECTORS EXMAG AND EYMAG HAVE THE COMPONENTS 
C  (CT0*CL0,CT0*SL0,-ST0) AND (-SL0,CL0,0), RESPECTIVELY. HENCE, IN 
C  GEI SYSTEM THE COMPONENTS ARE:
C  EXMAG:    CT0*CL0*COS(GST)-CT0*SL0*SIN(GST)
C            CT0*CL0*SIN(GST)+CT0*SL0*COS(GST)
C            -ST0
C  EYMAG:    -SL0*COS(GST)-CL0*SIN(GST)
C            -SL0*SIN(GST)+CL0*COS(GST)
C             0
C  THE COMPONENTS OF EYSM IN GEI WERE FOUND ABOVE AS Y1, Y2, AND Y3;
C  NOW WE ONLY HAVE TO COMBINE THE QUANTITIES INTO SCALAR PRODUCTS:

      EXMAGX=CT0*(CL0*CGST-SL0*SGST)
      EXMAGY=CT0*(CL0*SGST+SL0*CGST)
      EXMAGZ=-ST0
      EYMAGX=-(SL0*CGST+CL0*SGST)
      EYMAGY=-(SL0*SGST-CL0*CGST)
      CFI=Y1*EYMAGX+Y2*EYMAGY
      SFI=Y1*EXMAGX+Y2*EXMAGY+Y3*EXMAGZ

      XMUT=(ATAN2(SFI,CFI)+3.1415926536)*3.8197186342

C  THE ELEMENTS OF THE MATRIX GEO TO GSM ARE THE SCALAR PRODUCTS:

C  A11=(EXGEO,EXGSM), A12=(EYGEO,EXGSM), A13=(EZGEO,EXGSM),
C  A21=(EXGEO,EYGSM), A22=(EYGEO,EYGSM), A23=(EZGEO,EYGSM),
C  A31=(EXGEO,EZGSM), A32=(EYGEO,EZGSM), A33=(EZGEO,EZGSM),

C  ALL THE UNIT VECTORS IN BRACKETS ARE ALREADY DEFINED IN GEI:

C  EXGEO=(CGST,SGST,0), EYGEO=(-SGST,CGST,0), EZGEO=(0,0,1)
C  EXGSM=(S1,S2,S3),  EYGSM=(Y1,Y2,Y3),   EZGSM=(Z1,Z2,Z3)
C  AND  THEREFORE:

      A11=S1*CGST+S2*SGST
      A12=-S1*SGST+S2*CGST
      A13=S3
      A21=Y1*CGST+Y2*SGST
      A22=-Y1*SGST+Y2*CGST
      A23=Y3
      A31=Z1*CGST+Z2*SGST
      A32=-Z1*SGST+Z2*CGST
      A33=Z3

 10   FORMAT(//1X,
     * '****RECALC WARNS: YEAR IS OUT OF INTERVAL 1945-2005: IYR=',I4,
     * /,6X,'CALCULATIONS WILL BE DONE FOR IYR=',I4,/)

      RETURN
      END

C  *********************************************************************

      SUBROUTINE SUN(IYR,IDAY,IHOUR,MIN,ISEC,GST,SLONG,SRASN,SDEC)

C  CALCULATES FOUR QUANTITIES NECESSARY FOR COORDINATE TRANSFORMATIONS
C  WHICH DEPEND ON SUN POSITION (AND, HENCE, ON UNIVERSAL TIME AND 
C  SEASON)

C---INPUT PARAMETERS:
C  IYR,IDAY,IHOUR,MIN,ISEC - YEAR, DAY, AND UNIVERSAL TIME IN HOURS, 
C    MINUTES, AND SECONDS  (IDAY=1 CORRESPONDS TO JANUARY 1).

C---OUTPUT PARAMETERS:
C  GST - GREENWICH MEAN SIDEREAL TIME, SLONG - LONGITUDE ALONG ECLIPTIC
C  SRASN - RIGHT ASCENSION,  SDEC - DECLINATION  OF THE SUN (RADIANS)
C  THIS SUBROUTINE HAS BEEN COMPILED FROM: 
C  RUSSELL C.T., COSM.ELECTRODYN., 1971, V.2,PP.184-196.

C  AUTHOR: Gilbert D. Mead

      IMPLICIT NONE

      REAL GST,SLONG,SRASN,SDEC,RAD,T,VL,G,OBLIQ,SOB,SLP,SIND,COSD,SC
      INTEGER IYR,IDAY,IHOUR,MIN,ISEC
      DOUBLE PRECISION DJ,FDAY

      DATA RAD/57.295779513/

      IF(IYR.LT.1901.OR.IYR.GT.2099) RETURN
      FDAY=DFLOAT(IHOUR*3600+MIN*60+ISEC)/86400.D0
      DJ=365*(IYR-1900)+(IYR-1901)/4+IDAY-0.5D0+FDAY
      T=DJ/36525.
      VL=DMOD(279.696678+0.9856473354*DJ,360.D0)
      GST=DMOD(279.690983+.9856473354*DJ+360.*FDAY+180.,360.D0)/RAD
      G=DMOD(358.475845+0.985600267*DJ,360.D0)/RAD
      SLONG=(VL+(1.91946-0.004789*T)*SIN(G)+0.020094*SIN(2.*G))/RAD
      IF(SLONG.GT.6.2831853) SLONG=SLONG-6.2831853
      IF (SLONG.LT.0.) SLONG=SLONG+6.2831853
      OBLIQ=(23.45229-0.0130125*T)/RAD
      SOB=SIN(OBLIQ)
      SLP=SLONG-9.924E-5

C   THE LAST CONSTANT IS A CORRECTION FOR THE ANGULAR ABERRATION  
C   DUE TO THE ORBITAL MOTION OF THE EARTH

      SIND=SOB*SIN(SLP)
      COSD=SQRT(1.-SIND**2)
      SC=SIND/COSD
      SDEC=ATAN(SC)
      SRASN=3.141592654-ATAN2(COS(OBLIQ)/SOB*SC,-COS(SLP)/COSD)
      RETURN
      END
     
C  *********************************************************************

      SUBROUTINE SPHCAR(R,TETA,PHI,X,Y,Z,J)

C   CONVERTS SPHERICAL COORDS INTO CARTESIAN ONES AND VICA VERSA
C    (TETA AND PHI IN RADIANS).

C                  J>0            J<0
C-----INPUT:   J,R,TETA,PHI     J,X,Y,Z
C----OUTPUT:      X,Y,Z        R,TETA,PHI

C  AUTHOR: NIKOLAI A. TSYGANENKO, INSTITUTE OF PHYSICS, ST.-PETERSBURG
C      STATE UNIVERSITY, STARY PETERGOF 198904, ST.-PETERSBURG, RUSSIA
C      (now the NASA Goddard Space Fligth Center, Greenbelt, Maryland)

        IMPLICIT NONE

        REAL R,TETA,PHI,X,Y,Z,SQ

        INTEGER J

      IF(J.GT.0) GOTO 3
      SQ=X**2+Y**2
      R=SQRT(SQ+Z**2)
      IF (SQ.NE.0.) GOTO 2
      PHI=0.
      IF (Z.LT.0.) GOTO 1
      TETA=0.
      RETURN
  1   TETA=3.141592654
      RETURN
  2   SQ=SQRT(SQ)
      PHI=ATAN2(Y,X)
      TETA=ATAN2(SQ,Z)
      IF (PHI.LT.0.) PHI=PHI+6.28318531
      RETURN
  3   SQ=R*SIN(TETA)
      X=SQ*COS(PHI)
      Y=SQ*SIN(PHI)
      Z=R*COS(TETA)

      RETURN
      END

C  *********************************************************************

      SUBROUTINE BSPCAR(TETA,PHI,BR,BTET,BPHI,BX,BY,BZ)

C   CALCULATES CARTESIAN FIELD COMPONENTS FROM SPHERICAL ONES
C-----INPUT:   TETA,PHI - SPHERICAL ANGLES OF THE POINT IN RADIANS
C              BR,BTET,BPHI -  SPHERICAL COMPONENTS OF THE FIELD
C-----OUTPUT:  BX,BY,BZ - CARTESIAN COMPONENTS OF THE FIELD

C  AUTHOR: NIKOLAI A. TSYGANENKO, INSTITUTE OF PHYSICS, ST.-PETERSBURG
C      STATE UNIVERSITY, STARY PETERGOF 198904, ST.-PETERSBURG, RUSSIA
C      (now the NASA Goddard Space Fligth Center, Greenbelt, Maryland)

        IMPLICIT NONE

        REAL TETA,PHI,BR,BTET,BPHI,BX,BY,BZ,S,C,SF,CF,BE

      S=SIN(TETA)
      C=COS(TETA)
      SF=SIN(PHI)
      CF=COS(PHI)
      BE=BR*S+BTET*C
      BX=BE*CF-BPHI*SF
      BY=BE*SF+BPHI*CF
      BZ=BR*C-BTET*S
      RETURN
      END

C  *********************************************************************

      SUBROUTINE GEOMAG(XGEO,YGEO,ZGEO,XMAG,YMAG,ZMAG,J,IYR)

C CONVERTS GEOCENTRIC (GEO) TO DIPOLE (MAG) COORDINATES OR VICA VERSA.
C IYR IS YEAR NUMBER (FOUR DIGITS).

C                           J>0                J<0
C-----INPUT:  J,XGEO,YGEO,ZGEO,IYR   J,XMAG,YMAG,ZMAG,IYR
C-----OUTPUT:    XMAG,YMAG,ZMAG        XGEO,YGEO,ZGEO

C  AUTHOR: NIKOLAI A. TSYGANENKO, INSTITUTE OF PHYSICS, ST.-PETERSBURG
C      STATE UNIVERSITY, STARY PETERGOF 198904, ST.-PETERSBURG, RUSSIA
C      (now the NASA Goddard Space Fligth Center, Greenbelt, Maryland)

        IMPLICIT NONE

        REAL XGEO,YGEO,ZGEO,XMAG,YMAG,ZMAG,ST0,CT0,SL0,CL0,CTCL,
     *       STCL,CTSL,STSL,AB(19),BB(8)

        INTEGER J,IYR,K,IY,II

      COMMON/C1/ ST0,CT0,SL0,CL0,CTCL,STCL,CTSL,STSL,AB,K,IY,BB
      DATA II/1/
      IF(IYR.EQ.II) GOTO 1
      II=IYR
      CALL RECALC(II,0,25,0,0)
  1   CONTINUE
      IF(J.LT.0) GOTO 2
      XMAG=XGEO*CTCL+YGEO*CTSL-ZGEO*ST0
      YMAG=YGEO*CL0-XGEO*SL0
      ZMAG=XGEO*STCL+YGEO*STSL+ZGEO*CT0
      RETURN
  2   XGEO=XMAG*CTCL-YMAG*SL0+ZMAG*STCL
      YGEO=XMAG*CTSL+YMAG*CL0+ZMAG*STSL
      ZGEO=ZMAG*CT0-XMAG*ST0

      RETURN
      END

C  *********************************************************************

      SUBROUTINE MAGSM(XMAG,YMAG,ZMAG,XSM,YSM,ZSM,J)

C CONVERTS DIPOLE (MAG) TO SOLAR MAGNETIC (SM) COORDINATES OR VICA VERSA

C                    J>0              J<0
C-----INPUT: J,XMAG,YMAG,ZMAG     J,XSM,YSM,ZSM
C----OUTPUT:    XSM,YSM,ZSM       XMAG,YMAG,ZMAG
C  ATTENTION: SUBROUTINE RECALC MUST BE CALLED BEFORE MAGSM IN TWO CASES
C     /A/  BEFORE THE FIRST USE OF MAGSM
C     /B/  IF THE CURRENT VALUES OF IYEAR,IDAY,IHOUR,MIN,ISEC ARE 
C          DIFFERENT FROM THOSE IN THE PRECEDING CALL OF  MAGSM

C  AUTHOR: NIKOLAI A. TSYGANENKO, INSTITUTE OF PHYSICS, ST.-PETERSBURG
C      STATE UNIVERSITY, STARY PETERGOF 198904, ST.-PETERSBURG, RUSSIA
C      (now the NASA Goddard Space Fligth Center, Greenbelt, Maryland)

        IMPLICIT NONE

        REAL XMAG,YMAG,ZMAG,XSM,YSM,ZSM,SFI,CFI,A(8),B(7),
     *       AB(10),BA(8)

        INTEGER J,K,IY

      COMMON/C1/ A,SFI,CFI,B,AB,K,IY,BA
      IF (J.LT.0) GOTO 1
      XSM=XMAG*CFI-YMAG*SFI
      YSM=XMAG*SFI+YMAG*CFI
      ZSM=ZMAG
      RETURN
  1   XMAG=XSM*CFI+YSM*SFI
      YMAG=YSM*CFI-XSM*SFI
      ZMAG=ZSM

      RETURN
      END

C  *********************************************************************

       SUBROUTINE SMGSM(XSM,YSM,ZSM,XGSM,YGSM,ZGSM,J)

C CONVERTS SOLAR MAGNETIC (SM) TO SOLAR MAGNETOSPHERIC (GSM) COORDINATES
C   OR VICA VERSA.

C                  J>0                 J<0
C-----INPUT: J,XSM,YSM,ZSM        J,XGSM,YGSM,ZGSM
C----OUTPUT:  XGSM,YGSM,ZGSM       XSM,YSM,ZSM

C  ATTENTION: SUBROUTINE RECALC MUST BE CALLED BEFORE SMGSM IN TWO CASES
C     /A/  BEFORE THE FIRST USE OF SMGSM
C     /B/  IF THE CURRENT VALUES OF IYEAR,IDAY,IHOUR,MIN,ISEC ARE 
C          DIFFERENT FROM THOSE IN THE PRECEDING CALL OF SMGSM

C  AUTHOR: NIKOLAI A. TSYGANENKO, INSTITUTE OF PHYSICS, ST.-PETERSBURG
C      STATE UNIVERSITY, STARY PETERGOF 198904, ST.-PETERSBURG, RUSSIA
C      (now the NASA Goddard Space Fligth Center, Greenbelt, Maryland)

        IMPLICIT NONE

        REAL XSM,YSM,ZSM,XGSM,YGSM,ZGSM,SPS,CPS,A(10),B(15),AB(8)
        INTEGER J,K,IY

      COMMON/C1/ A,SPS,CPS,B,K,IY,AB
      IF (J.LT.0) GOTO 1
      XGSM=XSM*CPS+ZSM*SPS
      YGSM=YSM
      ZGSM=ZSM*CPS-XSM*SPS
      RETURN
  1   XSM=XGSM*CPS-ZGSM*SPS
      YSM=YGSM
      ZSM=XGSM*SPS+ZGSM*CPS

      RETURN
      END

C  *********************************************************************
