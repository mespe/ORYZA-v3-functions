MODULE COM

INTEGER IDOY, IYR, SET, ERROR
INTEGER EMD, EMDYR,IDOYTR, IYRTR, IDOYPI, IYRPI, IDOYFL, IYRFL, IDOYM, IYRM
INTEGER IDOYPISIM, IYRPISIM, IDOYFLSIM, IYRFLSIM, IDOYMSIM, IYRMSIM,IFLM,IFLMSIM,IEFL,IEFLSIM,IEPI,IEPISIM,IPIFL,IPIFLSIM
REAL CPTU, CPTUTRA,CPTUPI, CPTUFL, CPTUM,CPTUFLM,TMEFL,TMFLM,TMEM, CPTUFLPREV, CPTUFLMPREV
INTEGER EMD_array, TRA_array,PI_array,FL_array,M_array
REAL, allocatable :: TBD_set (:), TOD_set (:), TODNGHT_set(:), TMD_set(:), SHCKD_set(:),SPSP_set(:), MOPP_set (:), PPSE_set (:)
REAL, allocatable :: TSEN_set(:), TSENNGHT_set(:), TSENPSP_set(:), TSENPSPNGHT_set(:), TM_CORR_set(:)
!100 here means we reserve space for at most 100 experiments
INTEGER OBS_SET(100,11)
CHARACTER (LEN=20) :: SITE_STR_SET(100,1)
CHARACTER (LEN=20) :: SITE_STR
REAL DATASET_PIFLERROR(100),DATASET_EFLERROR(100),DATASET_FLMERROR(100),OUTPUT(47,100)
REAL LON, LAT, ALT, ALPHA, BETA2
CHARACTER (LEN=80) :: BETAOUTPUT
CHARACTER (LEN=80) :: BETASLOPECV
CHARACTER (LEN=80) :: SITE
INTEGER OUTPUT_NUM, EXP_NUM

CHARACTER (LEN=80) :: Pheno_in="Pheno_in.txt"

CONTAINS
!---------Calculating daylength
! DAYL_CAL = Astronomical daylength (base = 0 degrees), hours
REAL FUNCTION DAYL_CAL (day, lat)
     Integer day
     REAL lat
	 REAL DEC, SINLD, COSLD, AOB
	 REAL DSINB,SOLCON
     REAL :: PI=3.1415927
	 REAL :: DEGTRAD =0.017453292
      DEC   = -ASIN (SIN (23.45*DEGTRAD)*COS (2.*PI*(day+10.)/365.))
      SINLD = SIN (DEGTRAD*LAT)*SIN (DEC)
      COSLD = COS (DEGTRAD*LAT)*COS (DEC)
      AOB   = SINLD/COSLD

	  IF (AOB.LT.-1.) THEN
         WRITE (*,'(2A)') ' WARNING from SASTRO: latitude above polar circle, daylength=0 hours'
         DAYL_CAL  = 0.
         RETURN
      ELSE IF (AOB.GT.1.) THEN
         WRITE (*,'(2A)') ' WARNING from SASTRO: latitude within polar circle, daylength=24 hours'
         DAYL_CAL  = 24.
         RETURN
      ELSE
         DAYL_CAL  = 12.*(1.+2.*ASIN (AOB)/PI)
      END IF
      
RETURN
END FUNCTION
!---------Calculating daylength
! DAYLP_CAL = Photoperiodic daylength (base = -4 degrees), hours
REAL FUNCTION DAYLP_CAL (day, lat)
     Integer day
     REAL lat
	 REAL DEC, SINLD, COSLD, AOB
	 REAL DSINB,SOLCON
     REAL :: PI=3.1415927
	 REAL :: DEGTRAD =0.017453292
	 REAL :: BASE = -4.
      DEC   = -ASIN (SIN (23.45*DEGTRAD)*COS (2.*PI*(day+10.)/365.))
      SINLD = SIN (DEGTRAD*LAT)*SIN (DEC)
      COSLD = COS (DEGTRAD*LAT)*COS (DEC)
      AOB   = SINLD/COSLD

	  IF (AOB.LT.-1.) THEN
         WRITE (*,'(2A)') ' WARNING from SASTRO: latitude above polar circle, daylength=0 hours'
         DAYLP_CAL = 0.
         RETURN
      ELSE IF (AOB.GT.1.) THEN
         WRITE (*,'(2A)') ' WARNING from SASTRO: latitude within polar circle, daylength=24 hours'
         DAYLP_CAL = 24.
         RETURN
      ELSE
         DAYLP_CAL = 12.0*(1.+2.*ASIN ((-SIN(BASE*DEGTRAD)+SINLD)/COSLD)/PI)
      END IF
      
RETURN
END FUNCTION

!-------determine leapyear and daynumber of one year
INTEGER Function YR_day(YR)
   INTEGER YR
   LOGICAL LEAPYEAR
   IF (MOD(YR,400)==0) THEN 
      LEAPYEAR=.TRUE.
	  YR_day = 366
   ELSE
      IF(MOD(YR,100)==0) THEN
	    LEAPYEAR=.FALSE.
		YR_day = 365
	  ELSE
	    IF (MOD(YR,4)==0) THEN
		  LEAPYEAR=.TRUE.
		  YR_day = 366
		ELSE
		  LEAPYEAR=.FALSE.
		  YR_day = 365
		ENDIF
	  ENDIF
	ENDIF
RETURN
END FUNCTION

!------normalized EMD---------------------
SUBROUTINE normalize (A,N,M, day_normal)

INTEGER M,N
REAL A(N,M)
!REAL YEAR (M)
INTEGER YEAR (M)
REAL DAY(M)
REAL day_normal(M)
!REAL YR_min
INTEGER YR_min

DO I=1,M
	DAY(I)=A(2,I)
	YEAR(I)=A(3,I)
END DO

YR_min=YEAR(1)

! Finding the minimum value of YEAR
do i=2,M-1
	if (YR_min>=YEAR(i)) then
		YR_min=YEAR(i)
	end if
end do
day_normal_pre=0.0
DO I=1,M
	IF (YEAR(I)==YR_min) THEN
		day_normal(i)=DAY(i)
	ELSE
		DO J=YR_min, YEAR(i)-1,1
			day_normal_pre=day_normal_pre+YR_day(j)
		END DO
			day_normal(i)=day_normal_pre+day(i)
	END IF
		day_normal_pre=0.0
END DO

RETURN
END SUBROUTINE



!------calculating the ID number of observed phenology in YR_daily
SUBROUTINE Day_array (YR_daily)

DIMENSION YR_daily (10,731)
!WRITE (*,*) "dAY_ARRAY", EMD, EMDYR
DO I=1,731
	IF (YR_daily (2,I)==EMD.AND.YR_daily (1,I)==EMDYR) THEN
		EMD_array=I
	ELSEIF (YR_daily (2,I)==IDOYTR.AND.YR_daily (1,I)==IYRTR) THEN
		TRA_array=I
	ELSEIF (YR_daily (2,I)==IDOYPI.AND.YR_daily (1,I)==IYRPI) THEN
		PI_array=I
	ELSEIF (YR_daily (2,I)==IDOYFL.AND.YR_daily (1,I)==IYRFL) THEN
		FL_array=I
	ELSEIF (YR_daily (2,I)==IDOYM.AND.YR_daily (1,I)==IYRM) THEN
		M_array= I
	END IF
END DO
!WRITE (*,*) "dAY_ARRAY", EMD_ARRAY
RETURN
END SUBROUTINE

END MODULE