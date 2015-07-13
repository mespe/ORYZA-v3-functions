SUBROUTINE READ_WEA (YR_daily)
USE COM
!CHARACTER (LEN=100) :: WeaPath = "D:\Pepijn\Pheno_opt_rice\weather\china"
!CHARACTER (LEN=100) :: WeaPath = "D:\wur\pheno_opt_rice\pheno_opt_rice\weather\"
CHARACTER (LEN=100) :: WeaPath = "~/oryza_functions/pheno_opt_rice3"

TYPE :: WEATHER
   INTEGER SITE,YEAR, DAY
   REAL RADKJM2, TMIN,TMAX,VP,WIND,RAIN
END TYPE WEATHER


  TYPE (WEATHER) :: A
  DIMENSION YR_daily (10,731)
  CHARACTER (LEN=80) :: ClimFile
  CHARACTER (LEN=80) :: comment


  INTEGER EMDYRnext
  CHARACTER (LEN=80) :: EMDYRnext_STR
  CHARACTER (LEN=80) :: EMDYR_STR

	WRITE (EMDYR_STR,'(i4)') EMDYR
	ClimFile = TRIM(ADJUSTL(WeaPath))//TRIM(ADJUSTL(SITE_STR))//"."//TRIM(ADJUSTL(EMDYR_STR(2:4)))
!	ClimFile = TRIM(ADJUSTL(WeaPath))//TRIM(ADJUSTL(SITE))//"."//TRIM(ADJUSTL(EMDYR_STR(2:4)))

OPEN (11, FILE=ClimFile)

DO WHILE (.TRUE.)  ! for checking the start of data
100	  READ (11,"(A80)",IOSTAT=ERROR) comment

	  IF(ERROR/=0) RETURN

	  IF (comment(1:1)=="*") THEN
	  	GOTO 100
	  ELSE
		BACKSPACE (11)
        READ(11,*) LON,LAT,ALT,ALPHA,BETA2
		GOTO 101
	  END IF
END DO       ! end checking the start of data

101 continue
	DO I= 1, YR_day(EMDYR), 1
          READ(11,*,IOSTAT=ERROR) A%SITE,A%YEAR,A%DAY,A%RADKJM2,A%TMIN,A%TMAX,A%VP,A%WIND,A%RAIN
           IF (ERROR/=0) THEN
             CLOSE (11)
             GOTO 102
	       END IF
		   IF (ABS(I)/=A%DAY) THEN
				BACKSPACE(11)
			!	WRITE (*,*) I, A%DAY, YR_daily(4,i)
				GOTO 106
		   ELSE

				YR_daily(1,i)=A%YEAR
				YR_daily(2,i)=A%DAY
				YR_daily(3,i)= A%RADKJM2
				YR_daily(4,i)= A%TMIN
				YR_daily(5,i)= A%TMAX
				YR_daily(6,i)= A%VP
				YR_daily(7,i)= A%WIND
				YR_daily(8,i)= A%RAIN
				YR_daily(9,i)= DAYL_CAL (A%DAY, LAT)	! DAYL_CAL = Astronomical daylength (base = 0 degrees), hours
				YR_daily(10,i)= DAYLP_CAL (A%DAY, LAT)	! DAYLP_CAL = Photoperiodic daylength (base = -4 degrees), hours
				!WRITE (*,*) I, A%DAY, YR_daily(4,i)
				IF ((A%TMIN.EQ.-99).OR.(A%TMIN.EQ.99).OR.(A%TMAX.EQ.-99).OR.(A%TMAX.EQ.99)) THEN
					WRITE(*,*) ""
					WRITE(*,*) "ERROR: Weather file has missing data (-99 or 99) for TMIN or TMAX: ", ClimFile
					WRITE(*,*) "Check for yourself if there are more missing data"
					WRITE(*,*) ""
					CALL EXIT(0)
				END IF
		   END IF
106     END DO

102 continue
	CLOSE (11)
	!WRITE (*,*) "WEATHER"
!	write (*,*) YR_daily(2,350), YR_daily(2,350)

	EMDYRnext=EMDYR+1
	WRITE (EMDYRnext_STR,'(i4)') EMDYRnext
	ClimFile =TRIM(ADJUSTL(WeaPath))//TRIM(ADJUSTL(SITE_STR))//"."//TRIM(ADJUSTL(EMDYRnext_STR(2:4)))
!	ClimFile =TRIM(ADJUSTL(WeaPath))//TRIM(ADJUSTL(SITE))//"."//TRIM(ADJUSTL(EMDYRnext_STR(2:4)))



OPEN (11, FILE=ClimFile)
DO WHILE (.TRUE.)  ! for checking the start of data
103	  READ (11,"(A80)",IOSTAT=ERROR) comment

	  IF(ERROR/=0) RETURN

	  IF (comment(1:1)=="*") THEN
	  	GOTO 103
	  ELSE
		BACKSPACE (11)
        READ(11,*) LON,LAT,ALT,ALPHA,BETA
		GOTO 104
	  END IF
END DO       ! end checking the start of data

104 continue
      DO I= YR_day(EMDYR)+1, YR_day(EMDYR)+1+YR_day(EMDYRnext), 1
          READ(11,*,IOSTAT=ERROR) A%SITE, A%YEAR,A%DAY,A%RADKJM2,A%TMIN,A%TMAX,A%VP,A%WIND,A%RAIN
           IF (ERROR/=0) THEN
             CLOSE (11)
             GOTO 105
	       END IF

		   IF (ABS(I-(YR_day(EMDYR)))/=A%DAY) THEN
				BACKSPACE(11)
				!WRITE (*,*) I, A%DAY, YR_daily(4,i)
				GOTO 107
		   ELSE
				YR_daily(1,i)=A%YEAR
				YR_daily(2,i)=A%DAY
				YR_daily(3,i)= A%RADKJM2
				YR_daily(4,i)= A%TMIN
				YR_daily(5,i)= A%TMAX
				YR_daily(6,i)= A%VP
				YR_daily(7,i)= A%WIND
				YR_daily(8,i)= A%RAIN
				YR_daily(9,i)= DAYL_CAL (A%DAY, LAT)	! DAYL_CAL = Astronomical daylength (base = 0 degrees), hours
				YR_daily(10,i)= DAYLP_CAL (A%DAY, LAT)	! DAYLP_CAL = Photoperiodic daylength (base = -4 degrees), hours
				!WRITE (*,*) I, A%DAY, YR_daily(4,i)
				IF ((A%TMIN.EQ.-99).OR.(A%TMIN.EQ.99).OR.(A%TMAX.EQ.-99).OR.(A%TMAX.EQ.99)) THEN
					WRITE(*,*) ""
					WRITE(*,*) "ERROR: Weather file has missing data (-99 or 99) for TMIN or TMAX: ", ClimFile
					WRITE(*,*) "Check for yourself if there are more missing data"
					WRITE(*,*) ""
					CALL EXIT(0)
				END IF
		   END IF
107      END DO

105 continue
	CLOSE (11)


RETURN
END SUBROUTINE
