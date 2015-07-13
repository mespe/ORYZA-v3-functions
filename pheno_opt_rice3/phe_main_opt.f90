PROGRAM MAIN

USE COM
USE SUBPHENO
USE PTU_SUM
USE Rerun
USE start
USE STATISTICS

real etime          ! Declare the type of etime()
real elapsed(2)     ! For receiving user and system time
real total          ! For receiving total time

! Read observed phenological data and site nr
OPEN (10, FILE=pheno_in,STATUS='OLD')
J = 0
DO WHILE (.TRUE.)
    READ (10,*,IOSTAT=ERROR) COMMENT
	IF (ERROR/=0) THEN 
		GOTO 102
	ENDIF
	IF (COMMENT(1:1).EQ."*") THEN
        WRITE(*,*)"COMMENT LINE"
	ELSE 
        BACKSPACE (10)
        READ (10,*,IOSTAT=ERROR) SITE_STR, EMD, EMDYR,IDOYTR, IYRTR, IDOYPI, IYRPI, IDOYFL, IYRFL, IDOYM, IYRM
	    J = J+1
	    write(*,*)"J=",J,"; EMD = ",EMD,"; EMDYR = ",EMDYR,"; SITE_STR = ",SITE_STR
!	    write(*,*)"J=",J,"; IDOYTR = ",IDOYTR,"; IYRTR = ",IYRTR
!	    write(*,*)"J=",J,"; IDOYPI = ",IDOYPI,"; IYRPI = ",IYRPI
!	    write(*,*)"J=",J,"; IDOYFL = ",IDOYFL,"; IYRFL = ",IYRFL
!	    write(*,*)"J=",J,"; IDOYM = ",IDOYM,"; IYRM = ",IYRM
    !	OBS_SET(J,1) = SITE
		SITE_STR_SET(J,1) = SITE_STR
		OBS_SET(J,2) =  EMD
	    OBS_SET(J,3) =  EMDYR
	    OBS_SET(J,4) = IDOYTR
	    OBS_SET(J,5) =  IYRTR
	    OBS_SET(J,6) =  IDOYPI
	    OBS_SET(J,7) =  IYRPI
	    OBS_SET(J,8) =  IDOYFL
	    OBS_SET(J,9) =  IYRFL
	    OBS_SET(J,10) =  IDOYM
	    OBS_SET(J,11) =  IYRM
    END IF	
END DO	
102 continue
CLOSE(10)

! PAUSE "PAUSE. ENTER TO CONTINUE"

! activate one of the 5 sets below
CALL START_bilinear1()
CALL Read_Output_bilinear1()

!CALL START_bilinear2()
!CALL Read_Output_bilinear2()

!CALL START_bilinear3()
!CALL Read_Output_bilinear3()
!!! NOTICE: AVERAGE TEMPERATURES REPORTED HERE ARE WITH TM_CORR CORRECTION
! i.e. if TMEFL = 30.4 oC (average air temperature from emergence to flowering) 
! and if we assume canopy/water temperature is TM_CORR = -10.0 oC higher then
! here the output will report TMEFL = TMEFL + TM_CORR = 30.4 - 10 = 20.4 oC

!CALL START_beta()
!CALL Read_Output_BETA()

!notice in the NOPSP run TSENPSP = TSEN; TSENPSPNGHT = TSENNGHT; SPSP = 999.; MOPP = 0.; PPSE = 0.
!notice the number of runs to go as written to the DOS window is not correct, but this does not
!affect the results. If you want to print the correct number of runs to go as written to the DOS window
!then adjust in rerun_opt.f90
!CALL START_betaNOPSP()
!CALL Read_Output_BETA()


total = etime(elapsed)
print *, 'End: total=', total, ' user=', elapsed(1),' system=', elapsed(2)

END PROGRAM





  