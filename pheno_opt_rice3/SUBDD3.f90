MODULE SUBPHENO
USE COM
CHARACTER (LEN=80) OUT


CONTAINS
!-------Bilinear1: ORYZA2000 with SPSP and EPSP
SUBROUTINE Bilinear1 (ID, TMAX, TMIN, TMAXPREV, TMINNEXT, TBD, TOD, TMD, SPSP, EPSP, DAYL, DAYLP,  MOPP, PPSE, PTU,TM)
!-------Formal parameters
    REAL TMAX, TMIN,TBD, TOD, TMD 
	REAL SPSP, EPSP, DAYL, DAYLP,  MOPP, PPSE
	REAL TMAXPREV,TMINNEXT
	REAL PTU
	REAL :: PI=3.1415927
! Parameters for SinExp function, based on:
!# Ephrath, J.E., Goudriaan, J., Marani, A.,1996. Modelling diurnal patterns of air temperature, radiation wind speed and relative humidity by equations from daily characteristics. Agricultural Systems, 51 (4): 377-393.
!# Goudriaan, J. and van Laar, H.H., 1994. Modelling potential crop growth processes. Current Issues in Production Ecology, Kluwer Academic Publishers, Dordrecht, The Netherlands, 239 p.
	REAL :: P = 1.5 ! (h) time at which maximum temperature is reached in hours after time at which sun is at its highest point  (FunTSinExp function)
	REAL :: T = 4.0 ! (-) nocturnal time coefficient, determines shape of exponential temperature decline during night (FunTSinExp function)

!-------Local parameters
    REAL TH, TM, TT, DL, EFP
	REAL DAYLEN, NGHTLEN, SUNRISETIME, SUNSETTIME, TSSETPREV, TSSET, PEAKTEMPTIME, TIME,VTIME,TH2SINEXP
	INTEGER ID

    DAYLEN = DAYL
    NGHTLEN   = 24.0 - DAYLEN
    SUNRISETIME = 12. - 0.5*DAYLEN
    SUNSETTIME  = 12. + 0.5*DAYLEN
!	TMAXPREV = TMAX	! Pepijn: for testing purpose. Better would be to read value previous day
!	TMINNEXT = TMIN	! Pepijn: for testing purpose. Better would be to read value next day
    TSSETPREV = TMIN+(TMAXPREV-TMIN)*SIN(PI*(SUNSETTIME-12.0+0.5*DAYLEN)/(DAYLEN+2.0*P))
    TSSET     = TMINNEXT+(TMAX-TMINNEXT)*SIN(PI*(SUNSETTIME-12.0+0.5*DAYLEN)/(DAYLEN+2.0*P))
    PEAKTEMPTIME = 12.+P
	
	TM= 0.
	TT= 0.0
	EFP = 1.
!   1a EPSP marks end of PSP, but PSP can never take longer than up to flowering
!   1b PI marks end of PSP
!	IF ((CPTU.LT.SPSP).OR.(ID.GT.PI_array)) THEN
!   1c no PSP
!	IF (.TRUE.) THEN 
! Hourly temperatures
	DO I=1, 24
!		TH=TM+0.5*ABS(TMAX-TMIN)*COS(0.2618*FLOAT(I-14))
		TIME = REAL(I)
		!write (5,*) IDOY, I, TMIN, TMAX, TH
		IF (TIME.LE.SUNRISETIME) THEN
			VTIME = 24.0+TIME
        ELSE
            VTIME = TIME
		END IF
        IF (TIME.LT.SUNRISETIME) THEN
			A = EXP(-NGHTLEN/T)
            B = EXP(-(VTIME-SUNSETTIME)/T)
            TH2SINEXP = (TMIN-TSSETPREV*A + (TSSETPREV-TMIN)*B)/(1.0-A)
        ELSEIF (TIME.GE.SUNRISETIME.AND.TIME.LE.SUNSETTIME) THEN
			IF (TIME.LT.PEAKTEMPTIME) THEN
				TH2SINEXP = TMIN +(TMAX-TMIN)*SIN(PI*(VTIME-SUNRISETIME)/(DAYLEN+2.0*P))
            ELSE
				TH2SINEXP = TMINNEXT +(TMAX-TMINNEXT)*SIN(PI*(VTIME-SUNRISETIME)/(DAYLEN+2.0*P))
			END IF
		ELSE
            A = EXP(-NGHTLEN/T)
            B = EXP(-(VTIME-SUNSETTIME)/T)
            TH2SINEXP = (TMINNEXT-TSSET*A + (TSSET-TMINNEXT)*B)/(1.0-A)
        END IF
		TH = TH2SINEXP
		TM = TM + TH/24.
!phenological temperature response, hourly
		IF ((TH.GT.TBD).AND.(TH.LT.TMD)) THEN
			IF (TH.LT.TOD) THEN 
				TT=TT+(TH-TBD)/(TOD-TBD)
			ELSE
				TT=TT+(TMD-TH)/(TMD-TOD)
			END IF
		END IF
	END DO
!phenological photoperiod response, only during PSP
	IF (CPTU.GE.SPSP.AND.CPTU.LE.EPSP) THEN 
		IF (DAYLP.GT. MOPP) THEN
			EFP = 1.0-(DAYLP-MOPP)*PPSE
		END IF
		EFP = MIN (1.0, MAX(0.0,EFP))
	END IF
	PTU = TT/24.*EFP

	RETURN
END SUBROUTINE

!-------Bilinear 2: Bilinear model but with TOD and TODNGHT
SUBROUTINE Bilinear2 (ID, TMAX, TMIN, TMAXPREV, TMINNEXT, TBD, TOD, TMD, TODNGHT, SPSP, EPSP, DAYL, DAYLP,  MOPP, PPSE, PTU,TM)
!-------Formal parameters
    REAL TMAX, TMIN,TBD, TMD, TOD, TODNGHT 
	REAL SPSP, EPSP, DAYL, DAYLP,  MOPP, PPSE
	REAL TMAXPREV,TMINNEXT
	REAL PTU
	REAL :: PI=3.1415927
! Parameters for SinExp function, based on:
!# Ephrath, J.E., Goudriaan, J., Marani, A.,1996. Modelling diurnal patterns of air temperature, radiation wind speed and relative humidity by equations from daily characteristics. Agricultural Systems, 51 (4): 377-393.
!# Goudriaan, J. and van Laar, H.H., 1994. Modelling potential crop growth processes. Current Issues in Production Ecology, Kluwer Academic Publishers, Dordrecht, The Netherlands, 239 p.
	REAL :: P = 1.5 ! (h) time at which maximum temperature is reached in hours after time at which sun is at its highest point  (FunTSinExp function)
	REAL :: T = 4.0 ! (-) nocturnal time coefficient, determines shape of exponential temperature decline during night (FunTSinExp function)

!-------Local parameters
    REAL TH, TM, TT, DL, EFP
	REAL DAYLEN, NGHTLEN, SUNRISETIME, SUNSETTIME, TSSETPREV, TSSET, PEAKTEMPTIME, TIME,VTIME,TH2SINEXP
	INTEGER ID

    DAYLEN = DAYL
    NGHTLEN   = 24.0 - DAYLEN
    SUNRISETIME = 12. - 0.5*DAYLEN
    SUNSETTIME  = 12. + 0.5*DAYLEN
!	TMAXPREV = TMAX	! Pepijn: for testing purpose. Better would be to read value previous day
!	TMINNEXT = TMIN	! Pepijn: for testing purpose. Better would be to read value next day
    TSSETPREV = TMIN+(TMAXPREV-TMIN)*SIN(PI*(SUNSETTIME-12.0+0.5*DAYLEN)/(DAYLEN+2.0*P))
    TSSET     = TMINNEXT+(TMAX-TMINNEXT)*SIN(PI*(SUNSETTIME-12.0+0.5*DAYLEN)/(DAYLEN+2.0*P))
    PEAKTEMPTIME = 12.+P
	
	TM= 0.
	TT= 0.0
	EFP = 1.
!   1a EPSP marks end of PSP, but PSP can never take longer than up to flowering
!   1b PI marks end of PSP
!	IF ((CPTU.LT.SPSP).OR.(ID.GT.PI_array)) THEN
!   1c no PSP
!	IF (.TRUE.) THEN 
! Hourly temperatures
	DO I=1, 24
!		TH=TM+0.5*ABS(TMAX-TMIN)*COS(0.2618*FLOAT(I-14))
		TIME = REAL(I)
		!write (5,*) IDOY, I, TMIN, TMAX, TH
		IF (TIME.LE.SUNRISETIME) THEN
			VTIME = 24.0+TIME
        ELSE
            VTIME = TIME
		END IF
        IF (TIME.LT.SUNRISETIME) THEN
			A = EXP(-NGHTLEN/T)
            B = EXP(-(VTIME-SUNSETTIME)/T)
            TH2SINEXP = (TMIN-TSSETPREV*A + (TSSETPREV-TMIN)*B)/(1.0-A)
        ELSEIF (TIME.GE.SUNRISETIME.AND.TIME.LE.SUNSETTIME) THEN
			IF (TIME.LT.PEAKTEMPTIME) THEN
				TH2SINEXP = TMIN +(TMAX-TMIN)*SIN(PI*(VTIME-SUNRISETIME)/(DAYLEN+2.0*P))
            ELSE
				TH2SINEXP = TMINNEXT +(TMAX-TMINNEXT)*SIN(PI*(VTIME-SUNRISETIME)/(DAYLEN+2.0*P))
			END IF
		ELSE
            A = EXP(-NGHTLEN/T)
            B = EXP(-(VTIME-SUNSETTIME)/T)
            TH2SINEXP = (TMINNEXT-TSSET*A + (TSSET-TMINNEXT)*B)/(1.0-A)
        END IF
		TH = TH2SINEXP
		TM = TM + TH/24.
!phenological temperature response, hourly
		IF ((TH.GT.TBD).AND.(TH.LT.TMD)) THEN
			IF(TIME.GE.SUNRISETIME.AND.TIME.LE.SUNSETTIME) THEN
				IF (TH.LT.TOD) THEN 
					TT=TT+(TH-TBD)/(TOD-TBD)
				ELSE
					TT=TT+(TMD-TH)/(TMD-TOD)
				END IF
			ELSE
				IF (TH.LT.TODNGHT) THEN 
					TT=TT+(TH-TBD)/(TODNGHT-TBD)
				ELSE
					TT=TT+(TMD-TH)/(TMD-TODNGHT)
				END IF
			END IF
		END IF
	END DO
!phenological photoperiod response, only during PSP
	IF (CPTU.GE.SPSP.AND.CPTU.LE.EPSP) THEN 
		IF (DAYLP.GT. MOPP) THEN
			EFP = 1.0-(DAYLP-MOPP)*PPSE
		END IF
		EFP = MIN (1.0, MAX(0.0,EFP))
	END IF
	PTU = TT/24.*EFP

	RETURN
END SUBROUTINE

!-------Bilinear3: Bilinear 1 + TM_CORR 
!Pepijn: also send TM to sum_bilinear3
SUBROUTINE Bilinear3 (ID, TMAX, TMIN, TMAXPREV, TMINNEXT, TBD, TOD, TMD, TM_CORR, SPSP, EPSP, DAYL, DAYLP,  MOPP, PPSE, PTU,TM)
!-------Formal parameters
    REAL TMAX, TMIN,TBD, TOD, TMD 
	REAL SPSP, EPSP, DAYL, DAYLP,  MOPP, PPSE, TM_CORR
	REAL TMAXPREV,TMINNEXT
	REAL PTU
	REAL :: PI=3.1415927
! Parameters for SinExp function, based on:
!# Ephrath, J.E., Goudriaan, J., Marani, A.,1996. Modelling diurnal patterns of air temperature, radiation wind speed and relative humidity by equations from daily characteristics. Agricultural Systems, 51 (4): 377-393.
!# Goudriaan, J. and van Laar, H.H., 1994. Modelling potential crop growth processes. Current Issues in Production Ecology, Kluwer Academic Publishers, Dordrecht, The Netherlands, 239 p.
	REAL :: P = 1.5 ! (h) time at which maximum temperature is reached in hours after time at which sun is at its highest point  (FunTSinExp function)
	REAL :: T = 4.0 ! (-) nocturnal time coefficient, determines shape of exponential temperature decline during night (FunTSinExp function)

!-------Local parameters
    REAL TH, TM, TT, DL, EFP
	REAL DAYLEN, NGHTLEN, SUNRISETIME, SUNSETTIME, TSSETPREV, TSSET, PEAKTEMPTIME, TIME,VTIME,TH2SINEXP
	INTEGER ID

    DAYLEN = DAYL
    NGHTLEN   = 24.0 - DAYLEN
    SUNRISETIME = 12. - 0.5*DAYLEN
    SUNSETTIME  = 12. + 0.5*DAYLEN
!	TMAXPREV = TMAX	! Pepijn: for testing purpose. Better would be to read value previous day
!	TMINNEXT = TMIN	! Pepijn: for testing purpose. Better would be to read value next day
    TSSETPREV = TMIN+(TMAXPREV-TMIN)*SIN(PI*(SUNSETTIME-12.0+0.5*DAYLEN)/(DAYLEN+2.0*P))
    TSSET     = TMINNEXT+(TMAX-TMINNEXT)*SIN(PI*(SUNSETTIME-12.0+0.5*DAYLEN)/(DAYLEN+2.0*P))
    PEAKTEMPTIME = 12.+P
	
	TM= 0.
	TT= 0.0
	EFP = 1.
!   1a EPSP marks end of PSP, but PSP can never take longer than up to flowering
!   1b PI marks end of PSP
!	IF ((CPTU.LT.SPSP).OR.(ID.GT.PI_array)) THEN
!   1c no PSP
!	IF (.TRUE.) THEN 
! Hourly temperatures
	DO I=1, 24
!		TH=TM+0.5*ABS(TMAX-TMIN)*COS(0.2618*FLOAT(I-14))
		TIME = REAL(I)
		!write (5,*) IDOY, I, TMIN, TMAX, TH
		IF (TIME.LE.SUNRISETIME) THEN
			VTIME = 24.0+TIME
        ELSE
            VTIME = TIME
		END IF
        IF (TIME.LT.SUNRISETIME) THEN
			A = EXP(-NGHTLEN/T)
            B = EXP(-(VTIME-SUNSETTIME)/T)
            TH2SINEXP = (TMIN-TSSETPREV*A + (TSSETPREV-TMIN)*B)/(1.0-A)
        ELSEIF (TIME.GE.SUNRISETIME.AND.TIME.LE.SUNSETTIME) THEN
			IF (TIME.LT.PEAKTEMPTIME) THEN
				TH2SINEXP = TMIN +(TMAX-TMIN)*SIN(PI*(VTIME-SUNRISETIME)/(DAYLEN+2.0*P))
            ELSE
				TH2SINEXP = TMINNEXT +(TMAX-TMINNEXT)*SIN(PI*(VTIME-SUNRISETIME)/(DAYLEN+2.0*P))
			END IF
		ELSE
            A = EXP(-NGHTLEN/T)
            B = EXP(-(VTIME-SUNSETTIME)/T)
            TH2SINEXP = (TMINNEXT-TSSET*A + (TSSET-TMINNEXT)*B)/(1.0-A)
        END IF
		TH = TH2SINEXP + TM_CORR	! if  TM_CORR = 1.5 then throughout the simulation air temperature is raised with 1.5oC
		TM = TM + TH/24.
!phenological temperature response, hourly
		IF ((TH.GT.TBD).AND.(TH.LT.TMD)) THEN
			IF (TH.LT.TOD) THEN 
				TT=TT+(TH-TBD)/(TOD-TBD)
			ELSE
				TT=TT+(TMD-TH)/(TMD-TOD)
			END IF
		END IF
	END DO
!phenological photoperiod response, only during PSP
	IF (CPTU.GE.SPSP.AND.CPTU.LE.EPSP) THEN 
		IF (DAYLP.GT. MOPP) THEN
			EFP = 1.0-(DAYLP-MOPP)*PPSE
		END IF
		EFP = MIN (1.0, MAX(0.0,EFP))
	END IF
	PTU = TT/24.*EFP

	RETURN
END SUBROUTINE


!------Beta: Beta function in Xinyou's PhD thesis
SUBROUTINE Beta(ID,TMAX,TMIN,TMAXPREV,TMINNEXT,TBD,TOD,TMD,TODNGHT,TSEN,TSENNGHT, &
	SPSP,EPSP,DAYL,DAYLP,MOPP,PPSE,TSENPSP,TSENPSPNGHT,PTU,TM)
!------Formal parameters
    REAL TMAX, TMIN,TBD, TMD, TOD, TODNGHT, TSEN, TSENNGHT
	REAL SPSP, EPSP, DAYL, DAYLP,  MOPP, PPSE, TSENPSP, TSENPSPNGHT
	REAL TMAXPREV,TMINNEXT
	REAL PTU
	REAL :: PI=3.1415927
! Parameters for SinExp function, based on:
!# Ephrath, J.E., Goudriaan, J., Marani, A.,1996. Modelling diurnal patterns of air temperature, radiation wind speed and relative humidity by equations from daily characteristics. Agricultural Systems, 51 (4): 377-393.
!# Goudriaan, J. and van Laar, H.H., 1994. Modelling potential crop growth processes. Current Issues in Production Ecology, Kluwer Academic Publishers, Dordrecht, The Netherlands, 239 p.
	REAL :: P = 1.5 ! (h) time at which maximum temperature is reached in hours after time at which sun is at its highest point  (FunTSinExp function)
	REAL :: T = 4.0 ! (-) nocturnal time coefficient, determines shape of exponential temperature decline during night (FunTSinExp function)

!-------Local parameters
    REAL TH, TM, TT, DL, EFP
	REAL DAYLEN, NGHTLEN, SUNRISETIME, SUNSETTIME, TSSETPREV, TSSET, PEAKTEMPTIME, TIME,VTIME,TH2SINEXP
	INTEGER ID

    DAYLEN = DAYL
    NGHTLEN   = 24.0 - DAYLEN
    SUNRISETIME = 12. - 0.5*DAYLEN
    SUNSETTIME  = 12. + 0.5*DAYLEN
!	TMAXPREV = TMAX	! Pepijn: for testing purpose. Better would be to read value previous day
!	TMINNEXT = TMIN	! Pepijn: for testing purpose. Better would be to read value next day
    TSSETPREV = TMIN+(TMAXPREV-TMIN)*SIN(PI*(SUNSETTIME-12.0+0.5*DAYLEN)/(DAYLEN+2.0*P))
    TSSET     = TMINNEXT+(TMAX-TMINNEXT)*SIN(PI*(SUNSETTIME-12.0+0.5*DAYLEN)/(DAYLEN+2.0*P))
    PEAKTEMPTIME = 12.+P
	
	TM= 0.
	TT= 0.0
	EFP = 1.
!   1a EPSP marks end of PSP, but PSP can never take longer than up to flowering
!   1b PI marks end of PSP
!	IF ((CPTU.LT.SPSP).OR.(ID.GT.PI_array)) THEN
!   1c no PSP
!	IF (.TRUE.) THEN 
! Hourly temperatures
	DO I=1, 24
!		TH=TM+0.5*ABS(TMAX-TMIN)*COS(0.2618*FLOAT(I-14))
		TIME = REAL(I)
		!write (5,*) IDOY, I, TMIN, TMAX, TH
		IF (TIME.LE.SUNRISETIME) THEN
			VTIME = 24.0+TIME
        ELSE
            VTIME = TIME
		END IF
        IF (TIME.LT.SUNRISETIME) THEN
			A = EXP(-NGHTLEN/T)
            B = EXP(-(VTIME-SUNSETTIME)/T)
            TH2SINEXP = (TMIN-TSSETPREV*A + (TSSETPREV-TMIN)*B)/(1.0-A)
        ELSEIF (TIME.GE.SUNRISETIME.AND.TIME.LE.SUNSETTIME) THEN
			IF (TIME.LT.PEAKTEMPTIME) THEN
				TH2SINEXP = TMIN +(TMAX-TMIN)*SIN(PI*(VTIME-SUNRISETIME)/(DAYLEN+2.0*P))
            ELSE
				TH2SINEXP = TMINNEXT +(TMAX-TMINNEXT)*SIN(PI*(VTIME-SUNRISETIME)/(DAYLEN+2.0*P))
			END IF
		ELSE
            A = EXP(-NGHTLEN/T)
            B = EXP(-(VTIME-SUNSETTIME)/T)
            TH2SINEXP = (TMINNEXT-TSSET*A + (TSSET-TMINNEXT)*B)/(1.0-A)
        END IF
		TH = TH2SINEXP + TM_CORR	! if  TM_CORR = 1.5 then throughout the simulation air temperature is raised with 1.5oC
		TM = TM + TH/24.
!phenological temperature response, hourly
		IF (TH.LT.TBD) TH = TBD
		IF (TH.GT.TMD) TH = TMD
		IF (TIME.GE.SUNRISETIME.AND.TIME.LE.SUNSETTIME) THEN
			TT=TT+(((TH-TBD)/(TOD-TBD))*((TMD-TH)/(TMD-TOD))**((TMD-TOD)/(TOD-TBD)))**TSEN
		ELSE
			TT=TT+(((TH-TBD)/(TODNGHT-TBD))*((TMD-TH)/(TMD-TODNGHT))**((TMD-TODNGHT)/(TODNGHT-TBD)))**TSENNGHT
		END IF
	END DO
!phenological photoperiod response, only during PSP
	IF (CPTU.GE.SPSP.AND.CPTU.LE.EPSP) THEN 
		EFP = (((DAYLP-0.0)/(MOPP-0.0))*((24.0-DAYLP)/(24.0-MOPP))**((24.0-MOPP)/(MOPP-0.0)))**PPSE
	END IF
	PTU = TT/24.*EFP

	RETURN
END SUBROUTINE

END MODULE