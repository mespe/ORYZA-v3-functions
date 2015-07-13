MODULE PTU_SUM
USE COM
USE SUBPHENO

CONTAINS
!--------Sum based on bilinear1 function
SUBROUTINE SUM_bilinear1 (TBD, TOD, TMD, SPSP, EPSP, MOPP, PPSE,SHCKD,CPTUFL,CPTUFLM)
  REAL TBD, TOD, TMD, SPSP, EPSP, MOPP, PPSE, SHCKD, TM

  DIMENSION YR_daily (10,731)

  REAL PTU,CTM,CTMFL
  INTEGER I,DAE,DAFL
 
 DO M=1,10,1
	DO N=1,731
		YR_daily (M,N)=0.0
	END DO
 END DO

!Pepijn: CTM is cumulative mean temperature, DAS is days after emergence
  CTM = 0.0
  DAE = 0
  CPTU = 0.0
!set simulated day and year to zero at start of simulation
  IDOYPISIM = 0
  IYRPISIM = 0
  IDOYFLSIM = 0
  IYRFLSIM = 0 
  IDOYMSIM = 0
  IYRMSIM = 0

	CALL READ_WEA (YR_daily)
	CALL Day_array (YR_daily) 
!	WRITE (*,*) EMD_array, TRA_array,PI_array,FL_array,M_array
	
!note: in case of no transplanting set transplanting date equal to emergence date
	DO I = EMD_array, TRA_array-1, 1
		IDOY=YR_daily(2,I)
		IYR=YR_daily (1,I)       
!		CALL Bilinear1 (I,YR_daily(5,I),YR_daily(4,I),TBD,TOD,TMD,SPSP,EPSP,YR_daily(9,I),YR_daily(10,I),MOPP,PPSE,PTU,TM)
		CALL Bilinear1 (I,YR_daily(5,I),YR_daily(4,I),YR_daily(5,I-1),YR_daily(4,I+1),TBD,TOD,TMD,SPSP,EPSP, &
			YR_daily(9,I),YR_daily(10,I),MOPP,PPSE,PTU,TM)
		CPTU = CPTU + PTU
		CTM = CTM + TM
		DAE = DAE+1
!		WRITE (*,*) IDOY,IDOY-EMD+1,DAS, CTM,CTM/(IDOY-EMD+1),CTM/DAS
	END DO
!transplanting shock
	DO I = TRA_array-1,TRA_array-1, 1
		CPTUTRA=CPTU
		CPTU=CPTUTRA-MIN(CPTU,CPTU*SHCKD)
	END DO
		
! stop at last record of YR_daily
	DO I = TRA_array, 730, 1
		IDOY=YR_daily(2,I)
		IYR=YR_daily (1,I)       
! SUBROUTINE Bilinear1 (ID, TMAX, TMIN, TMAXPREV, TMINNEXT, TBD, TOD, TMD, SPSP, EPSP, DAYL, DAYLP,  MOPP, PPSE, PTU,TM)
		CALL Bilinear1 (I,YR_daily(5,I),YR_daily(4,I),YR_daily(5,I-1),YR_daily(4,I+1),TBD,TOD,TMD,SPSP,EPSP, &
			YR_daily(9,I),YR_daily(10,I),MOPP,PPSE,PTU,TM)
		CPTU = CPTU + PTU
		CTM = CTM + TM
		CTMFL = CTMFL + TM
		DAE = DAE+1
		DAFL = DAFL+1
!mean temperature from emergence to flowering
		IF (IDOY.EQ.IDOYFL.AND.IYR.EQ.IYRFL) THEN
			TMEFL = CTM/DAE
			CTMFL = 0.
			DAFL = 0
		ENDIF
!mean temperature from emergence/flowering to mature
		IF (IDOY.EQ.IDOYM.AND.IYR.EQ.IYRM) THEN
			TMEM = CTM/DAE
			TMFLM = CTMFL/DAFL
		ENDIF
!Panicle initiation
		IF (IDOYPI.EQ.IDOY.AND.IYRPI.EQ.IYR) THEN
			CPTUPI = CPTU
		ENDIF
		IF (CPTU.GT.EPSP.AND.IDOYPISIM.EQ.0) THEN
			IDOYPISIM = IDOY
			IYRPISIM = IYR
		ENDIF
!flowering
		IF (CPTU.GT.CPTUFL.AND.IDOYFLSIM.EQ.0) THEN
			IDOYFLSIM = IDOY
			IYRFLSIM = IYR
		ENDIF
!maturity
		IF (CPTU.GT.CPTUFL+CPTUFLM.AND.IDOYMSIM.EQ.0) THEN
			IDOYMSIM = IDOY
			IYRMSIM = IYR
		ENDIF
!		WRITE (*,*) IDOY,IDOY-EMD+1,DAS, CTM,CTM/(IDOY-EMD+1),CTM/DAS
!		WRITE (*,*) IDOY,CPTU,CPTUFL,IDOYFLSIM,IYRFLSIM
	END DO


RETURN
END SUBROUTINE



!--------Sum based on bilinear2 function
SUBROUTINE SUM_bilinear2 (TBD, TOD, TODNGHT, TMD, SPSP, EPSP, MOPP, PPSE,SHCKD,CPTUFL,CPTUFLM)
  REAL TBD, TOD, TODNGHT, TMD, SPSP, EPSP, MOPP, PPSE, SHCKD
  character (len=80) output_tem
  DIMENSION YR_daily (10,731)

  REAL PTU,CTM,CTMFL
  INTEGER I,DAE,DAFL
 
 DO M=1,10,1
	DO N=1,731
		YR_daily (M,N)=0.0
	END DO
 END DO

!Pepijn: CTM is cumulative mean temperature, DAS is days after emergence
  CTM = 0.0
  DAE = 0
  CPTU = 0.0
!set simulated day and year to zero at start of simulation
  IDOYPISIM = 0
  IYRPISIM = 0
  IDOYFLSIM = 0
  IYRFLSIM = 0 
  IDOYMSIM = 0
  IYRMSIM = 0

	CALL READ_WEA (YR_daily)
	CALL Day_array (YR_daily) 
!	WRITE (*,*) EMD_array, TRA_array,PI_array,FL_array,M_array
	
!note: in case of no transplanting set transplanting date equal to emergence date
	DO I = EMD_array, TRA_array-1, 1
		IDOY=YR_daily(2,I)
		IYR=YR_daily (1,I)       
! SUBROUTINE Bilinear2 (ID, TMAX, TMIN, TMAXPREV, TMINNEXT, TBD, TOD, TMD, TODNGHT, SPSP, EPSP, DAYL, DAYLP,  MOPP, PPSE, PTU,TM)
		CALL Bilinear2 (I,YR_daily(5,I),YR_daily(4,I),YR_daily(5,I-1),YR_daily(4,I+1),TBD,TOD,TMD,TODNGHT, &
			SPSP,EPSP,YR_daily(9,I),YR_daily(10,I),MOPP,PPSE,PTU,TM)
		CPTU = CPTU + PTU
		CTM = CTM + TM
		DAE = DAE+1
!		WRITE (*,*) IDOY,IDOY-EMD+1,DAS, CTM,CTM/(IDOY-EMD+1),CTM/DAS
	END DO
!transplanting shock
	DO I = TRA_array-1,TRA_array-1, 1
		CPTUTRA=CPTU
		CPTU=CPTUTRA-MIN(CPTU,CPTU*SHCKD)
	END DO
		
! stop at last record of YR_daily
	DO I = TRA_array, 730, 1
		IDOY=YR_daily(2,I)
		IYR=YR_daily (1,I)       
		CALL Bilinear2 (I,YR_daily(5,I),YR_daily(4,I),YR_daily(5,I-1),YR_daily(4,I+1),TBD,TOD,TMD,TODNGHT, &
			SPSP,EPSP,YR_daily(9,I),YR_daily(10,I),MOPP,PPSE,PTU,TM)
		CPTU = CPTU + PTU
		CTM = CTM + TM
		CTMFL = CTMFL + TM
		DAE = DAE+1
		DAFL = DAFL+1
!mean temperature from emergence to flowering
		IF (IDOY.EQ.IDOYFL.AND.IYR.EQ.IYRFL) THEN
			TMEFL = CTM/DAE
			CTMFL = 0.
			DAFL = 0
		ENDIF
!mean temperature from emergence/flowering to mature
		IF (IDOY.EQ.IDOYM.AND.IYR.EQ.IYRM) THEN
			TMEM = CTM/DAE
			TMFLM = CTMFL/DAFL
		ENDIF
!Panicle initiation
		IF (IDOYPI.EQ.IDOY.AND.IYRPI.EQ.IYR) THEN
			CPTUPI = CPTU
		ENDIF
		IF (CPTU.GT.EPSP.AND.IDOYPISIM.EQ.0) THEN
			IDOYPISIM = IDOY
			IYRPISIM = IYR
		ENDIF
!flowering
		IF (CPTU.GT.CPTUFL.AND.IDOYFLSIM.EQ.0) THEN
			IDOYFLSIM = IDOY
			IYRFLSIM = IYR
		ENDIF
!maturity
		IF (CPTU.GT.CPTUFL+CPTUFLM.AND.IDOYMSIM.EQ.0) THEN
			IDOYMSIM = IDOY
			IYRMSIM = IYR
		ENDIF
!		WRITE (*,*) IDOY,IDOY-EMD+1,DAS, CTM,CTM/(IDOY-EMD+1),CTM/DAS
!		WRITE (*,*) IDOY,CPTU,CPTUFL,IDOYFLSIM,IYRFLSIM
	END DO

RETURN
END SUBROUTINE

!--------Sum based on bilinear3 function
SUBROUTINE SUM_bilinear3 (TBD, TOD, TMD, SPSP, EPSP, MOPP, PPSE,SHCKD,TM_CORR,CPTUFL,CPTUFLM)
  REAL TBD, TOD, TMD, SPSP, EPSP, MOPP, PPSE, SHCKD, TM_CORR

  DIMENSION YR_daily (10,731)

  REAL PTU,CTM,CTMFL
  INTEGER I,DAE,DAFL
 
 DO M=1,10,1
	DO N=1,731
		YR_daily (M,N)=0.0
	END DO
 END DO

!Pepijn: CTM is cumulative mean temperature, DAS is days after emergence
  CTM = 0.0
  DAE = 0
  CPTU = 0.0
!set simulated day and year to zero at start of simulation
  IDOYPISIM = 0
  IYRPISIM = 0
  IDOYFLSIM = 0
  IYRFLSIM = 0 
  IDOYMSIM = 0
  IYRMSIM = 0

	CALL READ_WEA (YR_daily)
	CALL Day_array (YR_daily) 
!	WRITE (*,*) EMD_array, TRA_array,PI_array,FL_array,M_array
	
!note: in case of no transplanting set transplanting date equal to emergence date
	DO I = EMD_array, TRA_array-1, 1
		IDOY=YR_daily(2,I)
		IYR=YR_daily (1,I)
! SUBROUTINE Bilinear3 (ID, TMAX, TMIN, TMAXPREV, TMINNEXT, TBD, TOD, TMD, TM_CORR, SPSP, EPSP, DAYL, DAYLP,  MOPP, PPSE, PTU,TM)		
		CALL Bilinear3 (I,YR_daily(5,I),YR_daily(4,I),YR_daily(5,I-1),YR_daily(4,I+1),TBD,TOD,TMD,TM_CORR, &
			SPSP,EPSP,YR_daily(9,I),YR_daily(10,I),MOPP,PPSE,PTU,TM)
		CPTU = CPTU + PTU
		CTM = CTM + TM
		DAE = DAE+1
!		WRITE (*,*) IDOY,IDOY-EMD+1,DAS, CTM,CTM/(IDOY-EMD+1),CTM/DAS
	END DO
!transplanting shock
	DO I = TRA_array-1,TRA_array-1, 1
		CPTUTRA=CPTU
		CPTU=CPTUTRA-MIN(CPTU,CPTU*SHCKD)
	END DO
		
! stop at last record of YR_daily
	DO I = TRA_array, 730, 1
		IDOY=YR_daily(2,I)
		IYR=YR_daily (1,I)       
		CALL Bilinear3 (I,YR_daily(5,I),YR_daily(4,I),YR_daily(5,I-1),YR_daily(4,I+1),TBD,TOD,TMD,TM_CORR, &
			SPSP,EPSP,YR_daily(9,I),YR_daily(10,I),MOPP,PPSE,PTU,TM)
		CPTU = CPTU + PTU
		CTM = CTM + TM
		CTMFL = CTMFL + TM
		DAE = DAE+1
		DAFL = DAFL+1
!!! NOTICE: AVERAGE TEMPERATURES REPORTED HERE ARE WITH TM_CORR CORRECTION
! i.e. if TMEFL = 30.4 oC (average air temperature from emergence to flowering) 
! and if we assume canopy/water temperature is TM_CORR = -10.0 oC higher then
! here the output will report TMEFL = TMEFL + TM_CORR = 30.4 - 10 = 20.4 oC
!mean temperature from emergence to flowering
		IF (IDOY.EQ.IDOYFL.AND.IYR.EQ.IYRFL) THEN
			TMEFL = CTM/DAE
			CTMFL = 0.
			DAFL = 0
		ENDIF
!mean temperature from emergence/flowering to mature
		IF (IDOY.EQ.IDOYM.AND.IYR.EQ.IYRM) THEN
			TMEM = CTM/DAE
			TMFLM = CTMFL/DAFL
		ENDIF
!Panicle initiation
		IF (IDOYPI.EQ.IDOY.AND.IYRPI.EQ.IYR) THEN
			CPTUPI = CPTU
		ENDIF
		IF (CPTU.GT.EPSP.AND.IDOYPISIM.EQ.0) THEN
			IDOYPISIM = IDOY
			IYRPISIM = IYR
		ENDIF
!flowering
		IF (CPTU.GT.CPTUFL.AND.IDOYFLSIM.EQ.0) THEN
			IDOYFLSIM = IDOY
			IYRFLSIM = IYR
		ENDIF
!maturity
		IF (CPTU.GT.CPTUFL+CPTUFLM.AND.IDOYMSIM.EQ.0) THEN
			IDOYMSIM = IDOY
			IYRMSIM = IYR
		ENDIF
!		WRITE (*,*) IDOY,IDOY-EMD+1,DAS, CTM,CTM/(IDOY-EMD+1),CTM/DAS
!		WRITE (*,*) IDOY,CPTU,CPTUFL,IDOYFLSIM,IYRFLSIM
	END DO



RETURN
END SUBROUTINE



!--------Sum based on Beta function
SUBROUTINE SUM_BETA (TBD, TOD, TMD, TODNGHT, TSEN,TSENNGHT, SPSP, EPSP, MOPP, PPSE, TSENPSP, TSENPSPNGHT, SHCKD,CPTUFL,CPTUFLM)

  REAL TBD, TOD, TMD, TODNGHT, TSEN,TSENNGHT, SPSP, EPSP, MOPP, PPSE, TSENPSP, TSENPSPNGHT, SHCKD

  DIMENSION YR_daily (10,731)

  REAL PTU,CTM,CTMFL
  INTEGER I,DAE,DAFL
 
 DO M=1,10,1
	DO N=1,731
		YR_daily (M,N)=0.0
	END DO
 END DO

!Pepijn: CTM is cumulative mean temperature, DAS is days after emergence
  CTM = 0.0
  DAE = 0
  CPTU = 0.0
!set simulated day and year to zero at start of simulation
  IDOYPISIM = 0
  IYRPISIM = 0
  IDOYFLSIM = 0
  IYRFLSIM = 0 
  IDOYMSIM = 0
  IYRMSIM = 0

	CALL READ_WEA (YR_daily)
	CALL Day_array (YR_daily) 
!	WRITE (*,*) EMD_array, TRA_array,PI_array,FL_array,M_array
	
!note: in case of no transplanting set transplanting date equal to emergence date
	DO I = EMD_array, TRA_array-1, 1
		IDOY=YR_daily(2,I)
		IYR=YR_daily (1,I)       
! SUBROUTINE Beta(ID,TMAX,TMIN,TMAXPREV,TMINNEXT,TBD,TOD,TMD,TODNGHT,TSEN,TSENNGHT, &
! 	SPSP,EPSP,DAYL,DAYLP,MOPP,PPSE,TSENPSP,TSENPSPNGHT,PTU,TM)
		CALL Beta(I,YR_daily(5,I),YR_daily(4,I),YR_daily(5,I-1),YR_daily(4,I+1),TBD,TOD,TMD,TODNGHT,&
			TSEN,TSENNGHT,SPSP,EPSP,YR_daily(9,I),YR_daily(10,I),MOPP,PPSE,TSENPSP,TSENPSPNGHT,PTU,TM)
		CPTU = CPTU + PTU
		CTM = CTM + TM
		DAE = DAE+1
!		WRITE (*,*) IDOY,IDOY-EMD+1,DAS, CTM,CTM/(IDOY-EMD+1),CTM/DAS
	END DO
!transplanting shock
	DO I = TRA_array-1,TRA_array-1, 1
		CPTUTRA=CPTU
		CPTU=CPTUTRA-MIN(CPTU,CPTU*SHCKD)
	END DO
		
! stop at last record of YR_daily
	DO I = TRA_array, 730, 1
		IDOY=YR_daily(2,I)
		IYR=YR_daily (1,I)       
        CALL Beta(I,YR_daily(5,I),YR_daily(4,I),YR_daily(5,I-1),YR_daily(4,I+1),TBD,TOD,TMD,TODNGHT,&
			TSEN,TSENNGHT,SPSP,EPSP,YR_daily(9,I),YR_daily(10,I),MOPP,PPSE,TSENPSP,TSENPSPNGHT,PTU,TM)
		CPTU = CPTU + PTU
		CTM = CTM + TM
		CTMFL = CTMFL + TM
		DAE = DAE+1
		DAFL = DAFL+1
!mean temperature from emergence to flowering
		IF (IDOY.EQ.IDOYFL.AND.IYR.EQ.IYRFL) THEN
			TMEFL = CTM/DAE
			CTMFL = 0.
			DAFL = 0
		ENDIF
!mean temperature from emergence/flowering to mature
		IF (IDOY.EQ.IDOYM.AND.IYR.EQ.IYRM) THEN
			TMEM = CTM/DAE
			TMFLM = CTMFL/DAFL
		ENDIF
!Panicle initiation
		IF (IDOYPI.EQ.IDOY.AND.IYRPI.EQ.IYR) THEN
			CPTUPI = CPTU
		ENDIF
		IF (CPTU.GT.EPSP.AND.IDOYPISIM.EQ.0) THEN
			IDOYPISIM = IDOY
			IYRPISIM = IYR
		ENDIF
!flowering
		IF (CPTU.GT.CPTUFL.AND.IDOYFLSIM.EQ.0) THEN
			IDOYFLSIM = IDOY
			IYRFLSIM = IYR
		ENDIF
!maturity
		IF (CPTU.GT.CPTUFL+CPTUFLM.AND.IDOYMSIM.EQ.0) THEN
			IDOYMSIM = IDOY
			IYRMSIM = IYR
		ENDIF
!		WRITE (*,*) IDOY,IDOY-EMD+1,DAS, CTM,CTM/(IDOY-EMD+1),CTM/DAS
!		WRITE (*,*) IDOY,CPTU,CPTUFL,IDOYFLSIM,IYRFLSIM
	END DO


RETURN
END SUBROUTINE

END MODULE

