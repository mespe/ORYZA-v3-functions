MODULE START
USE COM
USE SUBPHENO
USE PTU_SUM
USE Rerun
!CHARACTER (LEN=80) :: Pheno_in="Pheno_in.txt"
CHARACTER (LEN=80) :: outputname
CHARACTER (LEN=200) :: TheEnd
INTEGER leftrun

CONTAINS

SUBROUTINE START_bilinear1()

CALL Rerun_Bilinear1()

!headers of output file
outputname="output_Bilinear1.txt"
OPEN (12, FILE=outputname)
WRITE (12,"(A270)") "*SET,TBD,TOD,TMD,SPSP,EPSP,MOPP,PPSE,SHCKD,CPTUFL,CPTUFLM,LON,LAT,ALT,TMEFL,TMFLM,TMEM,EMD,EMDYR,IDOYTR, &
IYRTR,IDOYPI,IYRPI,IDOYPISIM,IYRPISIM,IDOYFL,IYRFL,IDOYFLSIM,IYRFLSIM,IDOYM,IYRM,IDOYMSIM,IYRMSIM,IEPI,IEPISIM,IPIFL,IPIFLSIM, &
IEFL,IEFLSIM,IFLM,IFLMSIM,CPTUTRA"

leftrun=OUTPUT_NUM
SET = 1
WRITE(*,*)"AVG. ERROR E TO FL AND FL TO M < 1 DAY (CAN BE WITH LARGE SE)"
!WRITE(*,*)OUTPUT_NUM,"runs to go"
DO I_TBD=1,SIZE(TBD_set),1
	DO I_TOD=1,SIZE(TOD_set),1
		DO I_TMD=1,SIZE(TMD_set),1
			DO I_SPSP=1,SIZE(SPSP_set),1
				DO I_MOPP=1,SIZE(MOPP_set),1
					DO I_PPSE=1,SIZE(PPSE_set),1
						DO I_SHCKD=1,SIZE(SHCKD_set),1
!assumed initial values (iteratively estimated)
							CPTUFL = 50.
							CPTUFLM = 20.
							CPTUFLPREV = 50.
							CPTUFLMPREV = 20.
							EPSP = 40.
							EPSPPREV = 40.
							K = 0
! Here we have a loop in which CPTUFL and CPTUFLM are iteratively determined until the
! average error between simulated and actual days (E to FL) and (FL to M) is zero (can be with large SE)
104 CONTINUE
							K = K+1
							IF (K.GT.10) then
								write(*,*) "no convergence within 10 tries"
								GOTO 105 ! if no convergence within 10 tries then next parameter set
							END IF
							DO J = 1,EXP_NUM
								!SITE = OBS_SET(J,1)
								SITE_STR = SITE_STR_SET(J,1)
								EMD = OBS_SET(J,2)
								EMDYR = OBS_SET(J,3)
								IDOYTR = OBS_SET(J,4)
								IYRTR = OBS_SET(J,5)
								IDOYPI = OBS_SET(J,6)
								IYRPI = OBS_SET(J,7)
								IDOYFL = OBS_SET(J,8)
								IYRFL = OBS_SET(J,9)
								IDOYM = OBS_SET(J,10)
								IYRM = OBS_SET(J,11)

								CALL SUM_bilinear1 (TBD_set (I_TBD), TOD_set(I_TOD), TMD_set(I_TMD), SPSP_set(I_SPSP), &
								EPSP, MOPP_set(I_MOPP), PPSE_set(I_PPSE), SHCKD_set(I_SHCKD), CPTUFL, CPTUFLM) 

								OUTPUT(1,J) = REAL(SET)
								OUTPUT(2,J) = TBD_set (I_TBD)
								OUTPUT(3,J) =  TOD_set(I_TOD)
								OUTPUT(4,J) =  TMD_set(I_TMD)
								OUTPUT(5,J) =  SPSP_set(I_SPSP)
								OUTPUT(6,J) =  EPSP
								OUTPUT(7,J) =  MOPP_set(I_MOPP)
								OUTPUT(8,J) =  PPSE_set(I_PPSE)
								OUTPUT(9,J) =  SHCKD_SET(I_SHCKD)
								OUTPUT(10,J) =  CPTUFL
								OUTPUT(11,J) =  CPTUFLM
								OUTPUT(12,J) =  LON
								OUTPUT(13,J) =  LAT
								OUTPUT(14,J) =  ALT
								OUTPUT(15,J) =  TMEFL
								OUTPUT(16,J) = TMFLM
								OUTPUT(17,J) = TMEM
								OUTPUT(18,J) = REAL(EMD)
								OUTPUT(19,J) = REAL(EMDYR)
								OUTPUT(20,J) = REAL(IDOYTR)
								OUTPUT(21,J) = REAL(IYRTR)
								OUTPUT(22,J) = REAL(IDOYPI)
								OUTPUT(23,J) = REAL(IYRPI)
								OUTPUT(24,J) = REAL(IDOYPISIM)
								OUTPUT(25,J) = REAL(IYRPISIM)
								OUTPUT(26,J) = REAL(IDOYFL)
								OUTPUT(27,J) = REAL(IYRFL)
								OUTPUT(28,J) = REAL(IDOYFLSIM)
								OUTPUT(29,J) = REAL(IYRFLSIM)
								OUTPUT(30,J) = REAL(IDOYM)
								OUTPUT(31,J) = REAL(IYRM)
								OUTPUT(32,J) = REAL(IDOYMSIM)
								OUTPUT(33,J) = REAL(IYRMSIM)

!days from emergence to PI
								IF (IYRPI.EQ.EMDYR) THEN
									IEPI = IDOYPI - EMD
								ELSE
									IEPI = YR_day(EMDYR) - EMD + IDOYPI
								ENDIF
								IF (IYRPISIM.EQ.EMDYR) THEN
									IEPISIM = IDOYPISIM - EMD
								ELSE
									IEPISIM = YR_day(EMDYR) - EMD + IDOYPISIM
								ENDIF
!days from emergence to flowering
								IF (IYRFL.EQ.EMDYR) THEN
									IEFL = IDOYFL - EMD
								ELSE
									IEFL = YR_day(EMDYR) - EMD + IDOYFL
								ENDIF
								IF (IYRFLSIM.EQ.EMDYR) THEN
									IEFLSIM = IDOYFLSIM - EMD
								ELSE
									IEFLSIM = YR_day(EMDYR) - EMD + IDOYFLSIM
								ENDIF
!days from PI to flowering
								IPIFL = IEFL - IEPI
								IPIFLSIM = IEFLSIM - IEPISIM
!days from flowering to mature
								IF (IYRM.EQ.IYRFL) THEN
									IFLM = IDOYM - IDOYFL
								ELSE
									IFLM = YR_day(IYRFL) - IDOYFL + IDOYM
								ENDIF
								IF (IYRMSIM.EQ.IYRFLSIM) THEN
									IFLMSIM = IDOYMSIM - IDOYFLSIM
								ELSE
									IFLMSIM = YR_day(IYRFLSIM) - IDOYFLSIM + IDOYMSIM
								ENDIF
								DATASET_PIFLERROR(J) = IPIFL - IPIFLSIM
								DATASET_EFLERROR(J) = IEFL - IEFLSIM
								DATASET_FLMERROR(J) = IFLM - IFLMSIM 

								OUTPUT(34,J) = REAL(IEPI)
								OUTPUT(35,J) = REAL(IEPISIM)
								OUTPUT(36,J) = REAL(IPIFL)
								OUTPUT(37,J) = REAL(IPIFLSIM)
								OUTPUT(38,J) = REAL(IEFL)
								OUTPUT(39,J) = REAL(IEFLSIM)
								OUTPUT(40,J) = REAL(IFLM)
								OUTPUT(41,J) = REAL(IFLMSIM)
								OUTPUT(42,J) = CPTUTRA
							END DO
							101 CONTINUE
! ONLY WRITE OUTPUT TO FILE IN CASE OF ...
							CALL AVE(DATASET_PIFLERROR, EXP_NUM, AVGPIFLERROR)
							CALL AVE(DATASET_EFLERROR, EXP_NUM, AVGEFLERROR)
							CALL AVE(DATASET_FLMERROR, EXP_NUM, AVGFLMERROR)
! AVERAGE ERROR IN DURATION FROM EMERGENCE TO FLOWERING < 1 DAY (CAN BE WITH LARGE SE)
!							IF (ABS(AVGEFLERROR).LT.1.) THEN
! AVERAGE ERROR IN DURATION FROM EMERGENCE TO FLOWERING < 1 DAY + DURATION FLOWERING TO MATURE < 1 DAY(CAN BE WITH LARGE SE)
!							IF (ABS(AVGEFLERROR).LT.1..AND.ABS(AVGFLMERROR).LT.1.) THEN
! AVERAGE ERROR IN DURATION FROM PI TO FL < 1 DAY, E TO FL < 1 DAY, FL TO M < 1 DAY(CAN BE WITH LARGE SE)
							IF (ABS(AVGPIFLERROR).LT.1..AND.ABS(AVGEFLERROR).LT.1..AND.ABS(AVGFLMERROR).LT.1.) THEN
! AVERAGE ERROR IN DURATION FROM PI TO FL < 1 DAY, E TO FL < 1 DAY(CAN BE WITH LARGE SE)
!							IF (ABS(AVGPIFLERROR).LT.1..AND.ABS(AVGEFLERROR).LT.1.) THEN
!write this run to output file
								DO J=1,EXP_NUM,1
									write (12, 103) OUTPUT(1,J), OUTPUT(2,J), OUTPUT(3,J), OUTPUT(4,J), OUTPUT(5,J), &
									OUTPUT(6,J), OUTPUT(7,J), OUTPUT(8,J), OUTPUT(9,J), OUTPUT(10,J), OUTPUT(11,J), &
									OUTPUT(12,J), OUTPUT(13,J), OUTPUT(14,J), OUTPUT(15,J), OUTPUT(16,J), OUTPUT(17,J), &
									OUTPUT(18,J), OUTPUT(19,J), OUTPUT(20,J), OUTPUT(21,J), OUTPUT(22,J), OUTPUT(23,J), &
									OUTPUT(24,J), OUTPUT(25,J), OUTPUT(26,J), OUTPUT(27,J), OUTPUT(28,J), OUTPUT(29,J), &
									OUTPUT(30,J), OUTPUT(31,J), OUTPUT(32,J), OUTPUT(33,J), OUTPUT(34,J), OUTPUT(35,J), &
									OUTPUT(36,J), OUTPUT(37,J), OUTPUT(38,J), OUTPUT(39,J), OUTPUT(40,J), OUTPUT(41,J), &
									OUTPUT(42,J)
									103 FORMAT (F10.0,",",16(F8.2,","),24(F5.0,","),F8.2)
								END DO
								SET = SET+1
								IF (SET.GT.60000) THEN
									WRITE(*,*)"Terminated at 60000. runs written to output file "
									GOTO 102
								ENDIF
							ELSE
!								CPTUFL = CPTUFLPREV + 0.5*AVGEFLERROR
!								CPTUFLPREV = CPTUFL
!								CPTUFLM = CPTUFLMPREV + 0.5*AVGFLMERROR
!								CPTUFLMPREV = CPTUFLM
								CPTUFL = MIN(999.,MAX(0.,FLOAT(INT(CPTUFLPREV + 0.51*AVGEFLERROR+0.5))))
								CPTUFLPREV = CPTUFL
								EPSP = MIN(CPTUFL-1,MAX(0.,FLOAT(INT(EPSPPREV - 0.51*AVGPIFLERROR+0.5))))
								EPSPPREV = EPSP
								CPTUFLM = MIN(999.,MAX(0.,FLOAT(INT(CPTUFLMPREV + 0.51*AVGFLMERROR+0.5))))
								CPTUFLMPREV = CPTUFLM
!									WRITE(*,*)"AVGEFLERROR = ", AVGEFLERROR,"; AVGFLMERROR = ", AVGFLMERROR
!									WRITE(*,*)"Reset CPTUFL = ", CPTUFL,"; CPTUFLM = ", CPTUFLM
!									PAUSE 'ERRONEOUS RESULT DETECTED'
								GOTO 104
							ENDIF
105 CONTINUE
							leftrun=leftrun-EXP_NUM
							WRITE (*,*) "Bilinear 1: SET: ",SET-1,"; ",leftrun, " runs to go out of ",OUTPUT_NUM
						END DO
					END DO
				END DO
			END DO
		END DO
	END DO
END DO
102 CONTINUE

SET = SET-1

CLOSE (12)

DEALLOCATE (TBD_set)
DEALLOCATE (TOD_set)
DEALLOCATE (TMD_set)
DEALLOCATE (SPSP_set)
DEALLOCATE (MOPP_set)
DEALLOCATE (PPSE_set)
DEALLOCATE (SHCKD_set)

999 CONTINUE
END SUBROUTINE


SUBROUTINE START_bilinear2()

CALL Rerun_Bilinear2()

!headers of output file
outputname="output_Bilinear2.txt"
OPEN (12, FILE=outputname)
WRITE (12,"(A270)") "*SET,TBD,TOD,TODNGHT,TMD,SPSP,EPSP,MOPP,PPSE,SHCKD,CPTUFL,CPTUFLM,LON,LAT,ALT,TMEFL,TMFLM,TMEM,EMD,EMDYR, &
IDOYTR,IYRTR,IDOYPI,IYRPI,IDOYPISIM,IYRPISIM,IDOYFL,IYRFL,IDOYFLSIM,IYRFLSIM,IDOYM,IYRM,IDOYMSIM,IYRMSIM,IEPI,IEPISIM,IPIFL, &
IPIFLSIM,IEFL,IEFLSIM,IFLM,IFLMSIM,CPTUTRA"

leftrun=OUTPUT_NUM
SET =1
WRITE(*,*)"AVG. ERROR E TO FL AND FL TO M < 1 DAY (CAN BE WITH LARGE SE)"
!WRITE(*,*)OUTPUT_NUM,"runs to go"
DO I_TBD=1,SIZE(TBD_set),1
	DO I_TOD=1,SIZE(TOD_set),1
		DO I_TODNGHT=1,SIZE(TODNGHT_set),1
			DO I_TMD=1,SIZE(TMD_set),1
				DO I_SPSP=1,SIZE(SPSP_set),1
					DO I_MOPP=1,SIZE(MOPP_set),1
						DO I_PPSE=1,SIZE(PPSE_set),1
							DO I_SHCKD=1,SIZE(SHCKD_set),1
!assumed initial values (iteratively estimated)
								CPTUFL = 50.
								CPTUFLM = 20.
								CPTUFLPREV = 50.
								CPTUFLMPREV = 20.
								EPSP = 40.
								EPSPPREV = 40.
								K = 0
! Here we have a loop in which CPTUFL and CPTUFLM are iteratively determined until the
! average error between simulated and actual days (E to FL) and (FL to M) is zero (can be with large SE)
204 CONTINUE
								K = K+1
								IF (K.GT.10) then
									write(*,*) "205 no convergence within 10 tries"
									GOTO 205 ! if no convergence within 10 tries then next parameter set
								END IF
								DO J = 1,EXP_NUM
    								!SITE = OBS_SET(J,1)
	    							SITE_STR = SITE_STR_SET(J,1)
									EMD = OBS_SET(J,2)
									EMDYR = OBS_SET(J,3)
									IDOYTR = OBS_SET(J,4)
									IYRTR = OBS_SET(J,5)
									IDOYPI = OBS_SET(J,6)
									IYRPI = OBS_SET(J,7)
									IDOYFL = OBS_SET(J,8)
									IYRFL = OBS_SET(J,9)
									IDOYM = OBS_SET(J,10)
									IYRM = OBS_SET(J,11)

									CALL SUM_bilinear2 (TBD_set (I_TBD), TOD_set(I_TOD), TODNGHT_set(I_TODNGHT), &
									TMD_set(I_TMD), SPSP_set(I_SPSP), EPSP, MOPP_set(I_MOPP), PPSE_set(I_PPSE), &
									SHCKD_set(I_SHCKD), CPTUFL, CPTUFLM)
									OUTPUT(1,J) = REAL(SET)
									OUTPUT(2,J) = TBD_set (I_TBD)
									OUTPUT(3,J) =  TOD_set(I_TOD)
									OUTPUT(4,J) =  TODNGHT_set(I_TODNGHT)
									OUTPUT(5,J) =  TMD_set(I_TMD)
									OUTPUT(6,J) =  SPSP_set(I_SPSP)
									OUTPUT(7,J) =  EPSP
									OUTPUT(8,J) =  MOPP_set(I_MOPP)
									OUTPUT(9,J) =  PPSE_set(I_PPSE)
									OUTPUT(10,J) =  SHCKD_SET(I_SHCKD)
									OUTPUT(11,J) =  CPTUFL
									OUTPUT(12,J) =  CPTUFLM
									OUTPUT(13,J) =  LON
									OUTPUT(14,J) =  LAT
									OUTPUT(15,J) =  ALT
									OUTPUT(16,J) =  TMEFL
									OUTPUT(17,J) = TMFLM
									OUTPUT(18,J) = TMEM
									OUTPUT(19,J) = REAL(EMD)
									OUTPUT(20,J) = REAL(EMDYR)
									OUTPUT(21,J) = REAL(IDOYTR)
									OUTPUT(22,J) = REAL(IYRTR)
									OUTPUT(23,J) = REAL(IDOYPI)
									OUTPUT(24,J) = REAL(IYRPI)
									OUTPUT(25,J) = REAL(IDOYPISIM)
									OUTPUT(26,J) = REAL(IYRPISIM)
									OUTPUT(27,J) = REAL(IDOYFL)
									OUTPUT(28,J) = REAL(IYRFL)
									OUTPUT(29,J) = REAL(IDOYFLSIM)
									OUTPUT(30,J) = REAL(IYRFLSIM)
									OUTPUT(31,J) = REAL(IDOYM)
									OUTPUT(32,J) = REAL(IYRM)
									OUTPUT(33,J) = REAL(IDOYMSIM)
									OUTPUT(34,J) = REAL(IYRMSIM)
!days from emergence to PI
									IF (IYRPI.EQ.EMDYR) THEN
										IEPI = IDOYPI - EMD
									ELSE
										IEPI = YR_day(EMDYR) - EMD + IDOYPI
									ENDIF
									IF (IYRPISIM.EQ.EMDYR) THEN
										IEPISIM = IDOYPISIM - EMD
									ELSE
										IEPISIM = YR_day(EMDYR) - EMD + IDOYPISIM
									ENDIF
!days from emergence to flowering
									IF (IYRFL.EQ.EMDYR) THEN
										IEFL = IDOYFL - EMD
									ELSE
										IEFL = YR_day(EMDYR) - EMD + IDOYFL
									ENDIF
									IF (IYRFLSIM.EQ.EMDYR) THEN
										IEFLSIM = IDOYFLSIM - EMD
									ELSE
										IEFLSIM = YR_day(EMDYR) - EMD + IDOYFLSIM
									ENDIF
!days from PI to flowering
									IPIFL = IEFL -IEPI
									IPIFLSIM = IEFLSIM -IEPISIM
!days from flowering to mature
									IF (IYRM.EQ.IYRFL) THEN
										IFLM = IDOYM - IDOYFL
									ELSE
										IFLM = YR_day(IYRFL) - IDOYFL + IDOYM
									ENDIF
									IF (IYRMSIM.EQ.IYRFLSIM) THEN
										IFLMSIM = IDOYMSIM - IDOYFLSIM
									ELSE
										IFLMSIM = YR_day(IYRFLSIM) - IDOYFLSIM + IDOYMSIM
									ENDIF
									DATASET_PIFLERROR(J) = IPIFL - IPIFLSIM
									DATASET_EFLERROR(J) = IEFL - IEFLSIM
									DATASET_FLMERROR(J) = IFLM - IFLMSIM 

									OUTPUT(35,J) = REAL(IEPI)
									OUTPUT(36,J) = REAL(IEPISIM)
									OUTPUT(37,J) = REAL(IPIFL)
									OUTPUT(38,J) = REAL(IPIFLSIM)
									OUTPUT(39,J) = REAL(IEFL)
									OUTPUT(40,J) = REAL(IEFLSIM)
									OUTPUT(41,J) = REAL(IFLM)
									OUTPUT(42,J) = REAL(IFLMSIM)
									OUTPUT(43,J) = CPTUTRA
								END DO
								201 CONTINUE
								CALL AVE(DATASET_PIFLERROR, EXP_NUM, AVGPIFLERROR)
								CALL AVE(DATASET_EFLERROR, EXP_NUM, AVGEFLERROR)
								CALL AVE(DATASET_FLMERROR, EXP_NUM, AVGFLMERROR)
! AVERAGE ERROR IN DURATION FROM EMERGENCE TO FLOWERING < 1 DAY (CAN BE WITH LARGE SE)
!   							IF (ABS(AVGEFLERROR).LT.1.) THEN
! AVERAGE ERROR IN DURATION FROM EMERGENCE TO FLOWERING < 1 DAY + DURATION FLOWERING TO MATURE < 1 DAY(CAN BE WITH LARGE SE)
	    						IF (ABS(AVGEFLERROR).LT.1..AND.ABS(AVGFLMERROR).LT.1.) THEN
! AVERAGE ERROR IN DURATION FROM PI TO FL < 1 DAY, E TO FL < 1 DAY, FL TO M < 1 DAY(CAN BE WITH LARGE SE)
!		    					IF (ABS(AVGPIFLERROR).LT.1..AND.ABS(AVGEFLERROR).LT.1..AND.ABS(AVGFLMERROR).LT.1.) THEN
! AVERAGE ERROR IN DURATION FROM PI TO FL < 1 DAY, E TO FL < 1 DAY(CAN BE WITH LARGE SE)
!			    				IF (ABS(AVGPIFLERROR).LT.1..AND.ABS(AVGEFLERROR).LT.1.) THEN
!write this run to output file
									DO J=1,EXP_NUM,1
										write (12, 203) OUTPUT(1,J), OUTPUT(2,J), OUTPUT(3,J), OUTPUT(4,J), OUTPUT(5,J), &
										OUTPUT(6,J), OUTPUT(7,J), OUTPUT(8,J), OUTPUT(9,J), OUTPUT(10,J), OUTPUT(11,J), &
										OUTPUT(12,J), OUTPUT(13,J), OUTPUT(14,J), OUTPUT(15,J), OUTPUT(16,J), OUTPUT(17,J), &
										OUTPUT(18,J), OUTPUT(19,J), OUTPUT(20,J), OUTPUT(21,J), OUTPUT(22,J), OUTPUT(23,J), &
										OUTPUT(24,J), OUTPUT(25,J), OUTPUT(26,J), OUTPUT(27,J), OUTPUT(28,J), OUTPUT(29,J), &
										OUTPUT(30,J), OUTPUT(31,J), OUTPUT(32,J), OUTPUT(33,J), OUTPUT(34,J), OUTPUT(35,J), &
										OUTPUT(36,J), OUTPUT(37,J), OUTPUT(38,J), OUTPUT(39,J), OUTPUT(40,J), OUTPUT(41,J), &
										OUTPUT(42,J), OUTPUT(43,J)
										203 FORMAT (F10.0,",",17(F8.2,","),24(F5.0,","),F8.2)
									END DO
									SET = SET+1
									IF (SET.GT.60000) THEN
										WRITE(*,*)"Terminated at 60000. runs written to output file "
										GOTO 202
									ENDIF
								ELSE
    								CPTUFL = MIN(999.,MAX(0.,FLOAT(INT(CPTUFLPREV + 0.51*AVGEFLERROR+0.5))))
	    							CPTUFLPREV = CPTUFL
		    						EPSP = MIN(CPTUFL-1,MAX(0.,FLOAT(INT(EPSPPREV - 0.51*AVGPIFLERROR+0.5))))
			    					EPSPPREV = EPSP
									CPTUFLM = MIN(999.,MAX(0.,FLOAT(INT(CPTUFLMPREV + 0.51*AVGFLMERROR+0.5))))
									CPTUFLMPREV = CPTUFLM
!									WRITE(*,*)"AVGEFLERROR = ", AVGEFLERROR,"; AVGFLMERROR = ", AVGFLMERROR
!									WRITE(*,*)"Reset CPTUFL = ", CPTUFL,"; CPTUFLM = ", CPTUFLM
!									WRITE(*,*)"Reset CPTUFL = ", MIN(999.,MAX(0.,FLOAT(INT(CPTUFL+0.5)),"; CPTUFLM = ", MIN(999.,MAX(0.,FLOAT(INT(CPTUFLM+0.5))
!									PAUSE 'ERRONEOUS RESULT DETECTED'
									GOTO 204
								ENDIF
205 CONTINUE
								leftrun=leftrun-EXP_NUM
								WRITE (*,*) "Bilinear 2: SET: ",SET-1,"; ",leftrun, " runs to go out of ",OUTPUT_NUM
							END DO
						END DO
					END DO
				END DO
			END DO
		END DO
	END DO
END DO
202 CONTINUE

SET = SET-1

CLOSE (12)

DEALLOCATE (TBD_set)
DEALLOCATE (TOD_set)
DEALLOCATE (TODNGHT_set)
DEALLOCATE (TMD_set)
DEALLOCATE (SPSP_set)
DEALLOCATE (MOPP_set)
DEALLOCATE (PPSE_set)
DEALLOCATE (SHCKD_set)
END SUBROUTINE


SUBROUTINE START_bilinear3()
!!! NOTICE: AVERAGE TEMPERATURES REPORTED HERE ARE WITH TM_CORR CORRECTION
! i.e. if TMEFL = 30.4 oC (average air temperature from emergence to flowering) 
! and if we assume canopy/water temperature is TM_CORR = -10.0 oC higher then
! here the output will report TMEFL = TMEFL + TM_CORR = 30.4 - 10 = 20.4 oC

CALL Rerun_Bilinear3()

!headers of output file
outputname="output_Bilinear3.txt"
OPEN (12, FILE=outputname)
WRITE (12,"(A270)") "*SET,TBD,TOD,TODNGHT,TMD,SPSP,EPSP,MOPP,PPSE,SHCKD,CPTUFL,CPTUFLM,LON,LAT,ALT,TMEFL,TMFLM,TMEM,EMD,EMDYR, &
IDOYTR,IYRTR,IDOYPI,IYRPI,IDOYPISIM,IYRPISIM,IDOYFL,IYRFL,IDOYFLSIM,IYRFLSIM,IDOYM,IYRM,IDOYMSIM,IYRMSIM,IEPI,IEPISIM,IPIFL, &
IPIFLSIM,IEFL,IEFLSIM,IFLM,IFLMSIM,CPTUTRA"

leftrun=OUTPUT_NUM
SET =1
WRITE(*,*)"AVG. ERROR E TO FL AND FL TO M < 1 DAY (CAN BE WITH LARGE SE)"
DO I_TBD=1,SIZE(TBD_set),1
	DO I_TOD=1,SIZE(TOD_set),1
		DO I_TMD=1,SIZE(TMD_set),1
			DO I_SPSP=1,SIZE(SPSP_set),1
				DO I_MOPP=1,SIZE(MOPP_set),1
					DO I_PPSE=1,SIZE(PPSE_set),1
						DO I_SHCKD=1,SIZE(SHCKD_set),1
							DO I_TM_CORR=1,SIZE(TM_CORR_set),1
!assumed initial values (iteratively estimated)
								CPTUFL = 50.
								CPTUFLM = 20.
								CPTUFLPREV = 50.
								CPTUFLMPREV = 20.
								EPSP = 40.
								EPSPPREV = 40.
								K = 0
! Here we have a loop in which CPTUFL and CPTUFLM are iteratively determined until the
! average error between simulated and actual days (E to FL) and (FL to M) is zero (can be with large SE)
304 CONTINUE
								K = K+1
								IF (K.GT.10) then
									write(*,*) "305 no convergence within 10 tries"
									GOTO 305 ! if no convergence within 10 tries then next parameter set
								END IF
								DO J = 1,EXP_NUM
    								!SITE = OBS_SET(J,1)
	    							SITE_STR = SITE_STR_SET(J,1)
									EMD = OBS_SET(J,2)
									EMDYR = OBS_SET(J,3)
									IDOYTR = OBS_SET(J,4)
									IYRTR = OBS_SET(J,5)
									IDOYPI = OBS_SET(J,6)
									IYRPI = OBS_SET(J,7)
									IDOYFL = OBS_SET(J,8)
									IYRFL = OBS_SET(J,9)
									IDOYM = OBS_SET(J,10)
									IYRM = OBS_SET(J,11)

									CALL SUM_bilinear3 (TBD_set (I_TBD), TOD_set(I_TOD), TMD_set(I_TMD), SPSP_set(I_SPSP), &
									EPSP, MOPP_set(I_MOPP), PPSE_set(I_PPSE), SHCKD_set(I_SHCKD), TM_CORR_set(I_TM_CORR), &
									CPTUFL, CPTUFLM)
									
									OUTPUT(1,J) = REAL(SET)
									OUTPUT(2,J) = TBD_set (I_TBD)
									OUTPUT(3,J) =  TOD_set(I_TOD)
									OUTPUT(4,J) =  TMD_set(I_TMD)
									OUTPUT(5,J) =  TM_CORR_set(I_TM_CORR)
									OUTPUT(6,J) =  SPSP_set(I_SPSP)
									OUTPUT(7,J) =  EPSP
									OUTPUT(8,J) =  MOPP_set(I_MOPP)
									OUTPUT(9,J) =  PPSE_set(I_PPSE)
									OUTPUT(10,J) =  SHCKD_SET(I_SHCKD)
									OUTPUT(11,J) =  CPTUFL
									OUTPUT(12,J) =  CPTUFLM
									OUTPUT(13,J) =  LON
									OUTPUT(14,J) =  LAT
									OUTPUT(15,J) =  ALT
									OUTPUT(16,J) =  TMEFL
									OUTPUT(17,J) = TMFLM
									OUTPUT(18,J) = TMEM
									OUTPUT(19,J) = REAL(EMD)
									OUTPUT(20,J) = REAL(EMDYR)
									OUTPUT(21,J) = REAL(IDOYTR)
									OUTPUT(22,J) = REAL(IYRTR)
									OUTPUT(23,J) = REAL(IDOYPI)
									OUTPUT(24,J) = REAL(IYRPI)
									OUTPUT(25,J) = REAL(IDOYPISIM)
									OUTPUT(26,J) = REAL(IYRPISIM)
									OUTPUT(27,J) = REAL(IDOYFL)
									OUTPUT(28,J) = REAL(IYRFL)
									OUTPUT(29,J) = REAL(IDOYFLSIM)
									OUTPUT(30,J) = REAL(IYRFLSIM)
									OUTPUT(31,J) = REAL(IDOYM)
									OUTPUT(32,J) = REAL(IYRM)
									OUTPUT(33,J) = REAL(IDOYMSIM)
									OUTPUT(34,J) = REAL(IYRMSIM)
!days from emergence to PI
									IF (IYRPI.EQ.EMDYR) THEN
										IEPI = IDOYPI - EMD
									ELSE
										IEPI = YR_day(EMDYR) - EMD + IDOYPI
									ENDIF
									IF (IYRPISIM.EQ.EMDYR) THEN
										IEPISIM = IDOYPISIM - EMD
									ELSE
										IEPISIM = YR_day(EMDYR) - EMD + IDOYPISIM
									ENDIF
!days from emergence to flowering
									IF (IYRFL.EQ.EMDYR) THEN
										IEFL = IDOYFL - EMD
									ELSE
										IEFL = YR_day(EMDYR) - EMD + IDOYFL
									ENDIF
									IF (IYRFLSIM.EQ.EMDYR) THEN
										IEFLSIM = IDOYFLSIM - EMD
									ELSE
										IEFLSIM = YR_day(EMDYR) - EMD + IDOYFLSIM
									ENDIF
!days from PI to flowering
									IPIFL = IEFL -IEPI
									IPIFLSIM = IEFLSIM -IEPISIM
!days from flowering to mature
									IF (IYRM.EQ.IYRFL) THEN
										IFLM = IDOYM - IDOYFL
									ELSE
										IFLM = YR_day(IYRFL) - IDOYFL + IDOYM
									ENDIF
									IF (IYRMSIM.EQ.IYRFLSIM) THEN
										IFLMSIM = IDOYMSIM - IDOYFLSIM
									ELSE
										IFLMSIM = YR_day(IYRFLSIM) - IDOYFLSIM + IDOYMSIM
									ENDIF
									DATASET_PIFLERROR(J) = IPIFL - IPIFLSIM
									DATASET_EFLERROR(J) = IEFL - IEFLSIM
									DATASET_FLMERROR(J) = IFLM - IFLMSIM 

									OUTPUT(35,J) = REAL(IEPI)
									OUTPUT(36,J) = REAL(IEPISIM)
									OUTPUT(37,J) = REAL(IPIFL)
									OUTPUT(38,J) = REAL(IPIFLSIM)
									OUTPUT(39,J) = REAL(IEFL)
									OUTPUT(40,J) = REAL(IEFLSIM)
									OUTPUT(41,J) = REAL(IFLM)
									OUTPUT(42,J) = REAL(IFLMSIM)
									OUTPUT(43,J) = CPTUTRA
								END DO
								301 CONTINUE
								CALL AVE(DATASET_PIFLERROR, EXP_NUM, AVGPIFLERROR)
								CALL AVE(DATASET_EFLERROR, EXP_NUM, AVGEFLERROR)
								CALL AVE(DATASET_FLMERROR, EXP_NUM, AVGFLMERROR)
! AVERAGE ERROR IN DURATION FROM EMERGENCE TO FLOWERING < 1 DAY (CAN BE WITH LARGE SE)
!   							IF (ABS(AVGEFLERROR).LT.1.) THEN
! AVERAGE ERROR IN DURATION FROM EMERGENCE TO FLOWERING < 1 DAY + DURATION FLOWERING TO MATURE < 1 DAY(CAN BE WITH LARGE SE)
	    						IF (ABS(AVGEFLERROR).LT.1..AND.ABS(AVGFLMERROR).LT.1.) THEN
! AVERAGE ERROR IN DURATION FROM PI TO FL < 1 DAY, E TO FL < 1 DAY, FL TO M < 1 DAY(CAN BE WITH LARGE SE)
!		    					IF (ABS(AVGPIFLERROR).LT.1..AND.ABS(AVGEFLERROR).LT.1..AND.ABS(AVGFLMERROR).LT.1.) THEN
! AVERAGE ERROR IN DURATION FROM PI TO FL < 1 DAY, E TO FL < 1 DAY(CAN BE WITH LARGE SE)
!			    				IF (ABS(AVGPIFLERROR).LT.1..AND.ABS(AVGEFLERROR).LT.1.) THEN
!write this run to output file
									DO J=1,EXP_NUM,1
										write (12, 303) OUTPUT(1,J), OUTPUT(2,J), OUTPUT(3,J), OUTPUT(4,J), OUTPUT(5,J), &
										OUTPUT(6,J), OUTPUT(7,J), OUTPUT(8,J), OUTPUT(9,J), OUTPUT(10,J), OUTPUT(11,J), &
										OUTPUT(12,J), OUTPUT(13,J), OUTPUT(14,J), OUTPUT(15,J), OUTPUT(16,J), OUTPUT(17,J), &
										OUTPUT(18,J), OUTPUT(19,J), OUTPUT(20,J), OUTPUT(21,J), OUTPUT(22,J), OUTPUT(23,J), &
										OUTPUT(24,J), OUTPUT(25,J), OUTPUT(26,J), OUTPUT(27,J), OUTPUT(28,J), OUTPUT(29,J), &
										OUTPUT(30,J), OUTPUT(31,J), OUTPUT(32,J), OUTPUT(33,J), OUTPUT(34,J), OUTPUT(35,J), &
										OUTPUT(36,J), OUTPUT(37,J), OUTPUT(38,J), OUTPUT(39,J), OUTPUT(40,J), OUTPUT(41,J), &
										OUTPUT(42,J), OUTPUT(43,J)
										303 FORMAT (F10.0,",",17(F8.2,","),24(F5.0,","),F8.2)
									END DO
									SET = SET+1
									IF (SET.GT.60000) THEN
										WRITE(*,*)"Terminated at 60000. runs written to output file "
										GOTO 302
									ENDIF
								ELSE
    								CPTUFL = MIN(999.,MAX(0.,FLOAT(INT(CPTUFLPREV + 0.51*AVGEFLERROR+0.5))))
	    							CPTUFLPREV = CPTUFL
		    						EPSP = MIN(CPTUFL-1,MAX(0.,FLOAT(INT(EPSPPREV - 0.51*AVGPIFLERROR+0.5))))
			    					EPSPPREV = EPSP
									CPTUFLM = MIN(999.,MAX(0.,FLOAT(INT(CPTUFLMPREV + 0.51*AVGFLMERROR+0.5))))
									CPTUFLMPREV = CPTUFLM
!									WRITE(*,*)"AVGEFLERROR = ", AVGEFLERROR,"; AVGFLMERROR = ", AVGFLMERROR
!									WRITE(*,*)"Reset CPTUFL = ", CPTUFL,"; CPTUFLM = ", CPTUFLM
!									WRITE(*,*)"Reset CPTUFL = ", MIN(999.,MAX(0.,FLOAT(INT(CPTUFL+0.5)),"; CPTUFLM = ", MIN(999.,MAX(0.,FLOAT(INT(CPTUFLM+0.5))
!									PAUSE 'ERRONEOUS RESULT DETECTED'
									GOTO 304
								ENDIF
305 CONTINUE
								leftrun=leftrun-EXP_NUM
								WRITE (*,*) "Bilinear 3: SET: ",SET-1,"; ",leftrun, " runs to go out of ",OUTPUT_NUM
							END DO
						END DO
					END DO
				END DO
			END DO
		END DO
	END DO
END DO
302 CONTINUE

SET = SET-1

CLOSE (12)

DEALLOCATE (TBD_set)
DEALLOCATE (TOD_set)
DEALLOCATE (TMD_set)
DEALLOCATE (SPSP_set)
DEALLOCATE (MOPP_set)
DEALLOCATE (PPSE_set)
DEALLOCATE (SHCKD_set)
DEALLOCATE (TM_CORR_set)
END SUBROUTINE


SUBROUTINE START_beta()

CALL Rerun_Beta()

outputname="output_BetaPSP.txt"
BETAOUTPUT=outputname
BETASLOPECV = "SlopeCV_BetaPSP.txt"
OPEN (12, FILE=outputname) 
WRITE (12,"(A300)") "*SET,TBD,TMD,TOD,TODNGHT,TSEN,TSENNGHT,TSENPSP,TSENPSPNGHT,SPSP,EPSP,MOPP,PPSE,SHCKD,CPTUFL,CPTUFLM, &
LON,LAT,ALT,TMEFL,TMFLM,TMEM,EMD,EMDYR,IDOYTR,IYRTR,IDOYPI,IYRPI,IDOYPISIM,IYRPISIM,IDOYFL,IYRFL,IDOYFLSIM,IYRFLSIM,IDOYM, &
IYRM,IDOYMSIM,IYRMSIM,IEPI,IEPISIM,IPIFL,IPIFLSIM,IEFL,IEFLSIM,IFLM,IFLMSIM,CPTUTRA"

leftrun=OUTPUT_NUM
SET = 1
WRITE(*,*)"AVG. ERROR E TO FL AND FL TO M < 1 DAY (CAN BE WITH LARGE SE)"
DO I_TBD=1,SIZE(TBD_set),1
	DO I_TMD=1,SIZE(TMD_set),1
		DO I_TOD=1,SIZE(TOD_set),1
			DO I_TODNGHT=1,SIZE(TODNGHT_set),1
				DO I_TSEN=1,SIZE(TSEN_set),1
					DO I_TSENNGHT=1,SIZE(TSENNGHT_set),1
						DO I_TSENPSP=1,SIZE(TSENPSP_set),1
							DO I_TSENPSPNGHT=1,SIZE(TSENPSPNGHT_set),1
								DO I_SPSP=1,SIZE(SPSP_set),1
									DO I_MOPP=1,SIZE(MOPP_set),1
										DO I_PPSE=1,SIZE(PPSE_set),1
											DO I_SHCKD=1,SIZE(SHCKD_set),1
!assumed initial values (iteratively estimated)
												CPTUFL = 50.
												CPTUFLM = 20.
												CPTUFLPREV = 50.
												CPTUFLMPREV = 20.
												EPSP = 40.
												EPSPPREV = 40.
												K = 0
! Here we have a loop in which CPTUFL and CPTUFLM are iteratively determined until the
! average error between simulated and actual days (E to FL) and (FL to M) is zero (can be with large SE)
404 CONTINUE
												K = K+1
												IF (K.GT.10) then
													GOTO 405 ! if no convergence within 10 tries then next parameter set
													write(*,*) "405 no convergence within 10 tries"
												END IF
												DO J = 1,EXP_NUM
                    								!SITE = OBS_SET(J,1)
	    							                SITE_STR = SITE_STR_SET(J,1)
													EMD = OBS_SET(J,2)
													EMDYR = OBS_SET(J,3)
													IDOYTR = OBS_SET(J,4)
													IYRTR = OBS_SET(J,5)
													IDOYPI = OBS_SET(J,6)
													IYRPI = OBS_SET(J,7)
													IDOYFL = OBS_SET(J,8)
													IYRFL = OBS_SET(J,9)
													IDOYM = OBS_SET(J,10)
													IYRM = OBS_SET(J,11)
	
CALL SUM_BETA (TBD_set(I_TBD), TOD_set(I_TOD), TMD_set(I_TMD), TODNGHT_set(I_TODNGHT), TSEN_set(I_TSEN), &
TSENNGHT_set(I_TSENNGHT), SPSP_set(I_SPSP), EPSP, MOPP_set(I_MOPP), PPSE_set(I_PPSE), TSENPSP_set(I_TSENPSP), &
TSENPSPNGHT_set(I_TSENPSPNGHT), SHCKD_set(I_SHCKD), CPTUFL, CPTUFLM) 
	
													OUTPUT(1,J) = REAL(SET)
													OUTPUT(2,J) = TBD_set (I_TBD)
													OUTPUT(3,J) =  TMD_set(I_TMD)
													OUTPUT(4,J) =  TOD_set(I_TOD)
													OUTPUT(5,J) =  TODNGHT_set(I_TODNGHT)
													OUTPUT(6,J) =  TSEN_set(I_TSEN)
													OUTPUT(7,J) =  TSENNGHT_set(I_TSENNGHT)
													OUTPUT(8,J) =  TSENPSP_set(I_TSENPSP)
													OUTPUT(9,J) =  TSENPSPNGHT_set(I_TSENPSPNGHT)
													OUTPUT(10,J) =  SPSP_set(I_SPSP)
													OUTPUT(11,J) =  EPSP
													OUTPUT(12,J) =  MOPP_set(I_MOPP)
													OUTPUT(13,J) =  PPSE_set(I_PPSE)
													OUTPUT(14,J) =  SHCKD_SET(I_SHCKD)
													OUTPUT(15,J) =  CPTUFL
													OUTPUT(16,J) =  CPTUFLM
													OUTPUT(17,J) =  LON
													OUTPUT(18,J) =  LAT
													OUTPUT(19,J) =  ALT
													OUTPUT(20,J) =  TMEFL
													OUTPUT(21,J) = TMFLM
													OUTPUT(22,J) = TMEM
													OUTPUT(23,J) = REAL(EMD)
													OUTPUT(24,J) = REAL(EMDYR)
													OUTPUT(25,J) = REAL(IDOYTR)
													OUTPUT(26,J) = REAL(IYRTR)
													OUTPUT(27,J) = REAL(IDOYPI)
													OUTPUT(28,J) = REAL(IYRPI)
													OUTPUT(29,J) = REAL(IDOYPISIM)
													OUTPUT(30,J) = REAL(IYRPISIM)
													OUTPUT(31,J) = REAL(IDOYFL)
													OUTPUT(32,J) = REAL(IYRFL)
													OUTPUT(33,J) = REAL(IDOYFLSIM)
													OUTPUT(34,J) = REAL(IYRFLSIM)
													OUTPUT(35,J) = REAL(IDOYM)
													OUTPUT(36,J) = REAL(IYRM)
													OUTPUT(37,J) = REAL(IDOYMSIM)
													OUTPUT(38,J) = REAL(IYRMSIM)
!days from emergence to PI
													IF (IYRPI.EQ.EMDYR) THEN
														IEPI = IDOYPI - EMD
													ELSE
														IEPI = YR_day(EMDYR) - EMD + IDOYPI
													ENDIF
													IF (IYRPISIM.EQ.EMDYR) THEN
														IEPISIM = IDOYPISIM - EMD
													ELSE
														IEPISIM = YR_day(EMDYR) - EMD + IDOYPISIM
													ENDIF
!days from emergence to flowering	
													IF (IYRFL.EQ.EMDYR) THEN
														IEFL = IDOYFL - EMD
													ELSE
														IEFL = YR_day(EMDYR) - EMD + IDOYFL
													ENDIF
													IF (IYRFLSIM.EQ.EMDYR) THEN
														IEFLSIM = IDOYFLSIM - EMD
													ELSE
														IEFLSIM = YR_day(EMDYR) - EMD + IDOYFLSIM
													ENDIF
!days from PI to flowering
													IPIFL = IEFL -IEPI
													IPIFLSIM = IEFLSIM -IEPISIM
!days from flowering to mature
													IF (IYRM.EQ.IYRFL) THEN
														IFLM = IDOYM - IDOYFL
													ELSE
														IFLM = YR_day(IYRFL) - IDOYFL + IDOYM
													ENDIF
													IF (IYRMSIM.EQ.IYRFLSIM) THEN
														IFLMSIM = IDOYMSIM - IDOYFLSIM
													ELSE
														IFLMSIM = YR_day(IYRFLSIM) - IDOYFLSIM + IDOYMSIM
													ENDIF
													DATASET_PIFLERROR(J) = IPIFL - IPIFLSIM
													DATASET_EFLERROR(J) = IEFL - IEFLSIM
													DATASET_FLMERROR(J) = IFLM - IFLMSIM 
													OUTPUT(39,J) = REAL(IEPI)
													OUTPUT(40,J) = REAL(IEPISIM)
													OUTPUT(41,J) = REAL(IPIFL)
													OUTPUT(42,J) = REAL(IPIFLSIM)
													OUTPUT(43,J) = REAL(IEFL)
													OUTPUT(44,J) = REAL(IEFLSIM)
													OUTPUT(45,J) = REAL(IFLM)
													OUTPUT(46,J) = REAL(IFLMSIM)
													OUTPUT(47,J) = CPTUTRA

												END DO
401 CONTINUE
												CALL AVE(DATASET_PIFLERROR, EXP_NUM, AVGPIFLERROR)
												CALL AVE(DATASET_EFLERROR, EXP_NUM, AVGEFLERROR)
												CALL AVE(DATASET_FLMERROR, EXP_NUM, AVGFLMERROR)
! AVERAGE ERROR IN DURATION FROM EMERGENCE TO FLOWERING < 1 DAY (CAN BE WITH LARGE SE)
!                      							IF (ABS(AVGEFLERROR).LT.1.) THEN
! AVERAGE ERROR IN DURATION FROM EMERGENCE TO FLOWERING < 1 DAY + DURATION FLOWERING TO MATURE < 1 DAY(CAN BE WITH LARGE SE)
	    			                			IF (ABS(AVGEFLERROR).LT.1..AND.ABS(AVGFLMERROR).LT.1.) THEN
! AVERAGE ERROR IN DURATION FROM PI TO FL < 1 DAY, E TO FL < 1 DAY, FL TO M < 1 DAY(CAN BE WITH LARGE SE)
!		    					                IF (ABS(AVGPIFLERROR).LT.1..AND.ABS(AVGEFLERROR).LT.1..AND.ABS(AVGFLMERROR).LT.1.) THEN
! AVERAGE ERROR IN DURATION FROM PI TO FL < 1 DAY, E TO FL < 1 DAY(CAN BE WITH LARGE SE)
!			    				                IF (ABS(AVGPIFLERROR).LT.1..AND.ABS(AVGEFLERROR).LT.1.) THEN
!write this run to output file
													DO J=1,EXP_NUM,1
														write (12, 403) OUTPUT(1,J), OUTPUT(2,J), OUTPUT(3,J), OUTPUT(4,J), &
														OUTPUT(5,J), OUTPUT(6,J), OUTPUT(7,J), OUTPUT(8,J), OUTPUT(9,J), &
														OUTPUT(10,J), OUTPUT(11,J), OUTPUT(12,J), OUTPUT(13,J), OUTPUT(14,J), &
														OUTPUT(15,J), OUTPUT(16,J), OUTPUT(17,J), OUTPUT(18,J), OUTPUT(19,J), &
														OUTPUT(20,J), OUTPUT(21,J), OUTPUT(22,J), OUTPUT(23,J), OUTPUT(24,J), &
														OUTPUT(25,J), OUTPUT(26,J), OUTPUT(27,J), OUTPUT(28,J), OUTPUT(29,J), &
														OUTPUT(30,J), OUTPUT(31,J), OUTPUT(32,J), OUTPUT(33,J), OUTPUT(34,J), &
														OUTPUT(35,J), OUTPUT(36,J), OUTPUT(37,J), OUTPUT(38,J), OUTPUT(39,J), &
														OUTPUT(40,J), OUTPUT(41,J), OUTPUT(42,J), OUTPUT(43,J), OUTPUT(44,J), &
														OUTPUT(45,J), OUTPUT(46,J), OUTPUT(47,J)
														403 FORMAT (F10.0,",",21(F8.2,","),24(F5.0,","),F8.2)
													END DO
													SET = SET+1
													IF (SET.GT.60000) THEN
														WRITE(*,*)"Terminated at 60000. runs written to output file "
														GOTO 402
													ENDIF
												ELSE
                								    CPTUFL = MIN(999.,MAX(0.,FLOAT(INT(CPTUFLPREV + 0.51*AVGEFLERROR+0.5))))
	    							                CPTUFLPREV = CPTUFL
                		    						EPSP = MIN(CPTUFL-1,MAX(0.,FLOAT(INT(EPSPPREV - 0.51*AVGPIFLERROR+0.5))))
			    					                EPSPPREV = EPSP
    												CPTUFLM = MIN(999.,MAX(0.,FLOAT(INT(CPTUFLMPREV + 0.51*AVGFLMERROR+0.5))))
													CPTUFLMPREV = CPTUFLM
!													WRITE(*,*)"AVGEFLERROR = ", AVGEFLERROR,"; AVGFLMERROR = ", AVGFLMERROR
!													WRITE(*,*)"Reset CPTUFL = ", CPTUFL,"; CPTUFLM = ", CPTUFLM
!													WRITE(*,*)"Reset CPTUFL = ", MIN(999.,MAX(0.,FLOAT(INT(CPTUFL+0.5)),"; CPTUFLM = ", MIN(999.,MAX(0.,FLOAT(INT(CPTUFLM+0.5))
!													PAUSE 'ERRONEOUS RESULT DETECTED'
													GOTO 404
												ENDIF
405 CONTINUE
												leftrun=leftrun-EXP_NUM
												WRITE (*,*) "BetaPSP: SET: ",SET-1,"; ",leftrun, " runs to go out of ",OUTPUT_NUM
											END DO
										END DO
									END DO
								END DO
							END DO
						END DO
					END DO
				END DO
			END DO
		END DO
	END DO
END DO
402 CONTINUE
SET = SET-1

CLOSE (12)


DEALLOCATE (TBD_set)
DEALLOCATE (TOD_set)
DEALLOCATE (TODNGHT_set)
DEALLOCATE (TMD_set)
DEALLOCATE (SPSP_set)
DEALLOCATE (MOPP_set)
DEALLOCATE (PPSE_set)
DEALLOCATE (TSEN_set)
DEALLOCATE (TSENNGHT_set)
DEALLOCATE (TSENPSP_set)
DEALLOCATE (TSENPSPNGHT_set)
DEALLOCATE (SHCKD_set)

END SUBROUTINE

SUBROUTINE START_betaNOPSP()

CALL Rerun_Beta()

outputname="output_BetaNOPSP.txt"
BETAOUTPUT=outputname
BETASLOPECV = "SlopeCV_BetaNOPSP.txt"
OPEN (12, FILE=outputname) 
WRITE (12,"(A280)") "*SET,TBD,TMD,TOD,TODNGHT,TSEN,TSENNGHT,TSENPSP,TSENPSPNGHT,SPSP,EPSP,MOPP,PPSE,SHCKD,CPTUFL,CPTUFLM, &
LON,LAT,ALT,TMEFL,TMFLM,TMEM,EMD,EMDYR,IDOYTR,IYRTR,IDOYPI,IYRPI,IDOYPISIM,IYRPISIM,IDOYFL,IYRFL,IDOYFLSIM,IYRFLSIM, &
IDOYM,IYRM,IDOYMSIM,IYRMSIM,IFLM,IFLMSIM,IEFL,IEFLSIM,CPTUTRA"

leftrun=OUTPUT_NUM
SET = 1
WRITE(*,*)"AVG. ERROR E TO FL AND FL TO M < 1 DAY (CAN BE WITH LARGE SE)"
DO I_TBD=1,SIZE(TBD_set),1
	DO I_TMD=1,SIZE(TMD_set),1
		DO I_TOD=1,SIZE(TOD_set),1
			DO I_TODNGHT=1,SIZE(TODNGHT_set),1
				DO I_TSEN=1,SIZE(TSEN_set),1
					DO I_TSENNGHT=1,SIZE(TSENNGHT_set),1
!						DO I_TSENPSP=1,SIZE(TSENPSP_set),1
!							DO I_TSENPSPNGHT=1,SIZE(TSENPSPNGHT_set),1
!								DO I_SPSP=1,SIZE(SPSP_set),1
!									DO I_MOPP=1,SIZE(MOPP_set),1
!										DO I_PPSE=1,SIZE(PPSE_set),1
											DO I_SHCKD=1,SIZE(SHCKD_set),1
!assumed initial values (iteratively estimated)
												CPTUFL = 50.
												CPTUFLM = 20.
												CPTUFLPREV = 50.
												CPTUFLMPREV = 20.
												EPSP = 40.
												EPSPPREV = 40.
												K = 0
! Here we have a loop in which CPTUFL and CPTUFLM are iteratively determined until the
! average error between simulated and actual days (E to FL) and (FL to M) is zero (can be with large SE)
504 CONTINUE
												K = K+1
												IF (K.GT.10) then
													GOTO 505 ! if no convergence within 10 tries then next parameter set
													write(*,*) "505 no convergence within 10 tries"
												END IF
												DO J = 1,EXP_NUM
                    								!SITE = OBS_SET(J,1)
	    							                SITE_STR = SITE_STR_SET(J,1)
													EMD = OBS_SET(J,2)
													EMDYR = OBS_SET(J,3)
													IDOYTR = OBS_SET(J,4)
													IYRTR = OBS_SET(J,5)
													IDOYPI = OBS_SET(J,6)
													IYRPI = OBS_SET(J,7)
													IDOYFL = OBS_SET(J,8)
													IYRFL = OBS_SET(J,9)
													IDOYM = OBS_SET(J,10)
													IYRM = OBS_SET(J,11)
	
CALL SUM_BETA (TBD_set(I_TBD), TOD_set(I_TOD), TMD_set(I_TMD), TODNGHT_set(I_TODNGHT), TSEN_set(I_TSEN),TSENNGHT_set(I_TSENNGHT) &
,999., EPSP, 0., 0., TSEN_set(I_TSEN), TSENNGHT_set(I_TSENNGHT), SHCKD_set(I_SHCKD), CPTUFL, CPTUFLM) 
	
													OUTPUT(1,J) = REAL(SET)
													OUTPUT(2,J) = TBD_set (I_TBD)
													OUTPUT(3,J) =  TMD_set(I_TMD)
													OUTPUT(4,J) =  TOD_set(I_TOD)
													OUTPUT(5,J) =  TODNGHT_set(I_TODNGHT)
													OUTPUT(6,J) =  TSEN_set(I_TSEN)
													OUTPUT(7,J) =  TSENNGHT_set(I_TSENNGHT)
													OUTPUT(8,J) =  TSEN_set(I_TSEN)
													OUTPUT(9,J) =  TSENNGHT_set(I_TSENNGHT)
													OUTPUT(10,J) =  999.
													OUTPUT(11,J) =  EPSP
													OUTPUT(12,J) =  0.
													OUTPUT(13,J) =  0.
													OUTPUT(14,J) =  SHCKD_SET(I_SHCKD)
													OUTPUT(15,J) =  CPTUFL
													OUTPUT(16,J) =  CPTUFLM
													OUTPUT(17,J) =  LON
													OUTPUT(18,J) =  LAT
													OUTPUT(19,J) =  ALT
													OUTPUT(20,J) =  TMEFL
													OUTPUT(21,J) = TMFLM
													OUTPUT(22,J) = TMEM
													OUTPUT(23,J) = REAL(EMD)
													OUTPUT(24,J) = REAL(EMDYR)
													OUTPUT(25,J) = REAL(IDOYTR)
													OUTPUT(26,J) = REAL(IYRTR)
													OUTPUT(27,J) = REAL(IDOYPI)
													OUTPUT(28,J) = REAL(IYRPI)
													OUTPUT(29,J) = REAL(IDOYPISIM)
													OUTPUT(30,J) = REAL(IYRPISIM)
													OUTPUT(31,J) = REAL(IDOYFL)
													OUTPUT(32,J) = REAL(IYRFL)
													OUTPUT(33,J) = REAL(IDOYFLSIM)
													OUTPUT(34,J) = REAL(IYRFLSIM)
													OUTPUT(35,J) = REAL(IDOYM)
													OUTPUT(36,J) = REAL(IYRM)
													OUTPUT(37,J) = REAL(IDOYMSIM)
													OUTPUT(38,J) = REAL(IYRMSIM)
!days from emergence to PI
													IF (IYRPI.EQ.EMDYR) THEN
														IEPI = IDOYPI - EMD
													ELSE
														IEPI = YR_day(EMDYR) - EMD + IDOYPI
													ENDIF
													IF (IYRPISIM.EQ.EMDYR) THEN
														IEPISIM = IDOYPISIM - EMD
													ELSE
														IEPISIM = YR_day(EMDYR) - EMD + IDOYPISIM
													ENDIF
!days from emergence to flowering	
													IF (IYRFL.EQ.EMDYR) THEN
														IEFL = IDOYFL - EMD
													ELSE
														IEFL = YR_day(EMDYR) - EMD + IDOYFL
													ENDIF
													IF (IYRFLSIM.EQ.EMDYR) THEN
														IEFLSIM = IDOYFLSIM - EMD
													ELSE
														IEFLSIM = YR_day(EMDYR) - EMD + IDOYFLSIM
													ENDIF
!days from flowering to mature
													IF (IYRM.EQ.IYRFL) THEN
														IFLM = IDOYM - IDOYFL
													ELSE
														IFLM = YR_day(IYRFL) - IDOYFL + IDOYM
													ENDIF
													IF (IYRMSIM.EQ.IYRFLSIM) THEN
														IFLMSIM = IDOYMSIM - IDOYFLSIM
													ELSE
														IFLMSIM = YR_day(IYRFLSIM) - IDOYFLSIM + IDOYMSIM
													ENDIF
													DATASET_PIFLERROR(J) = IPIFL - IPIFLSIM
													DATASET_EFLERROR(J) = IEFL - IEFLSIM
													DATASET_FLMERROR(J) = IFLM - IFLMSIM 
													OUTPUT(39,J) = REAL(IEPI)
													OUTPUT(40,J) = REAL(IEPISIM)
													OUTPUT(41,J) = REAL(IPIFL)
													OUTPUT(42,J) = REAL(IPIFLSIM)
													OUTPUT(43,J) = REAL(IEFL)
													OUTPUT(44,J) = REAL(IEFLSIM)
													OUTPUT(45,J) = REAL(IFLM)
													OUTPUT(46,J) = REAL(IFLMSIM)
													OUTPUT(47,J) = CPTUTRA

												END DO
501 CONTINUE
												CALL AVE(DATASET_PIFLERROR, EXP_NUM, AVGPIFLERROR)
												CALL AVE(DATASET_EFLERROR, EXP_NUM, AVGEFLERROR)
												CALL AVE(DATASET_FLMERROR, EXP_NUM, AVGFLMERROR)
! AVERAGE ERROR IN DURATION FROM EMERGENCE TO FLOWERING < 1 DAY + DURATION FLOWERING TO MATURE < 1 DAY(CAN BE WITH LARGE SE)
												IF (ABS(AVGEFLERROR).LT.1..AND.ABS(AVGFLMERROR).LT.1.) THEN
! AVERAGE ERROR IN DURATION FROM PI TO FL < 1 DAY, E TO FL < 1 DAY, FL TO M < 1 DAY(CAN BE WITH LARGE SE)
!												IF (ABS(AVGPIFLERROR).LT.1..AND.ABS(AVGEFLERROR).LT.1..AND.ABS(AVGFLMERROR).LT.1.) THEN
													DO J=1,EXP_NUM,1
														write (12, 503) OUTPUT(1,J), OUTPUT(2,J), OUTPUT(3,J), OUTPUT(4,J), &
														OUTPUT(5,J), OUTPUT(6,J), OUTPUT(7,J), OUTPUT(8,J), OUTPUT(9,J), &
														OUTPUT(10,J), OUTPUT(11,J), OUTPUT(12,J), OUTPUT(13,J), OUTPUT(14,J), &
														OUTPUT(15,J), OUTPUT(16,J), OUTPUT(17,J), OUTPUT(18,J), OUTPUT(19,J), &
														OUTPUT(20,J), OUTPUT(21,J), OUTPUT(22,J), OUTPUT(23,J), OUTPUT(24,J), &
														OUTPUT(25,J), OUTPUT(26,J), OUTPUT(27,J), OUTPUT(28,J), OUTPUT(29,J), &
														OUTPUT(30,J), OUTPUT(31,J), OUTPUT(32,J), OUTPUT(33,J), OUTPUT(34,J), &
														OUTPUT(35,J), OUTPUT(36,J), OUTPUT(37,J), OUTPUT(38,J), OUTPUT(39,J), &
														OUTPUT(40,J), OUTPUT(41,J), OUTPUT(42,J), OUTPUT(43,J), OUTPUT(44,J), &
														OUTPUT(45,J), OUTPUT(46,J), OUTPUT(47,J)
														503 FORMAT (F10.0,",",21(F8.2,","),24(F5.0,","),F8.2)
													END DO
													SET = SET+1
													IF (SET.GT.60000) THEN
														WRITE(*,*)"Terminated at 60000. runs written to output file "
														GOTO 502
													ENDIF
												ELSE
                								    CPTUFL = MIN(999.,MAX(0.,FLOAT(INT(CPTUFLPREV + 0.51*AVGEFLERROR+0.5))))
	    							                CPTUFLPREV = CPTUFL
                		    						EPSP = MIN(CPTUFL-1,MAX(0.,FLOAT(INT(EPSPPREV - 0.51*AVGPIFLERROR+0.5))))
			    					                EPSPPREV = EPSP
													CPTUFLM = MIN(999.,MAX(0.,FLOAT(INT(CPTUFLMPREV + 0.51*AVGFLMERROR+0.5))))
													CPTUFLMPREV = CPTUFLM
!													WRITE(*,*)"AVGEFLERROR = ", AVGEFLERROR,"; AVGFLMERROR = ", AVGFLMERROR
!													WRITE(*,*)"Reset CPTUFL = ", CPTUFL,"; CPTUFLM = ", CPTUFLM
!													WRITE(*,*)"Reset CPTUFL = ", MIN(999.,MAX(0.,FLOAT(INT(CPTUFL+0.5)),"; CPTUFLM = ", MIN(999.,MAX(0.,FLOAT(INT(CPTUFLM+0.5))
!													PAUSE 'ERRONEOUS RESULT DETECTED'
													GOTO 504
												ENDIF
505 CONTINUE
												leftrun=leftrun-EXP_NUM
												WRITE (*,*) "BetaNOPSP: SET: ",SET-1,"; ",leftrun, " runs to go out of ",OUTPUT_NUM
											END DO
!										END DO
!									END DO
!								END DO
!							END DO
!						END DO
					END DO
				END DO
			END DO
		END DO
	END DO
END DO
502 CONTINUE
SET = SET-1

CLOSE (12)


DEALLOCATE (TBD_set)
DEALLOCATE (TOD_set)
DEALLOCATE (TODNGHT_set)
DEALLOCATE (TMD_set)
DEALLOCATE (SPSP_set)
DEALLOCATE (MOPP_set)
DEALLOCATE (PPSE_set)
DEALLOCATE (TSEN_set)
DEALLOCATE (TSENNGHT_set)
DEALLOCATE (TSENPSP_set)
DEALLOCATE (TSENPSPNGHT_set)
DEALLOCATE (SHCKD_set)

END SUBROUTINE

!-----SUBROUTINE TO CALCULATE SLOPE, CV, R
SUBROUTINE SlopeCVR(X,M,N,SLOPE, R, SE)
DIMENSION X(N,M),Y(N),R(N-1),SLOPE(N-1),INTERCEPT(N-1),SE(N-1),YY(N),A(N-1),B(N)
REAL X,Y,R,SLOPE,INTERCEPT,SE,YY,A,B, C
  DO I=1,N
   YY(I)=0.0
      DO J=1,M
    YY(I)=YY(I)+X(I,J)
      ENDDO
    Y(I)=YY(I)/M   !Y(1)is average for X; Y(2) is average for Y
  ENDDO

  DO I=1,N-1
   A(I)=0.0
      DO J=1,M
         A(I)=A(I)+(X(I,J)-Y(I))*(X(N,J)-Y(N)) !LXY
      ENDDO  
  ENDDO
DO I=1,N
  B(I)=0.0
      DO J=1,M
  B(I)=B(I)+(X(I,J)-Y(I))**2 !LXX,LYY
      ENDDO
ENDDO

DO I=N,N
	C=0.0
	D=0.0
	DO J=1,M
		C=C+(X(I,J)-Y(I))**2  !LYY
		D=D+X(I,J) 
	END DO
		D=D/M                 !AVERAGE OF Y
END DO

DO I=1,N-1
	R(I)=A(I)/SQRT(B(I)*B(N))
	SLOPE(I)=A(I)/B(I)
	INTERCEPT(I)=Y(N)-SLOPE(I)*Y(I)
!	CV(I)=SQRT(C/(M-1))/D*100
	SE(I)=SQRT(C/(M-1))
ENDDO

RETURN
END SUBROUTINE

!-----SUBROUTINE TO CALCULATE AVERAGE
SUBROUTINE AVE(X,M,AVE_SUB)
INTEGER M
REAL X(M)
REAL AVE_SUB


REAL SUM_SUB

SUM_SUB=0.0

DO I=1,M
	SUM_SUB=SUM_SUB+X(I)
END DO

AVE_SUB=SUM_SUB/FLOAT(M)

RETURN
END SUBROUTINE

END MODULE



  