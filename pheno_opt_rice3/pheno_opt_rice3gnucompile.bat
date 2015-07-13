REM Compile pheno_opt_rice3 subroutines

REM Note: important that files compiled in this order!
gfortran Common_opt3.f90 -c -O3
gfortran SUBDD3.f90 -c -O3
gfortran PTUSUM3.f90 -c -O3
gfortran ReadWeather3.f90 -c -O3
gfortran Rerun_opt.f90 -c -O3
gfortran start_opt3.f90 -c -O3
gfortran stats_opt.f90 -c -O3

REM Compile and link main program
echo "Compiling and linking main program."
gfortran -O3 -o pheno_opt_rice3.exe phe_main_opt.f90 *.o
del *.o
del *.mod
move pheno_opt_rice3.exe gnucompilation
REM Before running pheno_opt_rice3.exe you have of course thought about
REM (1) filling in the correct WeaPath in ReadWeather3.f90
REM (2) selecting the model(s) to be calibrated in phe_main_opt.f90
REM (3) setting parameter ranges in Rerun_opt.f90
REM !!! IF NOT THEN DO THIS AND RUN THIS BATCH FILE AGAIN !!!
REM Once done, don't forget:
REM (4) placing the pheno_in.txt file in correct format in directory \gnucompilation
pause
