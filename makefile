FC=gfortran
subjects1=extract_seed_hour.o abstime.o decimate.o rm_resp.o avg.o sacio.o
subjects2=extract_seed_eq.o abstime1.o decimate.o rm_resp.o avg.o sacio.o
subjects3=extract_seed_sec.o abstime.o decimate.o rm_resp.o avg.o sacio.o usage.o
subjects8=extract_seed_sec_mars.o abstime.o decimate.o rm_resp.o avg.o sacio.o usage.o
subjects4=extract_seed_sec_month.o abstime.o decimate.o rm_resp.o avg.o sacio.o usage.o
subjects5=extract_seed_sec_specific_day.o abstime.o decimate.o rm_resp.o avg.o sacio.o usage.o
subjects6=get_daily_sac.o abstime.o decimate.o rm_resp.o avg.o sacio.o usage.o
subjects7=cut_any_sac.o location.o decimate.o rm_resp1.o avg.o sacio.o usage.o
subjects10=cut_any_sac_20210402.o location.o decimate.o rm_resp.o avg.o sacio.o usage.o
subjects11=cut_sac_20210402.o abstime.o decimate.o rm_resp.o avg.o sacio.o usage.o
subjects13=cut_sac_20210404.o abstime.o decimate.o rm_resp.o avg.o sacio.o usage.o
subjects9=extract_seed_eq_20210330.o abstime1.o decimate.o rm_resp.o avg.o sacio.o
subjects12=extract_seed_eq_20210402.o abstime1.o decimate.o rm_resp.o avg.o sacio.o
subjects15=extract_seed_eq_20210405.o abstime1.o decimate.o rm_resp.o avg.o sacio.o
subjects14=cut_sac_for_missing_resp_20210405.o abstime.o decimate.o rm_resp.o avg.o sacio.o usage.o
all:sacio.mod ../bin/extract_seed_eq ../bin/extract_seed_sec ../bin/extract_seed_sec_month ../bin/extract_seed_sec_specific_day ../bin/get_daily_sac ../bin/cut_any_sac ../bin/extract_seed_sec_mars ../bin/extract_seed_eq_20210330 ../bin/cut_any_sac_20210402 ../bin/cut_sac_20210402 ../bin/extract_seed_eq_20210402 ../bin/cut_sac_20210404 ../bin/cut_sac_for_missing_resp_20210405 ../bin/extract_seed_eq_20210405
sacio.mod:sacio.f90
	$(FC) -c $^
../bin/extract_seed_hour:$(subjects1)
	$(FC) $^ -o $@ 
../bin/extract_seed_eq:$(subjects2)
	$(FC) $^ -o $@ 
../bin/extract_seed_sec:$(subjects3)
	$(FC) $^ -o $@ -fbounds-check
../bin/extract_seed_sec_mars:$(subjects8)
	$(FC) $^ -o $@ -fbounds-check
../bin/extract_seed_sec_month:$(subjects4)
	$(FC) $^ -o $@ -fbounds-check
../bin/extract_seed_sec_specific_day:$(subjects5)
	$(FC) $^ -o $@ -fbounds-check
../bin/get_daily_sac:$(subjects6)
	$(FC) $^ -o $@ -fbounds-check
../bin/cut_any_sac:$(subjects7)
	$(FC) $^ -o $@ -fbounds-check
../bin/extract_seed_eq_20210330:$(subjects9)
	$(FC) $^ -o $@ 
../bin/cut_any_sac_20210402:$(subjects10)
	$(FC) $^ -o $@ -fbounds-check
../bin/cut_sac_20210402:$(subjects11)
	$(FC) $^ -o $@ -fbounds-check
../bin/extract_seed_eq_20210402:$(subjects12)
	$(FC) $^ -o $@ 
../bin/cut_sac_20210404:$(subjects13)
	$(FC) $^ -o $@ -fbounds-check
../bin/cut_sac_for_missing_resp_20210405:$(subjects14)
	$(FC) $^ -o $@ -fbounds-check
../bin/extract_seed_eq_20210405:$(subjects15)
	$(FC) $^ -o $@ 
%.o:%.f90
	$(FC) $^ -c
clean:
	rm *.o *.mod
