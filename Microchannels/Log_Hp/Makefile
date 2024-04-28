all: plots/Plot_deltaHP_mc_selection.png

plots:
	mkdir -p $@

data:
	mkdir -p $@ 

data/hplog_p280_plot_moore1.txt: standard_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 280 -s 1 > $@

data/hplog_p280_plot_moore2.txt: standard_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 280 -s 2 > $@

data/hplog_p280_plot_moore3.txt: standard_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 280 -s 3 > $@

data/hplog_p280_plot_moore4.txt: standard_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 280 -s 4 > $@

data/hplog_p280_plot_moore5.txt: standard_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 280 -s 5 > $@

data/hplog_p300_plot_moore1.txt: standard_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 300 -s 1 > $@

data/hplog_p300_plot_moore2.txt: standard_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 300 -s 2 > $@

data/hplog_p300_plot_moore3.txt: standard_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 300 -s 3 > $@

data/hplog_p300_plot_moore4.txt: standard_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 300 -s 4 > $@

data/hplog_p300_plot_moore5.txt: standard_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 300 -s 5 > $@

data/hplog_p320_plot_moore1.txt: standard_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 320 -s 1 > $@

data/hplog_p320_plot_moore2.txt: standard_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 320 -s 2 > $@

data/hplog_p320_plot_moore3.txt: standard_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 320 -s 3 > $@

data/hplog_p320_plot_moore4.txt: standard_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 320 -s 4 > $@

data/hplog_p320_plot_moore5.txt: standard_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 320 -s 5 > $@

data/hplog_p340_plot_moore1.txt: standard_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 340 -s 1 > $@

data/hplog_p340_plot_moore2.txt: standard_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 340 -s 2 > $@

data/hplog_p340_plot_moore3.txt: standard_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 340 -s 3 > $@

data/hplog_p340_plot_moore4.txt: standard_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 340 -s 4 > $@

data/hplog_p340_plot_moore5.txt: standard_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 340 -s 5 > $@

data/hplog_p280_plot_moore.txt: data/hplog_p280_plot_moore1.txt data/hplog_p280_plot_moore2.txt data/hplog_p280_plot_moore3.txt \
data/hplog_p280_plot_moore4.txt data/hplog_p280_plot_moore5.txt
	cat $? > $@

data/hplog_p300_plot_moore.txt: data/hplog_p300_plot_moore1.txt data/hplog_p300_plot_moore2.txt data/hplog_p300_plot_moore3.txt \
data/hplog_p300_plot_moore4.txt data/hplog_p300_plot_moore5.txt
	cat $? > $@

data/hplog_p320_plot_moore.txt: data/hplog_p320_plot_moore1.txt data/hplog_p320_plot_moore2.txt data/hplog_p320_plot_moore3.txt \
data/hplog_p320_plot_moore4.txt data/hplog_p320_plot_moore5.txt
	cat $? > $@

data/hplog_p340_plot_moore.txt: data/hplog_p340_plot_moore1.txt data/hplog_p340_plot_moore2.txt data/hplog_p340_plot_moore3.txt \
data/hplog_p340_plot_moore4.txt data/hplog_p340_plot_moore5.txt
	cat $? > $@

data/hplog_p280_plot_sixth_theory1.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 280 -s 1 -x 0.166666666666666666667 > $@

data/hplog_p280_plot_sixth_theory2.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 280 -s 2 -x 0.166666666666666666667 > $@

data/hplog_p280_plot_sixth_theory3.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 280 -s 3 -x 0.166666666666666666667 > $@

data/hplog_p280_plot_sixth_theory4.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 280 -s 4 -x 0.166666666666666666667 > $@

data/hplog_p280_plot_sixth_theory5.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 300 -s 5 -x 0.166666666666666666667 > $@

data/hplog_p300_plot_sixth_theory1.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 300 -s 1 -x 0.166666666666666666667 > $@

data/hplog_p300_plot_sixth_theory2.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 300 -s 2 -x 0.166666666666666666667 > $@

data/hplog_p300_plot_sixth_theory3.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 300 -s 3 -x 0.166666666666666666667 > $@

data/hplog_p300_plot_sixth_theory4.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 300 -s 4 -x 0.166666666666666666667 > $@

data/hplog_p300_plot_sixth_theory5.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 300 -s 5 -x 0.166666666666666666667 > $@

data/hplog_p320_plot_sixth_theory1.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 320 -s 1 -x 0.166666666666666666667 > $@

data/hplog_p320_plot_sixth_theory2.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 320 -s 2 -x 0.166666666666666666667 > $@

data/hplog_p320_plot_sixth_theory3.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 320 -s 3 -x 0.166666666666666666667 > $@

data/hplog_p320_plot_sixth_theory4.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 320 -s 4 -x 0.166666666666666666667 > $@

data/hplog_p320_plot_sixth_theory5.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 320 -s 5 -x 0.166666666666666666667 > $@

data/hplog_p340_plot_sixth_theory1.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 340 -s 1 -x 0.166666666666666666667 > $@

data/hplog_p340_plot_sixth_theory2.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 340 -s 2 -x 0.166666666666666666667 > $@

data/hplog_p340_plot_sixth_theory3.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 340 -s 3 -x 0.166666666666666666667 > $@

data/hplog_p340_plot_sixth_theory4.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 340 -s 4 -x 0.166666666666666666667 > $@

data/hplog_p340_plot_sixth_theory5.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 340 -s 5 -x 0.166666666666666666667 > $@

data/hplog_p280_plot_sixth_theory.txt: data/hplog_p280_plot_sixth_theory1.txt data/hplog_p280_plot_sixth_theory2.txt data/hplog_p280_plot_sixth_theory3.txt \
data/hplog_p280_plot_sixth_theory4.txt data/hplog_p280_plot_sixth_theory5.txt
	cat $? > $@

data/hplog_p300_plot_sixth_theory.txt: data/hplog_p300_plot_sixth_theory1.txt data/hplog_p300_plot_sixth_theory2.txt data/hplog_p300_plot_sixth_theory3.txt \
data/hplog_p300_plot_sixth_theory4.txt data/hplog_p300_plot_sixth_theory5.txt
	cat $? > $@

data/hplog_p320_plot_sixth_theory.txt: data/hplog_p320_plot_sixth_theory1.txt data/hplog_p320_plot_sixth_theory2.txt data/hplog_p320_plot_sixth_theory3.txt \
data/hplog_p320_plot_sixth_theory4.txt data/hplog_p320_plot_sixth_theory5.txt
	cat $? > $@

data/hplog_p340_plot_sixth_theory.txt: data/hplog_p340_plot_sixth_theory1.txt data/hplog_p340_plot_sixth_theory2.txt data/hplog_p340_plot_sixth_theory3.txt \
data/hplog_p340_plot_sixth_theory4.txt data/hplog_p340_plot_sixth_theory5.txt
	cat $? > $@

data/hplog_p280_plot_sixth_measured1.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 280 -s 1 -x 0.2391752577 > $@

data/hplog_p280_plot_sixth_measured2.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 280 -s 2 -x 0.2391752577 > $@

data/hplog_p280_plot_sixth_measured3.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 280 -s 3 -x 0.2391752577 > $@

data/hplog_p280_plot_sixth_measured4.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 280 -s 4 -x 0.2391752577 > $@

data/hplog_p280_plot_sixth_measured5.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 280 -s 5 -x 0.2391752577 > $@

data/hplog_p300_plot_sixth_measured1.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 300 -s 1 -x 0.2404447533 > $@

data/hplog_p300_plot_sixth_measured2.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 300 -s 2 -x 0.2404447533 > $@

data/hplog_p300_plot_sixth_measured3.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 300 -s 3 -x 0.2404447533 > $@

data/hplog_p300_plot_sixth_measured4.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 300 -s 4 -x 0.2404447533 > $@

data/hplog_p300_plot_sixth_measured5.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 300 -s 5 -x 0.2404447533 > $@

data/hplog_p320_plot_sixth_measured1.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 320 -s 1 -x 0.2404447533 > $@

data/hplog_p320_plot_sixth_measured2.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 320 -s 2 -x 0.2404447533 > $@

data/hplog_p320_plot_sixth_measured3.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 320 -s 3 -x 0.2404447533 > $@

data/hplog_p320_plot_sixth_measured4.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 320 -s 4 -x 0.2404447533 > $@

data/hplog_p320_plot_sixth_measured5.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 320 -s 5 -x 0.2404447533 > $@

data/hplog_p340_plot_sixth_measured1.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 340 -s 1 -x 0.2396694215 > $@

data/hplog_p340_plot_sixth_measured2.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 340 -s 2 -x 0.2396694215 > $@

data/hplog_p340_plot_sixth_measured3.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 340 -s 3 -x 0.2396694215 > $@

data/hplog_p340_plot_sixth_measured4.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 340 -s 4 -x 0.2396694215 > $@

data/hplog_p340_plot_sixth_measured5.txt: sixth_perimconstraint_mc_amoeb_kera_log_hp.js | data/
	node $< -p 340 -s 5 -x 0.2396694215 > $@

data/hplog_p280_plot_sixth_measured.txt: data/hplog_p280_plot_sixth_measured1.txt data/hplog_p280_plot_sixth_measured2.txt data/hplog_p280_plot_sixth_measured3.txt \
data/hplog_p280_plot_sixth_measured4.txt data/hplog_p280_plot_sixth_measured5.txt
	cat $? > $@

data/hplog_p300_plot_sixth_measured.txt: data/hplog_p300_plot_sixth_measured1.txt data/hplog_p300_plot_sixth_measured2.txt data/hplog_p300_plot_sixth_measured3.txt \
data/hplog_p300_plot_sixth_measured4.txt data/hplog_p300_plot_sixth_measured5.txt
	cat $? > $@

data/hplog_p320_plot_sixth_measured.txt: data/hplog_p320_plot_sixth_measured1.txt data/hplog_p320_plot_sixth_measured2.txt data/hplog_p320_plot_sixth_measured3.txt \
data/hplog_p320_plot_sixth_measured4.txt data/hplog_p320_plot_sixth_measured5.txt
	cat $? > $@

data/hplog_p340_plot_sixth_measured.txt: data/hplog_p340_plot_sixth_measured1.txt data/hplog_p340_plot_sixth_measured2.txt data/hplog_p340_plot_sixth_measured3.txt \
data/hplog_p340_plot_sixth_measured4.txt data/hplog_p340_plot_sixth_measured5.txt
	cat $? > $@

plots/Plot_deltaHP_mc_selection.png: Plot_Hp_mc.R | data/hplog_p280_plot_moore.txt data/hplog_p300_plot_moore.txt \
data/hplog_p320_plot_moore.txt data/hplog_p340_plot_moore.txt \
data/hplog_p280_plot_sixth_theory.txt data/hplog_p300_plot_sixth_theory.txt \
data/hplog_p320_plot_sixth_theory.txt data/hplog_p340_plot_sixth_theory.txt \
data/hplog_p280_plot_sixth_measured.txt data/hplog_p300_plot_sixth_measured.txt \
data/hplog_p320_plot_sixth_measured.txt data/hplog_p340_plot_sixth_measured.txt plots/
	Rscript $<
