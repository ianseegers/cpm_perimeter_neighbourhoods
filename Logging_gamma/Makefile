all: plots/Final_plot_gamma.png 

plots:
	mkdir -p $@

data:
	mkdir -p $@ 

#The .js file used below already contains five different seeds!

data/gammalog_nomv_plot.txt: log_gamma_effective.js | data/
	node $< -p 280 -v 500 -m 10 -a 10 > $@

data/gammalog_amoeb_plot.txt: log_gamma_effective.js | data/
	node $< -p 280 -v 500 -m 40 -a 100 > $@

data/gammalog_intermediate_plot.txt: log_gamma_effective.js | data/
	node $< -p 280 -v 500 -m 60 -a 200 > $@

data/gammalog_kera_plot.txt: log_gamma_effective.js | data/
	node $< -p 280 -v 500 -m 100 -a 300 > $@

plots/Final_plot_gamma.png : Plot_test_gamma.R | data/gammalog_nomv_plot.txt \
data/gammalog_amoeb_plot.txt \
data/gammalog_intermediate_plot.txt \
data/gammalog_kera_plot.txt plots/
	Rscript $< 
