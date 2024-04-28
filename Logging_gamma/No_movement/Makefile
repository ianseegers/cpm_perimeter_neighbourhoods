all: plots/Final_plot_gamma_no_mv_sizes_selection.png

plots:
	mkdir -p $@

data:
	mkdir -p $@ 

data/gammalog_v350_no_mv1.txt: log_gamma_effective_no_mv.js | data/
	node $< -p 280 -v 350 -s 1 > $@

data/gammalog_v350_no_mv2.txt: log_gamma_effective_no_mv.js | data/
	node $< -p 280 -v 350 -s 2 > $@

data/gammalog_v350_no_mv3.txt: log_gamma_effective_no_mv.js | data/
	node $< -p 280 -v 350 -s 3 > $@

data/gammalog_v350_no_mv4.txt: log_gamma_effective_no_mv.js | data/
	node $< -p 280 -v 350 -s 4 > $@

data/gammalog_v350_no_mv5.txt: log_gamma_effective_no_mv.js | data/
	node $< -p 280 -v 350 -s 5 > $@

runs=1 2 3 4 5

data/gammalog_v350_no_mv.txt : $(foreach c, $(runs), data/gammalog_v350_no_mv$(c).txt) | data/gammalog_v350_no_mv5.txt
	cat $^ > $@

data/gammalog_v500_no_mv1.txt: log_gamma_effective_no_mv.js | data/
	node $< -p 280 -v 500 -s 1 > $@

data/gammalog_v500_no_mv2.txt: log_gamma_effective_no_mv.js | data/
	node $< -p 280 -v 500 -s 2 > $@

data/gammalog_v500_no_mv3.txt: log_gamma_effective_no_mv.js | data/
	node $< -p 280 -v 500 -s 3 > $@

data/gammalog_v500_no_mv4.txt: log_gamma_effective_no_mv.js | data/
	node $< -p 280 -v 500 -s 4 > $@

data/gammalog_v500_no_mv5.txt: log_gamma_effective_no_mv.js | data/
	node $< -p 280 -v 500 -s 5 > $@

runs=1 2 3 4 5

data/gammalog_v500_no_mv.txt : $(foreach c, $(runs), data/gammalog_v500_no_mv$(c).txt) | data/gammalog_v500_no_mv5.txt
	cat $^ > $@

data/gammalog_v650_no_mv1.txt: log_gamma_effective_no_mv.js | data/
	node $< -p 280 -v 650 -s 1 > $@

data/gammalog_v650_no_mv2.txt: log_gamma_effective_no_mv.js | data/
	node $< -p 280 -v 650 -s 2 > $@

data/gammalog_v650_no_mv3.txt: log_gamma_effective_no_mv.js | data/
	node $< -p 280 -v 650 -s 3 > $@

data/gammalog_v650_no_mv4.txt: log_gamma_effective_no_mv.js | data/
	node $< -p 280 -v 650 -s 4 > $@

data/gammalog_v650_no_mv5.txt: log_gamma_effective_no_mv.js | data/
	node $< -p 280 -v 650 -s 5 > $@

runs=1 2 3 4 5

data/gammalog_v650_no_mv.txt : $(foreach c, $(runs), data/gammalog_v650_no_mv$(c).txt) | data/gammalog_v650_no_mv5.txt
	cat $^ > $@

data/gammalog_v800_no_mv1.txt: log_gamma_effective_no_mv.js | data/
	node $< -p 280 -v 800 -s 1 > $@

data/gammalog_v800_no_mv2.txt: log_gamma_effective_no_mv.js | data/
	node $< -p 280 -v 800 -s 2 > $@

data/gammalog_v800_no_mv3.txt: log_gamma_effective_no_mv.js | data/
	node $< -p 280 -v 800 -s 3 > $@

data/gammalog_v800_no_mv4.txt: log_gamma_effective_no_mv.js | data/
	node $< -p 280 -v 800 -s 4 > $@

data/gammalog_v800_no_mv5.txt: log_gamma_effective_no_mv.js | data/
	node $< -p 280 -v 800 -s 5 > $@

runs=1 2 3 4 5

data/gammalog_v800_no_mv.txt : $(foreach c, $(runs), data/gammalog_v800_no_mv$(c).txt) | data/gammalog_v800_no_mv5.txt
	cat $^ > $@

plots/Final_plot_gamma_no_mv_sizes_selection.png : Plot_test_gamma_no_mv_selection.R | data/gammalog_v350_no_mv.txt \
data/gammalog_v500_no_mv.txt \
data/gammalog_v650_no_mv.txt \
data/gammalog_v800_no_mv.txt plots/
	Rscript $<