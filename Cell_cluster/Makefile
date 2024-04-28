all: data/res_one_ck.txt Plots_single_kind/Plots.png

Plots_single_kind:
	mkdir -p $@

data:
	mkdir -p $@ 

data/res1.txt: Cell_sorting_neighborhoods_one_cellkind.js | data/
	node $< -s 1 > $@

data/res2.txt: Cell_sorting_neighborhoods_one_cellkind.js | data/
	node $< -s 2 > $@

data/res3.txt: Cell_sorting_neighborhoods_one_cellkind.js | data/
	node $< -s 3 > $@

data/res4.txt: Cell_sorting_neighborhoods_one_cellkind.js | data/
	node $< -s 4 > $@

data/res5.txt: Cell_sorting_neighborhoods_one_cellkind.js | data/
	node $< -s 5 > $@

data/res6.txt: Cell_sorting_neighborhoods_one_cellkind.js | data/
	node $< -s 6 > $@

data/res7.txt: Cell_sorting_neighborhoods_one_cellkind.js | data/
	node $< -s 7 > $@

data/res8.txt: Cell_sorting_neighborhoods_one_cellkind.js | data/
	node $< -s 8 > $@

data/res9.txt: Cell_sorting_neighborhoods_one_cellkind.js | data/
	node $< -s 9 > $@

data/res10.txt: Cell_sorting_neighborhoods_one_cellkind.js | data/
	node $< -s 10 > $@

runs=1 2 3 4 5 6 7 8 9 10

data/res_one_ck.txt : $(foreach c, $(runs), data/res$(c).txt) | data/res10.txt
	cat $^ > $@

Plots_single_kind/Plots.png : Plot_connectedness_movement_blob.R data/res_one_ck.txt | Plots_single_kind/
	Rscript $< Plot_connectedness_movement_blob.R
