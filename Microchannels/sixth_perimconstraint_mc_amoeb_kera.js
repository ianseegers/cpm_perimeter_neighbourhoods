var argv = require('minimist')(process.argv.slice(2));
let perim = parseInt(argv.p)

let CPM = require("../../artistoo/build/artistoo-cjs.js")

/*	----------------------------------
	CONFIGURATION SETTINGS
	----------------------------------
*/

//				 		| ttt |
//		   | ttll | ttl | ttm | ttr | ttrr |
// 		   | tll  | tl	| tm  | tr  | trr  |
// 	 | lll | ll   | l   | III | r   | rr   | rrr |
//         | bll  | bl  | bm  | br  | brr  |
// 	       | bbll | bbl	| bbm | bbr | bbrr |
// 				        | bbbm|

class PerimeterConstraint2 extends CPM.PerimeterConstraint{

neighi2( i, torus = this.C.conf.torus ){	
	// normal computation of neighbor indices in the 6th order neighbourhood
	
	let xlim = this.C.extents[0]
	let ylim = this.C.extents[1]
	let xstep = this.C.grid.X_STEP

	let tttm, 
	ttll, ttl, ttm, ttr, ttrr, 
	tll, tl, tm, tr, trr, 
	lll, ll, l, r, rr, rrr, 
	bll, bl, bm, br, brr, 
	bbll, bbl, bbm, bbr, bbrr, 
	bbbm;
	
	tttm = i - 3;
	ttll = i - 2 - (2 * xstep); ttl = i - 2 - xstep; ttm = i - 2; ttr = i - 2 + xstep; ttrr = i - 2 + (2 * xstep);
	tll = i - 1 - (2 * xstep); tl = i - 1 - xstep; tm = i - 1; tr = i - 1 + xstep; trr = i - 1 + (2 * xstep);
	lll = i - (3 * xstep); ll = i - (2 * xstep); l = i - xstep; r = i + xstep; rr = i + (2 * xstep); rrr = i + (3 * xstep);
	bll = i + 1 - (2 * xstep); bl = i + 1 - xstep; bm = i + 1; br = i + 1 + xstep; brr = i + 1 + (2 * xstep);
	bbll = i + 2 - (2 * xstep); bbl = i + 2 - xstep; bbm = i + 2; bbr = i + 2 + xstep; bbrr = i + 2 + (2 * xstep);
	bbbm = i + 3
	
	// if pixel is part of one of the borders, adjust the 
	// indices accordingly
	let add = NaN; // if torus is false, return NaN for all neighbors that cross
	// the border.
	
	// left border
	if( (i - (2 * xstep)) <= xstep && (i - xstep) >= xstep ){
		if( torus[0] ){
			add = xlim * xstep;
		}
		lll += add;
	}
	
	if( (i - xstep) < xstep && i >= xstep ){
		if( torus[0] ){
			add = xlim * xstep;
		}
		ttll += add; tll += add; lll += add; ll += add; bll += add; bbll += add;
	}

	if( i < xstep ){
		if( torus[0] ){
			add =  xlim * xstep;
		}
		ttll += add; ttl += add; tll += add; tl += add; lll += add; ll += add; l += add; bll += add; bl += add; bbll += add; bbl += add;
	}

	// right border
	if( (i + (2 * xstep)) > ((xlim - 1) * xstep - 1) && (i + xstep) < ((xlim - 1) * xstep - 1) ){
		if( torus[0] ){
			add = -(xlim * xstep);
		}
		rrr += add;
	}

	if( (i + xstep) > ((xlim - 1) * xstep - 1) && i < ((xlim - 1) * xstep - 1) ){
		if( torus[0] ){
			add = -(xlim * xstep);
		}
		ttrr += add; trr += add; rrr += add; rr += add; brr += add; bbrr += add;
	}

	if( i > ((xlim - 1) * xstep - 1) && (i - xstep) < ((xlim - 1) * xstep - 1) ){
		if( torus[0] ){
			add = -(xlim * xstep);
		}
		ttrr += add; ttr += add; trr += add; tr += add; rrr += add; rr += add; r += add; brr += add; br += add; bbrr += add; bbr += add;
	}

	add = NaN;
	// top border
	if( (i - 2) % xstep == 0 ){
		if( torus[1] ){
			add = ylim;
		}
		tttm += add;
	}

	if( (i - 1) % xstep == 0 ){
		if( torus[1] ){
			add = ylim;
		}
		tttm += add; ttll += add; ttl += add; ttm += add; ttr += add; ttrr += add;
	}

	if( i % xstep == 0 ){
		if( torus[1] ){
			add = ylim;
		}
		tttm += add; ttll += add; ttl += add; ttm += add; ttr += add; ttrr += add; tll += add; tl += add; tm += add; tr += add; trr += add; 	
	}
	
	// bottom border
	if( (i + 2 - (ylim - 1)) % xstep == 0 ){
		if( torus[1] ){
			add = -ylim;
		}
		bbbm += add; 
	}

	if( (i + 1 - (ylim - 1)) % xstep == 0 ){
		if( torus[1] ){
			add = -ylim;
		}
		bbll += add; bbl += add; bbm += add; bbr += add; bbrr += add; bbbm += add; 
	}

	if( (i - (ylim - 1)) % xstep == 0 ){
		if( torus[1] ){
			add = -ylim;
		}
		bll += add; bl += add; bm += add; br += add; brr += add; bbll += add; bbl += add; bbm += add; bbr += add; bbrr += add; bbbm += add; 
	}

	if( !(torus[0]&&torus[1]) ){
		return [ tttm, ttll, ttl, ttm, ttr, ttrr, tll, tl, tm, tr, trr, lll, ll, l, r, rr, rrr, bll, bl, bm, br, brr, bbll, bbl, bbm, bbr, bbrr, bbbm ].filter( isFinite )
	} else {
		return [ tttm, ttll, ttl, ttm, ttr, ttrr, tll, tl, tm, tr, trr, lll, ll, l, r, rr, rrr, bll, bl, bm, br, brr, bbll, bbl, bbm, bbr, bbrr, bbbm ]
	}
}


deltaH( sourcei, targeti, src_type, tgt_type ){

	if( src_type === tgt_type ){
		return 0
	}
	const ls = this.cellParameter("LAMBDA_P2", src_type);
	const lt = this.cellParameter("LAMBDA_P2", tgt_type);
	if( !(ls>0) && !(lt>0) ){
		return 0
	}
	const Ni = this.neighi2( targeti );
	let pchange = {};
	pchange[src_type] = 0; pchange[tgt_type] = 0;
	for( let i = 0 ; i < Ni.length ; i ++  ){
		const nt = this.C.pixti(Ni[i]);
		if( nt !== src_type ){
			pchange[src_type] = pchange[src_type] + (1/1); 
		}
		if( nt !== tgt_type ){
			pchange[tgt_type] = pchange[tgt_type] - (1/1);
		}
		if( nt === tgt_type ){
			pchange[nt] = pchange[nt] + (1/1);
		}
		if( nt === src_type ){
			pchange[nt] = pchange[nt] - (1/1);
		}
	}
	let r = 0.0;
	if( ls > 0 ){
		const pt = this.cellParameter("P2", src_type),
			ps = this.cellperimeters[src_type];
		const hnew = ((ps+pchange[src_type])-pt),
			hold = (ps-pt);
		r += ls*((hnew*hnew)-(hold*hold));
	}
	if( lt > 0 ){
		const pt = this.cellParameter("P2", tgt_type),
			ps = this.cellperimeters[tgt_type];
		const hnew = ((ps+pchange[tgt_type])-pt),
			hold = (ps-pt);
		r += lt*((hnew*hnew)-(hold*hold));
	}
	// eslint-disable-next-line
	//console.log( r )
	return r
}

postSetpixListener( i, t_old, t_new ){
	if( t_old === t_new ){ return }

	// Neighborhood of the pixel that changes
	const Ni = this.neighi2( i )
	
	// Keep track of perimeter before and after copy
	let n_new = 0, n_old = 0
	
	// Loop over the neighborhood. 
	for( let i = 0 ; i < Ni.length ; i ++  ){
		const nt = this.C.pixti(Ni[i])
		
		// neighbors are added to the perimeter if they are
		// of a different cellID than the current pixel
		if( nt !== t_new ){
			n_new = n_new + (1/1) 
		}
		if( nt !== t_old ){
			n_old = n_old + (1/1)
		}
		
		// if the neighbor is non-background, the perimeter
		// of the cell it belongs to may also have to be updated.
		if( nt !== 0 ){
		
			// if it was of t_old, its perimeter goes up because the
			// current pixel will no longer be t_old. This means it will
			// have a different type and start counting as perimeter.
			if( nt === t_old ){
				this.cellperimeters[nt] = this.cellperimeters[nt] + (1/1)
			}
			// opposite if it is t_new.
			if( nt === t_new ){
				this.cellperimeters[nt] = this.cellperimeters[nt] - (1/1)
			}
		}
	}
	
	// update perimeters of the old and new type if they are non-background
	if( t_old !== 0 ){
		this.cellperimeters[t_old] -= n_old
	}
	if( t_new !== 0 ){
		if( !(t_new in this.cellperimeters) ){
			this.cellperimeters[t_new] = 0
		}
		this.cellperimeters[t_new] += n_new
	}
}

 get perimeterNeighbours(){
this._neighbours2 = new Uint16Array(this.grid.p2i(field_size))

for( let pixel of this.C.grid.pixels()){
	const p = pixel[0]
	const pt = pixel[1]
	const i = this.C.grid.p2i(p)
	const Ni = this.neighi2(i)

	for( let a = 0 ; a < Ni.length ; a ++  ){
		const nt = this.C.pixti(Ni[a]);
		if( nt !== pt ){
			this._neighbours2[i] = this._neighbours2[i] + (1/1); 
			}
		}
}

 return this._neighbours2

 }

initializePerimeters(){
	for( let bp of this.C.cellBorderPixels() ){
		const p = bp[0];
		let cid = this.C.pixt(p);
		if( !(cid in this.cellperimeters) ){
		this.cellperimeters[cid] = 0;
	}
		const i = this.C.grid.p2i( p );
		this.cellperimeters[cid] += this.perimeterNeighbours[i];
	}

}

}

let conf = {
	torus : [true,false],			// wrap around in x-direction only
	seed : 1,						
	T : 20,							// standard value.
	
	// Adhesion: 
	J : [ [0,20], [20,100] ],
	
	// VolumeConstraint parameters
	LAMBDA_V : [0,30],				// VolumeConstraint importance per cellkind
	V : [0,500],					// Target volume of each cellkind
	
	// PerimeterConstraint parameters
	LAMBDA_P : [0,2],				// PerimeterConstraint importance per cellkind
	P : [0,perim],					// Target perimeter of each cellkind
	LAMBDA_P2 : [0,0], 				// Importance of perimeter constraint
	P2 : [0,perim], 
	
	LAMBDA_ACT : [0,250],
	MAX_ACT:[0,40],
	ACT_MEAN : "geometric"
}

let conf2 = {
	torus : [true,false],			// wrap around in x-direction only
	seed : 2,						
	T : 20,							// standard value.
	
	// Adhesion: 
	J : [ [0,20], [20,100] ],
	
	// VolumeConstraint parameters
	LAMBDA_V : [0,30],				// VolumeConstraint importance per cellkind
	V : [0,500],					// Target volume of each cellkind
	
	// PerimeterConstraint parameters
	LAMBDA_P : [0,2],				// PerimeterConstraint importance per cellkind
	P : [0,perim],					// Target perimeter of each cellkind
	LAMBDA_P2 : [0,0], 				// Importance of perimeter constraint
	P2 : [0,perim], 
	
	LAMBDA_ACT : [0,250],
	MAX_ACT:[0,40],
	ACT_MEAN : "geometric"
}

let conf3 = {
	torus : [true,false],			// wrap around in x-direction only
	seed : 3,						
	T : 20,							// standard value.
	
	// Adhesion: 
	J : [ [0,20], [20,100] ],
	
	// VolumeConstraint parameters
	LAMBDA_V : [0,30],				// VolumeConstraint importance per cellkind
	V : [0,500],					// Target volume of each cellkind
	
	// PerimeterConstraint parameters
	LAMBDA_P : [0,2],				// PerimeterConstraint importance per cellkind
	P : [0,perim],					// Target perimeter of each cellkind
	LAMBDA_P2 : [0,0], 				// Importance of perimeter constraint
	P2 : [0,perim], 
	
	LAMBDA_ACT : [0,250],
	MAX_ACT:[0,40],
	ACT_MEAN : "geometric"
}

let conf4 = {
	torus : [true,false],			// wrap around in x-direction only
	seed : 4,						
	T : 20,							// standard value.
	
	// Adhesion: 
	J : [ [0,20], [20,100] ],
	
	// VolumeConstraint parameters
	LAMBDA_V : [0,30],				// VolumeConstraint importance per cellkind
	V : [0,500],					// Target volume of each cellkind
	
	// PerimeterConstraint parameters
	LAMBDA_P : [0,2],				// PerimeterConstraint importance per cellkind
	P : [0,perim],					// Target perimeter of each cellkind
	LAMBDA_P2 : [0,0], 				// Importance of perimeter constraint
	P2 : [0,perim], 
	
	LAMBDA_ACT : [0,250],
	MAX_ACT:[0,40],
	ACT_MEAN : "geometric"
}

let conf5 = {
	torus : [true,false],			// wrap around in x-direction only
	seed : 5,						
	T : 20,							// standard value.
	
	// Adhesion: 
	J : [ [0,20], [20,100] ],
	
	// VolumeConstraint parameters
	LAMBDA_V : [0,30],				// VolumeConstraint importance per cellkind
	V : [0,500],					// Target volume of each cellkind
	
	// PerimeterConstraint parameters
	LAMBDA_P : [0,2],				// PerimeterConstraint importance per cellkind
	P : [0,perim],					// Target perimeter of each cellkind
	LAMBDA_P2 : [0,0], 				// Importance of perimeter constraint
	P2 : [0,perim], 
	
	LAMBDA_ACT : [0,250],
	MAX_ACT:[0,40],
	ACT_MEAN : "geometric"
}

/*	---------------------------------- */
let C, gm, C2, gm2, C3, gm3, C4, gm4, C5, gm5

function setup(fs){
	C = new CPM.CPM( fs, conf)
	C.add( new PerimeterConstraint2(conf))
	gm = new CPM.GridManipulator(C)
	C2 = new CPM.CPM( fs, conf2)
	C2.add( new PerimeterConstraint2(conf2))
	gm2 = new CPM.GridManipulator(C2)
	C3 = new CPM.CPM( fs, conf3)
	C3.add( new PerimeterConstraint2(conf3))
	gm3 = new CPM.GridManipulator(C3)
	C4 = new CPM.CPM( fs, conf4)
	C4.add( new PerimeterConstraint2(conf4))
	gm4 = new CPM.GridManipulator(C4)
	C5 = new CPM.CPM( fs, conf5)
	C5.add( new PerimeterConstraint2(conf5))
	gm5 = new CPM.GridManipulator(C5)
}

function initialize(){
	setup([250,12])
	buildChannel(C)
	buildChannel(C2)
	buildChannel(C3)
	buildChannel(C4)
	buildChannel(C5)
	gm.seedCellAt(1, C.midpoint)
	gm2.seedCellAt(1, C2.midpoint)
	gm3.seedCellAt(1, C3.midpoint)
	gm4.seedCellAt(1, C4.midpoint)
	gm5.seedCellAt(1, C5.midpoint)
	step()
}

function buildChannel(C){
	C.channelvoxels = gm.makePlane( [], 1, 0 )
	let gridheight = C.extents[1]
	C.channelvoxels = gm.makePlane( C.channelvoxels, 1, gridheight - 1 )
	C.add( new CPM.BorderConstraint({
		BARRIER_VOXELS : C.channelvoxels
	}) )	
}

function step(){
	C.timeStep()
	C2.timeStep()
	C3.timeStep()
	C4.timeStep()
	C5.timeStep()
	if(C.time > 500){
		console.log(C.time + "\t" + C.getConstraint("PerimeterConstraint").cellperimeters[1]/C.getConstraint("PerimeterConstraint2").cellperimeters[1])
		console.log(C2.time + "\t" + C2.getConstraint("PerimeterConstraint").cellperimeters[1]/C2.getConstraint("PerimeterConstraint2").cellperimeters[1])
		console.log(C3.time + "\t" + C3.getConstraint("PerimeterConstraint").cellperimeters[1]/C3.getConstraint("PerimeterConstraint2").cellperimeters[1])
		console.log(C4.time + "\t" + C4.getConstraint("PerimeterConstraint").cellperimeters[1]/C4.getConstraint("PerimeterConstraint2").cellperimeters[1])
		console.log(C5.time + "\t" + C5.getConstraint("PerimeterConstraint").cellperimeters[1]/C5.getConstraint("PerimeterConstraint2").cellperimeters[1])
	}
}

initialize()
while(C.time < 10000){
	step()
}

