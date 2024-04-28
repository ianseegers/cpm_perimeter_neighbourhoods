var argv = require('minimist')(process.argv.slice(2));
let perim = parseInt(argv.p)
let sd = parseInt(argv.s)

let CPM = require("../../../artistoo/build/artistoo-cjs.js")

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

class CPM2 extends CPM.CPM {
	deltaH ( sourcei, targeti, src_type, tgt_type ){
		let r = 0.0;
		for( let t of this.soft_constraints ){
			r += t.deltaH( sourcei, targeti, src_type, tgt_type );
		}
		if(src_type == 1 && this.time > 499){
			if(Math.random() < 0.05){
				console.log(this.time + "\t" +
					sourcei + "\t" + 
					targeti + "\t" + 
					"Total" + "\t" +
					r)
				console.log(this.time + "\t" + 
					sourcei + "\t" + 
					targeti + "\t" + 
					"Perim" + "\t" +
					this.getConstraint("PerimeterConstraint2").deltaH(sourcei, targeti, src_type, tgt_type))
				if(r < 0){
					console.log(this.time + "\t" +
						sourcei + "\t" + 
						targeti + "\t" + 
						"pcopy" + "\t" +
						1)
				} else {
					console.log(this.time + "\t" +
						sourcei + "\t" + 
						targeti + "\t" + 
						"pcopy" + "\t" +
						Math.exp( -r / this.conf.T ))
				}
			}
		}
		return r
	}
}

class PerimeterConstraint2 extends CPM.PerimeterConstraint{

deltaH( sourcei, targeti, src_type, tgt_type ){

	if( src_type === tgt_type ){
		return 0
	}
	const ls = this.cellParameter("LAMBDA_P2", src_type);
	const lt = this.cellParameter("LAMBDA_P2", tgt_type);
	if( !(ls>0) && !(lt>0) ){
		return 0
	}
	const Ni = this.C.neighi( targeti );
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
	let r = 0.0
	if( ls > 0 ){
		const pt = this.cellParameter("P2", src_type),
			ps = this.cellperimeters[src_type]
		const hnew = (((ps+pchange[src_type])) - pt),
			hold = ((ps) - pt)
		r += ls*((hnew*hnew)-(hold*hold))
	}
	if( lt > 0 ){
		const pt = this.cellParameter("P2", tgt_type),
			ps = this.cellperimeters[tgt_type]
		const hnew = (((ps+pchange[tgt_type])) - pt),
			hold = ((ps) - pt)
		r += lt*((hnew*hnew)-(hold*hold))
	}
	// eslint-disable-next-line
	return r
}

}

let conf = {
	torus : [true,false],			// wrap around in x-direction only
	seed : sd,						
	T : 20,							// standard value.
	
	// Adhesion: 
	J : [ [0,20], [20,100] ],
	
	// VolumeConstraint parameters
	LAMBDA_V : [0,30],				// VolumeConstraint importance per cellkind
	V : [0,500],					// Target volume of each cellkind
	
	// PerimeterConstraint parameters
	LAMBDA_P : [0,0],				// PerimeterConstraint importance per cellkind
	P : [0,perim],					// Target perimeter of each cellkind
	LAMBDA_P2 : [0,2],				
	P2 : [0,perim],
	
	LAMBDA_ACT : [0,250],
	MAX_ACT:[0,40],
	ACT_MEAN : "geometric"
}

/*	---------------------------------- */
let C, gm

function setup(fs){
	C = new CPM2( fs, conf)
	C.add(new PerimeterConstraint2(conf))
	gm = new CPM.GridManipulator(C)
}

function initialize(){
	setup([250,12])
	buildChannel(C)
	gm.seedCellAt(1, C.midpoint)
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
	// if(C2.time % 20 == 0){
	// 	console.log(C.getConstraint("PerimeterConstraint").cellperimeters[1] + "\t" +
	// 	C2.getConstraint("PerimeterConstraint").cellperimeters[1] + "\t" +
	// 	C2.getConstraint("PerimeterConstraint2").cellperimeters[1])
	// }
}

initialize()
while(C.time < 10000){
	step()
}
