/*  MONothetic Analysis --- MONA

    Program for divisive hierarchical clustering of binary data,
    using association analysis.
*/

/* mona.f -- translated by f2c (version 20031025).
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,
		http://www.netlib.org/f2c/libf2c.zip
   Further transformed by
   $Id: f2c-clean,v 1.11 2012/05/04 19:34:33 maechler Exp $
*/

#include <R.h>
// #include <Rinternals.h>

#include <R_ext/Print.h>/* for diagnostics */
#include <R_ext/Utils.h>/* for interrupting */

// iabs(j) = |j|
static int iabs(int j)
{
    return (j >= 0) ? (j) : -(j);
}

void clmona_(int *nn, // = number of objects
	     int *pp, // = number of variables
	     int *x,  /* x[i,j]: binary (0/1/NA) data  (obs. i, var.j)
			 where  NA =  missing values, are all values > 1 ;
			 actually are NA == 2 when called from R's mona() */
	     // int jerr[1] : verbose(ness) in {0, 1, ..}

	     // Result / Output / Return Values:
	     int *jerr, // error return code in {1,2,3,4,5}
	     int *nban, // [1:nn]
	     int *ner,  // [1:nn]
	     int *kwan, // [1:nn]
	     int *lava, // [1:nn]
	     int *jlack)// [1:pp] : jlack[j] := #{NA's in x[,j]}
{
    int verbose = jerr[0];
    int a, b, c, d, j, j0, j1;
    int jma, nel, lama, jnat, jtel, jtelz; //, jptwe

    /* Parameter adjustments */
    --lava;
    --kwan;
    --ner;
    --nban;
    --jlack;
    int x_dim1 = *nn,
	x_offset = 1 + x_dim1;
    x -= x_offset;

    /* Function Body */
    if(*pp < 2) {
	*jerr = 5; return; // not implemented currently (patches welcome!)
    }

    const int nhalf = (*nn + 1) / 2;
    // jptwe = (*pp + 4) / 5;
    Rboolean has_NA = FALSE;
    for (int l = 1; l <= *nn; ++l) {
	int n_miss = 0;
	for (j = 1; j <= *pp; ++j) {
	    if (x[l + j * x_dim1] > 1) ++n_miss;
	}
	if (n_miss == *pp) { // all variables missing for this object
	    *jerr = 1; return;
	}
	if(n_miss) {
	    has_NA = TRUE; break;
	}
    }

    if (has_NA) { // -------------- Missing Values  Treatment -----------

	int lack = 0;
	for (j = 1; j <= *pp; ++j) {
	    j0 = 0;
	    j1 = 0;
	    for (int l = 1; l <= *nn; ++l) {
		if      (x[l + j * x_dim1] == 0) ++j0;
		else if (x[l + j * x_dim1] == 1) ++j1;
	    }
	    jlack[j] = *nn - j0 - j1;
	    if (jlack[j] != 0) {
		++lack;
	    }
	    if (jlack[j] >= nhalf) {
		// at least 50% of the objects have missing values for this variable
		*jerr = 2; return;
	    }
	    if (j0 == 0 || j1 == 0) {
		// all non missing values are identical for this variable
		*jerr = 3; return;
	    }
	}
	if (lack == *pp) { /*     all variables have missing values */
	    *jerr = 4; return;
	}

	/* ---------- Filling in missing values --------------------- */

	for (j = 1; j <= *pp; ++j)
	    if (jlack[j] != 0) {
		lama = -1;
		Rboolean syn = TRUE;
		for (int ja = 1; ja <= *pp; ++ja)
		    if (jlack[ja] == 0) { /* no missing in x[, ja] */
			a = 0;
			b = 0;
			c = 0;
			d = 0;
			for (int k = 1; k <= *nn; ++k) {
			    if (x[k + j * x_dim1] != 1) {
				if      (x[k + ja * x_dim1] == 0) ++a;
				else if (x[k + ja * x_dim1] == 1) ++b;
			    } else  { // x[...] == 1
				if      (x[k + ja * x_dim1] == 0) ++c;
				else if (x[k + ja * x_dim1] == 1) ++d;
			    }
			}
			int kal = a * d - b * c,
			    kalf = iabs(kal);
			if (kalf >= lama) {
			    lama = kalf;
			    jma = ja;
			    if (kal < 0)
				syn = FALSE;
			}
		    }

		for (int k = 1; k <= *nn; ++k)
		    if (x[k + j * x_dim1] > 1) { // missing
			if (syn) {
			    x[k + j * x_dim1] = x[k + jma * x_dim1];
			} else {
			    if (x[k + jma * x_dim1] == 1)
				x[k +   j * x_dim1] = 0;
			    if (x[k + jma * x_dim1] == 0)
				x[k +   j * x_dim1] = 1;
			}
		    }
	    }

    } /* ---  end of treating missing values ---- */
    *jerr = 0; // it may have had "verbose"

/*  initialization --------------------------- */

    for (int k = 1; k <= *nn; ++k) {
	kwan[k] = 0;
	ner[k] = k;
	lava[k] = 0;
    }
    int npass = 1; //  number of passes
    kwan[1] = *nn;

/*  algorithm -------------------------------- */

    int nclu = 1;
    int ka = 1;
/* --- Loop --- */
L310:
    R_CheckUserInterrupt(); // (had infinite loop whenever pp == 1 !)

    int kb = ka + kwan[ka] - 1;
    if(verbose) Rprintf("Loop npass = %d: (ka,kb) = (%d,%d)\n", npass, ka, kb);
    lama = -1;
    jnat = *pp;
    for (j = 1; j <= *pp; ++j) {
	if (nclu == 1) {
	    goto L330; // jump *inside* if(.) .. hmm...
	}
	j0 = 0;
	j1 = 0;
	for (int k = ka; k <= kb; ++k) {
	    nel = ner[k];
	    if      (x[nel + j * x_dim1] == 0) ++j0;
	    else if (x[nel + j * x_dim1] == 1) ++j1;
	}
	if (j1 != 0 && j0 != 0) {
      L330:
	    --jnat;
	    int lams = 0;
	    for (int jb = 1; jb <= *pp; ++jb) {
		if (jb != j) { // FIXME: if (p == 1)  have j == jb == 1 here
		    // then this branch is never used ==> lama = -1 < lams = 0
		    // but (a,b,c,d)  will we remain  unitialized
		    // -Wall in Fortran did make us intialize them to 0
		    a = 0;
		    b = 0;
		    c = 0;
		    d = 0;
		    for (int k = ka; k <= kb; ++k) {
			nel = ner[k];
			if (x[nel + j * x_dim1] == 0) {
			    if      (x[nel + jb * x_dim1] == 0) ++a;
			    else if (x[nel + jb * x_dim1] == 1) ++b;
			} else {
			    if      (x[nel + jb * x_dim1] == 0) ++c;
			    else if (x[nel + jb * x_dim1] == 1) ++d;
			}
		    }
		    lams += iabs(a * d - b * c);
		}
	    }
	    if (lama < lams) {
		lama = lams;
		jtel = c + d;
		jtelz = a + b;
		jma = j;
	    }
	}
    } // end -- for(j in 1:p)

    if(verbose) Rprintf("  for(j ..) -> jma=%d, jtel(.,z) = (%d, %d)", jma, jtel, jtelz);

    if (jnat < *pp) {

	// ---- Splitting -------------------------------
	int nzf, jtel2;

	// L375:
	nel = ner[ka];
	if (x[nel + jma * x_dim1] == 1) {
	    nzf = 0;
	    jtel2 = jtel;
	} else {
	    nzf = 1;
	    jtel2 = jtelz;
	}
	int jres = kb - ka + 1 - jtel2,
	    km = ka + jtel2;
	if(verbose)
	    Rprintf(" --> splitting: (nel; jres, ka, km) = (%d; %d, %d, %d)\n",
		    nel, jres, ka, km);

	/*------- inner loop ------------------ */
	if(verbose >= 2) Rprintf(" inner loop: for(k in ka:km) use ner[k]: ");
	int k = ka;
	do { // L378: almost  for(k=ka; k < km; ++k)  (but see 'continue' below)
	    nel = ner[k];
	    if(verbose >= 2) Rprintf(" %d", nel);

	    if (x[nel + jma * x_dim1] == nzf) {
		int lcc, nelbb;
		for (int lbb = k; lbb <= kb; ++lbb) {
		    nelbb = ner[lbb];
		    if (x[nelbb + jma * x_dim1] != nzf) {
			lcc = lbb - 1;
			break; // goto L382;
		    }
		}
		// L382:
		for (int laa = k; laa <= lcc; ++laa) {
		    int ldd = lcc + k - laa;
		    ner[ldd + 1] = ner[ldd];
		}
		ner[k] = nelbb;
		continue; // the inner loop _without_ increasing 'k' !
	    }
	    ++k;
	} while (k < km);
	if(verbose >= 2) Rprintf("\n");
	/*------- end{inner loop} -- */

	/* L390: */
	++nclu;
	nban[km] = npass;
	kwan[ka] = jtel2;
	kwan[km] = jres;
	lava[km] = jma;
	ka += kwan[ka];

    } else { // jnat == *pp
	if(verbose) Rprintf(" --> _NO_ splitting\n");
	kwan[ka] = -kwan[ka];
    }
    if(verbose) Rprintf(" --> nclu = %d, kwan[ka] = %d\n", nclu, kwan[ka]);

    // L400:
    if (kb == *nn) {
	goto L500;
    }

    do {// L410:
	ka += iabs(kwan[ka]);
	if (ka > *nn) {
	    goto L500;
	}
    } while (kwan[ka] < 2);

    goto L310;
    //-----------> Loop

L500:
    ++npass;
    for (ka = 1; ka <= *nn; ++ka) {
	if (kwan[ka] >= 2) {
	    if(verbose) Rprintf("L500; found kwan[%d] = %d >= 2  ==> Loop again\n",
				ka, kwan[ka]);
	    goto L310;
	    //-----------> Loop
	}
    }
    return;
} /* clmona_ */
