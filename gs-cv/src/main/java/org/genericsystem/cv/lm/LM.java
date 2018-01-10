package org.genericsystem.cv.lm;

public class LM {
	private final int LMITER = 6; // max number of L-M iterations
	private final double LMBOOST = 2.0; // damping increase per failed step
	private final double LMSHRINK = 0.10; // damping decrease per successful step
	private final double LAMBDAZERO = 0.001; // initial damping
	private final double LAMBDAMAX = 1E9; // max damping
	private final double LMTOL = 1E-5; // exit tolerance
	private final double BIGVAL = 9e99; // trouble flag

	private double sos, sosprev, lambda;

	private final Levenberg myH;
	private final int nadj;
	private final int npts;
	private final double[] delta;
	private final double[] beta;
	private final double[][] alpha;
	private final double[][] amatrix;

	public LM(Levenberg gH, int gnadj, int gnpts) {
		myH = gH;
		nadj = gnadj;
		npts = gnpts;
		delta = new double[nadj];
		beta = new double[nadj];
		alpha = new double[nadj][nadj];
		amatrix = new double[nadj][nadj];
		lambda = LAMBDAZERO;
		int niter = 0;
		boolean done = false;
		do {
			done = bLMiter();
			niter++;
		} while (!done && (niter < LMITER));
	}

	private boolean bLMiter() {
		for (int k = 0; k < nadj; k++)
			delta[k] = 0.0;
		sos = myH.dNudge(delta);
		if (sos == BIGVAL) {
			System.out.println("  bLMiter finds faulty initial dNudge()");
			return false;
		}
		sosprev = sos;

		// System.out.println("  bLMiter..SumOfSquares= " + sos);
		if (!myH.bBuildJacobian()) {
			System.out.println("  bLMiter finds bBuildJacobian()=false");
			return false;
		}
		for (int k = 0; k < nadj; k++) // get downhill gradient beta
		{
			beta[k] = 0.0;
			for (int i = 0; i < npts; i++)
				beta[k] -= myH.dGetResid(i) * myH.dGetJac(i, k);
		}
		for (int k = 0; k < nadj; k++)
			// get curvature matrix alpha
			for (int j = 0; j < nadj; j++) {
				alpha[j][k] = 0.0;
				for (int i = 0; i < npts; i++)
					alpha[j][k] += myH.dGetJac(i, j) * myH.dGetJac(i, k);
			}
		double rrise = 0;
		do // inner damping loop searches for one downhill step
		{
			for (int k = 0; k < nadj; k++)
				// copy and damp it
				for (int j = 0; j < nadj; j++)
					amatrix[j][k] = alpha[j][k] + ((j == k) ? lambda : 0.0);

			gaussj(amatrix, nadj); // invert

			for (int k = 0; k < nadj; k++) // compute delta[]
			{
				delta[k] = 0.0;
				for (int j = 0; j < nadj; j++)
					delta[k] += amatrix[j][k] * beta[j];
			}
			sos = myH.dNudge(delta); // try it out.
			if (sos == BIGVAL) {
				System.out.println("  LMinner failed SOS step");
				return false;
			}
			rrise = (sos - sosprev) / (1 + sos);
			if (rrise <= 0.0) // good step!
			{
				lambda *= LMSHRINK; // shrink lambda
				break; // leave lmInner.
			}
			for (int q = 0; q < nadj; q++)
				// reverse course!
				delta[q] *= -1.0;
			myH.dNudge(delta); // sosprev should still be OK
			if (rrise < LMTOL) // finished but keep prev parms
				break; // leave inner loop
			lambda *= LMBOOST; // else try more damping.
		} while (lambda < LAMBDAMAX);
		boolean done = (rrise > -LMTOL) || (lambda > LAMBDAMAX);
		return done;
	}

	private double gaussj(double[][] a, int N) {
		double det = 1.0, big, save;
		int i, j, k, L;
		int[] ik = new int[100];
		int[] jk = new int[100];
		for (k = 0; k < N; k++) {
			big = 0.0;
			for (i = k; i < N; i++)
				for (j = k; j < N; j++)
					// find biggest element
					if (Math.abs(big) <= Math.abs(a[i][j])) {
						big = a[i][j];
						ik[k] = i;
						jk[k] = j;
					}
			if (big == 0.0)
				return 0.0;
			i = ik[k];
			if (i > k)
				for (j = 0; j < N; j++) // exchange rows
				{
					save = a[k][j];
					a[k][j] = a[i][j];
					a[i][j] = -save;
				}
			j = jk[k];
			if (j > k)
				for (i = 0; i < N; i++) {
					save = a[i][k];
					a[i][k] = a[i][j];
					a[i][j] = -save;
				}
			for (i = 0; i < N; i++)
				// build the inverse
				if (i != k)
					a[i][k] = -a[i][k] / big;
			for (i = 0; i < N; i++)
				for (j = 0; j < N; j++)
					if ((i != k) && (j != k))
						a[i][j] += a[i][k] * a[k][j];
			for (j = 0; j < N; j++)
				if (j != k)
					a[k][j] /= big;
			a[k][k] = 1.0 / big;
			det *= big; // bomb point
		}
		for (L = 0; L < N; L++) {
			k = N - L - 1;
			j = ik[k];
			if (j > k)
				for (i = 0; i < N; i++) {
					save = a[i][k];
					a[i][k] = -a[i][j];
					a[i][j] = save;
				}
			i = jk[k];
			if (i > k)
				for (j = 0; j < N; j++) {
					save = a[k][j];
					a[k][j] = -a[i][j];
					a[i][j] = save;
				}
		}
		return det;
	}
}
