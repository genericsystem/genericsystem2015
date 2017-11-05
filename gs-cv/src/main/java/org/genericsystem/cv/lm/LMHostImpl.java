package org.genericsystem.cv.lm;

import java.util.Arrays;
import java.util.Collection;
import java.util.function.BiFunction;

public class LMHostImpl<T> implements LMHost {
	// --------constants for LM---------------
	private final double DELTAP = 1e-6; // parm step
	private final double BIGVAL = 9e99; // fault flag

	// ---constants for fitting: pixels & parameters-----

	private final Collection<T> datas;

	private final int NPTS;
	private final double parms[];
	private final int NPARMS;

	private final double resid[];
	private final double jac[][];
	private final BiFunction<T, double[], Double> error;

	public LMHostImpl(BiFunction<T, double[], Double> error, Collection<T> datas, double[] guess) {
		this.datas = datas;
		this.NPTS = datas.size();
		this.parms = guess;
		this.NPARMS = parms.length;
		this.resid = new double[NPTS];
		this.jac = new double[NPTS][NPARMS];
		this.error = error;

		// listParms("Start Parms", parms);
		LM myLM = new LM(this, NPARMS, NPTS);
		// listParms("Fitted Parms", parms);
	}

	public static void main(String args[]) {

		BiFunction<double[], double[], Double> error = (rowData, p) -> {
			double x = rowData[0];
			double x2 = x * x;
			double denom = 1 + p[1] * x2;
			return p[0] / denom - rowData[1];
		};

		new LMHostImpl<>(error, Arrays.asList(new double[][] { { 0.00, 0.6793 }, { 0.03, 0.6787 }, { 0.06, 0.6768 }, { 0.09, 0.6736 }, { 0.12, 0.6691 }, { 0.15, 0.6634 }, { 0.18, 0.6565 }, { 0.21, 0.6482 }, { 0.24, 0.6388 }, { 0.27, 0.6280 },
				{ 0.30, 0.6161 }, { 0.33, 0.6030 }, { 0.36, 0.5887 }, { 0.39, 0.5733 }, { 0.42, 0.5568 }, { 0.45, 0.5394 }, { 0.48, 0.5210 }, { 0.51, 0.5019 }, { 0.54, 0.4820 }, { 0.57, 0.4614 }, { 0.60, 0.4404 }, { 0.63, 0.4191 }, { 0.66, 0.3975 },
				{ 0.69, 0.3758 }, { 0.72, 0.3542 }, { 0.75, 0.3328 }, { 0.78, 0.3117 }, { 0.81, 0.2910 }, { 0.84, 0.2710 }, { 0.87, 0.2515 }, { 0.90, 0.2328 }, { 0.93, 0.2149 }, { 0.96, 0.1979 }, { 0.99, 0.1818 }, { 1.02, 0.1667 }, { 1.05, 0.1524 },
				{ 1.08, 0.1392 }, { 1.11, 0.1268 }, { 1.14, 0.1154 }, { 1.17, 0.1048 }, { 1.20, 0.0951 }, { 1.23, 0.0862 }, { 1.26, 0.0781 }, { 1.29, 0.0706 }, { 1.32, 0.0639 }, { 1.35, 0.0577 }, { 1.38, 0.0521 }, { 1.41, 0.0471 }, { 1.44, 0.0425 },
				{ 1.47, 0.0384 }, { 1.50, 0.0347 }, { 1.53, 0.0313 }, { 1.56, 0.0283 }, { 1.59, 0.0255 }, { 1.62, 0.0231 }, { 1.65, 0.0209 }, { 1.68, 0.0189 }, { 1.71, 0.0171 }, { 1.74, 0.0155 }, { 1.77, 0.0140 }, { 1.80, 0.0127 }, { 1.83, 0.0115 },
				{ 1.86, 0.0105 }, { 1.89, 0.0095 }, { 1.92, 0.0087 }, { 1.95, 0.0079 }, { 1.98, 0.0072 } }), new double[] { 0, 0 });
	}

	// ------------mathematical helpers for FitHost--------------

	void listParms(String title, double p[]) {
		System.out.print(title + "----");
		for (int i = 0; i < NPARMS; i++)
			System.out.print(String.format("%12.6f", p[i]));
		System.out.println("");
	}

	private double dComputeResiduals() {
		double sumsq = 0.0;
		int i = 0;
		for (T rowData : datas) {
			resid[i] = error.apply(rowData, parms);
			sumsq += resid[i] * resid[i];
			i++;
		}
		return sumsq;
	}

	// ------the four mandated interface methods------------

	@Override
	public double dNudge(double dp[])
	// Allows LM to modify parms[] and reevaluate its fit.
	// Returns sum-of-squares for nudged params.
	// This is the only place that parms[] are modified.
	// If NADJ<NPARMS, this is the place for your LUT.
	{
		for (int j = 0; j < NPARMS; j++)
			parms[j] += dp[j];
		return dComputeResiduals();
	}

	@Override
	public boolean bBuildJacobian()
	// Allows LM to compute a new Jacobian.
	// Uses current parms[] and two-sided finite difference.
	// If current parms[] is bad, returns false.
	{
		double delta[] = new double[NPARMS];
		double FACTOR = 0.5 / DELTAP;
		double d = 0;

		for (int j = 0; j < NPARMS; j++) {
			for (int k = 0; k < NPARMS; k++)
				delta[k] = (k == j) ? DELTAP : 0.0;

			d = dNudge(delta); // resid at pplus
			if (d == BIGVAL) {
				System.out.println("Bad dBuildJacobian() exit 2");
				return false;
			}
			for (int i = 0; i < NPTS; i++)
				jac[i][j] = dGetResid(i);

			for (int k = 0; k < NPARMS; k++)
				delta[k] = (k == j) ? -2 * DELTAP : 0.0;

			d = dNudge(delta); // resid at pminus
			if (d == BIGVAL) {
				System.out.println("Bad dBuildJacobian() exit 3");
				return false;
			}

			for (int i = 0; i < NPTS; i++)
				jac[i][j] -= dGetResid(i); // fetches resid[]

			for (int i = 0; i < NPTS; i++)
				jac[i][j] *= FACTOR;

			for (int k = 0; k < NPARMS; k++)
				delta[k] = (k == j) ? DELTAP : 0.0;

			d = dNudge(delta);
			if (d == BIGVAL) {
				System.out.println("Bad dBuildJacobian() exit 4");
				return false;
			}
		}
		return true;
	}

	@Override
	public double dGetResid(int i) {
		return resid[i];
	}

	@Override
	public double dGetJac(int i, int j) {
		return jac[i][j];
	}

	public double[] getParams() {
		return parms;
	}
}