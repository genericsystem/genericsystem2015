package org.genericsystem.cv.lm;

interface LMHost {
	double dNudge(double dp[]);

	// Allows myLM.bLMiter to modify parms[] and reevaluate.
	// This is the only modifier of parms[].
	// So, if NADJ<NPARMS, put your LUT here.

	boolean bBuildJacobian();

	// Allows LM to request a new Jacobian.

	double dGetResid(int i);

	// Allows LM to see one element of the resid[] vector.

	double dGetJac(int i, int j);
	// Allows LM to see one element of the Jacobian matrix.
}