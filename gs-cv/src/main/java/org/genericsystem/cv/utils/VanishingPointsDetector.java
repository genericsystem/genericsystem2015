package org.genericsystem.cv.utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

public class VanishingPointsDetector {

	private static final int MODE_LS = 0;
	private static final int MODE_NIETO = 1;

	private Mat li = new Mat(3, 1, CvType.CV_32F);

	private boolean verbose;
	private float width, height;
	private int mode;
	private float epsilon = (float) 1e-6;
	// private float P_inlier = (float) 0.95;

	private static float T_noise_squared = (float) 0.01623 * 2;
	private static int min_iters = 5;

	// Parameters
	int minimal_sample_set_dimension = 2;
	private Mat K;
	private Mat Li;
	private Mat Mi;
	private Mat Lengths;

	int[] CS_idx;

	public VanishingPointsDetector(Size imSize, boolean verbose) {
		this.verbose = verbose;
		this.width = (float) imSize.width;
		this.height = (float) imSize.height;
		this.verbose = verbose;
		this.mode = MODE_LS;

		// (Default) Calibration
		this.K = new Mat(3, 3, CvType.CV_32F, new Scalar(0));
		this.K.put(0, 0, new float[] { this.width });
		this.K.put(0, 2, new float[] { this.width / 2 });
		this.K.put(1, 1, new float[] { this.height });
		this.K.put(1, 2, new float[] { this.height / 2 });
		this.K.put(2, 2, new float[] { 1 });
	}

	private void fillDataContainers(List<Point[]> lineSegments) {
		int numLines = lineSegments.size();
		if (this.verbose)
			System.out.println("Line segments: " + numLines);

		// Transform all line segments
		// this.Li = [l_00 l_01 l_02; l_10 l_11 l_12; l_20 l_21 l_22; ...]; where li=[l_i0;l_i1;l_i2]^T is li=an x bn;
		Li = new Mat(numLines, 3, CvType.CV_32F);

		Mi = new Mat(numLines, 3, CvType.CV_32F);
		Lengths = new Mat(numLines, numLines, CvType.CV_32F, new Scalar(0));
		// Lengths.setTo(0);

		// Fill data containers (this.Li, this.Mi, this.Lenghts)
		double sum_lengths = 0;
		Mat a = new Mat(3, 1, CvType.CV_32F);
		Mat b = new Mat(3, 1, CvType.CV_32F);
		for (int i = 0; i < numLines; i++) {
			// Extract the end-points
			Point p1 = lineSegments.get(i)[0];
			Point p2 = lineSegments.get(i)[1];
			a.put(0, 0, new float[] { Double.valueOf(p1.x).floatValue() });
			a.put(1, 0, new float[] { Double.valueOf(p1.y).floatValue() });
			a.put(2, 0, new float[] { Double.valueOf(1d).floatValue() });
			b.put(0, 0, new float[] { Double.valueOf(p2.x).floatValue() });
			b.put(1, 0, new float[] { Double.valueOf(p2.y).floatValue() });
			b.put(2, 0, new float[] { Double.valueOf(1d).floatValue() });
			Mat c = new Mat(3, 1, CvType.CV_32F);
			if (this.mode == MODE_NIETO)
				Core.addWeighted(a, 0.5, b, 0.5, 0, c);

			double length = Math.sqrt((b.get(0, 0)[0] - a.get(0, 0)[0]) * (b.get(0, 0)[0] - a.get(0, 0)[0]) + (b.get(1, 0)[0] - a.get(1, 0)[0]) * (b.get(1, 0)[0] - a.get(1, 0)[0]));
			sum_lengths += length;
			Lengths.put(i, i, new float[] { (float) length });
			Mat an = new Mat(3, 1, CvType.CV_32F);
			Mat bn = new Mat(3, 1, CvType.CV_32F);
			if (this.mode == MODE_LS) {
				// Normalize into the sphere
				Core.gemm(K.inv(), a, 1, new Mat(), 0, an);
				Core.gemm(K.inv(), b, 1, new Mat(), 0, bn);
			} else // this.mode == MODE_NIETO requires not to calibrate into the sphere
			{
				an = a;
				bn = b;
			}

			// Compute the general form of the line
			li = an.cross(bn);
			Core.normalize(li, li);
			// Insert line into appended array
			Li.put(i, 0, li.get(0, 0));
			Li.put(i, 1, li.get(1, 0));
			Li.put(i, 2, li.get(2, 0));

			if (this.mode == MODE_NIETO) {
				// Store mid-Point too
				Mi.put(i, 0, c.get(0, 0));
				Mi.put(i, 1, c.get(1, 0));
				Mi.put(i, 2, c.get(2, 0));
			}
		}
		Core.multiply(Lengths, new Scalar(1 / sum_lengths), Lengths);
	}

	public void multipleVPEstimation(List<Point[]> lineSegments, List<List<Point[]>> lineSegmentsClusters, List<Integer> numInliers, List<Mat> vps, int numVps) {
		// Make a copy of lineSegments because it is modified in the code (it will be restored at the end of this function)
		List<Point[]> lineSegmentsCopy = new ArrayList<>(lineSegments);

		// Loop over maximum number of vanishing points
		for (int vpNum = 0; vpNum < numVps; vpNum++) {
			// Fill data structures
			fillDataContainers(lineSegments);
			int numLines = lineSegments.size();

			if (this.verbose)
				System.out.println("VP " + vpNum + "-----");

			// Break if the number of elements is lower than minimal sample set
			if (numLines < 3 || numLines < this.minimal_sample_set_dimension) {
				if (this.verbose)
					System.out.println("Not enough line segments to compute vanishing point");
				break;
			}

			int N_I_best = this.minimal_sample_set_dimension;
			float J_best = Float.MAX_VALUE;

			int iter = 0;
			int T_iter = Integer.MAX_VALUE;

			// Define containers of CS (Consensus set): this.CS_best to store the best one, and this.CS_idx to evaluate a new candidate
			int[] CS_best = new int[numLines];
			CS_idx = new int[numLines];

			// Allocate Error matrix
			float[] E = new float[numLines];

			// MSAC
			if (this.verbose) {
				if (this.mode == MODE_LS)
					System.out.println("Method: Calibrated Least Squares");
				if (this.mode == MODE_NIETO)
					System.out.println("Method: Nieto");

				System.out.println("Start MSAC");
			}
			Mat vp = new Mat(3, 1, CvType.CV_32F);

			// RANSAC loop
			while ((iter <= min_iters) || ((iter <= T_iter))) {
				iter++;

				// Hypothesize ------------------------
				// Select MSS
				int[] MSS = new int[minimal_sample_set_dimension];
				if (Li.rows() < MSS.length)
					break;
				Mat vpAux = new Mat(3, 1, CvType.CV_32F);
				getMinimalSampleSet(Li, Lengths, Mi, MSS, vpAux); // output this.vpAux is calibrated

				// Test --------------------------------
				// Find the consensus set and cost
				int[] N_I = new int[] { 0 };
				float J = getConsensusSet(vpNum, Li, Lengths, Mi, vpAux, E, N_I); // the CS is indexed in CS_idx

				boolean notify;
				boolean update_T_iter = false;
				// Update ------------------------------
				// If the new cost is better than the best one, update
				if (N_I[0] >= this.minimal_sample_set_dimension && (J < J_best) || ((J == J_best) && (N_I[0] > N_I_best))) {
					notify = true;

					J_best = J;
					CS_best = CS_idx;
					vp = vpAux; // Store into this.vp (current best hypothesis): this.vp is therefore calibrated

					if (N_I[0] > N_I_best)
						update_T_iter = true;

					N_I_best = N_I[0];

					if (update_T_iter) {
						// Update number of iterations
						double q = 0;
						if (minimal_sample_set_dimension > N_I_best) {
							// Error!
							new IllegalStateException("The number of inliers must be higher than minimal sample set");
						}
						if (numLines == N_I_best) {
							q = 1;
						} else {
							q = 1;
							for (int j = 0; j < minimal_sample_set_dimension; j++)
								q *= (double) (N_I_best - j) / (numLines - j);
						}
						// Estimate the number of iterations for RANSAC
						if ((1 - q) > 1e-12)
							T_iter = (int) Math.ceil(Math.log(epsilon) / Math.log(1 - q));
						else
							T_iter = 0;
					}
				} else
					notify = false;

				// Verbose
				if (this.verbose && notify) {
					int aux = Math.max(T_iter, min_iters);
					System.out.println("Iteration = " + iter + "/" + aux);
					System.out.println("Inliers = " + N_I_best + "/" + numLines + " (cost is J = " + J_best);

					if (this.verbose)
						System.out.println("MSS Cal.VP = (" + vp.get(0, 0)[0] + "," + vp.get(1, 0)[0] + "," + vp.get(2, 0)[0] + ")");
				}

				// Check CS length (for the case all line segments are in the CS)
				if (N_I_best == numLines) {
					if (this.verbose)
						System.out.println("All line segments are inliers. End MSAC at iteration : " + iter);
					break;
				}
			}

			// Reestimate ------------------------------
			if (this.verbose) {
				System.out.println("Number of iterations: " + iter);
				System.out.println("Final number of inliers = " + N_I_best + "/" + numLines);
			}

			// Vector containing indexes for current vp
			List<Integer> ind_CS = new ArrayList<Integer>();

			// Fill ind_CS with this.CS_best
			List<Point[]> lineSegmentsCurrent = new ArrayList<>();
			for (int i = 0; i < numLines; i++) {
				if (CS_best[i] == vpNum) {
					ind_CS.add(i);
					lineSegmentsCurrent.add(lineSegments.get(i));
				}
			}

			if (J_best > 0 && ind_CS.size() > minimal_sample_set_dimension) // if J==0 maybe its because all line segments are perfectly parallel and the vanishing point is at the infinity
			{
				if (this.verbose) {
					System.out.println("Reestimating the solution... ");
				}

				if (this.mode == MODE_LS)
					estimateLS(Li, Lengths, ind_CS, N_I_best, vp);
				else if (this.mode == MODE_NIETO)
					;// estimateNIETO(Li, Lengths, Mi, ind_CS, N_I_best, this.vp); // Output this.vp is calibrated
				else
					throw new IllegalStateException("ERROR: mode not supported, please use {LS, LIEB, NIETO}\n");

				if (this.verbose)
					System.out.println("done!");

				// Uncalibrate
				if (this.verbose)
					System.out.println("Cal.VP = (" + vp.get(0, 0)[0] + "," + vp.get(1, 0)[0] + "," + vp.get(2, 0)[0] + ")");
				Core.gemm(K, vp, 1, new Mat(), 0, vp);

				if (vp.get(2, 0)[0] != 0) {
					vp.put(0, 0, new float[] { Double.valueOf(vp.get(0, 0)[0] / vp.get(2, 0)[0]).floatValue() });
					vp.put(1, 0, new float[] { Double.valueOf(vp.get(1, 0)[0] / vp.get(2, 0)[0]).floatValue() });
					vp.put(2, 0, new float[] { 1 });
				} else {
					// Since this is infinite, it is better to leave it calibrated
					Core.gemm(K.inv(), vp, 1, new Mat(), 0, vp);
				}
				if (this.verbose)
					System.out.println("VP = (" + vp.get(0, 0)[0] + "," + vp.get(1, 0)[0] + "," + vp.get(2, 0)[0] + ")");

				// Copy to output vector
				vps.add(vp);
			} else {
				if (Math.abs(J_best - 1) < 0.000001) {
					if (this.verbose) {
						System.out.println("The cost of the best MSS is 0! No need to reestimate");
						System.out.println("Cal. VP = (" + Double.valueOf(vp.get(0, 0)[0]) + "," + Double.valueOf(vp.get(1, 0)[0]) + "," + Double.valueOf(vp.get(2, 0)[0]) + ")");
					}

					// Uncalibrate
					Core.gemm(K, vp, 1, new Mat(), 0, vp);

					if (vp.get(2, 0)[0] != 0) {
						vp.put(0, 0, new float[] { Double.valueOf(vp.get(0, 0)[0] / vp.get(2, 0)[0]).floatValue() });
						vp.put(1, 0, new float[] { Double.valueOf(vp.get(1, 0)[0] / vp.get(2, 0)[0]).floatValue() });
						vp.put(2, 0, new float[] { 1 });

						if (this.verbose)
							System.out.println("VP = (" + vp.get(0, 0)[0] + "," + vp.get(1, 0)[0] + "," + vp.get(2, 0)[0] + ")");
					} else {
						// Calibrate
						vp = this.K.inv().mul(vp);
						Core.gemm(K.inv(), vp, 1, new Mat(), 0, vp);

					}
					// Copy to output vector
					vps.add(vp);
				}
			}

			// Fill lineSegmentsClusters containing the indexes of inliers for current vps
			if (N_I_best > 2) {

				for (int index = ind_CS.size() - 1; index >= 0; index--) {
					lineSegments.remove(ind_CS.get(index));
				}

				// while (ind_CS.size() > 0) {
				// lineSegments.remove(ind_CS[ind_CS.length - 1]);
				// ind_CS.pop_back();
				// }
			}

			// Fill current CS
			lineSegmentsClusters.add(lineSegmentsCurrent);

			// Fill numInliers
			numInliers.add(N_I_best);
		}

		// Restore lineSegments
		lineSegments = lineSegmentsCopy;
	}

	// RANSAC
	public void getMinimalSampleSet(Mat Li, Mat Lengths, Mat Mi, int[] MSS, Mat vp) {
		int N = Li.rows();

		// Generate a pair of samples
		while (N <= (MSS[0] = (int) (Math.random() * (N - 1))))
			;
		while (N <= (MSS[1] = (int) (Math.random() * (N - 1))))
			;
		// Estimate the vanishing point and the residual error
		if (this.mode == MODE_LS)
			estimateLS(Li, Lengths, Arrays.asList(MSS[0], MSS[1]), 2, vp);
		else if (this.mode == MODE_NIETO)
			;// estimateNIETO(Li, Mi, Lengths, MSS, 2, vp);
		else
			new IllegalStateException("ERROR: mode not supported. Please use {LS, LIEB, NIETO}");
	}

	private float getConsensusSet(int vpNum, Mat Li, Mat Lengths, Mat Mi, Mat vp, float[] E, int[] CS_counter) {
		// Compute the error of each line segment of LSS with respect to v_est
		// If it is less than the threshold, add to the CS
		for (int i = 0; i < CS_idx.length; i++)
			this.CS_idx[i] = -1;

		float J = 0;

		if (this.mode == MODE_LS)
			J = errorLS(vpNum, Li, vp, E, CS_counter);
		else if (this.mode == MODE_NIETO)
			;// J = errorNIETO(vpNum, Li, Lengths, Mi, vp, E, CS_counter);
		else
			new IllegalStateException("ERROR: mode not supported, please use {LS, LIEB, NIETO}\n");

		return J;
	}

	// Estimation functions
	private void estimateLS(Mat Li, Mat Lengths, List<Integer> set, int set_length, Mat vp) {
		// System.out.println("zzz" + set.size() + " " + set_length);
		if (set_length == this.minimal_sample_set_dimension) {
			// Just the cross product
			// DATA IS CALIBRATED in MODE_LS
			Mat ls0 = new Mat(3, 1, CvType.CV_32F);
			Mat ls1 = new Mat(3, 1, CvType.CV_32F);

			ls0.put(0, 0, Li.get(set.get(0), 0));
			ls0.put(1, 0, Li.get(set.get(0), 1));
			ls0.put(2, 0, Li.get(set.get(0), 2));

			ls1.put(0, 0, Li.get(set.get(1), 0));
			ls1.put(1, 0, Li.get(set.get(1), 1));
			ls1.put(2, 0, Li.get(set.get(1), 2));

			vp = ls0.cross(ls1);

			Core.normalize(vp, vp);

			return;
		} else if (set_length < minimal_sample_set_dimension) {
			new IllegalStateException("Error: at least 2 line-segments are required");
			return;
		}

		// Extract the line segments corresponding to the indexes contained in the set
		Mat li_set = new Mat(3, set_length, CvType.CV_32F);
		Mat Lengths_set = new Mat(set_length, set_length, CvType.CV_32F, new Scalar(0, 0, 0));
		// Lengths_set.setTo(0);

		// Fill line segments info
		for (int i = 0; i < set_length; i++) {
			li_set.put(0, i, Li.get(set.get(i), 0));
			li_set.put(1, i, Li.get(set.get(i), 1));
			li_set.put(2, i, Li.get(set.get(i), 2));

			Lengths_set.put(i, i, Lengths.get(set.get(i), set.get(i)));
		}

		// Least squares solution
		// Generate the matrix ATA (a partir de LSS_set=A)
		Mat L = li_set.t();
		Mat Tau = Lengths_set;
		Mat ATA = new Mat(3, 3, CvType.CV_32F);
		Mat dst = new Mat();

		Core.gemm(L.t(), Tau.t(), 1, new Mat(), 0, dst);
		Core.gemm(dst, Tau, 1, new Mat(), 0, dst);
		Core.gemm(dst, L, 1, new Mat(), 0, ATA);

		// Obtain eigendecomposition
		Mat w = new Mat();
		Mat vt = new Mat();
		Mat v = new Mat();

		Core.SVDecomp(ATA, w, v, vt);

		// Check eigenvecs after SVDecomp
		if (v.rows() < 3)
			return;

		// print v, w, vt...
		// std::cout << "w=" << w << endl;
		// std::cout << "v=" << v << endl;
		// std::cout << "vt" << vt << endl;

		// Assign the result (the last column of v, corresponding to the eigenvector with lowest eigenvalue)
		vp.put(0, 0, v.get(0, 2));
		vp.put(1, 0, v.get(1, 2));
		vp.put(2, 0, v.get(2, 2));

		Core.normalize(vp, vp);

		return;
	}

	// public void estimateNIETO(Mat Li,Mat Lengths, Mat Mi, int[] set, int set_length, Mat vp)
	// {
	// if (set_length == this.minimal_sample_set_dimension)
	// {
	// // Just the cross product
	// // DATA IS NOT CALIBRATED for MODE_NIETO
	// Mat ls0 = new Mat(3,1,CvType.CV_32F);
	// Mat ls1 = new Mat(3,1,CvType.CV_32F);
	//
	//
	// ls0.at(0,0) = Li.at(set[0],0);
	// ls0.at(1,0) = Li.at(set[0],1);
	// ls0.at(2,0) = Li.at(set[0],2);
	//
	// ls1.at(0,0) = Li.at(set[1],0);
	// ls1.at(1,0) = Li.at(set[1],1);
	// ls1.at(2,0) = Li.at(set[1],2);
	//
	// vp = ls0.cross(ls1);
	//
	// // Calibrate (and normalize) vp
	// vp = this.K.inv()*vp;
	// Core.normalize(vp, vp);
	// return;
	// }
	// else if (set_length<this.minimal_sample_set_dimension)
	// {
	// new IllegalStateException("Error: at least 2 line-segments are required\n");
	// return;
	// }
	//
	// // Extract the line segments corresponding to the indexes contained in the set
	// Mat li_set = new Mat(3, set_length, CvType.CV_32F);
	// Mat Lengths_set = new Mat(set_length, set_length,CvType.CV_32F,new Scalar(0));
	// Mat mi_set = new Mat(3, set_length,CvType. CV_32F);
	// //Lengths_set.setTo(0);
	//
	// // Fill line segments info
	// for (int i=0; i<set_length; i++)
	// {
	// li_set.at(0,i) = Li.at(set[i], 0);
	// li_set.at(1,i) = Li.at(set[i], 1);
	// li_set.at(2,i) = Li.at(set[i], 2);
	//
	// Lengths_set.at(i,i) = Lengths.at(set[i],set[i]);
	//
	// mi_set.at(0,i) = Mi.at(set[i], 0);
	// mi_set.at(1,i) = Mi.at(set[i], 1);
	// mi_set.at(2,i) = Mi.at(set[i], 2);
	// }
	//
	// //#ifdef DEBUG_MAP
	// // double dtheta = 0.01;
	// // double dphi = 0.01;
	// //
	// // int numTheta = (int)CV_PI/(2*dtheta);
	// // int numPhi = (int)2*CV_PI/dphi;
	// // Mat debugMap(numTheta, numPhi, CvType.CV_32F);
	// // debugMap.setTo(0);
	// //
	// // data_struct dataTest(li_set, Lengths_set, mi_set, this.K);
	// // double[] fvecTest = new double[set_length];
	// // int infoTest;
	// // int aux = 0;
	// // infoTest = aux;
	// //
	// // // Image limits
	// // Mat pt0 = Mat(3,1,CV_32F);
	// // Mat pt3 = Mat(3,1,CV_32F);
	// // pt0.at(0,0) = 0; pt0.at(1,0) = 0; pt0.at(2,0) = 1;
	// // pt3.at(0,0) = this.width; pt3.at(1,0) = this.height; pt3.at(2,0) = 1;
	// //
	// // Mat pt0C = this.K.inv()*pt0; normalize(pt0C, pt0C);
	// // Mat pt3C = this.K.inv()*pt3; normalize(pt3C, pt3C);
	// //
	// // double theta0 = acos(pt0C.at(2,0));
	// // double phi0 = atan2(pt0C.at(1,0), pt0C.at(0,0));
	// // printf("\nPt0(sph): (%.2f, %.2f)\n", theta0, phi0);
	// //
	// // double theta3 = acos(pt3C.at(2,0));
	// // double phi3 = atan2(pt3C.at(1,0), pt3C.at(0,0));
	// // printf("Pt3(sph): (%.2f, %.2f)\n", theta3, phi3);
	// //
	// // double paramTest [] = {0, 0};
	// // double maxE = 0, minE = FLT_MAX;
	// // for(int t=0; t<numTheta; t++)
	// // {
	// // double theta = dtheta*t;
	// // for(int p=0; p<numPhi; p++)
	// // {
	// // double phi = dphi*p - CV_PI;
	// // paramTest[0] = theta;
	// // paramTest[1] = phi;
	// //
	// // evaluateNieto(paramTest, set_length, (const void*)&dataTest, fvecTest, infoTest);
	// //
	// // for(int m=0; m<set_length; m++)
	// // debugMap.at(t,p) += fvecTest[m];
	// //
	// // if(debugMap.at(t,p) < minE)
	// // minE = debugMap.at(t,p);
	// // else if(debugMap.at(t,p) > maxE)
	// // maxE = debugMap.at(t,p);
	// // }
	// // }
	// // Mat debugMapIm(numTheta, numPhi, CV_8UC1);
	// // double scale = 255/(maxE-minE);
	// //
	// // convertScaleAbs(debugMap, debugMapIm, scale);
	// //
	// // delete[] fvecTest;
	// //
	// // imshow("DebugMap", debugMapIm);
	// // waitKey(0);
	// //
	// //
	// // #endif
	//
	// // Lev.-Marq. solution
	// int m_dat = set_length;
	// //int num_par = 3;
	// int num_par = 2;
	//
	// // The starting point is the provided vp which is already calibrated
	// if(this.verbose)
	// {
	// printf("\nInitial Cal.VP = (%.3f,%.3f,%.3f)\n", vp.at(0,0), vp.at(1,0), vp.at(2,0));
	// Mat vpUnc = new Mat(3,1,CvType.CV_32F);
	// vpUnc = this.K*vp;
	// if(vpUnc.at(2,0) != 0)
	// {
	// vpUnc.at(0,0) /= vpUnc.at(2,0);
	// vpUnc.at(1,0) /= vpUnc.at(2,0);
	// vpUnc.at(2,0) = 1;
	// }
	// printf("Initial VP = (%.3f,%.3f,%.3f)\n", vpUnc.at(0,0), vpUnc.at(1,0), vpUnc.at(2,0));
	// }
	//
	// // Convert to spherical coordinates to move on the sphere surface (restricted to r=1)
	// double x = (double)vp.at(0,0);
	// double y = (double)vp.at(1,0);
	// double z = (double)vp.at(2,0);
	// double r = Core.norm(vp);
	// double theta = Math.acos(z/r);
	// double phi = Math.atan2(y,x);
	//
	// if(this.verbose)
	// printf("Initial Cal.VP (Spherical) = (%.3f,%.3f,%.3f)\n", theta, phi, r);
	//
	// //double par[] = {(double)vp.at(0,0), (double)vp.at(1,0), (double)vp.at(2,0)};
	// double par[] = {theta, phi};
	//
	// lm_control_struct control = lm_control_double;
	// control.epsilon = 1e-5; // less than 1º
	// if(this.verbose)
	// control.printflags = 2; //monitor status (+1) and parameters (+2), (4): residues at end of fit, (8): residuals at each step
	// else
	// control.printflags = 0;
	// lm_status_struct status;
	// data_struct data(li_set, Lengths_set, mi_set, this.K);
	//
	// lmmin(num_par, par, m_dat, data, evaluateNieto, &control, &status, lm_printout_std);
	//
	// if(this.verbose)
	// System.out.println("Converged Cal.VP (Spherical) = "+ "("+par[0]+","+ par[1]+","+ r+")");
	//
	// // Store into vp
	// // 1) From spherical to cartesian
	// theta = par[0];
	// phi = par[1];
	// x = r*Math.cos(phi)*Math.sin(theta);
	// y = r*Math.sin(phi)*Math.sin(theta);
	// z = r*Math.cos(theta);
	//
	// vp.at(0,0) = (float)x;
	// vp.at(1,0) = (float)y;
	// vp.at(2,0) = (float)z;
	//
	// }

	// Error functions
	public float errorLS(int vpNum, Mat Li, Mat vp, float[] E, int[] CS_counter) {
		Mat vn = vp;
		double vn_norm = Core.norm(vn);

		Mat li = new Mat(3, 1, CvType.CV_32F);
		double li_norm = 0;
		float di = 0;

		float J = 0;
		for (int i = 0; i < Li.rows(); i++) {
			li.put(0, 0, Li.get(i, 0));
			li.put(1, 0, Li.get(i, 1));
			li.put(2, 0, Li.get(i, 2));

			li_norm = Core.norm(li); // esto lo podria precalcular antes
			di = (float) vn.dot(li);
			di /= (float) (vn_norm * li_norm);

			E[i] = di * di;

			/* Add to CS if error is less than expected noise */
			if (E[i] <= T_noise_squared) {
				CS_idx[i] = vpNum; // set index to 1
				CS_counter[0]++;

				// Torr method
				J += E[i];
			} else {
				J += T_noise_squared;
			}
		}

		J /= CS_counter[0];

		return J;
	}

	// public float errorNIETO(int vpNum, Mat Li, Mat Lengths, Mat Mi, Mat vp, float[] E, int CS_counter) {
	// float J = 0;
	// float di = 0;
	//
	// Mat lineSegment = new Mat(3, 1, CvType.CV_32F);
	// float lengthLineSegment = 0;
	// Mat midPoint = new Mat(3, 1, CvType.CV_32F);
	//
	// // The vp arrives here calibrated, need to uncalibrate (check it anyway)
	// Mat vn = new Mat(3, 1, CvType.CV_32F);
	// double vpNorm = Core.norm(vp);
	// if (fabs(vpNorm - 1) < 0.001) {
	// // Calibrated -> uncalibrate
	// vn = this.K * vp;
	// if (vn.at(2, 0) != 0) {
	// vn.at(0, 0) /= vn.at(2, 0);
	// vn.at(1, 0) /= vn.at(2, 0);
	// vn.at(2, 0) = 1;
	// }
	// }
	//
	// for (int i = 0; i < Li.rows; i++) {
	// lineSegment.at(0, 0) = Li.at(i, 0);
	// lineSegment.at(1, 0) = Li.at(i, 1);
	// lineSegment.at(2, 0) = Li.at(i, 1);
	//
	// lengthLineSegment = Lengths.at(i, i);
	//
	// midPoint.at(0, 0) = Mi.at(i, 0);
	// midPoint.at(1, 0) = Mi.at(i, 1);
	// midPoint.at(2, 0) = Mi.at(i, 2);
	//
	// di = distanceNieto(vn, lineSegment, lengthLineSegment, midPoint);
	//
	// E[i] = di * di;
	//
	// /* Add to CS if error is less than expected noise */
	// if (E[i] <= this.T_noise_squared) {
	// this.CS_idx[i] = vpNum; // set index to 1
	// (CS_counter)++;
	//
	// // Torr method
	// J += E[i];
	// } else {
	// J += this.T_noise_squared;
	// }
	//
	// J += E[i];
	// }
	//
	// J /= (CS_counter);
	//
	// return J;
	// }

	public void drawCS(Mat im, List<List<Point[]>> lineSegmentsClusters, List<Mat> vps) {
		List<Scalar> colors = new ArrayList<>();
		colors.add(new Scalar(0, 0, 255)); // First is RED
		colors.add(new Scalar(0, 255, 0)); // Second is GREEN
		colors.add(new Scalar(255, 0, 0)); // Third is BLUE

		// Paint vps
		for (int vpNum = 0; vpNum < vps.size(); vpNum++) {
			if (vps.get(vpNum).get(2, 0)[0] != 0) {
				Point vp = new Point(vps.get(vpNum).get(0, 0)[0], vps.get(vpNum).get(1, 0)[0]);

				// Paint vp if inside the image
				if (vp.x >= 0 && vp.x < im.cols() && vp.y >= 0 && vp.y < im.rows()) {
					Imgproc.circle(im, vp, 4, colors.get(vpNum), 2);
					Imgproc.circle(im, vp, 3, new Scalar(0, 0, 0), -1);
				}
			}
		}
		// Paint line segments
		for (int localc = 0; localc < lineSegmentsClusters.size(); localc++) {
			for (int i = 0; i < lineSegmentsClusters.get(localc).size(); i++) {
				Point pt1 = lineSegmentsClusters.get(localc).get(i)[0];
				Point pt2 = lineSegmentsClusters.get(localc).get(i)[1];

				Imgproc.line(im, pt1, pt2, colors.get(localc), 1);
			}
		}
	}
}
