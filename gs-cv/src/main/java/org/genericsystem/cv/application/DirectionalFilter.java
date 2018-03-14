package org.genericsystem.cv.application;

import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;
import org.opencv.videoio.VideoCapture;

import java.util.ArrayList;
import java.util.List;

public class DirectionalFilter {

	static {
		NativeLibraryLoader.load();
	}

	private final double difScl = 0.7;
	private final int hSz = (int) Math.ceil(3 * difScl);
	private final Mat filterGauss = Mat.zeros(2 * hSz + 1, 1, CvType.CV_64FC1);
	private final Mat filterGaussDerivative = Mat.zeros(2 * hSz + 1, 1, CvType.CV_64FC1);
	private double u = hSz + 1;

	public DirectionalFilter() {
		for (int i = 0; i < filterGauss.rows(); i++)
			filterGauss.put(i, 0, Math.exp(-Math.pow(i - u, 2) / 2 / Math.pow(difScl, 2) / difScl / Math.sqrt(2 * Math.PI)));

		for (int i = 0; i < filterGaussDerivative.rows(); i++)
			filterGaussDerivative.put(i, 0, -(i - u) * Math.exp(-Math.pow(i - u, 2) / 2 / Math.pow(difScl, 2) / Math.pow(difScl, 3) / Math.sqrt(2 * Math.PI)));
	}

	public Mat gx(Mat frame) {
		Mat gx = new Mat();
		Imgproc.sepFilter2D(frame, gx, CvType.CV_64FC1, filterGauss, filterGaussDerivative);
		return gx;
	}

	public Mat gy(Mat frame) {
		Mat gy = new Mat();
		Imgproc.sepFilter2D(frame, gy, CvType.CV_64FC1, filterGaussDerivative, filterGauss);
		return gy;
	}

	public Mat bin(Mat ori, int nBin) {
		Core.add(ori, new Scalar(-Math.PI), ori);

		// for (int row = 0; row < ori.rows(); row++) {
		// for (int col = 0; col < ori.cols(); col++) {
		// System.out.print(ori.get(row, col)[0] + " ");
		// }
		// System.out.println();
		// }
		// System.out.println("--------------------------------------------------------------------");

		List<Double> edges = new ArrayList<>();
		for (double angle = -Math.PI; angle < Math.PI + 0.000001; angle += 2 * Math.PI / (nBin + 1)) {
			edges.add(angle);
		}

		List<Double> edgesBoundary = new ArrayList<>();
		for (int i = 0; i <= nBin; i++) {
			edgesBoundary.add((edges.get(i) + edges.get(i + 1)) / 2);
		}

		Mat binning = Mat.ones(ori.size(), CvType.CV_64FC1);
		for (int i = 0; i < nBin - 1; i++) {
			Mat filtered = new Mat();
			Imgproc.threshold(ori, filtered, edgesBoundary.get(i), 1, Imgproc.THRESH_BINARY);
			Core.addWeighted(binning, 1, filtered, 1, 0, binning);
		}

		double max = edgesBoundary.get(nBin);
		Mat mask = new Mat();
		Core.inRange(ori, new Scalar(max), new Scalar(Double.MAX_VALUE), mask);
		Mat.ones(ori.size(), CvType.CV_64FC1).copyTo(binning, mask);

		Core.inRange(binning, new Scalar(Integer.valueOf(nBin).doubleValue() / 2), new Scalar(1000), mask);
		Core.add(binning, new Scalar(-Integer.valueOf(nBin).doubleValue() / 2), binning, mask);

		return binning;
	}

	public static void main(String[] args) {
		VideoCapture vc = new VideoCapture(0);
		Mat frame = new Mat();
		for (;;) {
			vc.read(frame);
			Imgproc.cvtColor(frame, frame, Imgproc.COLOR_BGR2GRAY);
			DirectionalFilter df = new DirectionalFilter();

			Mat gx = df.gx(frame);
			Mat gy = df.gy(frame);
			Mat mag = new Mat();
			Mat ori = new Mat();
			Core.cartToPolar(gx, gy, mag, ori);
			Mat bin = df.bin(ori, 2 * 64);
			Mat histo = df.getHistogram(mag, bin, 64);

			double maxValue = Double.MIN_VALUE;
			double nbin = Double.MIN_VALUE;
			for (int row = 0; row < histo.rows(); row++) {
				double value = histo.get(row, 0)[0];
				System.out.print((int) value + " ");
				if (value > maxValue) {
					maxValue = value;
					nbin = row;
				}
			}
			System.out.println();
			System.out.println("max : " + maxValue);
			System.out.println("Result : " + nbin / 64 * 180);
			try {
				Thread.sleep(300);
			} catch (InterruptedException e) {
				throw new IllegalStateException(e);
			}
		}

	}

	public Mat getHistogram(Mat mag, Mat binning, int nBin) {
		Mat histogram = Mat.zeros(nBin, 1, CvType.CV_64FC1);
		for (int i = 1; i <= nBin; i++) {
			Mat mask = new Mat();
			Core.inRange(binning, new Scalar(i), new Scalar(i), mask);
			Mat result = Mat.zeros(binning.size(), CvType.CV_64FC1);
			mag.copyTo(result, mask);
			double resul = Core.sumElems(result).val[0];
			histogram.put(i - 1, 0, resul);
		}
		return histogram;
	}

	// public void scale(Mat img,double scale) {
	// int nScale = 15;
	// double scaleFactor = 0.8;
	//
	// Mat[] imgLayers = new Mat[nScale];
	// imgLayers[0] = img;
	//
	// double[] meanMags = new double [nScale];
	// for (int i = 0;i<nScale;i++) {
	// //fprintf(1, 'scale = %d\n', i);
	// // % compute the mean edge density...
	// Mat gx = gx(imgLayers[i-1]);
	// Mat gy = gy(imgLayers[i-1]);
	// Mat mag = new Mat();
	// Mat ori = new Mat();
	// Core.cartToPolar(gx, gy, mag, ori);// original mag is square
	// meanMags[i] = Core.mean(mag).val[0];
	// if( i < nScale)
	// imgLayers [i] = imresize(imgLayers[i-1], scaleFactor);
	// }
	//
	// //% find the first peak from the left..
	// localmax = ( meanMags(2:end-1) > meanMags(1:end-2) ) & ( meanMags(2:end-1) > meanMags(3:end) );
	// maxIndex = find(localmax, 1) + 1;
	// //%[maxVal, maxIndex] = max(meanMags);
	// //% plus 4, is the factor ideal for
	//
	// //our approach (-_-||)
	// if ~isempty(maxIndex)
	// scale = scaleFactor ^ (maxIndex);
	// else
	// scale = scaleFactor ^ 3;
	// //end;
	//
	// imgD = imresize(img, scale);
	// }

}