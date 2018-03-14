package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.List;

import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.opencv.videoio.VideoCapture;

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
			filterGauss.put(i, 0, Math.exp(-Math.pow(i - u, 2) / 2 / Math.pow(difScl, 2)) / difScl / Math.sqrt(2 * Math.PI));

		for (int i = 0; i < filterGaussDerivative.rows(); i++)
			filterGaussDerivative.put(i, 0, -(i - u) * Math.exp(-Math.pow(i - u, 2) / 2 / Math.pow(difScl, 2)) / Math.pow(difScl, 3) / Math.sqrt(2 * Math.PI));
	}

	public Mat gx(Mat frame) {
		Mat gx = new Mat();
		Imgproc.sepFilter2D(frame, gx, CvType.CV_64FC1, filterGauss, filterGaussDerivative, new Point(-1, -1), 0, Core.BORDER_REPLICATE);
		return gx;
	}

	public Mat gy(Mat frame) {
		Mat gy = new Mat();
		Imgproc.sepFilter2D(frame, gy, CvType.CV_64FC1, filterGaussDerivative, filterGauss, new Point(-1, -1), 0, Core.BORDER_REPLICATE);
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
			filtered.release();
		}

		double max = edgesBoundary.get(nBin);
		// System.out.println("" + nBin + " " + max);
		Mat mask = new Mat();
		Core.inRange(ori, new Scalar(max), new Scalar(Double.MAX_VALUE), mask);

		Mat toCopy = Mat.ones(ori.size(), CvType.CV_64FC1);
		toCopy.copyTo(binning, mask);
		toCopy.release();
		// for (int row = 0; row < ori.rows(); row++) {
		// for (int col = 0; col < ori.cols(); col++) {
		// System.out.print(ori.get(row, col)[0] + " ");
		// }
		// System.out.println();
		// }

		// for (int row = 0; row < mask.rows(); row++) {
		// for (int col = 0; col < mask.cols(); col++) {
		// System.out.print(mask.get(row, col)[0] + " ");
		// }
		// System.out.println();
		// }

		Core.inRange(binning, new Scalar(Integer.valueOf(nBin).doubleValue() / 2 + 1), new Scalar(Double.MAX_VALUE), mask);
		Core.add(binning, new Scalar(-Integer.valueOf(nBin).doubleValue() / 2), binning, mask);
		mask.release();
		// for (int row = 0; row < binning.rows(); row++) {
		// for (int col = 0; col < binning.cols(); col++) {
		// System.out.print(binning.get(row, col)[0] + " ");
		// }
		// System.out.println();
		// }
		return binning;
	}

	public static void main(String[] args) {
		VideoCapture vc = new VideoCapture(0);
		Mat frame = new Mat();
		DirectionalFilter df = new DirectionalFilter();
		for (;;) {
			vc.read(frame);

			Imgproc.cvtColor(frame, frame, Imgproc.COLOR_BGR2GRAY);

			Mat scaledFrame = df.scale(frame);
			Mat gx = df.gx(scaledFrame);
			Mat gy = df.gy(scaledFrame);
			Mat mag = new Mat();
			Mat ori = new Mat();
			Core.cartToPolar(gx, gy, mag, ori);
			Mat bin = df.bin(ori, 2 * 64);

			Mat histo = df.getHistogram(mag, bin, 64);

			double maxValue = Double.MIN_VALUE;
			double nbin = Double.MIN_VALUE;
			for (int row = 0; row < histo.rows(); row++) {
				double value = histo.get(row, 0)[0];
				// System.out.print((int) value + " ");
				if (value > maxValue) {
					maxValue = value;
					nbin = row;
				}
			}
			System.out.println("Result : " + nbin);
			frame.release();
			scaledFrame.release();
			gx.release();
			gy.release();
			mag.release();
			ori.release();
			bin.release();
			histo.release();
		}
	}

	public Mat getHistogram(Mat mag, Mat binning, int nBin) {
		Mat histogram = Mat.zeros(nBin, 1, CvType.CV_64FC1);
		for (int i = 1; i <= nBin; i++) {
			Mat mask = new Mat();
			Core.inRange(binning, new Scalar(Integer.valueOf(i).doubleValue()), new Scalar(Integer.valueOf(i).doubleValue()), mask);
			Mat result = Mat.zeros(binning.size(), CvType.CV_64FC1);
			mag.copyTo(result, mask);
			double resul = Core.sumElems(result).val[0];
			histogram.put(i - 1, 0, resul);
			result.release();
			mask.release();
		}
		return histogram;
	}

	public double getMeanMag(Mat layer) {
		Mat gx = gx(layer);
		Mat gy = gy(layer);
		Mat mag = new Mat();
		Mat ori = new Mat();
		Core.cartToPolar(gx, gy, mag, ori);// original mag is square
		gx.release();
		gy.release();
		double result = Core.mean(mag).val[0];
		mag.release();
		ori.release();
		return result;
	}

	public Mat scale(Mat img) {
		int nScale = 15;
		double scaleFactor = 0.8;

		Mat[] imgLayers = new Mat[nScale];
		imgLayers[0] = img;

		Double[] meanMags = new Double[nScale];
		for (int i = 0; i < nScale; i++) {
			meanMags[i] = getMeanMag(imgLayers[i]);
			// System.out.println("Mean : " + meanMags[i]);
			if (i < nScale - 1) {
				imgLayers[i + 1] = new Mat();
				Imgproc.resize(imgLayers[i], imgLayers[i + 1], new Size(imgLayers[i].width() * scaleFactor, imgLayers[i].height() * scaleFactor), 0, 0, Imgproc.INTER_CUBIC);
			}
		}
		int maxIndex;
		for (maxIndex = 1; maxIndex < meanMags.length - 1; maxIndex++) {
			if (meanMags[maxIndex] > meanMags[maxIndex - 1] && meanMags[maxIndex] > meanMags[maxIndex + 1])
				break;
		}

		double scale = Math.pow(scaleFactor, maxIndex);
		// System.out.println("Index : " + maxIndex + " Scale : " + scale);
		Mat imgD = new Mat();
		Imgproc.resize(img, imgD, new Size(img.width() * scale, img.height() * scale));
		for (Mat layer : imgLayers)
			layer.release();
		return imgD;
	}

}