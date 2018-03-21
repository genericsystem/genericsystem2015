package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.genericsystem.cv.utils.Tools;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Range;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;
import org.opencv.videoio.VideoCapture;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class DirectionalFilter extends AbstractApp {

	private static final Logger logger = LoggerFactory.getLogger(DirectionalFilter.class);

	static {
		NativeLibraryLoader.load();
	}

	private final double difScl = 0.7;
	private final int hSz = (int) Math.ceil(3 * difScl);
	private final Mat filterGauss = Mat.zeros(2 * hSz + 1, 1, CvType.CV_64FC1);
	private final Mat filterGaussDerivative = Mat.zeros(2 * hSz + 1, 1, CvType.CV_64FC1);
	private double u = hSz + 1;
	// Store results of orientDistance for speed (works because the second argument of orientDistance is always the same).
	private final Map<Integer, int[]> orientDistances = new HashMap<>();

	public DirectionalFilter() {
		for (int i = 0; i < filterGauss.rows(); i++)
			filterGauss.put(i, 0, Math.exp(-Math.pow(i - u, 2) / 2 / Math.pow(difScl, 2)) / difScl / Math.sqrt(2 * Math.PI));

		for (int i = 0; i < filterGaussDerivative.rows(); i++)
			filterGaussDerivative.put(i, 0, -(i - u) * Math.exp(-Math.pow(i - u, 2) / 2 / Math.pow(difScl, 2)) / Math.pow(difScl, 3) / Math.sqrt(2 * Math.PI));
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		int firstBin = 1;
		int nBin = 64;
		int nSide = 20;
		int lambda = 7;
		VideoCapture vc = new VideoCapture(0);
		Mat frame = new Mat();
		for (;;) {
			vc.read(frame);
			Imgproc.cvtColor(frame, frame, Imgproc.COLOR_BGR2GRAY);

			Mat scaledFrame = scale(frame);
			Mat gx = gx(scaledFrame);
			Mat gy = gy(scaledFrame);
			Mat mag = new Mat();
			Mat ori = new Mat();
			Core.cartToPolar(gx, gy, mag, ori);
			Mat bin = bin(ori, nBin);

			double[] histo = getHistogram(mag, bin, nBin);

			double maxValue = Double.MIN_VALUE;
			double nbin = Double.MIN_VALUE;
			for (int row = 0; row < histo.length; row++) {
				double value = histo[row];
				// System.out.print((int) value + " ");
				if (value > maxValue) {
					maxValue = value;
					nbin = row;
				}
			}
			System.out.println("Result : " + nbin);
			System.out.println(scaledFrame);
			Mat dirs = findSecondDirection(scaledFrame, bin, mag, nSide, firstBin, nBin, lambda);
			System.out.println("Directions: ");
			for (int row = 0; row < dirs.rows(); row++) {
				for (int col = 0; col < dirs.cols(); col++)
					System.out.printf("%2d ", (int) dirs.get(row, col)[0]);
				System.out.println();
			}
			Mat imgDirs = addDirs(scaledFrame, dirs, nSide, nBin);
			mainGrid.add(new ImageView(Tools.mat2jfxImage(scaledFrame)), 0, 0);
			mainGrid.add(new ImageView(Tools.mat2jfxImage(imgDirs)), 1, 0);
			gx.release();
			gy.release();
			mag.release();
			ori.release();
			bin.release();
			imgDirs.release();
		}
	}

	public Mat addDirs(Mat img, Mat dirs, int nSide, int nBin) {
		// TODO: Modify findSecondDirection so it returns these lists.
		List<Integer> patchXs = imgPartition(img, nSide, .5f, false);
		List<Integer> patchYs = imgPartition(img, nSide, .5f, true);
		Mat imgDirs = new Mat();
		img.copyTo(imgDirs);
		imgDirs.convertTo(imgDirs, CvType.CV_8SC3);
		for (int j = 0; j < patchXs.size(); j++)
			for (int i = 0; i < patchYs.size(); i++) {
				int centerX = patchXs.get(j) + nSide / 2;
				int centerY = patchYs.get(i) + nSide / 2;
				Imgproc.line(imgDirs, new Point(centerX, centerY), getLineEnd(centerX, centerY, (int) dirs.get(i, j)[0], nBin, nSide / 3), new Scalar(0, 0, 0), 2);
			}
		return imgDirs;
	}

	public Point getLineEnd(int startX, int startY, int dir, int nBin, int length) {
		double step = Math.PI / nBin;
		double theta = (dir - .5) * step;
		return new Point(startX + length * Math.cos(theta), startY + length * Math.sin(theta));
	}

	public Mat gx(Mat frame) {
		Mat gx = new Mat();
		Imgproc.sepFilter2D(frame, gx, CvType.CV_64FC1, filterGauss, filterGaussDerivative, new Point(-1, -1), 0, Core.BORDER_REPLICATE);
		return cleanContour(gx);
	}

	public Mat gy(Mat frame) {
		Mat gy = new Mat();
		Imgproc.sepFilter2D(frame, gy, CvType.CV_64FC1, filterGaussDerivative, filterGauss, new Point(-1, -1), 0, Core.BORDER_REPLICATE);
		return cleanContour(gy);
	}

	public Mat cleanContour(Mat mat) {
		for (int row = 0; row < mat.rows(); row++) {
			mat.put(row, 0, 0);
			mat.put(row, mat.cols() - 1, 0);
		}
		for (int col = 0; col < mat.cols(); col++) {
			mat.put(0, col, 0);
			mat.put(mat.rows() - 1, col, 0);
		}
		return mat;
	}

	// Returns table of ints between 1 and nBin.
	// Return value at indices (i, j) == b iff
	// (b - 1) Pi / nBin < ori(i, j) + Pi / (2 nBin) mod Pi <= b Pi / nBin
	public Mat bin(Mat ori, int nBin) {
		double step = Math.PI / nBin;
		Mat binning = Mat.zeros(ori.size(), CvType.CV_64FC1);
		for (int r = 0; r < binning.rows(); r++)
			for (int c = 0; c < binning.cols(); c++) {
				double angle = ori.get(r, c)[0] + step / 2;
				int bin = (int) Math.ceil(angle / step);
				while (bin > nBin)
					bin -= nBin;
				binning.put(r, c, bin);
			}

		//		for (int row = 0; row < ori.rows(); row++) {
		//			for (int col = 0; col < ori.cols(); col++) {
		//				System.out.printf("%3d ", (int) (ori.get(row, col)[0] * 360 / (2 * Math.PI)));
		//			}
		//			System.out.println();
		//		}

		//		for (int row = 0; row < binning.rows(); row++) {
		//			for (int col = 0; col < binning.cols(); col++) {
		//				System.out.printf("%2d ", (int) binning.get(row, col)[0]);
		//			}
		//			System.out.println();
		//		}
		return binning;
	}

	public static void main(String[] args) {
		launch(args);
	}

	public double[] getHistogram(Mat mag, Mat binning, int nBin) {
		double[] histogram = new double[nBin];
		for (int i = 0; i < nBin; i++) {
			Mat mask = new Mat();
			Core.inRange(binning, new Scalar(i + 1), new Scalar(i + 1), mask);
			Mat result = Mat.zeros(binning.size(), CvType.CV_64FC1);
			mag.copyTo(result, mask);
			double resul = Core.sumElems(result).val[0];
			histogram[i] = resul;
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
		Core.pow(mag, 2, mag);
		double result = Core.mean(mag).val[0];
		mag.release();
		ori.release();
		return result;
	}

	public Mat scale(Mat img) {
		int nScale = 10;
		double scaleFactor = 0.8;

		Mat[] imgLayers = new Mat[nScale];
		imgLayers[0] = img;

		Double[] meanMags = new Double[nScale];
		for (int i = 0; i < nScale; i++) {
			meanMags[i] = getMeanMag(imgLayers[i]);
			// System.out.println("Mean : " + meanMags[i]);
			if (i < nScale - 1) {
				imgLayers[i + 1] = new Mat();
				double scale = Math.pow(scaleFactor, i + 1);
				Imgproc.resize(imgLayers[0], imgLayers[i + 1], new Size(0, 0), scale, scale, Imgproc.INTER_CUBIC);
			}
		}
		int maxIndex;
		for (maxIndex = 1; maxIndex < meanMags.length - 1; maxIndex++) {
			if (meanMags[maxIndex] > meanMags[maxIndex - 1] && meanMags[maxIndex] > meanMags[maxIndex + 1])
				break;
		}

		double scale = Math.pow(scaleFactor, Integer.valueOf(maxIndex).doubleValue());
		// System.out.println(" Scale : " + scale);
		Mat result = new Mat();
		Imgproc.resize(img, result, new Size(0, 0), scale, scale, Imgproc.INTER_CUBIC);
		for (int i = 1; i < imgLayers.length; i++)
			imgLayers[i].release();
		return result;
	}

	// TODO: Split
	public Mat findSecondDirection(Mat img, Mat binning, Mat mag, int nSide, int firstBin, int nBin, int lambda) {
		float ratio = .5f;
		List<Integer> patchXs = imgPartition(img, nSide, ratio, false);
		List<Integer> patchYs = imgPartition(img, nSide, ratio, true);
		int nXs = patchXs.size();
		int nYs = patchYs.size();

		// Step 1: Find local histograms.
		double[][][] hists = new double[nXs][nYs][nBin];
		for (int i = 0; i < nXs; i++) {
			Range xSel = new Range(patchXs.get(i), patchXs.get(i) + nSide);
			for (int j = 0; j < nYs; j++) {
				Range ySel = new Range(patchYs.get(j), patchYs.get(j) + nSide);
				hists[i][j] = getHistogram(new Mat(mag, ySel, xSel), new Mat(binning, ySel, xSel), nBin);
			}
		}

		// Step 2: Find intersecting histograms.
		List<int[]> histsIntersectLabels = new ArrayList<>();
		List<double[]> histsIntersect = new ArrayList<>();
		for (int i1 = 0; i1 < nXs; i1++) {
			Range xSel1 = new Range(patchXs.get(i1), patchXs.get(i1) + nSide);
			for (int i2 = 0; i2 < nXs; i2++) {
				Range xSel2 = new Range(patchXs.get(i2), patchXs.get(i2) + nSide);

				Range xSel = intersect(xSel1, xSel2);
				if (xSel.empty())
					continue;

				for (int j1 = 0; j1 < nYs; j1++) {
					Range ySel1 = new Range(patchYs.get(j1), patchYs.get(j1) + nSide);
					for (int j2 = 0; j2 < nYs; j2++) {
						Range ySel2 = new Range(patchYs.get(j2), patchYs.get(j2) + nSide);

						Range ySel = intersect(ySel1, ySel2);
						if (i1 == i2 && j1 == j2 || ySel.empty())
							continue;

						histsIntersectLabels.add(new int[] { i1, j1, i2, j2 });
						histsIntersect.add(getHistogram(new Mat(mag, ySel, xSel), new Mat(binning, ySel, xSel), nBin));
					}
				}
			}
		}

		// Step 3: Coordinate descent.
		int initGuess = 32;
		Mat dirs = new Mat(nYs, nXs, CvType.CV_32S, new Scalar(initGuess));
		int maxIter = 100;
		double funcVal = Double.MAX_VALUE;

		for (int iter = 0; iter < maxIter; iter++) {
			double prevFuncVal = funcVal;
			funcVal = computeObjective(dirs, mag, binning, firstBin, nBin, patchXs, patchYs, nSide, lambda);

			logger.info("Iteration {}, funcVal = {}.", iter, funcVal);

			if (Math.abs(prevFuncVal - funcVal) < Math.pow(10, -8) * Math.abs(funcVal))
				break;

			for (int i = 0; i < nXs; i++)
				for (int j = 0; j < nYs; j++) {
					List<Integer> indices = new ArrayList<>();
					for (int ind = 0; ind < histsIntersectLabels.size(); ind++) {
						int[] labels = histsIntersectLabels.get(ind);
						if (labels[0] == i && labels[1] == j)
							indices.add(ind);
					}
					int nNeighbor = indices.size();

					double[][] histograms = new double[nBin][nNeighbor + 1];
					int[] dirsThis = new int[nNeighbor + 1];

					for (int k = 0; k < nNeighbor; k++) {
						int histIndex = indices.get(k);
						for (int r = 0; r < nBin; r++)
							histograms[r][k] = histsIntersect.get(histIndex)[r];
						int intersectI = histsIntersectLabels.get(histIndex)[2];
						int intersectJ = histsIntersectLabels.get(histIndex)[3];
						dirsThis[k] = (int) dirs.get(intersectJ, intersectI)[0];
					}
					// Histogram of this region.
					for (int r = 0; r < nBin; r++)
						histograms[r][nNeighbor] = hists[i][j][r];

					double[] incValues = new double[nBin];
					for (int candidateDir = 0; candidateDir < nBin; candidateDir++) {
						dirsThis[nNeighbor] = candidateDir;
						incValues[candidateDir] = computeObjectiveIJ(histograms, dirsThis, lambda, firstBin);
					}

					double minValue = Double.MAX_VALUE;
					int minDir = -1;
					for (int k = 0; k < incValues.length; k++)
						if (incValues[k] < minValue) {
							minValue = incValues[k];
							minDir = k;
						}

					dirs.put(i, j, minDir);
				}
		}
		return dirs;
	}

	public double computeObjectiveIJ(double[][] histograms, int[] dirs, int lambda, int firstBin) {
		int nBin = histograms.length;
		int nHist = histograms[0].length;
		double[] dists = new double[nBin];

		for (int i = 0; i < nHist; i++) {
			int[] distance = orientDistance(dirs[i], firstBin, nBin);
			for (int j = 0; j < nBin; j++)
				dists[j] = dists[j] + (distance[j] - lambda) * (int) histograms[j][i];
		}
		double sum = 0;
		for (double elt : dists)
			if (elt < 0)
				sum += elt;
		return sum;
	}

	// TODO: Return mask of selected pixels.
	public double computeObjective(Mat dirs, Mat mag, Mat binning, int firstBin, int nBin, List<Integer> patchXs, List<Integer> patchYs, int nSide, int lambda) {
		int nXs = patchXs.size();
		int nYs = patchYs.size();
		Mat dists = new Mat(mag.size(), CvType.CV_64FC1);
		Mat binPatch = new Mat();

		for (int i = 0; i < nXs; i++) {
			Range xSel = new Range(patchXs.get(i), patchXs.get(i) + nSide);
			for (int j = 0; j < nYs; j++) {
				Range ySel = new Range(patchYs.get(j), patchYs.get(j) + nSide);
				int[] distance = orientDistance((int) dirs.get(j, i)[0], firstBin, nBin);
				for (int k = ySel.start; k < ySel.end; k++)
					for (int l = xSel.start; l < xSel.end; l++)
						dists.put(k, l, dists.get(k, l)[0] + (distance[(int) binning.get(k, l)[0] - firstBin] - lambda) * mag.get(k, l)[0]);
			}
		}
		binPatch.release();
		double sum = 0;
		for (int i = 0; i < dists.rows(); i++)
			for (int j = 0; j < dists.cols(); j++) {
				double c = dists.get(i, j)[0];
				sum += c < 0 ? c : 0;
			}

		dists.release();
		return sum;
	}

	public int[] orientDistance(int ind, int firstBin, int nBin) {
		if (!orientDistances.containsKey(ind)) {
			int[] distances = new int[nBin];
			for (int i = 0; i < nBin; i++)
				distances[i] = computeDistance(ind, firstBin + i, nBin);
			orientDistances.put(ind, distances);
		}
		return orientDistances.get(ind);
	}

	private int computeDistance(int ind, int other, int nBin) {
		return Math.min(Math.min(Math.abs(ind - other), Math.abs(ind - other - nBin)), Math.abs(ind - other + nBin));
	}

	public Range intersect(Range r1, Range r2) {
		int start = Math.max(r1.start, r2.start);
		int end = Math.min(r1.end, r2.end);
		return new Range(start, end);
	}

	public List<Integer> imgPartition(Mat img, int w, float ratio, boolean vertical) {
		int length;
		if (vertical)
			length = img.height();
		else
			length = img.width();
		int step;
		if (Math.abs(ratio) < Math.pow(10, -8))
			step = 1;
		else
			step = (int) Math.floor(w * ratio);
		List<Integer> patches = new ArrayList<>();
		int x = 0;
		do {
			patches.add(x);
			x += step;
		} while (x <= length - w);
		if (length - w - patches.get(patches.size() - 1) > step / 2)
			patches.add(length - w);
		return patches;
	}
}