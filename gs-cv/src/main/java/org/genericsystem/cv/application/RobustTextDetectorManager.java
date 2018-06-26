package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.List;

import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Core;
import org.opencv.core.Core.MinMaxLocResult;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfDouble;
import org.opencv.core.MatOfInt;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfRect;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.features2d.MSER;
import org.opencv.imgproc.Imgproc;
import org.opencv.imgproc.Moments;
import org.opencv.utils.Converters;

public class RobustTextDetectorManager {

	static {
		NativeLibraryLoader.load();
	}

	private final Mat gray;
	private Mat mserMask, cannyMask, mserAndCannyMask, mserAndCannyGrownMask, edgeEnhancedMserMask, edgeEnhancedMserCCMask, filteredStrokeWidthMask;
	private final int delta;

	public RobustTextDetectorManager(Mat gray, int delta) {
		this.gray = gray;
		this.delta = delta;
	}

	public Mat getMserMask() {
		return mserMask != null ? mserMask : (mserMask = buildMserMask(gray));
	}

	private Mat buildMserMask(Mat frame) {
		MSER detector = MSER.create(delta, 10, 2000, 0.25, 0.2, 200, 1.01, 0.03, 5);
		ArrayList<MatOfPoint> regions = new ArrayList<>();
		MatOfRect mor = new MatOfRect();
		detector.detectRegions(frame, regions, mor);
		Mat mserMask = new Mat(frame.size(), CvType.CV_8UC1, new Scalar(0));
		for (MatOfPoint mop : regions) {
			for (Point p : mop.toArray())
				mserMask.put((int) p.y, (int) p.x, 255);
		}
		return mserMask;
	}

	public Mat getMserAndCannyMask() {
		return mserAndCannyMask != null ? mserAndCannyMask : (mserAndCannyMask = buildMserAndCannyMask());
	}

	public Mat getCannyMask() {
		return cannyMask != null ? cannyMask : (cannyMask = buildCannyMask());
	}

	private Mat buildCannyMask() {
		Mat canny = new Mat();
		Imgproc.Canny(gray, canny, 20, 80, 3, false);
		return canny;
	}

	private Mat buildMserAndCannyMask() {
		Mat mserAndCanny = new Mat();
		Core.bitwise_and(getMserMask(), getCannyMask(), mserAndCanny);
		return mserAndCanny;
	}

	public Mat getMserAndCannyGrownMask() {
		return mserAndCannyGrownMask != null ? mserAndCannyGrownMask : (mserAndCannyGrownMask = buildMserAndCannyGrownMask());
	}

	private Mat buildMserAndCannyGrownMask() {
		Mat sobelx = new Mat();
		Imgproc.Sobel(gray, sobelx, CvType.CV_64F, 1, 0, -1, 1, 0);
		Mat sobely = new Mat();
		Imgproc.Sobel(gray, sobely, CvType.CV_64F, 0, 1, -1, 1, 0);
		Mat gradAngle = new Mat();
		Mat gradMag = new Mat();
		Core.cartToPolar(sobelx, sobely, gradMag, gradAngle, false);
		Mat edgeMser = getMserAndCannyMask();
		Mat edgeGrown = new Mat();
		edgeMser.copyTo(edgeGrown);
		for (int i = 0; i < edgeGrown.height(); i++)
			for (int j = 0; j < edgeGrown.width(); j++)
				if (edgeMser.get(i, j)[0] != 0) {
					int x1 = j;
					int y1 = i;
					for (int l = 0; l < 2; l++) {
						int length = l + 1;
						double angle = gradAngle.get(y1, x1)[0];
						x1 = j + (int) Math.round(length * Math.cos(angle));
						y1 = i + (int) Math.round(length * Math.sin(angle));
						if (x1 >= edgeGrown.width() || y1 >= edgeGrown.height() || x1 < 0 || y1 < 0)
							break;
						edgeGrown.put(y1, x1, 255);
					}
				}
		return edgeGrown;

	}

	public Mat getEdgeEnhancedMserMask() {
		return edgeEnhancedMserMask != null ? edgeEnhancedMserMask : (edgeEnhancedMserMask = buildEdgedEnhancedMserMask());
	}

	public Mat buildEdgedEnhancedMserMask() {
		Mat notMserAndCannyGrown = new Mat();
		Core.bitwise_not(getMserAndCannyGrownMask(), notMserAndCannyGrown);
		Mat edgeEnhancedMser = new Mat();
		Core.bitwise_and(notMserAndCannyGrown, mserMask, edgeEnhancedMser);
		return edgeEnhancedMser;
	}

	public Mat getEdgeEnhanceMserCCMask() {
		return edgeEnhancedMserCCMask != null ? edgeEnhancedMserCCMask : (edgeEnhancedMserCCMask = buildEdgeEnhancedMserCCMask());
	}

	private Mat buildEdgeEnhancedMserCCMask() {
		Mat labels = new Mat();
		Mat stats = new Mat();
		Mat centroid = new Mat();
		int labelsIds = Imgproc.connectedComponentsWithStats(getEdgeEnhancedMserMask(), labels, stats, centroid, 8, CvType.CV_32S);
		Mat edgeEnhancedMserCCMask = Mat.zeros(labels.size(), CvType.CV_8UC1);
		for (int labelId = 0; labelId < labelsIds; labelId++) {
			double area = stats.get(labelId, Imgproc.CC_STAT_AREA)[0];
			if (area < 3 || area > 200)
				continue;

			Mat labelMask = new Mat();
			Core.inRange(labels, new Scalar(labelId), new Scalar(labelId), labelMask);
			Moments moment = Imgproc.moments(labelMask);

			double left_comp = (moment.nu20 + moment.nu02) / 2.0;
			double right_comp = Math.sqrt((4 * moment.nu11 * moment.nu11) + (moment.nu20 - moment.nu02) * (moment.nu20 - moment.nu02)) / 2.0;

			double eig_val_1 = left_comp + right_comp;
			double eig_val_2 = left_comp - right_comp;

			double eccentricity = Math.sqrt(1.0 - (eig_val_2 / eig_val_1));
			double minEccentricity = 0.1;
			double maxEccentricity = 0.995;

			// if (eccentricity < minEccentricity || eccentricity > maxEccentricity) {
			// continue;
			// }
			List<MatOfPoint> contours = new ArrayList<>();
			Imgproc.findContours(labelMask, contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
			MatOfInt hull = new MatOfInt();
			Imgproc.convexHull(contours.get(0), hull);
			MatOfPoint mopHull = new MatOfPoint();
			mopHull.create((int) hull.size().height, 1, CvType.CV_32SC2);
			for (int j = 0; j < hull.size().height; j++) {
				int index = (int) hull.get(j, 0)[0];
				double[] point = new double[] { contours.get(0).get(index, 0)[0], contours.get(0).get(index, 0)[1] };
				mopHull.put(j, 0, point);
			}
			double solidity = area / Imgproc.contourArea(mopHull);
			double minSolidity = 0.35;// 0.5
			if (solidity < minSolidity)
				continue;
			Core.bitwise_or(edgeEnhancedMserCCMask, labelMask, edgeEnhancedMserCCMask);
		}
		return edgeEnhancedMserCCMask;
	}

	public Mat getFilteredStrokeWidthMask() {
		return filteredStrokeWidthMask != null ? filteredStrokeWidthMask : (filteredStrokeWidthMask = buildFilteredStrokeWidthMask());
	}

	private Mat buildFilteredStrokeWidthMask() {
		// // bw8u : we want to calculate the SWT of this. NOTE: Its background pixels are 0 and forground pixels are 1 (not 255!)
		// Mat bw32f, swt32f, kernel;
		// double min, max;
		// int strokeRadius;
		//
		// bw8u.convertTo(bw32f, CV_32F); // format conversion for multiplication
		// distanceTransform(bw8u, swt32f, CV_DIST_L2, 5); // distance transform
		// minMaxLoc(swt32f, NULL, &max); // find max
		// strokeRadius = (int)ceil(max); // half the max stroke width
		// kernel = getStructuringElement(MORPH_RECT, Size(3, 3)); // 3x3 kernel used to select 8-connected neighbors
		//
		// for (int j = 0; j < strokeRadius; j++)
		// {
		// dilate(swt32f, swt32f, kernel); // assign the max in 3x3 neighborhood to each center pixel
		// swt32f = swt32f.mul(bw32f); // apply mask to restore original shape and to avoid unnecessary max propogation
		// }
		// // swt32f : resulting SWT image

		Mat normalized = new Mat();
		Imgproc.threshold(getEdgeEnhanceMserCCMask(), normalized, 1, 1, Imgproc.THRESH_BINARY);

		Mat mask32F = new Mat();
		normalized.convertTo(mask32F, CvType.CV_32F);

		Mat result32F = new Mat();
		Imgproc.distanceTransform(normalized, result32F, Imgproc.DIST_L2, 5);

		MinMaxLocResult minMaxLocResult = Core.minMaxLoc(result32F);
		int strokeRadius = (int) Math.ceil(minMaxLocResult.maxVal);
		Mat kernel = Imgproc.getStructuringElement(Imgproc.MORPH_RECT, new Size(3, 3));

		for (int j = 0; j < strokeRadius; j++) {
			Imgproc.dilate(result32F, result32F, kernel); // assign the max in 3x3 neighborhood to each center pixel
			result32F = result32F.mul(mask32F); // apply mask to restore original shape and to avoid unnecessary max propogation
		}

		Mat strokeWidth = result32F;// computeStrokeWidth(result32F);
		Mat filteredStrokeWidth = new Mat(strokeWidth.size(), CvType.CV_8UC1, new Scalar(0));
		Mat strokeWithCV8U = new Mat();
		strokeWidth.convertTo(strokeWithCV8U, CvType.CV_8UC1);
		Mat labels = new Mat();
		Mat stats = new Mat();
		Mat centroid = new Mat();
		int labelsIds = Imgproc.connectedComponentsWithStats(strokeWithCV8U, labels, stats, centroid, 8, CvType.CV_32S);
		for (int labelId = 0; labelId < labelsIds; labelId++) {
			Mat labelMask = new Mat();
			Core.inRange(labels, new Scalar(labelId), new Scalar(labelId), labelMask);
			Mat temp = new Mat(strokeWithCV8U.size(), strokeWithCV8U.type(), new Scalar(0));
			strokeWithCV8U.copyTo(temp, labelMask);
			int area = Core.countNonZero(temp);
			MatOfDouble meanD = new MatOfDouble();
			MatOfDouble stdDev = new MatOfDouble();
			Core.meanStdDev(strokeWithCV8U, meanD, stdDev, labelMask);

			if (area != 0) {
				/* Filter out those which are out of the prespecified ratio */
				if ((stdDev.get(0, 0)[0] / meanD.get(0, 0)[0]) > 0.3)
					continue;

				/* Collect the filtered stroke width */
				Core.bitwise_or(filteredStrokeWidth, labelMask, filteredStrokeWidth);
			}
		}
		return filteredStrokeWidth;
	}

	private static int booleansToInt(boolean[] arr) {
		int n = 0;
		for (boolean b : arr)
			n = (n << 1) | (b ? 1 : 0);
		return n;
	}

	private static int getNeighborsLessThan(Mat mat, int y, int x) {
		boolean[] neighbors = new boolean[8];
		neighbors[0] = mat.get(y, x - 1)[0] == 0 ? false : mat.get(y, x - 1)[0] < mat.get(y, x)[0];
		neighbors[1] = mat.get(y - 1, x - 1)[0] == 0 ? false : mat.get(y - 1, x - 1)[0] < mat.get(y, x)[0];
		neighbors[2] = mat.get(y - 1, x)[0] == 0 ? false : mat.get(y - 1, x)[0] < mat.get(y, x)[0];
		neighbors[3] = mat.get(y - 1, x + 1)[0] == 0 ? false : mat.get(y - 1, x + 1)[0] < mat.get(y, x)[0];
		neighbors[4] = mat.get(y, x + 1)[0] == 0 ? false : mat.get(y, x + 1)[0] < mat.get(y, x)[0];
		neighbors[5] = mat.get(y + 1, x + 1)[0] == 0 ? false : mat.get(y + 1, x + 1)[0] < mat.get(y, x)[0];
		neighbors[6] = mat.get(y + 1, x)[0] == 0 ? false : mat.get(y + 1, x)[0] < mat.get(y, x)[0];
		neighbors[7] = mat.get(y + 1, x - 1)[0] == 0 ? false : mat.get(y + 1, x - 1)[0] < mat.get(y, x)[0];
		return booleansToInt(neighbors);
	}

	private static Mat computeStrokeWidth(Mat dist) {
		/* Pad the distance transformed matrix to avoid boundary checking */
		Mat padded = new Mat(dist.rows() + 1, dist.cols() + 1, dist.type(), new Scalar(0));
		dist.copyTo(new Mat(padded, new Rect(1, 1, dist.cols(), dist.rows())));

		Mat lookup = new Mat(padded.size(), CvType.CV_8UC1, new Scalar(0));
		for (int y = 1; y < padded.rows() - 1; y++) {
			for (int x = 1; x < padded.cols() - 1; x++) {
				/* Extract all the neighbors whose value < curr_ptr[x], encoded in 8-bit uchar */
				if (padded.get(y, x)[0] != 0)
					lookup.put(y, x, (double) getNeighborsLessThan(padded, y, x));
			}
		}

		/* Get max stroke from the distance transformed */
		MinMaxLocResult minMaxLocResult = Core.minMaxLoc(padded);
		int maxStroke = (int) Math.round(minMaxLocResult.maxVal);
		for (double stroke = maxStroke; stroke > 0; stroke--) {
			Mat stroke_indices_mat = new Mat();
			Mat mask = new Mat();
			Core.inRange(padded, new Scalar(stroke - 0.1), new Scalar(stroke + 0.1), mask);

			Mat masked = new Mat();
			padded.copyTo(masked, mask);
			masked.convertTo(masked, CvType.CV_8UC1);
			Core.findNonZero(masked, stroke_indices_mat);
			List<Point> stroke_indices = new ArrayList<>();
			if (stroke_indices_mat.cols() > 0)
				Converters.Mat_to_vector_Point(stroke_indices_mat, stroke_indices);

			List<Point> neighbors = new ArrayList<>();
			for (Point stroke_index : stroke_indices) {
				List<Point> temp = convertToCoords((int) stroke_index.x, (int) stroke_index.y, (int) lookup.get((int) stroke_index.y, (int) stroke_index.x)[0]);
				neighbors.addAll(temp);
			}

			while (!neighbors.isEmpty()) {
				for (Point neighbor : neighbors)
					padded.put((int) neighbor.y, (int) neighbor.x, stroke);
				neighbors.clear();
				List<Point> temp = new ArrayList<>(neighbors);
				neighbors.clear();
				/* Recursively gets neighbors of the current neighbors */
				for (Point neighbor : temp) {
					List<Point> temp2 = convertToCoords((int) neighbor.x, (int) neighbor.y, (int) lookup.get((int) neighbor.y, (int) neighbor.x)[0]);
					neighbors.addAll(temp2);
				}
			}

		}

		return new Mat(padded, new Rect(1, 1, dist.cols(), dist.rows()));
	}

	private static List<Point> convertToCoords(int x, int y, int neighbors) {
		List<Point> coords = new ArrayList<>();
		if (((neighbors & ((int) Math.pow(2, 7))) != 0))
			coords.add(new Point(x - 1, y));
		if (((neighbors & ((int) Math.pow(2, 6))) != 0))
			coords.add(new Point(x - 1, y - 1));
		if (((neighbors & ((int) Math.pow(2, 5))) != 0))
			coords.add(new Point(x, y - 1));
		if (((neighbors & ((int) Math.pow(2, 4))) != 0))
			coords.add(new Point(x + 1, y - 1));
		if (((neighbors & ((int) Math.pow(2, 3))) != 0))
			coords.add(new Point(x + 1, y));
		if (((neighbors & ((int) Math.pow(2, 2))) != 0))
			coords.add(new Point(x + 1, y + 1));
		if (((neighbors & ((int) Math.pow(2, 1))) != 0))
			coords.add(new Point(x, y + 1));
		if (((neighbors & ((int) Math.pow(2, 0))) != 0))
			coords.add(new Point(x - 1, y + 1));
		return coords;
	}

	public static int toBin(double angle, int neighbors) {
		float divisor = 180.0f / neighbors;
		return (int) ((((Math.floor(angle / divisor) - 1) / 2) + 1) % neighbors + 1);
	}

	public static Mat growEdges(Mat gray, Mat edges) {
		Mat grad_x = new Mat(), grad_y = new Mat();

		Imgproc.Sobel(gray, grad_x, CvType.CV_64FC1, 1, 0, -1, 1, 0);
		Imgproc.Sobel(gray, grad_y, CvType.CV_64FC1, 0, 1, -1, 1, 0);
		Mat grad_mag = new Mat(), grad_dir = new Mat();

		Core.cartToPolar(grad_x, grad_y, grad_mag, grad_dir, true);

		/*
		 * Convert the angle into predefined 3x3 neighbor locations | 2 | 3 | 4 | | 1 | 0 | 5 | | 8 | 7 | 6 |
		 */
		for (int y = 0; y < grad_dir.rows(); y++)
			for (int x = 0; x < grad_dir.cols(); x++)
				grad_dir.put(y, x, toBin((grad_dir.get(y, x))[0], 8));
		grad_dir.convertTo(grad_dir, CvType.CV_8UC1);

		/* Perform region growing based on the gradient direction */
		Mat result = new Mat();
		edges.copyTo(result);
		for (int y = 1; y < edges.rows() - 1; y++) {
			for (int x = 1; x < edges.cols() - 1; x++) {
				/* Only consider the contours */
				if (edges.get(y, x)[0] != 0) {

					/* .. there should be a better way .... */
					switch ((int) grad_dir.get(y, x)[0]) {
					case 1:
						result.put(y, x + 1, 255);
						break;
					case 2:
						result.put(y + 1, x + 1, 255);
						break;
					case 3:
						result.put(y + 1, x, 255);
						break;
					case 4:
						result.put(y + 1, x - 1, 255);
						break;
					case 5:
						result.put(y, x - 1, 255);
						break;
					case 6:
						result.put(y - 1, x - 1, 255);
						break;
					case 7:
						result.put(y - 1, x, 255);
						break;
					case 8:
						result.put(y - 1, x + 1, 255);
						break;

					default:
						System.out.println("Error : " + (int) grad_dir.get(y, x)[0]);
						break;
					}
				}
			}

		}

		return result;
	}
}
