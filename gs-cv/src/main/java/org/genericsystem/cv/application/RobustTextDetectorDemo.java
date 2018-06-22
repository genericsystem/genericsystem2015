package org.genericsystem.cv.application;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Core;
import org.opencv.core.Core.MinMaxLocResult;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfDouble;
import org.opencv.core.MatOfInt;
import org.opencv.core.MatOfPoint;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.opencv.imgproc.Moments;
import org.opencv.utils.Converters;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javafx.application.Platform;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class RobustTextDetectorDemo extends AbstractApp {

	public static void main(String[] args) {
		launch(args);
	}

	static {
		NativeLibraryLoader.load();
	}

	private final GSCapture gsCapture = new GSVideoCapture(0, GSVideoCapture.HD, GSVideoCapture.VGA);
	private Img frame = gsCapture.read();
	private ScheduledExecutorService timer = new BoundedScheduledThreadPoolExecutor();
	private Config config = new Config();
	private final ImageView[][] imageViews = new ImageView[][] { new ImageView[3], new ImageView[3], new ImageView[3] };

	private void startTimer() {
		timer.scheduleAtFixedRate(() -> {
			try {
				Image[] images = doWork();
				if (images != null)
					Platform.runLater(() -> {
						Iterator<Image> it = Arrays.asList(images).iterator();
						for (int row = 0; row < imageViews.length; row++)
							for (int col = 0; col < imageViews[row].length; col++)
								if (it.hasNext())
									imageViews[row][col].setImage(it.next());
					});
			} catch (Throwable e) {
				e.printStackTrace();
			}
		}, 30, 30, TimeUnit.MILLISECONDS);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		double displaySizeReduction = 0.5;
		for (int col = 0; col < imageViews.length; col++)
			for (int row = 0; row < imageViews[col].length; row++) {
				ImageView imageView = new ImageView();
				imageViews[col][row] = imageView;
				mainGrid.add(imageViews[col][row], col, row);
				imageView.setFitWidth(frame.width() / displaySizeReduction);
				imageView.setFitHeight(frame.height() / displaySizeReduction);
			}
		startTimer();
	}

	Mat convertContourToMat(MatOfPoint contour) {
		Point[] pts = contour.toArray();
		Mat result = new Mat(pts.length, 2, CvType.CV_64FC1);
		for (int i = 0; i < result.rows(); ++i) {
			result.put(i, 0, pts[i].x);
			result.put(i, 1, pts[i].y);
		}
		return result;
	}

	private Image[] doWork() {

		System.out.println("do work");
		if (!config.stabilizedMode)
			frame = gsCapture.read();
		Image[] images = new Image[12];

		Img gray = frame.bgr2Gray();
		RobustTextDetectorManager manager = new RobustTextDetectorManager(gray.getSrc());

		Mat mserMask = manager.getMserMask();
		images[0] = new Img(mserMask, false).toJfxImage();

		Mat edges = new Mat();
		Imgproc.Canny(gray.getSrc(), edges, 30, 110);
		Mat edge_mser_intersection = new Mat();
		Core.bitwise_and(edges, mserMask, edge_mser_intersection);
		images[1] = new Img(edge_mser_intersection, false).toJfxImage();

		Mat gradientGrown = growEdges(gray.getSrc(), edge_mser_intersection);
		images[2] = new Img(gradientGrown, false).toJfxImage();

		Mat edgeEnhancedMser = new Mat();
		Mat notGradientGrown = new Mat();
		Core.bitwise_not(gradientGrown, notGradientGrown);
		Core.bitwise_and(notGradientGrown, mserMask, edgeEnhancedMser);
		images[3] = new Img(edgeEnhancedMser, false).toJfxImage();

		Mat labels = new Mat();
		Mat stats = new Mat();
		Mat centroid = new Mat();
		int labelsIds = Imgproc.connectedComponentsWithStats(edgeEnhancedMser, labels, stats, centroid, 4, CvType.CV_32S);
		Mat result2 = new Mat(labels.size(), CvType.CV_8UC1, new Scalar(0));
		for (int labelId = 0; labelId < labelsIds; labelId++) {
			double area = stats.get(labelId, Imgproc.CC_STAT_AREA)[0];
			if (area < 3 || area > 600)
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

			if (eccentricity < minEccentricity || eccentricity > maxEccentricity) {
				continue;
			}
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
			double minSolidity = 0.4;// 0.5
			if (solidity < minSolidity)
				continue;
			Core.bitwise_or(result2, labelMask, result2);
		}
		images[4] = new Img(result2, false).toJfxImage();

		Imgproc.distanceTransform(result2, result2, Imgproc.DIST_L2, 3);
		Mat tmp = new Mat();
		Core.multiply(result2, new Scalar(200), tmp);
		images[5] = new Img(tmp, false).toJfxImage();
		result2.convertTo(result2, CvType.CV_32SC1);

		Mat strokeWidth = computeStrokeWidth(result2);
		Mat filtered_stroke_width = new Mat(strokeWidth.size(), CvType.CV_8UC1, new Scalar(0));

		Mat strokeWithCV8U = new Mat();
		strokeWidth.convertTo(strokeWithCV8U, CvType.CV_8UC1);
		labelsIds = Imgproc.connectedComponentsWithStats(strokeWithCV8U, labels, stats, centroid, 4, CvType.CV_32S);
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
				if ((stdDev.get(0, 0)[0] / meanD.get(0, 0)[0]) > 0.5)
					continue;

				/* Collect the filtered stroke width */
				Core.bitwise_or(filtered_stroke_width, labelMask, filtered_stroke_width);
			}
		}
		images[6] = new Img(filtered_stroke_width, false).toJfxImage();
		Mat bounding_region = new Mat();
		Imgproc.morphologyEx(filtered_stroke_width, bounding_region, Imgproc.MORPH_CLOSE, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(25, 25)));
		// Imgproc.morphologyEx(bounding_region, bounding_region, Imgproc.MORPH_OPEN, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(25, 25)));
		// Mat result3 = new Mat();
		// superFrame.getFrame().getSrc().copyTo(result3, filtered_stroke_width);
		images[7] = new Img(bounding_region, false).toJfxImage();

		return images;
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

	@Override
	protected void onS() {
		config.stabilizedMode = !config.stabilizedMode;
	}

	@Override
	protected void onSpace() {
		if (config.isOn)
			timer.shutdown();
		else {
			timer = new BoundedScheduledThreadPoolExecutor();
			startTimer();
		}
		config.isOn = !config.isOn;
	}

	@Override
	public void stop() throws Exception {
		super.stop();
		timer.shutdown();
		timer.awaitTermination(5000, TimeUnit.MILLISECONDS);
		gsCapture.release();
	}

}
