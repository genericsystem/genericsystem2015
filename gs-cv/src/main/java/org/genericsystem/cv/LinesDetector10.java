package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.BiFunction;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

import org.genericsystem.cv.LinesDetector8.Line;
import org.genericsystem.cv.LinesDetector8.Lines;
import org.genericsystem.cv.lm.LMHostImpl;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.genericsystem.cv.utils.Tools;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.opencv.utils.Converters;
import org.opencv.videoio.VideoCapture;

public class LinesDetector10 extends AbstractApp {

	static final double f = 6.053 / 0.009;
	static {
		NativeLibraryLoader.load();
	}

	public static void main(String[] args) {
		launch(args);
	}

	private final VideoCapture capture = new VideoCapture(0);
	private ScheduledExecutorService timer = Executors.newSingleThreadScheduledExecutor();
	private Lines lines;
	private Point[] trapezePoints;
	double[][] vps;
	private boolean stabilize = false;

	private Mat dumpTrapezePointsHomography(Mat homography, double dumpSize, Size size) {
		MatOfPoint2f results = new MatOfPoint2f();
		MatOfPoint2f framePoints = new MatOfPoint2f(new Point(0, 0), new Point(size.width, 0), new Point(size.width, size.height), new Point(0, size.height));
		Core.perspectiveTransform(Converters.vector_Point2f_to_Mat(Arrays.asList(new Point(0, 0), new Point(size.width, 0), new Point(size.width, size.height), new Point(0, size.height))), results, homography);
		dumpTrapezePoints(results.toArray(), dumpSize);
		return Imgproc.getPerspectiveTransform(framePoints, new MatOfPoint2f(trapezePoints));
	}

	private void dumpTrapezePoints(Point[] newPoints, double dumpSize) {
		for (int i = 0; i < trapezePoints.length; i++)
			trapezePoints[i] = new Point(((dumpSize - 1) * trapezePoints[i].x + newPoints[i].x) / dumpSize, ((dumpSize - 1) * trapezePoints[i].y + newPoints[i].y) / dumpSize);
	}

	@Override
	protected void onSpace() {
		stabilize = !stabilize;
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		Mat frame = new Mat();
		Mat dePerspectived = new Mat();

		capture.read(frame);

		ImageView frameView = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(frameView, 0, 0);
		ImageView deskiewedView = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(deskiewedView, 0, 1);
		ImageView gradView = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(gradView, 1, 0);
		ImageView gradView2 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(gradView2, 1, 1);
		ImageView gradView3 = new ImageView(Tools.mat2jfxImage(frame));

		mainGrid.add(gradView3, 2, 1);
		trapezePoints = new Point[] { new Point(0, 0), new Point(frame.width(), 0), new Point(frame.width(), frame.height()), new Point(0, frame.height()) };

		Img[] binarized = new Img[1];
		timer.scheduleAtFixedRate(() -> {
			try {
				if (!stabilize)
					capture.read(frame);

				Mat diffFrame = getDiffFrame(frame);
				List<Circle> circles = detectCircles(frame, diffFrame, 50, 120);
				Collection<Circle> selectedCircles = selectRandomCirles(circles, 3);
				for (Circle circle : selectedCircles) {
					Img circledImg = getCircledImg(frame, circle.radius, circle.center);
					double angle = getBestAngle(circledImg, 30, 12, 8, 192, binarized) / 180 * Math.PI;
					displayLine(frame, circle.center, angle, 50);
					Imgproc.circle(frame, circle.center, (int) circle.radius, new Scalar(0, 255, 0), 1);
					System.out.println("zzz");
				}
				// int radius = 100;
				// Point center = new Point(frame.width() / 4, frame.height() / 2);
				// Img circledImg = getCircledImg(frame, radius, center);
				// double angle = getBestAngle(circledImg, 30, 12, 8, 192, binarized) / 180 * Math.PI;
				// displayLine(frame, center, angle, 100);
				//
				// center = new Point(3 * frame.width() / 4, frame.height() / 2);
				// circledImg = getCircledImg(frame, radius, center);
				// angle = getBestAngle(circledImg, 30, 12, 8, 192, null) / 180 * Math.PI;
				// displayLine(frame, center, angle, 100);
				//
				// gradView3.setImage(binarized[0].toJfxImage());

				// Img display = new Img(circledImg.getSrc(), true);
				// Imgproc.putText(display.getSrc(), "angle : " + angle * 180 / Math.PI, new Point(5, 20), Core.FONT_HERSHEY_TRIPLEX, 0.5, new Scalar(255));
				// gradView2.setImage(display.toJfxImage());
				// circledImg.getSrc().convertTo(circledImg.getSrc(), CvType.CV_8UC1);

				Img grad = new Img(frame, false).morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(3, 3)).otsu();
				Img closed = grad.morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(10, 10)).morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(3, 3));
				Img closed2 = grad.morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(30, 40)).morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(3, 3));
				gradView.setImage(closed.toJfxImage());
				if (!stabilize) {
					lines = new Lines(closed.houghLinesP(1, Math.PI / 180, 10, 20, 5));
					lines.lines.addAll(new Lines(closed2.houghLinesP(1, Math.PI / 180, 10, 50, 10)).lines);
					// lines.lines.add(line);
				}

				if (lines.size() > 10) {
					// lines = lines.reduce(30);
					Mat colorFrame = frame.clone();
					lines.draw(colorFrame, new Scalar(0, 0, 0));
					LinesDetector linesDetector = new LinesDetector(new Img(frame, false), lines.lines, vps);
					// if (!stabilize)
					vps = linesDetector.getVps();

					Map<Integer, List<Integer>> clusters = linesDetector.lines2Vps(6.0 / 180.0 * Math.PI);
					for (int cluster : clusters.keySet())
						for (int lineId : clusters.get(cluster))
							lines.lines.get(lineId).draw(colorFrame, new Scalar(cluster == 0 ? 255 : 0, cluster == 1 ? 255 : 0, cluster == 2 ? 255 : 0));

					frameView.setImage(Tools.mat2jfxImage(colorFrame));
					Mat homographyMat = findHomography(frame.size(), vps, new double[] { frame.width() / 2, frame.height() / 2, 1.0 }, f);
					Mat dumpedHomographyMat = dumpTrapezePointsHomography(homographyMat, 1, frame.size());
					Imgproc.warpPerspective(frame, dePerspectived, dumpedHomographyMat, frame.size(), Imgproc.INTER_LINEAR, Core.BORDER_CONSTANT, Scalar.all(255));
					deskiewedView.setImage(Tools.mat2jfxImage(dePerspectived));
				} else
					System.out.println("Not enough lines : " + lines.size());

			} catch (Throwable e) {
				e.printStackTrace();
			}

		}, 30, 50, TimeUnit.MILLISECONDS);

	}

	private Mat getDiffFrame(Mat frame) {
		Mat result = new Mat();
		Imgproc.cvtColor(frame, result, Imgproc.COLOR_BGR2GRAY);
		Imgproc.GaussianBlur(result, result, new Size(3, 3), 0);
		Mat diffFrame = new Mat();
		Core.absdiff(result, new Scalar(255), diffFrame);
		Imgproc.adaptiveThreshold(diffFrame, diffFrame, 255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY_INV, 7, 3);
		return diffFrame;
	}

	private Collection<Circle> selectRandomCirles(List<Circle> circles, int circlesNumber) {
		if (circles.size() <= circlesNumber)
			return circles;
		Set<Circle> result = new HashSet<>();
		while (result.size() < circlesNumber)
			result.add(circles.get((int) (Math.random() * circles.size())));
		return result;
	}

	private List<Circle> detectCircles(Mat frame, Mat diffFrame, int minRadius, int maxRadius) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(diffFrame, contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		List<Circle> circles = new ArrayList<>();
		for (int i = 0; i < contours.size(); i++) {
			MatOfPoint contour = contours.get(i);
			double contourarea = Imgproc.contourArea(contour);
			if (contourarea > 50) {
				float[] radius = new float[1];
				Point center = new Point();
				MatOfPoint2f contour2F = new MatOfPoint2f(contour.toArray());
				Imgproc.minEnclosingCircle(contour2F, center, radius);
				if (radius[0] > minRadius && radius[0] < maxRadius) {
					circles.add(new Circle(center, radius[0]));
					// Imgproc.circle(frame, center, (int) radius[0], new Scalar(0, 0, 255));
				}
				// Imgproc.drawContours(frame, Arrays.asList(contour), 0, new Scalar(0, 255, 0), 1);
			}
		}
		return circles;
	}

	private static class Circle {
		public Circle(Point center, float radius) {
			this.center = center;
			this.radius = radius;
		}

		Point center;
		float radius;
	}

	public Img getCircledImg(Mat frame, float radius, Point center) {
		Mat mask = new Mat(new Size(radius * 2, radius * 2), CvType.CV_8UC1, new Scalar(0));
		Imgproc.circle(mask, new Point(radius, radius), (int) radius, new Scalar(255), -1);
		Rect rect = new Rect(new Point(center.x - radius, center.y - radius), new Point(center.x + radius, center.y + radius));
		Mat roi = new Img(new Mat(frame, rect), true).bilateralFilter().adaptativeGaussianInvThreshold(3, 3).getSrc();
		Mat circled = new Mat();
		roi.copyTo(circled, mask);
		Img circledImg = new Img(circled, false);
		return circledImg;
	}

	public void displayLine(Mat mat, Point center, double angle, double size) {
		double x1 = center.x - Math.sin(angle) * size;
		double y1 = center.y + Math.cos(angle) * size;
		double x2 = center.x + Math.sin(angle) * size;
		double y2 = center.y - Math.cos(angle) * size;
		Line line = new Line(new Point(x1, y1), new Point(x2, y2));
		line.draw(mat, new Scalar(0, 0, 0), 2);
	}

	public double score(Img circled, double angle, int filterSize, double threshold) {
		Mat M = Imgproc.getRotationMatrix2D(new Point(circled.width() / 2, circled.width() / 2), angle, 1);
		Mat rotated = new Mat();
		Imgproc.warpAffine(circled.getSrc(), rotated, M, new Size(circled.width(), circled.width()));
		Img binarized = new Img(rotated, false).directionalFilter(filterSize).thresHold(threshold, 255, Imgproc.THRESH_BINARY);
		Mat result = new Mat();
		Core.reduce(binarized.getSrc(), result, 1, Core.REDUCE_SUM, CvType.CV_64F);
		Core.reduce(result, result, 0, Core.REDUCE_SUM, CvType.CV_64F);
		return result.get(0, 0)[0];
	}

	public double getBestAngle(Img circledImg, int absMinMax, double step, int filterSize, double threshold, Img[] binarized) {
		double maxScore = 0;
		double bestAngle = -1;
		if (binarized != null)
			binarized[0] = new Img(new Mat(new Size(2 * absMinMax * 10, 200), CvType.CV_8UC1, new Scalar(0)), false);
		List<double[]> results = new ArrayList<double[]>();
		for (double angle = -absMinMax; angle <= absMinMax; angle += step) {
			double score = score(circledImg, angle, filterSize, threshold);
			if (angle != 0 && score > maxScore) {
				maxScore = score;
				bestAngle = angle;
			}
			if (angle != 0)
				results.add(new double[] { angle, score });
			System.out.println(score);
			if (binarized != null)
				new Line((absMinMax + angle) * 10, 0, (absMinMax + angle) * 10, score / 1000).draw(binarized[0].getSrc(), new Scalar(255, 0, 0), 10);
		}
		BiFunction<Double, double[], Double> f = (x, params) -> params[0] * x * x * x * x + params[1] * x * x * x + params[2] * x * x + params[3] * x + params[4];
		BiFunction<double[], double[], Double> e = (xy, params) -> f.apply(xy[0], params) - xy[1];
		double[] result = new LMHostImpl<>(e, results, new double[] { 1, 1, 1, 1, 1 }).getParams();
		Point point = null;
		double polynomAngle = 0.0;
		double max = 0.0;
		for (double angle = -absMinMax; angle <= absMinMax; angle++) {
			Point oldPoint = point;
			double score = f.apply(angle, result);
			point = new Point((absMinMax + angle) * 10, score / 1000);
			if (score > max) {
				max = score;
				polynomAngle = angle;
			}
			if (binarized != null && oldPoint != null)
				new Line(oldPoint, point).draw(binarized[0].getSrc(), new Scalar(255, 0, 0));
		}
		if (binarized != null) {
			Imgproc.circle(binarized[0].getSrc(), new Point((absMinMax + polynomAngle) * 10, max / 1000), 10, new Scalar(255, 255, 0), 3);
			// new Line(new Point((absMinMax + bestAngle) * 10, maxScore / 1000), new Point((absMinMax + bestAngle) * 10, 0)).draw(binarized[0].getSrc(), new Scalar(255, 255, 0), 3);
		}
		System.out.println(Arrays.toString(result));

		return polynomAngle;
	}

	@Override
	public void stop() throws Exception {
		super.stop();
		timer.shutdown();
		timer.awaitTermination(5000, TimeUnit.MILLISECONDS);
		capture.release();
	}

	public static class LinesDetector {

		private final List<Line> lines;
		private final double[] pp;
		private double noiseRatio = 0.5;
		// private List<LineInfo> lineInfos = new ArrayList<>();
		double vps[][];

		static {
			NativeLibraryLoader.load();
		}

		private class LineInfo {
			double[] para;
			double length;
			double orientation;

			public LineInfo(double[] para, double length, double orientation) {
				this.para = para;
				this.length = length;
				this.orientation = orientation;
			}
		}

		public LinesDetector(Img img, List<Line> lines, double[][] oldVps) {
			this.lines = lines;
			this.pp = new double[] { img.width() / 2, img.height() / 2 };
			List<LineInfo> linesInfos = getLinesInfos(lines);
			double[][] sphereGrid = getSphereGrids(linesInfos);
			this.vps = getBestVpsHyp(getVpsHypos(linesInfos), sphereGrid);
			double vpsRes = evaluateVpsHypo(vps, sphereGrid);
			if (oldVps != null) {
				double oldVpsRes = evaluateVpsHypo(oldVps, sphereGrid);
				if (oldVpsRes > vpsRes) {
					System.out.println("Warning, old vps is better : " + oldVpsRes + " " + vpsRes);
					System.out.println("old : " + Arrays.deepToString(getVp2DFromVps(oldVps, pp, f)));
					System.out.println("new : " + Arrays.deepToString(getVp2DFromVps(vps, pp, f)));
					this.vps = oldVps;
				} else
					System.out.println("All is ok : " + oldVpsRes + " " + vpsRes);
			}
		}

		public double[][] getVps() {
			return vps;
		}

		private List<LineInfo> getLinesInfos(List<Line> lines) {
			List<LineInfo> lineInfos = new ArrayList<>();
			for (Line line : lines) {
				double[] p1 = new double[] { line.x1, line.y1, 1.0 };
				double[] p2 = new double[] { line.x2, line.y2, 1.0 };
				double[] para = cross(p1, p2);
				double dx = line.x1 - line.x2;
				double dy = line.y1 - line.y2;
				double length = Math.sqrt(dx * dx + dy * dy);
				double orientation = Math.atan2(dy, dx);
				if (orientation < 0)
					orientation += Math.PI;
				lineInfos.add(new LineInfo(para, length, orientation));
			}
			return lineInfos;
		}

		private List<double[][]> getVpsHypos(List<LineInfo> linesInfos) {
			double p = 1.0 / 3.0 * Math.pow(1.0 - noiseRatio, 2);
			double confEfficience = 0.9999;
			int iterations = (int) (Math.log(1 - confEfficience) / Math.log(1.0 - p));
			System.out.println("Iterations : " + iterations);
			int numVp2 = 360;
			double stepVp2 = 2.0 * Math.PI / numVp2;
			List<double[][]> vpHypo = new ArrayList<>(iterations * numVp2);

			int num = linesInfos.size();
			for (int i = 0; i < iterations; i++) {
				int idx1 = (int) (Math.random() * num);
				int idx2 = (int) (Math.random() * num);
				while (idx2 == idx1)
					idx2 = (int) (Math.random() * num);
				double[] vp1_Img = cross(linesInfos.get(idx1).para, linesInfos.get(idx2).para);
				if (vp1_Img[2] == 0) {
					i--;
					continue;
				}
				double[] vp1 = getVpFromVp2D(vp1_Img, pp, f);
				double[] vp2 = new double[3];
				double[] vp3 = new double[3];
				for (int j = 0; j < numVp2; ++j) {
					// vp2
					double lambda = j * stepVp2;

					double k1 = vp1[0] * Math.sin(lambda) + vp1[1] * Math.cos(lambda);
					double k2 = vp1[2];
					double phi = Math.atan(-k2 / k1);

					double Z = Math.cos(phi);
					double X = Math.sin(phi) * Math.sin(lambda);
					double Y = Math.sin(phi) * Math.cos(lambda);

					vp2[0] = X;
					vp2[1] = Y;
					vp2[2] = Z;
					if (vp2[2] == 0.0)
						vp2[2] = 0.0011;
					double N = Math.sqrt(vp2[0] * vp2[0] + vp2[1] * vp2[1] + vp2[2] * vp2[2]);
					vp2[0] *= 1.0 / N;
					vp2[1] *= 1.0 / N;
					vp2[2] *= 1.0 / N;
					if (vp2[2] < 0) {
						vp2[0] *= -1.0;
						vp2[1] *= -1.0;
						vp2[2] *= -1.0;
					}

					// vp3
					vp3 = cross(vp1, vp2);
					if (vp3[2] == 0.0)
						vp3[2] = 0.0011;
					N = Math.sqrt(vp3[0] * vp3[0] + vp3[1] * vp3[1] + vp3[2] * vp3[2]);
					vp3[0] *= 1.0 / N;
					vp3[1] *= 1.0 / N;
					vp3[2] *= 1.0 / N;
					if (vp3[2] < 0) {
						vp3[0] *= -1.0;
						vp3[1] *= -1.0;
						vp3[2] *= -1.0;
					}
					vpHypo.add(reorderXyz(new double[][] { vp1, vp2, vp3 }));

				}
			}
			return vpHypo;
		}

		private double[][] getSphereGrids(List<LineInfo> linesInfos) {
			// build sphere grid with 1 degree accuracy
			double angelAccuracy = 1.0 / 180.0 * Math.PI;
			double angleSpanLA = Math.PI / 2.0;
			double angleSpanLO = Math.PI * 2.0;
			int gridLA = (int) (angleSpanLA / angelAccuracy);
			int gridLO = (int) (angleSpanLO / angelAccuracy);

			double[][] sphereGrid = new double[gridLA][gridLO];
			// put intersection points into the grid
			double angelTolerance = 60.0 / 180.0 * Math.PI;
			double latitude = 0.0, longitude = 0.0;
			int LA = 0, LO = 0;
			double angleDev = 0.0;
			for (int i = 0; i < linesInfos.size() - 1; ++i) {
				for (int j = i + 1; j < linesInfos.size(); ++j) {
					LineInfo lii = linesInfos.get(i);
					LineInfo lij = linesInfos.get(j);
					double[] ptIntersect = cross(lii.para, lij.para);
					if (ptIntersect[2] == 0)
						continue;
					double X = ptIntersect[0] / ptIntersect[2] - pp[0];
					double Y = ptIntersect[1] / ptIntersect[2] - pp[1];
					double Z = f;
					double N = Math.sqrt(X * X + Y * Y + Z * Z);
					latitude = Math.acos(Z / N);
					longitude = Math.atan2(X, Y) + Math.PI;
					LA = (int) (latitude / angelAccuracy);
					if (LA >= gridLA)
						LA = gridLA - 1;

					LO = (int) (longitude / angelAccuracy);
					if (LO >= gridLO)
						LO = gridLO - 1;
					angleDev = Math.abs(lii.orientation - lij.orientation);
					angleDev = Math.min(Math.PI - angleDev, angleDev);
					if (angleDev > angelTolerance)
						continue;
					sphereGrid[LA][LO] += Math.sqrt(lii.length * lij.length) * (Math.sin(2.0 * angleDev) + 0.2);
				}
			}

			//
			int halfSize = 1;
			int winSize = halfSize * 2 + 1;
			int neighNum = winSize * winSize;

			// get the weighted line length of each grid
			double[][] sphereGridNew = new double[gridLA][gridLO];
			for (int i = halfSize; i < gridLA - halfSize; ++i) {
				for (int j = halfSize; j < gridLO - halfSize; ++j) {
					double neighborTotal = 0.0;
					for (int m = 0; m < winSize; ++m)
						for (int n = 0; n < winSize; ++n)
							neighborTotal += sphereGrid[i - halfSize + m][j - halfSize + n];
					sphereGridNew[i][j] = sphereGrid[i][j] + neighborTotal / neighNum;
				}
			}
			// sphereGrid = sphereGridNew;
			return sphereGrid;
		}

		private double evaluateVpsHypo(double[][] hypo, double[][] sphereGrid) {
			double oneDegree = 1.0 / 180.0 * Math.PI;
			double lineLength = 0;
			for (int j = 0; j < 2; ++j) {// j<3 for 3d objects
				if (hypo[j][2] == 0.0)
					continue;
				double latitude = Math.acos(hypo[j][2]);
				double longitude = Math.atan2(hypo[j][0], hypo[j][1]) + Math.PI;
				int gridLA = (int) (latitude / oneDegree);
				if (gridLA == 90)
					gridLA = 89;
				int gridLO = (int) (longitude / oneDegree);
				if (gridLO == 360)
					gridLO = 359;
				lineLength += sphereGrid[gridLA][gridLO];
			}
			return lineLength;
		}

		private double[][] getBestVpsHyp(List<double[][]> vpHypos, double[][] sphereGrid) {
			double[][] result = null;
			double maxLength = 0.0;
			for (double[][] vpHypo : vpHypos) {
				double lineLength = evaluateVpsHypo(vpHypo, sphereGrid);
				if (lineLength > maxLength) {
					maxLength = lineLength;
					result = vpHypo;
				}
			}
			return result;
		}

		private int index(double[][] hypo, int axe1, int axe2) {
			int index = 0;
			double minXY = Double.MAX_VALUE;
			for (int k = 0; k < 3; k++) {
				double currentXY = hypo[k][axe1] * hypo[k][axe1] + hypo[k][axe2] * hypo[k][axe2];
				if (currentXY < minXY) {
					index = k;
					minXY = currentXY;
				}
			}
			return index;
		}

		private double[][] reorderXyz(double[][] vps) {
			return new double[][] { vps[index(vps, 1, 2)], vps[index(vps, 0, 2)], vps[index(vps, 0, 1)] };
		}

		public Map<Integer, List<Integer>> lines2Vps(double thAngle) {
			List<double[]> vp2D = new ArrayList<>();
			for (int i = 0; i < 3; ++i)
				vp2D.add(getVp2DFromVp(vps[i], pp, f));
			Map<Integer, List<Integer>> clusters = new HashMap<Integer, List<Integer>>() {
				@Override
				public List<Integer> get(Object key) {
					List<Integer> ids = super.get(key);
					if (ids == null)
						put((Integer) key, ids = new ArrayList<>());
					return ids;
				}
			};
			for (int i = 0; i < lines.size(); ++i) {
				double x1 = lines.get(i).x1;
				double y1 = lines.get(i).y1;
				double x2 = lines.get(i).x2;
				double y2 = lines.get(i).y2;
				double xm = (x1 + x2) / 2.0;
				double ym = (y1 + y2) / 2.0;

				double v1x = x1 - x2;
				double v1y = y1 - y2;
				double N1 = Math.sqrt(v1x * v1x + v1y * v1y);
				v1x /= N1;
				v1y /= N1;

				double minAngle = 1000.0;
				int bestIdx = 0;
				for (int j = 0; j < 3; ++j) {
					double v2x = vp2D.get(j)[0] - xm;
					double v2y = vp2D.get(j)[1] - ym;
					double N2 = Math.sqrt(v2x * v2x + v2y * v2y);
					v2x /= N2;
					v2y /= N2;

					double crossValue = v1x * v2x + v1y * v2y;
					if (crossValue > 1.0)
						crossValue = 1.0;
					if (crossValue < -1.0)
						crossValue = -1.0;
					double angle = Math.acos(crossValue);
					angle = Math.min(Math.PI - angle, angle);

					if (angle < minAngle) {
						minAngle = angle;
						bestIdx = j;
					}
				}
				if (minAngle < thAngle)
					clusters.get(bestIdx).add(i);
			}
			return clusters;
		}
	}

	public static Mat findHomography(Size size, double[][] vps, double[] pp, double f) {

		double[][] vps2D = getVp2DFromVps(vps, pp, f);
		System.out.println("vps2D : " + Arrays.deepToString(vps2D));

		double phi = Math.atan2(vps[0][1], vps[0][0]);
		double theta = Math.acos(vps[0][2]);
		double phi2 = Math.atan2(vps[1][1], vps[1][0]);
		double theta2 = Math.acos(vps[1][2]);
		double phi3 = Math.atan2(vps[2][1], vps[2][0]);
		double theta3 = Math.acos(vps[2][2]);

		double x = size.width / 8;

		double[] A = new double[] { size.width / 2, size.height / 2, 1 };
		double[] B = new double[] { Math.cos(phi) < 0 ? size.width / 2 - x : size.width / 2 + x, size.height / 2 };
		double[] D = new double[] { size.width / 2, Math.sin(phi2) < 0 ? size.height / 2 - x : size.height / 2 + x, 1 };
		double[] C = new double[] { Math.cos(phi) < 0 ? size.width / 2 - x : size.width / 2 + x, Math.sin(phi2) < 0 ? size.height / 2 - x : size.height / 2 + x };

		System.out.println("vp1 (" + phi * 180 / Math.PI + "°, " + theta * 180 / Math.PI + "°)");
		System.out.println("vp2 (" + phi2 * 180 / Math.PI + "°, " + theta2 * 180 / Math.PI + "°)");
		System.out.println("vp3 (" + phi3 * 180 / Math.PI + "°, " + theta3 * 180 / Math.PI + "°)");

		double[] A_ = A;
		double[] B_ = new double[] { size.width / 2 + x * Math.sin(theta) * Math.sin(theta) * Math.cos(phi), size.height / 2 + x * Math.sin(theta) * Math.sin(theta) * Math.sin(phi), 1 };
		double[] D_ = new double[] { size.width / 2 + x * Math.sin(theta2) * Math.sin(theta2) * Math.cos(phi2), size.height / 2 + x * Math.sin(theta2) * Math.sin(theta2) * Math.sin(phi2), 1 };
		double[] C_ = cross2D(cross(B_, vps2D[1]), cross(D_, vps2D[0]));

		// double[] A_ = A;
		// double[] B_ = new double[] { size.width / 2 + x * Math.sin(theta) * vps[0][0], size.height / 2 + x * Math.sin(theta) * vps[0][1], 1 };
		// double[] D_ = new double[] { size.width / 2 + x * Math.sin(theta2) *vps[1][0], size.height / 2 + x * Math.sin(theta2) *vps[1][1], 1 };
		// double[] C_ = cross2D(cross(B_, vps2D[1]), cross(D_, vps2D[0]));

		return Imgproc.getPerspectiveTransform(new MatOfPoint2f(new Point(A_), new Point(B_), new Point(C_), new Point(D_)), new MatOfPoint2f(new Point(A), new Point(B), new Point(C), new Point(D)));
	}

	static double[] cross(double[] a, double b[]) {
		return new double[] { a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0] };
	}

	static double det(double[] u, double v[], double w[]) {
		return u[0] * v[1] * w[2] + u[2] * v[0] * w[1] + u[1] * v[2] * w[0] - u[2] * v[1] * w[0] - u[1] * v[0] * w[2] - u[0] * v[2] * w[1];
	}

	static double[] cross2D(double[] a, double b[]) {
		return uncalibrate(cross(a, b));
	}

	static double[] uncalibrate(double[] a) {
		return new double[] { a[0] / a[2], a[1] / a[2], 1 };
	}

	static double[] getVpFromVp2D(double[] vpImg, double[] pp, double f) {
		double[] vp = new double[] { vpImg[0] / vpImg[2] - pp[0], vpImg[1] / vpImg[2] - pp[1], f };
		if (vp[2] == 0)
			vp[2] = 0.0011;
		double N = Math.sqrt(vp[0] * vp[0] + vp[1] * vp[1] + vp[2] * vp[2]);
		vp[0] *= 1.0 / N;
		vp[1] *= 1.0 / N;
		vp[2] *= 1.0 / N;
		return vp;
	}

	public static double[][] getVp2DFromVps(double vps[][], double[] pp, double f) {
		double[][] result = new double[3][3];
		for (int i = 0; i < 3; i++) {
			result[i][0] = vps[i][0] * f / vps[i][2] + pp[0];
			result[i][1] = vps[i][1] * f / vps[i][2] + pp[1];
			result[i][2] = 1.0;
		}
		return result;
	}

	static double[] getVp2DFromVp(double[] vp, double[] pp, double f) {
		return new double[] { vp[0] * f / vp[2] + pp[0], vp[1] * f / vp[2] + pp[1] };
	}

}