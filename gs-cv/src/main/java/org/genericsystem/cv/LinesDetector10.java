package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

import org.genericsystem.cv.LinesDetector8.Line;
import org.genericsystem.cv.LinesDetector8.Lines;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.genericsystem.cv.utils.Tools;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.opencv.videoio.VideoCapture;

public class LinesDetector10 extends AbstractApp {
	// static final double f = 6.053 / 0.009;
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
	double[][] vps;
	private boolean stabilize = false;

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

		timer.scheduleAtFixedRate(() -> {
			try {
				if (!stabilize)
					capture.read(frame);
				Img grad = new Img(frame, false).morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(10, 10)).otsu();
				Img closed = grad.morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(10, 20)).morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(3, 3));
				Img closed2 = grad.morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(30, 40)).morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(3, 3));
				gradView.setImage(closed.toJfxImage());
				Lines allLines = lines != null ? lines : new Lines(Collections.emptyList());
				if (!stabilize || allLines == null) {
					lines = new Lines(closed.houghLinesP(1, Math.PI / 180, 10, 20, 5));
					lines.lines.addAll(new Lines(closed2.houghLinesP(1, Math.PI / 180, 10, 50, 10)).lines);
					allLines.lines.addAll(lines.lines);
				}

				if (allLines.size() > 10) {
					// lines = lines.reduce(30);
				Mat colorFrame = frame.clone();
				allLines.draw(colorFrame, new Scalar(0, 0, 0));
				LinesDetector linesDetector = new LinesDetector(new Img(frame, false), allLines.lines);
				// if (!stabilize)
				vps = linesDetector.getVps();

				Map<Integer, List<Integer>> clusters = linesDetector.lines2Vps(6.0 / 180.0 * Math.PI);
				for (int cluster : clusters.keySet())
					for (int lineId : clusters.get(cluster))
						allLines.lines.get(lineId).draw(colorFrame, new Scalar(cluster == 0 ? 255 : 0, cluster == 1 ? 255 : 0, cluster == 2 ? 255 : 0));

				frameView.setImage(Tools.mat2jfxImage(colorFrame));
				Mat homographyMat = findHomography(frame.size(), vps, new double[] { frame.width() / 2, frame.height() / 2, 1.0 }, f);
				Imgproc.warpPerspective(frame, dePerspectived, homographyMat, frame.size(), Imgproc.INTER_LINEAR, Core.BORDER_CONSTANT, Scalar.all(255));
				deskiewedView.setImage(Tools.mat2jfxImage(dePerspectived));
			} else
				System.out.println("Not enough lines : " + lines.size());

		} catch (Throwable e) {
			e.printStackTrace();
		}

	}, 30, 50, TimeUnit.MILLISECONDS);

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
		private List<LineInfo> lineInfos = new ArrayList<>();
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

		public LinesDetector(Img img, List<Line> lines) {
			this.lines = lines;
			this.pp = new double[] { img.width() / 2, img.height() / 2 };
			vps = getBestVpsHyp(getVPHypVia2Lines(), getSphereGrids());
		}

		public double[][] getVps() {
			return vps;
		}

		List<double[][]> getVPHypVia2Lines() {
			int num = lines.size();

			double p = 1.0 / 3.0 * Math.pow(1.0 - noiseRatio, 2);

			double confEfficience = 0.99999;
			int it = (int) (Math.log(1 - confEfficience) / Math.log(1.0 - p));
			// System.out.println("Iterations : " + it);
			int numVp2 = 360;
			double stepVp2 = 2.0 * Math.PI / numVp2;

			// get the parameters of each line
			lineInfos = new ArrayList<>();
			for (int i = 0; i < num; ++i) {
				double[] p1 = new double[] { lines.get(i).x1, lines.get(i).y1, 1.0 };
				double[] p2 = new double[] { lines.get(i).x2, lines.get(i).y2, 1.0 };
				double[] para = cross(p1, p2);
				double dx = lines.get(i).x1 - lines.get(i).x2;
				double dy = lines.get(i).y1 - lines.get(i).y2;
				double length = Math.sqrt(dx * dx + dy * dy);
				double orientation = Math.atan2(dy, dx);
				if (orientation < 0)
					orientation += Math.PI;
				lineInfos.add(new LineInfo(para, length, orientation));
			}

			// get vp hypothesis for each iteration
			List<double[][]> vpHypo = new ArrayList<>(it * numVp2);
			// srand((unsigned)time(NULL));
			for (int i = 0; i < it; ++i) {
				int idx1 = (int) (Math.random() * num);
				int idx2 = (int) (Math.random() * num);
				while (idx2 == idx1)
					idx2 = (int) (Math.random() * num);

				// get the vp1
				double[] vp1_Img = cross(lineInfos.get(idx1).para, lineInfos.get(idx2).para);
				if (vp1_Img[2] == 0) {
					i--;
					continue;
				}
				double[] vp1 = getVpFromVp2D(vp1_Img, pp, f);

				// get the vp2 and vp3
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
					vpHypo.add(new double[][] { new double[] { vp1[0], vp1[1], vp1[2] }, new double[] { vp2[0], vp2[1], vp2[2] }, new double[] { vp3[0], vp3[1], vp3[2] } });

				}
			}
			return vpHypo;
		}

		double[][] getSphereGrids() {
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
			for (int i = 0; i < lines.size() - 1; ++i) {
				for (int j = i + 1; j < lines.size(); ++j) {
					double[] ptIntersect = cross(lineInfos.get(i).para, lineInfos.get(j).para);
					if (ptIntersect[2] == 0)
						continue;
					double x = ptIntersect[0] / ptIntersect[2];
					double y = ptIntersect[1] / ptIntersect[2];

					double X = x - pp[0];
					double Y = y - pp[1];
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
					angleDev = Math.abs(lineInfos.get(i).orientation - lineInfos.get(j).orientation);
					angleDev = Math.min(Math.PI - angleDev, angleDev);
					if (angleDev > angelTolerance)
						continue;
					sphereGrid[LA][LO] += Math.sqrt(lineInfos.get(i).length * lineInfos.get(j).length) * (Math.sin(2.0 * angleDev) + 0.05);
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
					for (int m = 0; m < winSize; ++m) {
						for (int n = 0; n < winSize; ++n)
							neighborTotal += sphereGrid[i - halfSize + m][j - halfSize + n];
					}
					sphereGridNew[i][j] = sphereGrid[i][j] + neighborTotal / neighNum;
				}
			}
			sphereGrid = sphereGridNew;
			return sphereGrid;
		}

		double[][] getBestVpsHyp(List<double[][]> vpHypo, double[][] sphereGrid) {
			int num = vpHypo.size();
			// System.out.println("vpHypo : " + vpHypo.size());
			double oneDegree = 1.0 / 180.0 * Math.PI;

			// get the corresponding line length of every hypotheses
			double[] lineLength = new double[num];
			for (int i = 0; i < num; ++i) {

				double[][] hypo = vpHypo.get(i);
				int zIndex = 0;
				double minXY = Double.MAX_VALUE;
				for (int k = 0; k < 3; k++) {
					double currentXY = hypo[k][0] * hypo[k][0] + hypo[k][1] * hypo[k][1];
					if (currentXY < minXY) {
						zIndex = k;
						minXY = currentXY;
					}
				}

				for (int j = 0; j < 3; ++j) {
					if (j == zIndex || vpHypo.get(i)[j][2] == 0.0)
						continue;
					double latitude = Math.acos(vpHypo.get(i)[j][2]);
					double longitude = Math.atan2(vpHypo.get(i)[j][0], vpHypo.get(i)[j][1]) + Math.PI;
					int gridLA = (int) (latitude / oneDegree);
					if (gridLA == 90)
						gridLA = 89;
					int gridLO = (int) (longitude / oneDegree);
					if (gridLO == 360)
						gridLO = 359;
					lineLength[i] += sphereGrid[gridLA][gridLO];
				}
			}

			// get the best hypotheses
			int bestIdx = 0;
			double maxLength = 0.0;
			for (int i = 0; i < num; ++i) {
				if (lineLength[i] > maxLength) {
					maxLength = lineLength[i];
					bestIdx = i;
					// System.out.println("Select : " + Arrays.deepToString(getUncalibratedVps(vpHypo.get(bestIdx), pp, f)) + " score : " + maxLength);
				}
			}
			double[][] hypo = vpHypo.get(bestIdx);
			// System.out.println("------------------------------");
			// System.out.println("Select : " + Arrays.deepToString(getUncalibratedVps(vpHypo.get(bestIdx), pp, f)) + " score : " + maxLength);

			int xIndex = -1;
			int yIndex = -1;
			int zIndex = -1;
			double minXY = Double.MAX_VALUE;
			double minYZ = Double.MAX_VALUE;
			double minXZ = Double.MAX_VALUE;
			for (int i = 0; i < 3; i++) {
				double currentXY = Math.sqrt(hypo[i][0] * hypo[i][0] + hypo[i][1] * hypo[i][1]);
				double currentYZ = Math.sqrt(hypo[i][1] * hypo[i][1] + hypo[i][2] * hypo[i][2]);
				double currentXZ = Math.sqrt(hypo[i][0] * hypo[i][0] + hypo[i][2] * hypo[i][2]);
				if (currentXY < minXY) {
					zIndex = i;
					minXY = currentXY;
				}
				if (currentYZ < minYZ) {
					xIndex = i;
					minYZ = currentYZ;
				}
				if (currentXZ < minXZ) {
					yIndex = i;
					minXZ = currentXZ;
				}
			}

			double[] vpx = hypo[xIndex];
			double[] vpy = hypo[yIndex];
			double[] vpz = hypo[zIndex];
			return new double[][] { vpx, vpy, vpz };
		}

		Map<Integer, List<Integer>> lines2Vps(double thAngle) {

			// get the corresponding vanish points on the image plane
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

	public static double[][] getUncalibratedVps(double vps[][], double[] pp, double f) {
		double[][] result = new double[3][3];
		for (int i = 0; i < 3; i++) {
			result[i][0] = vps[i][0] * f / vps[i][2] + pp[0];
			result[i][1] = vps[i][1] * f / vps[i][2] + pp[1];
			result[i][2] = 1.0;
		}
		return result;
	}

	public static Mat findHomography(Size size, double[][] vps, double[] pp, double f) {

		double[][] vps2D = getUncalibratedVps(vps, pp, f);
		System.out.println("vps2D : " + Arrays.deepToString(vps2D));

		double phi = Math.atan2(vps[0][1], vps[0][0]);
		double theta = Math.acos(vps[0][2]);
		double phi2 = Math.atan2(vps[1][1], vps[1][0]);
		double theta2 = Math.acos(vps[1][2]);
		double phi3 = Math.atan2(vps[2][1], vps[2][0]);
		double theta3 = Math.acos(vps[2][2]);

		double x = size.width / 4;

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

	static double[] getVp2DFromVp(double[] vp, double[] pp, double f) {
		return new double[] { vp[0] * f / vp[2] + pp[0], vp[1] * f / vp[2] + pp[1] };
	}

}