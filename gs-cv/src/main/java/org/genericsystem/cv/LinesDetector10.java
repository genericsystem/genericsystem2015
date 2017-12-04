package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.apache.commons.math3.linear.LUDecomposition;
import org.apache.commons.math3.linear.MatrixUtils;
import org.apache.commons.math3.linear.RealMatrix;
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

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class LinesDetector10 extends AbstractApp {

	static {
		NativeLibraryLoader.load();
	}

	public static void main(String[] args) {
		launch(args);
	}

	private final VideoCapture capture = new VideoCapture(0);
	private ScheduledExecutorService timer = Executors.newSingleThreadScheduledExecutor();

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
				capture.read(frame);
				Img grad = new Img(frame, false).morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(10, 10)).otsu().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(20, 20)).morphologyEx(Imgproc.MORPH_GRADIENT,
						Imgproc.MORPH_ELLIPSE, new Size(3, 3));
				gradView.setImage(grad.toJfxImage());
				Lines lines = new Lines(grad.houghLinesP(1, Math.PI / 180, 10, 100, 10));

				if (lines.size() > 10) {
					lines = lines.reduce(30);
					lines.draw(frame, new Scalar(0, 0, 0));
					LinesDetector linesDetector = new LinesDetector(new Img(frame, false), lines.lines);
					double[][] vanishingPoints = linesDetector.getVanishingPoints();
					System.out.println("Vanishing points : " + Arrays.deepToString(linesDetector.getVanishingPoints()));
					Map<Integer, List<Integer>> clusters = linesDetector.lines2Vps(6.0 / 180.0 * Math.PI);
					for (int cluster : clusters.keySet())
						for (int lineId : clusters.get(cluster))
							lines.lines.get(lineId).draw(frame, new Scalar(cluster == 0 ? 255 : 0, cluster == 1 ? 255 : 0, cluster == 2 ? 255 : 0));

					frameView.setImage(Tools.mat2jfxImage(frame));
					Mat homographyMat = findHomography(frame.size(), new double[] { frame.width() / 2, frame.height() / 2, 1.0 }, vanishingPoints[0], vanishingPoints[1]);

					// double[][] homography = findHomography(frame.size(), vanishingPoints[0], vanishingPoints[1], true, 1);
					// Mat homographyMat = new Mat(3, 3, CvType.CV_64FC1);
					// for (int row = 0; row < 3; row++)
					// for (int col = 0; col < 3; col++)
					// homographyMat.put(row, col, homography[row][col]);
					Imgproc.warpPerspective(frame, dePerspectived, homographyMat, frame.size(), Imgproc.INTER_LINEAR, Core.BORDER_CONSTANT, Scalar.all(0));
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
		private final double f = 6.053 / 0.009;
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

		// public static void main(String[] args) {
		// Img img = new Img(Imgcodecs.imread("resources/text.jpg"));
		// // Img grad = img.bgr2Gray().canny(20, 80).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(30, 30)).grad(3, 3);// .morphologyEx(Imgproc.MORPH_DILATE, Imgproc.MORPH_ELLIPSE, new Size(5, 5));
		// // Img grad = img.bilateralFilter(10, 80, 80).bgr2Gray().adaptativeThresHold(255, Imgproc.ADAPTIVE_THRESH_GAUSSIAN_C, Imgproc.THRESH_BINARY_INV, 11, 3).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(5, 5))
		// // .morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(30, 30)).grad(3, 3);
		// Img grad = img.bgr2Gray().grad(15, 15).thresHold(80, 255, Imgproc.THRESH_BINARY).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(40, 40)).grad(5, 5);
		// // new Size(5, 5));
		//
		// Lines lines = new Lines(grad.houghLinesP(1, Math.PI / 180, 10, 100, 10));
		// double f = 6.053 / 0.009; // Focal length (in pixel)
		// Imgcodecs.imwrite("resources/img4.jpg", grad.getSrc());
		// // lines.draw(img.getSrc(), new Scalar(255));
		// Img linesImg = new Img(img.getSrc(), true);
		// lines.draw(linesImg.getSrc(), new Scalar(0, 255, 0));
		// Imgcodecs.imwrite("resources/img5.jpg", linesImg.getSrc());
		// new LinesDetector(img, lines.lines);
		// }

		public LinesDetector(Img img, List<Line> lines) {
			this.lines = lines;
			this.pp = new double[] { img.width() / 2, img.height() / 2 };
			vps = getBestVpsHyp(getVPHypVia2Lines(), getSphereGrids());
		}

		public double[][] getVps() {
			return vps;
		}

		public double[][] getVanishingPoints() {
			double[][] uncalibrates = new double[3][3];
			for (int i = 0; i < 3; i++) {
				uncalibrates[i][0] = vps[i][0] * f / vps[i][2] + pp[0];
				uncalibrates[i][1] = vps[i][1] * f / vps[i][2] + pp[1];
				uncalibrates[i][2] = 1.0;
			}

			double bestIndex = 1;
			double minXY = Double.MAX_VALUE;
			for (int i = 0; i < 3; i++) {
				double currentXY = Math.sqrt(vps[i][0] * vps[i][0] + vps[i][1] * vps[i][1]);
				if (currentXY < minXY) {
					bestIndex = i;
					minXY = currentXY;
				}
			}
			return bestIndex == 0 ? new double[][] { uncalibrates[1], uncalibrates[2] } : bestIndex == 1 ? new double[][] { uncalibrates[0], uncalibrates[2] } : new double[][] { uncalibrates[0], uncalibrates[1] };
		}

		List<double[][]> getVPHypVia2Lines() {
			int num = lines.size();

			double p = 1.0 / 3.0 * Math.pow(1.0 - noiseRatio, 2);

			double confEfficience = 0.9999;
			int it = (int) (Math.log(1 - confEfficience) / Math.log(1.0 - p));
			System.out.println("Iterations : " + it);
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
				double[] vp1 = new double[] { vp1_Img[0] / vp1_Img[2] - pp[0], vp1_Img[1] / vp1_Img[2] - pp[1], f };
				if (vp1[2] == 0)
					vp1[2] = 0.0011;
				double N = Math.sqrt(vp1[0] * vp1[0] + vp1[1] * vp1[1] + vp1[2] * vp1[2]);
				vp1[0] *= 1.0 / N;
				vp1[1] *= 1.0 / N;
				vp1[2] *= 1.0 / N;

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
					N = Math.sqrt(vp2[0] * vp2[0] + vp2[1] * vp2[1] + vp2[2] * vp2[2]);
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
			System.out.println("vpHypo : " + vpHypo.size());
			double oneDegree = 1.0 / 180.0 * Math.PI;

			// get the corresponding line length of every hypotheses
			double[] lineLength = new double[num];
			for (int i = 0; i < num; ++i) {
				for (int j = 0; j < 3; ++j) {
					if (vpHypo.get(i)[j][2] == 0.0)
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
				}
			}
			double[][] vpsOnImg = new double[3][2];
			double[][] vps = vpHypo.get(bestIdx);
			for (int index = 0; index < 3; index++) {
				vpsOnImg[index][0] = vps[index][0] * f / vps[index][2] + pp[0];
				vpsOnImg[index][1] = vps[index][1] * f / vps[index][2] + pp[1];
				// vpsOnImg[index][2] = 1;
			}
			System.out.println("find best model : " + lineLength[bestIdx] + "        " + Arrays.deepToString(vpsOnImg));

			return vpHypo.get(bestIdx);
		}

		Map<Integer, List<Integer>> lines2Vps(double thAngle) {

			// get the corresponding vanish points on the image plane
			List<double[]> vp2D = new ArrayList<>();
			for (int i = 0; i < 3; ++i)
				vp2D.add(new double[] { vps[i][0] * f / vps[i][2] + pp[0], vps[i][1] * f / vps[i][2] + pp[1] });
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

				//
				if (minAngle < thAngle)
					clusters.get(bestIdx).add(i);
			}
			return clusters;
		}
	}

	public static double[][] findHomography(Size size, double[] vp1, double[] vp2, boolean clip/* true */, int clip_factor/* 3 */) {
		double[] vanishing_line = new double[] { vp1[1] * vp2[2] - vp1[2] * vp2[1], vp1[2] * vp2[0] - vp1[0] * vp2[2], vp1[0] * vp2[1] - vp1[1] * vp2[0] };
		// System.out.println(Arrays.toString(vanishing_line));

		double[][] H = new double[][] { new double[] { 1, 0, 0 }, new double[] { 0, 1, 0 }, new double[] { 0, 0, 1 } };
		H[2] = new double[] { vanishing_line[0] / vanishing_line[2], vanishing_line[1] / vanishing_line[2], 1 };
		// H=H/H[2,2];
		// System.out.println(Arrays.deepToString(H));
		double[] v_post1 = new double[] { H[0][0] * vp1[0] + H[0][1] * vp1[1] + H[0][2] * vp1[2], H[1][0] * vp1[0] + H[1][1] * vp1[1] + H[1][2] * vp1[2], H[2][0] * vp1[0] + H[2][1] * vp1[1] + H[2][2] * vp1[2] };
		double[] v_post2 = new double[] { H[0][0] * vp2[0] + H[0][1] * vp2[1] + H[0][2] * vp2[2], H[1][0] * vp2[0] + H[1][1] * vp2[1] + H[1][2] * vp2[2], H[2][0] * vp2[0] + H[2][1] * vp2[1] + H[2][2] * vp2[2] };

		double norm1 = Math.sqrt(v_post1[0] * v_post1[0] + v_post1[1] * v_post1[1]);
		v_post1 = new double[] { v_post1[0] / norm1, v_post1[1] / norm1, v_post1[2] / norm1 };

		double norm2 = Math.sqrt(v_post2[0] * v_post2[0] + v_post2[1] * v_post2[1]);

		v_post2 = new double[] { v_post2[0] / norm2, v_post2[1] / norm2, v_post2[2] / norm2 };

		double[][] directions = new double[][] { new double[] { v_post1[0], -v_post1[0], v_post2[0], -v_post2[0] }, new double[] { v_post1[1], -v_post1[1], v_post2[1], -v_post2[1] } };
		// System.out.println(Arrays.deepToString(directions));

		double thetas[] = new double[] { Math.atan2(directions[0][0], directions[1][0]), Math.atan2(directions[0][1], directions[1][1]), Math.atan2(directions[0][2], directions[1][2]), Math.atan2(directions[0][3], directions[1][3]) };
		// System.out.println(Arrays.toString(thetas));
		int h_ind = 0;
		double min = Double.MAX_VALUE;
		for (int i = 0; i < thetas.length; i++) {
			double absTheta = Math.abs(thetas[i]);
			if (absTheta < min) {
				h_ind = i;
				min = absTheta;
			}
		}
		// System.out.println(h_ind);
		int v_ind = thetas[2] >= thetas[3] ? 0 : 1;
		if (h_ind / 2 == 0)
			v_ind += 2;
		// System.out.println(v_ind);
		double[][] A1 = new double[][] { new double[] { directions[0][v_ind], directions[0][h_ind], 0 }, new double[] { directions[1][v_ind], directions[1][h_ind], 0 }, new double[] { 0, 0, 1 } };

		LUDecomposition realA1 = new LUDecomposition(MatrixUtils.createRealMatrix(A1));
		if (realA1.getDeterminant() < 0)
			for (int i = 0; i < A1.length; i++)
				A1[i][0] = -A1[i][0];

		RealMatrix A = new LUDecomposition(MatrixUtils.createRealMatrix(A1)).getSolver().getInverse();
		// System.out.println(Arrays.deepToString(A.getData()));
		RealMatrix inter_matrix = A.multiply(MatrixUtils.createRealMatrix(H));
		// System.out.println(image.size());
		double[][] cords = inter_matrix.multiply(MatrixUtils.createRealMatrix(new double[][] { new double[] { 0, 0, size.width, size.width }, new double[] { 0, size.height, 0, size.height }, new double[] { 1, 1, 1, 1 } })).getData();
		cords = new double[][] { new double[] { cords[0][0] / cords[2][0], cords[0][1] / cords[2][1], cords[0][2] / cords[2][2], cords[0][3] / cords[2][3] },
				new double[] { cords[1][0] / cords[2][0], cords[1][1] / cords[2][1], cords[1][2] / cords[2][2], cords[1][3] / cords[2][3] } };
		// System.out.println(Arrays.deepToString(cords));
		double tx = Math.min(0, Math.min(Math.min(Math.min(cords[0][0], cords[0][1]), cords[0][2]), cords[0][3]));
		double ty = Math.min(0, Math.min(Math.min(Math.min(cords[1][0], cords[1][1]), cords[1][2]), cords[1][3]));
		double max_x = Math.max(Math.max(Math.max(cords[0][0], cords[0][1]), cords[0][2]), cords[0][3]) - tx;
		double max_y = Math.max(Math.max(Math.max(cords[1][0], cords[1][1]), cords[1][2]), cords[1][3]) - ty;
		if (clip) {
			int max_offset = (int) (Math.max(size.width, size.height) * clip_factor / 2);
			tx = Math.max(tx, -max_offset);
			ty = Math.max(ty, -max_offset);

			max_x = Math.min(max_x, -tx + max_offset);
			max_y = Math.min(max_y, -ty + max_offset);
		}
		// System.out.println(max_x + " " + max_y);
		double[][] T = new double[][] { new double[] { 1, 0, -tx }, new double[] { 0, 1, -ty }, new double[] { 0, 0, 1 } };

		return MatrixUtils.createRealMatrix(T).multiply(inter_matrix).getData();
	}

	public static Mat findHomography(Size size, double[] origin, double[] vp1, double[] vp2) {

		double[] A = new double[] { 0, 0, 1 };
		double[] B = new double[] { size.width, 0, 1 };
		double[] C = new double[] { size.width, size.height, 1 };
		double[] D = new double[] { 0, size.height, 1 };
		double[] X = cross(cross(vp2, origin), cross(A, D));
		System.out.println("X=" + Arrays.toString(X));

		double[] Y = cross(cross(vp1, origin), cross(A, B));
		System.out.println("Y=" + Arrays.toString(Y));
		double[] Z = cross(cross(vp2, origin), cross(B, C));
		double[] W = cross(cross(vp1, origin), cross(D, C));
		double[] A_ = cross(cross(vp1, X), cross(vp2, Y));
		double[] B_ = cross(cross(vp1, Z), cross(vp2, Y));
		double[] C_ = cross(cross(vp1, Z), cross(vp2, W));
		double[] D_ = cross(cross(vp1, X), cross(vp2, W));
		System.out.println("A_=" + Arrays.toString(A_));
		System.out.println("B_=" + Arrays.toString(B_));
		System.out.println("C_=" + Arrays.toString(C_));
		System.out.println("D_=" + Arrays.toString(D_));
		return Imgproc.getPerspectiveTransform(new MatOfPoint2f(new Point(A_), new Point(B_), new Point(C_), new Point(D_)), new MatOfPoint2f(new Point(A), new Point(B), new Point(C), new Point(D)));

	}

	static double[] cross(double[] a, double b[]) {
		return new double[] { a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0] };
	}

	static double[] toZ1(double[] a) {
		return new double[] { a[0] / a[2], a[1] / a[2], 1.0 };
	}

}