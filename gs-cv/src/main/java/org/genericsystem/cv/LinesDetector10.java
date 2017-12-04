package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.genericsystem.cv.LinesDetector8.Line;
import org.genericsystem.cv.LinesDetector8.Lines;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;

public class LinesDetector10 {

	private final List<Line> lines;
	private final double[] pp;
	private final double f;
	private double noiseRatio = 0.5;
	private List<LineInfo> lineInfos = new ArrayList<>();

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

	public static void main(String[] args) {
		Img img = new Img(Imgcodecs.imread("resources/img.jpg"));
		Img grad = img.bgr2Gray().canny(20, 80).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(60, 60)).grad(6, 6);// .morphologyEx(Imgproc.MORPH_DILATE, Imgproc.MORPH_ELLIPSE, new Size(5, 5));
		// Img grad = img.bilateralFilter(20, 80, 80).bgr2Gray().adaptativeThresHold(255, Imgproc.ADAPTIVE_THRESH_GAUSSIAN_C, Imgproc.THRESH_BINARY_INV, 17, 3).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(60, 60)).grad(5, 5)
		// .morphologyEx(Imgproc.MORPH_DILATE, Imgproc.MORPH_ELLIPSE, new Size(5, 5));
		// Img grad = img.grad(3, 3).bgr2Gray().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(60, 60)).grad(5, 5).morphologyEx(Imgproc.MORPH_DILATE, Imgproc.MORPH_ELLIPSE, new Size(5, 5));

		Lines lines = new Lines(grad.houghLinesP(1, Math.PI / 180, 10, 100, 10));
		double f = 6.053 / 0.009; // Focal length (in pixel)
		Imgcodecs.imwrite("resources/img4.jpg", grad.getSrc());
		// lines.draw(img.getSrc(), new Scalar(255));
		Imgcodecs.imwrite("resources/img5.jpg", img.getSrc());
		new LinesDetector10(img, lines.lines, new double[] { img.width() / 2, img.height() / 2 }, f);
	}

	public LinesDetector10(Img img, List<Line> lines, double[] pp, double f) {

		this.lines = lines;
		this.pp = pp;
		this.f = f;

		List<double[][]> vpHypo = getVPHypVia2Lines();

		double[][] sphereGrid = getSphereGrids();

		double vps[][] = getBestVpsHyp(sphereGrid, vpHypo);

		// System.out.println(Arrays.deepToString(vps));
		double[][] vpsOnImg = new double[3][3];
		for (int i = 0; i < 3; i++) {
			vpsOnImg[i][0] = vps[i][0] * f / vps[i][2] + pp[0];
			vpsOnImg[i][1] = vps[i][1] * f / vps[i][2] + pp[1];
			vpsOnImg[i][2] = 1;
		}
		System.out.println(Arrays.deepToString(vpsOnImg));
		double thAngle = 6.0 / 180.0 * Math.PI;
		Map<Integer, List<Integer>> clusters = lines2Vps(thAngle, vps);
		for (int cluster : clusters.keySet()) {
			for (int lineId : clusters.get(cluster))
				lines.get(lineId).draw(img.getSrc(), new Scalar(cluster == 0 ? 255 : 0, cluster == 1 ? 255 : 0, cluster == 2 ? 255 : 0));

		}
		// double[][] vpsOnImg = new double[][] { new double[] { -1005.4075382803298, 942.3380447585396, 1.0 }, new double[] { 3000, 1000, 1.0 }, new double[] { 320.9855258236744, -1007516.031731498057, 1.0 } };
		Imgcodecs.imwrite("resources/img1.jpg", LinesDetector9.computeHomographyAndWarp(img, vpsOnImg[0], vpsOnImg[1], true, 3).getSrc());
		Imgcodecs.imwrite("resources/img2.jpg", LinesDetector9.computeHomographyAndWarp(img, vpsOnImg[1], vpsOnImg[2], true, 3).getSrc());
		Imgcodecs.imwrite("resources/img3.jpg", LinesDetector9.computeHomographyAndWarp(img, vpsOnImg[0], vpsOnImg[2], true, 3).getSrc());
	}

	double[] cross(double[] a, double b[]) {
		return new double[] { a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0] };
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
			}

			//
			vpHypo.add(new double[][] { new double[] { vp1[0], vp1[1], vp1[2] }, new double[] { vp2[0], vp2[1], vp2[2] }, new double[] { vp3[0], vp3[1], vp3[2] } });
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

	double[][] getBestVpsHyp(double[][] sphereGrid, List<double[][]> vpHypo) {
		int num = vpHypo.size();
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
				double[][] vpsOnImg = new double[3][2];
				double[][] vps = vpHypo.get(i);
				for (int index = 0; index < 3; index++) {
					vpsOnImg[index][0] = vps[index][0] * f / vps[index][2] + pp[0];
					vpsOnImg[index][1] = vps[index][1] * f / vps[index][2] + pp[1];
					// vpsOnImg[index][2] = 1;
				}
				System.out.println("find best model : " + lineLength[i] + "        " + Arrays.deepToString(vpsOnImg));
				bestIdx = i;

			}
		}
		return vpHypo.get(bestIdx);
	}

	Map<Integer, List<Integer>> lines2Vps(double thAngle, double[][] vps) {

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