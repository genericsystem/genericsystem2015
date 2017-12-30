package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

import org.apache.commons.math3.linear.DecompositionSolver;
import org.apache.commons.math3.linear.MatrixUtils;
import org.apache.commons.math3.linear.SingularValueDecomposition;
import org.genericsystem.cv.LinesDetector8.Line;
import org.genericsystem.cv.LinesDetector8.Lines;
import org.genericsystem.cv.lm.LMHostImpl;
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

public class LinesDetector9 extends AbstractApp {

	static final double f = 6.053 / 0.009;
	private final VideoCapture capture = new VideoCapture(0);
	private ScheduledExecutorService timer = Executors.newSingleThreadScheduledExecutor();
	private boolean stabilize;
	private Lines lines;
	double[] vps0 = new double[] { 5000, 0, 1 };
	// double[] vps1 = new double[] { 0, 0, 1 };
	static {
		NativeLibraryLoader.load();
	}

	@Override
	protected void onSpace() {
		stabilize = !stabilize;
	}

	@Override
	public void stop() throws Exception {
		super.stop();
		timer.shutdown();
		timer.awaitTermination(5000, TimeUnit.MILLISECONDS);
		capture.release();
	}

	public static void main(String[] args) {
		launch(args);

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
				Img closed = grad.morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(10, 10)).morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(3, 3));
				gradView.setImage(closed.toJfxImage());
				if (!stabilize)
					lines = new Lines(closed.houghLinesP(1, Math.PI / 180, 10, 20, 5));

				if (lines.size() > 10) {
					// lines = lines.reduce(30);
				Mat colorFrame = frame.clone();
				lines.draw(colorFrame, new Scalar(0, 0, 0));

				Img img = new Img(frame, false);

				List<Edgelet> edgelets1 = computeEdgelets(img);

				// double[] vp1 = ransacVanishingPoint(edgelets1, 500, 5);
				// System.out.println("vp1 : " + Arrays.toString(vp1));
				// System.out.println("vp1 : " + vp1[0] / vp1[2] + ", " + vp1[1] / vp1[2] + " ,1");
				// vp1 = reestimate_model(vp1, edgelets1, 5);
				// System.out.println("vp1 reestimation : " + Arrays.toString(vp1));
				// List<Edgelet> edgelets2 = removeInliers(vp1, edgelets1, 30);

				// double[] vp2 = ransacVanishingPoint(edgelets2, 500, 5);
				// System.out.println("vp2 : " + Arrays.toString(vp2));
				// System.out.println("vp2 : " + vp2[0] / vp2[2] + ", " + vp2[1] / vp2[2] + " ,1");
				// vp2 = reestimate_model(vp2, edgelets2, 5);
				// System.out.println("vp2 reestimation : " + Arrays.toString(vp2));

				double[] pp = new double[] { frame.width() / 2, frame.height() / 2 };

				vps0 = new LMHostImpl<>((edgelet, model) -> computeVote(edgelet, model, 5), edgelets1, vps0).getParams();
				double[] calibratedVps0 = new Calibrated(vps0, pp, f).getCalibratexyz();
				// double[] calibratedVps1 = getOrthoVpFromVp(calibratedVps0, 0);
				double[] calibratedVps1 = new Calibrated(new double[] { 320, 240000, 1 }, pp, f).getCalibratexyz();// getOrthoVpFromVp(calibratedVps0, 0);
				System.out.println("SCALAR :" + (calibratedVps0[0] * calibratedVps1[0] + calibratedVps0[1] * calibratedVps1[1] + calibratedVps0[2] * calibratedVps1[2]));
				// vps1 = new LMHostImpl<>((edgelet, model) -> +computeVote(edgelet, new double[] { model[0], model[1], model[2] }, 5), removeInliers(vps0, edgelets1, 10), vps1).getParams();

				// double[][] vps = ransac_3_line(edgelets1, f, pp, 4000, 5);
				// double[][] vps = reorderXyz(new double[][] { calibratedVps0, calibratedVps1 });
				// System.out.println("vp1 ransac normal : " + vp1[0] / vp1[2] + ", " + vp1[1] / vp1[2] + " ,1");

				frameView.setImage(Tools.mat2jfxImage(colorFrame));
				Mat homographyMat = findHomography(frame.size(), reorderXyz(new double[][] { calibratedVps0, calibratedVps1 }), new double[] { frame.width() / 2, frame.height() / 2 }, f);
				Imgproc.warpPerspective(frame, dePerspectived, homographyMat, frame.size(), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(255));
				deskiewedView.setImage(Tools.mat2jfxImage(dePerspectived));
			} else
				System.out.println("Not enough lines : " + lines.size());

		} catch (Throwable e) {
			e.printStackTrace();
		}

	}, 30, 50, TimeUnit.MILLISECONDS);

	}

	public double[] getOrthoVpFromVp(double[] vp1, double lambda) {
		double k1 = vp1[0] * Math.sin(lambda) + vp1[1] * Math.cos(lambda);
		double k2 = vp1[2];
		double phi = Math.atan(-k2 / k1);
		double[] result = new double[] { Math.sin(phi) * Math.sin(lambda), Math.sin(phi) * Math.cos(lambda), Math.cos(phi) };

		if (result[2] == 0.0)
			result[2] = 0.0011;
		double N = Math.sqrt(result[0] * result[0] + result[1] * result[1] + result[2] * result[2]);
		result[0] *= 1.0 / N;
		result[1] *= 1.0 / N;
		result[2] *= 1.0 / N;
		return result;
	}

	public static Mat findHomography(Size size, double[][] vps, double[] pp, double f) {

		// System.out.println(vps[0][0] * vps[0][0] + vps[0][1] * vps[0][1] + vps[0][2] * vps[0][2]);
		// System.out.println(vps[1][0] * vps[1][0] + vps[1][1] * vps[1][1] + vps[1][2] * vps[1][2]);
		// System.out.println(vps[0][0] * vps[1][0] + vps[0][1] * vps[1][1] + vps[0][2] * vps[1][2]);

		double[][] vps2D = getVp2DFromVps(vps, pp, f);
		// System.out.println("vps : " + Arrays.deepToString(vps));
		System.out.println("vps2D : " + Arrays.deepToString(vps2D));

		double theta = Math.acos(vps[0][2]);
		double phi = Math.atan2(vps[0][1], vps[0][0]);

		double theta2 = Math.acos(vps[1][2]);
		double phi2 = Math.atan2(vps[1][1], vps[1][0]);

		// double phi3 = Math.atan2(vps[2][1], vps[2][0]);
		// double theta3 = Math.acos(vps[2][2]);

		double x = size.width / 4;

		double[] A = new double[] { size.width / 2, size.height / 2, 1 };
		double[] B = new double[] { Math.cos(phi) < 0 ? size.width / 2 - x : size.width / 2 + x, size.height / 2 };
		double[] D = new double[] { size.width / 2, Math.sin(phi2) < 0 ? size.height / 2 - x : size.height / 2 + x, 1 };
		double[] C = new double[] { Math.cos(phi) < 0 ? size.width / 2 - x : size.width / 2 + x, Math.sin(phi2) < 0 ? size.height / 2 - x : size.height / 2 + x };

		System.out.println("vp1 (" + theta * 180 / Math.PI + "°, " + phi * 180 / Math.PI + "°)");
		System.out.println("vp2 (" + theta2 * 180 / Math.PI + "°, " + phi2 * 180 / Math.PI + "°)");
		// System.out.println("vp3 (" + phi3 * 180 / Math.PI + "°, " + theta3 * 180 / Math.PI + "°)");

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
		double[][] result = new double[2][3];
		for (int i = 0; i < 2; i++) {
			result[i][0] = vps[i][0] * f / vps[i][2] + pp[0];
			result[i][1] = vps[i][1] * f / vps[i][2] + pp[1];
			result[i][2] = 1.0;
		}
		return result;
	}

	private static class Edgelet {
		final double[] location;
		final double[] direction;
		final double strength;

		public Edgelet(Line line) {
			location = new double[] { (line.x1 + line.x2) / 2, (line.y1 + line.y2) / 2 };
			double dx = line.x2 - line.x1;
			double dy = line.y2 - line.y1;
			strength = Math.sqrt(dx * dx + dy * dy);
			direction = new double[] { dx / strength, dy / strength };
		}
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

	public static List<Edgelet> computeEdgelets(Img image) {
		Img grad = image.morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(10, 10)).otsu();
		Img closed = grad.morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(10, 20)).morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(3, 3));
		Lines lines = new Lines(closed.houghLinesP(1, Math.PI / 180, 10, 20, 5));
		return lines.lines.stream().map(Edgelet::new).collect(Collectors.toList());
	}

	public static List<double[]> edgeletLines(List<Edgelet> edgelets) {
		return edgelets.stream().map(edgelet -> new double[] { edgelet.direction[1], -edgelet.direction[0], edgelet.direction[0] * edgelet.location[1] - edgelet.direction[1] * edgelet.location[0] }).collect(Collectors.toList());
	}

	public static List<Double> computeVotes(List<Edgelet> edgelets, double[] model, double threshold_inlier) {
		double[] vp = new double[] { model[0] / model[2], model[1] / model[2] };
		List<Double> result = new ArrayList<>();
		for (Edgelet edgelet : edgelets)
			result.add(computeVote(edgelet, vp, threshold_inlier));
		return result;
	}

	public static Double computeVote(Edgelet edgelet, double[] vp, double threshold_inlier) {
		double est_directions0 = edgelet.location[0] - vp[0];
		double est_directions1 = edgelet.location[1] - vp[1];
		double dotProd = est_directions0 * edgelet.direction[0] + est_directions1 * edgelet.direction[1];
		double absProd = Math.sqrt(edgelet.direction[0] * edgelet.direction[0] + edgelet.direction[1] * edgelet.direction[1]) * Math.sqrt(est_directions0 * est_directions0 + est_directions1 * est_directions1);
		if (absProd == 0)
			absProd = 1e-5;
		double theta = Math.acos(Math.abs(dotProd / absProd));
		double theta_thresh = threshold_inlier * Math.PI / 180;
		return theta < theta_thresh ? edgelet.strength * (Math.sin(2 * theta) + 0.2) : 0;
	}

	private static List<Integer> reverseArgSort(final List<Double> a) {
		List<Integer> indexes = new ArrayList<>(a.size());
		for (int i = 0; i < a.size(); i++)
			indexes.add(i);
		Collections.sort(indexes, (i1, i2) -> -Double.compare(a.get(i1), a.get(i2)));
		return indexes;
	}

	public static double[] ransacVanishingPoint(List<Edgelet> edgelets, int numRansacIter, double thresholdInlier) {
		List<Double> strengths = edgelets.stream().map(edgelet -> edgelet.strength).collect(Collectors.toList());
		List<double[]> lines = edgeletLines(edgelets);
		List<Integer> sorted = reverseArgSort(strengths);
		List<Integer> first_index_space = new ArrayList<>(sorted.subList(0, strengths.size() / 5));
		List<Integer> second_index_space = new ArrayList<>(sorted.subList(0, strengths.size() / 2));
		double[] best_model = null;
		double best_votes = 0;
		for (int ransacIter = 0; ransacIter < numRansacIter; ransacIter++) {
			double[] l1 = lines.get(first_index_space.get((int) (Math.random() * first_index_space.size())));
			double[] l2 = lines.get(second_index_space.get((int) (Math.random() * second_index_space.size())));
			double[] current_model = cross(l1, l2);
			if (current_model[0] * current_model[0] + current_model[1] * current_model[1] + current_model[2] * current_model[2] < 1 || current_model[2] == 0)
				continue;
			double current_votes = computeVotes(edgelets, current_model, thresholdInlier).stream().mapToDouble(d -> d).sum();
			if (current_votes > best_votes) {
				best_model = current_model;
				best_votes = current_votes;
			}
		}
		return best_model;
	}

	// public static double[][] ransac_3_line(List<Edgelet> edgelets, double f, double[] pp, int num_ransac_iter/* 2000 */, int thresholdInlier/* =5 */) {
	// List<Double> strengths = edgelets.stream().map(edgelet -> edgelet.strength).collect(Collectors.toList());
	// List<double[]> lines = edgeletLines(edgelets);
	// List<Integer> sorted = reverseArgSort(strengths);
	// List<Integer> first_index_space = new ArrayList<>(sorted.subList(0, strengths.size() / 2));
	// List<Integer> second_index_space = new ArrayList<>(sorted.subList(0, strengths.size() / 2));
	// List<Integer> third_index_space = new ArrayList<>(sorted.subList(0, strengths.size()));
	//
	// double[][] best_model = null;
	// double best_votes = 0;
	//
	// for (int ransacIter = 0; ransacIter < num_ransac_iter; ransacIter++) {
	// double[] l1 = lines.get(first_index_space.get((int) (Math.random() * first_index_space.size())));
	// double[] l2 = lines.get(second_index_space.get((int) (Math.random() * second_index_space.size())));
	// double[] l3 = lines.get(third_index_space.get((int) (Math.random() * third_index_space.size())));
	//
	// double[] vp1 = cross(l1, l2);
	// if ((vp1[0] * vp1[0] + vp1[1] * vp1[1] + vp1[2] * vp1[2] < 1) || vp1[2] == 0)
	// continue;
	// double[] h = new double[] { vp1[0] / f, vp1[1] / f, vp1[2] };
	// double[] vp2 = cross(h, l3);
	// if ((vp2[0] * vp2[0] + vp2[1] * vp2[1] + vp2[2] * vp2[2] < 1) || vp2[2] == 0)
	// continue;
	// double current_votes = computeVotes(edgelets, vp1, thresholdInlier).stream().filter(d -> d > 0).mapToDouble(d -> d).sum() + computeVotes(edgelets, vp2, thresholdInlier).stream().filter(d -> d > 0).mapToDouble(d -> d).sum();
	// if (current_votes > best_votes) {
	// best_model = new double[][] { vp1, vp2 };
	// best_votes = current_votes;
	// // System.out.println("Current best model has : " + current_votes + " votes at iteration : " + ransacIter);
	// }
	// }
	// return reorderXyz(best_model, pp);
	// }

	private static double[][] reorderXyz(double[][] calibrateds) {
		if (calibrateds[0][1] * calibrateds[0][1] < calibrateds[1][1] * calibrateds[1][1])
			return new double[][] { calibrateds[0], calibrateds[1] };
		else
			return new double[][] { calibrateds[1], calibrateds[0] };
	}

	public static double[] reestimate_model(double[] model, List<Edgelet> edgelets, int threshold_reestimate/* =5 */) {
		List<Double> votes = computeVotes(edgelets, model, threshold_reestimate);
		List<Edgelet> inliersEdgelets = new ArrayList<>();
		for (int i = 0; i < edgelets.size(); i++)
			if (votes.get(i) > 0)
				inliersEdgelets.add(edgelets.get(i));
		List<double[]> lines = edgeletLines(inliersEdgelets);
		double[][] a = lines.stream().map(line -> new double[] { line[0], line[1] }).toArray(double[][]::new);
		double[] b = lines.stream().mapToDouble(line -> -line[2]).toArray();
		DecompositionSolver ds = new SingularValueDecomposition(MatrixUtils.createRealMatrix(a)).getSolver();
		double[] est_model = ds.solve(MatrixUtils.createRealVector(b)).toArray();
		return new double[] { est_model[0], est_model[1], 1.0 };
	}

	public static List<Edgelet> removeInliers(double[] model, List<Edgelet> edgelets, int threshold_inlier/* 10 */) {
		List<Double> votes = computeVotes(edgelets, model, threshold_inlier);
		List<Edgelet> ouliers = new ArrayList<>();
		for (int i = 0; i < edgelets.size(); i++)
			if (votes.get(i) <= 0)
				ouliers.add(edgelets.get(i));
		return ouliers;
	}

	// public static Img computeHomographyAndWarp(Img image, double[] vp1, double[] vp2, boolean clip/* true */, int clip_factor/* 3 */) {
	// double[] vanishing_line = new double[] { vp1[1] * vp2[2] - vp1[2] * vp2[1], vp1[2] * vp2[0] - vp1[0] * vp2[2], vp1[0] * vp2[1] - vp1[1] * vp2[0] };
	// // System.out.println(Arrays.toString(vanishing_line));
	//
	// double[][] H = new double[][] { new double[] { 1, 0, 0 }, new double[] { 0, 1, 0 }, new double[] { 0, 0, 1 } };
	// H[2] = new double[] { vanishing_line[0] / vanishing_line[2], vanishing_line[1] / vanishing_line[2], 1 };
	// // H=H/H[2,2];
	// // System.out.println(Arrays.deepToString(H));
	// double[] v_post1 = new double[] { H[0][0] * vp1[0] + H[0][1] * vp1[1] + H[0][2] * vp1[2], H[1][0] * vp1[0] + H[1][1] * vp1[1] + H[1][2] * vp1[2], H[2][0] * vp1[0] + H[2][1] * vp1[1] + H[2][2] * vp1[2] };
	// double[] v_post2 = new double[] { H[0][0] * vp2[0] + H[0][1] * vp2[1] + H[0][2] * vp2[2], H[1][0] * vp2[0] + H[1][1] * vp2[1] + H[1][2] * vp2[2], H[2][0] * vp2[0] + H[2][1] * vp2[1] + H[2][2] * vp2[2] };
	//
	// double norm1 = Math.sqrt(v_post1[0] * v_post1[0] + v_post1[1] * v_post1[1]);
	// v_post1 = new double[] { v_post1[0] / norm1, v_post1[1] / norm1, v_post1[2] / norm1 };
	//
	// double norm2 = Math.sqrt(v_post2[0] * v_post2[0] + v_post2[1] * v_post2[1]);
	//
	// v_post2 = new double[] { v_post2[0] / norm2, v_post2[1] / norm2, v_post2[2] / norm2 };
	//
	// double[][] directions = new double[][] { new double[] { v_post1[0], -v_post1[0], v_post2[0], -v_post2[0] }, new double[] { v_post1[1], -v_post1[1], v_post2[1], -v_post2[1] } };
	// // System.out.println(Arrays.deepToString(directions));
	//
	// double thetas[] = new double[] { Math.atan2(directions[0][0], directions[1][0]), Math.atan2(directions[0][1], directions[1][1]), Math.atan2(directions[0][2], directions[1][2]), Math.atan2(directions[0][3], directions[1][3]) };
	// // System.out.println(Arrays.toString(thetas));
	// int h_ind = 0;
	// double min = Double.MAX_VALUE;
	// for (int i = 0; i < thetas.length; i++) {
	// double absTheta = Math.abs(thetas[i]);
	// if (absTheta < min) {
	// h_ind = i;
	// min = absTheta;
	// }
	// }
	// // System.out.println(h_ind);
	// int v_ind = thetas[2] >= thetas[3] ? 0 : 1;
	// if (h_ind / 2 == 0)
	// v_ind += 2;
	// // System.out.println(v_ind);
	// double[][] A1 = new double[][] { new double[] { directions[0][v_ind], directions[0][h_ind], 0 }, new double[] { directions[1][v_ind], directions[1][h_ind], 0 }, new double[] { 0, 0, 1 } };
	//
	// LUDecomposition realA1 = new LUDecomposition(MatrixUtils.createRealMatrix(A1));
	// if (realA1.getDeterminant() < 0)
	// for (int i = 0; i < A1.length; i++)
	// A1[i][0] = -A1[i][0];
	//
	// RealMatrix A = new LUDecomposition(MatrixUtils.createRealMatrix(A1)).getSolver().getInverse();
	// // System.out.println(Arrays.deepToString(A.getData()));
	// RealMatrix inter_matrix = A.multiply(MatrixUtils.createRealMatrix(H));
	// // System.out.println(image.size());
	// double[][] cords = inter_matrix.multiply(MatrixUtils.createRealMatrix(new double[][] { new double[] { 0, 0, image.cols(), image.cols() }, new double[] { 0, image.rows(), 0, image.rows() }, new double[] { 1, 1, 1, 1 } })).getData();
	// cords = new double[][] { new double[] { cords[0][0] / cords[2][0], cords[0][1] / cords[2][1], cords[0][2] / cords[2][2], cords[0][3] / cords[2][3] },
	// new double[] { cords[1][0] / cords[2][0], cords[1][1] / cords[2][1], cords[1][2] / cords[2][2], cords[1][3] / cords[2][3] } };
	// // System.out.println(Arrays.deepToString(cords));
	// double tx = Math.min(0, Math.min(Math.min(Math.min(cords[0][0], cords[0][1]), cords[0][2]), cords[0][3]));
	// double ty = Math.min(0, Math.min(Math.min(Math.min(cords[1][0], cords[1][1]), cords[1][2]), cords[1][3]));
	// double max_x = Math.max(Math.max(Math.max(cords[0][0], cords[0][1]), cords[0][2]), cords[0][3]) - tx;
	// double max_y = Math.max(Math.max(Math.max(cords[1][0], cords[1][1]), cords[1][2]), cords[1][3]) - ty;
	// if (clip) {
	// int max_offset = Math.max(image.width(), image.height()) * clip_factor / 2;
	// tx = Math.max(tx, -max_offset);
	// ty = Math.max(ty, -max_offset);
	//
	// max_x = Math.min(max_x, -tx + max_offset);
	// max_y = Math.min(max_y, -ty + max_offset);
	// }
	// // System.out.println(max_x + " " + max_y);
	// double[][] T = new double[][] { new double[] { 1, 0, -tx }, new double[] { 0, 1, -ty }, new double[] { 0, 0, 1 } };
	//
	// double[][] final_homography = MatrixUtils.createRealMatrix(T).multiply(inter_matrix).getData();
	// Mat warped_img = new Mat();
	// // System.out.println(Arrays.deepToString(final_homography));
	// Mat homography = new Mat(3, 3, CvType.CV_64FC1);
	// for (int row = 0; row < 3; row++)
	// for (int col = 0; col < 3; col++)
	// homography.put(row, col, final_homography[row][col]);
	// Imgproc.warpPerspective(image.getSrc(), warped_img, homography, new Size(max_x, max_y), Imgproc.INTER_LINEAR, Core.BORDER_CONSTANT, Scalar.all(0));
	// return new Img(warped_img, false);
	// }
}