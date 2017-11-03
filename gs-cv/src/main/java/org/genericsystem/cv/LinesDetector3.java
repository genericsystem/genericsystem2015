package org.genericsystem.cv;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

import org.genericsystem.cv.lm.LMHostImpl;
import org.genericsystem.cv.utils.Line;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.genericsystem.cv.utils.Ransac;
import org.genericsystem.cv.utils.Ransac.Model;
import org.genericsystem.cv.utils.Tools;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.opencv.videoio.VideoCapture;

public class LinesDetector3 extends AbstractApp {

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
		capture.read(frame);
		ImageView frameView = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(frameView, 0, 0);
		ImageView deskewedView = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(deskewedView, 0, 1);
		Mat dePerspectived = frame.clone();
		timer.scheduleAtFixedRate(() -> {
			try {
				capture.read(frame);
				Img grad = new Img(frame, false).morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(2, 2)).otsu().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(7, 7));
				// Img grad = new Img(frame, false).canny(60, 180);
				// Img grad = new Img(frame, false).bilateralFilter(20, 120, 120).bgr2Gray().adaptativeThresHold(255, Imgproc.ADAPTIVE_THRESH_GAUSSIAN_C, Imgproc.THRESH_BINARY_INV, 11, 3)
				// .morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(5, 5));
				Lines lines = new Lines(grad.houghLinesP(1, Math.PI / 180, 10, 100, 10));
				System.out.println("Average angle: " + lines.getMean() / Math.PI * 180);
				if (lines.size() > 10) {
					lines.draw(frame, new Scalar(0, 0, 255));
					frameView.setImage(Tools.mat2jfxImage(frame));
					Ransac<Line> ransac = lines.vanishingPointRansac(frame.width(), frame.height());
					Mat vp_mat = (Mat) ransac.getBestModel().getParams()[0];
					Point vp = new Point(vp_mat.get(0, 0)[0], vp_mat.get(1, 0)[0]);
					Matrix vpCalib = Matrix.convert(Lines.K.inv()).times(new Matrix(new double[][] { { vp.x }, { vp.y }, { 1d } }), 1);
					LMHostImpl<Line> fitHost = new LMHostImpl<>((datas, params) -> {
						Matrix lineMat = Matrix.convert(Lines.getLineMat(datas));
						double di = params[0] * lineMat.get(0, 0) + params[1] * lineMat.get(1, 0) + params[2] * lineMat.get(2, 0);
						di /= (Math.sqrt(params[0] * params[0] + params[1] * params[1] + params[2] * params[2]) * lineMat.norm());
						return di * di;
					}, ransac.getBestDataSet().values(), new double[] { vpCalib.get(0, 0), vpCalib.get(1, 0), vpCalib.get(2, 0) });
					double[] newVp = fitHost.getParms();
					Matrix result = Matrix.convert(Lines.K).times(new Matrix(new double[][] { { newVp[0] }, { newVp[1] }, { newVp[2] } }), 1);
					if (result.get(2, 0) != 0) {
						result.set(0, 0, result.get(0, 0) / result.get(2, 0));
						result.set(1, 0, result.get(1, 0) / result.get(2, 0));
						result.set(2, 0, 1d);
					} else {
						// Since this is infinite, it is better to leave it calibrated
						// Core.gemm(K.inv(), vp, 1, new Mat(), 0, vp);
						result = Matrix.convert(Lines.K).times(result, 1);
					}
					System.out.println("Old vp = " + vp);
					System.out.println("New vp = " + result.get(0, 0) + " " + result.get(1, 0));
					Point bary = new Point(frame.width() / 2, frame.height() / 2);
					Mat homography = findHomography(new Point(vp.x, vp.y), bary, frame.width(), frame.height());
					lines = Lines.of(ransac.getBestDataSet().values());
					lines = Lines.of(lines.perspectivTransform(homography));

					Mat mask = new Mat(frame.size(), CvType.CV_8UC1, new Scalar(255));
					Mat maskWarpped = new Mat();
					Imgproc.warpPerspective(mask, maskWarpped, homography, frame.size());
					Mat tmp = new Mat();
					Imgproc.warpPerspective(frame, tmp, homography, frame.size(), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(255));
					tmp.copyTo(dePerspectived, maskWarpped);
					lines.draw(dePerspectived, new Scalar(0, 255, 0));
					deskewedView.setImage(Tools.mat2jfxImage(dePerspectived));

				} else
					System.out.println("Not enough lines : " + lines.size());

			} catch (Throwable e) {
				e.printStackTrace();
			}

		}, 33, 250, TimeUnit.MILLISECONDS);

	}

	public Point[] rotate(Point bary, double alpha, Point... p) {
		Mat matrix = Imgproc.getRotationMatrix2D(bary, alpha / Math.PI * 180, 1);
		MatOfPoint2f results = new MatOfPoint2f();
		Core.transform(new MatOfPoint2f(p), results, matrix);
		return results.toArray();
	}

	private Mat findHomography(Point vp, Point bary, double width, double height) {

		double alpha_ = Math.atan2((vp.y - bary.y), (vp.x - bary.x));
		if (alpha_ < -Math.PI / 2 && alpha_ > -Math.PI)
			alpha_ = alpha_ + Math.PI;
		if (alpha_ < Math.PI && alpha_ > Math.PI / 2)
			alpha_ = alpha_ - Math.PI;
		double alpha = alpha_;

		Point rotatedVp = rotate(bary, alpha, vp)[0];

		Point A = new Point(0, 0);
		Point B = new Point(width, 0);
		Point C = new Point(width, height);
		Point D = new Point(0, height);

		Point AB2 = new Point(width / 2, 0);
		Point CD2 = new Point(width / 2, height);

		Point A_, B_, C_, D_;
		if (rotatedVp.x >= width / 2) {
			A_ = new Line(AB2, rotatedVp).intersection(0);
			D_ = new Line(CD2, rotatedVp).intersection(0);
			C_ = new Line(A_, bary).intersection(new Line(CD2, rotatedVp));
			B_ = new Line(D_, bary).intersection(new Line(AB2, rotatedVp));
		} else {
			B_ = new Line(AB2, rotatedVp).intersection(width);
			C_ = new Line(CD2, rotatedVp).intersection(width);
			A_ = new Line(C_, bary).intersection(new Line(AB2, rotatedVp));
			D_ = new Line(B_, bary).intersection(new Line(CD2, rotatedVp));
		}

		System.out.println("vp : " + vp);
		System.out.println("rotated vp : " + rotatedVp);
		System.out.println("Alpha : " + alpha * 180 / Math.PI);
		// System.out.println("A : " + A + " " + A_);
		// System.out.println("B : " + B + " " + B_);
		// System.out.println("C : " + C + " " + C_);
		// System.out.println("D : " + D + " " + D_);

		return Imgproc.getPerspectiveTransform(new MatOfPoint2f(rotate(bary, -alpha, A_, B_, C_, D_)), new MatOfPoint2f(A, B, C, D));
	}

	public static class Lines extends org.genericsystem.cv.utils.Lines {

		private static Mat K;

		public Lines(Mat src) {
			super(src);
		}

		public Lines(Collection<Line> lines) {
			super(lines);
		}

		public static Lines of(Collection<Line> lines) {
			return new Lines(lines);
		}

		public static void calibrate(Mat uncalibrate) {
			Core.gemm(Lines.K.inv(), uncalibrate, 1, new Mat(), 0, uncalibrate);
			Core.normalize(uncalibrate, uncalibrate);
		}

		public static Mat uncalibrate(Mat calibrated) {
			Mat uncalibrate = new Mat(3, 1, CvType.CV_64FC1);
			Core.gemm(Lines.K, calibrated, 1, new Mat(), 0, uncalibrate);
			if (uncalibrate.get(2, 0)[0] != 0) {
				uncalibrate.put(0, 0, uncalibrate.get(0, 0)[0] / uncalibrate.get(2, 0)[0]);
				uncalibrate.put(1, 0, uncalibrate.get(1, 0)[0] / uncalibrate.get(2, 0)[0]);
				uncalibrate.put(2, 0, 1);
			}
			return uncalibrate;
		}

		private static Mat getLineMat(Line line) {
			Mat a = new Mat(3, 1, CvType.CV_64FC1);
			Mat b = new Mat(3, 1, CvType.CV_64FC1);
			a.put(0, 0, line.getX1());
			a.put(1, 0, line.getY1());
			a.put(2, 0, 1d);
			b.put(0, 0, line.getX2());
			b.put(1, 0, line.getY2());
			b.put(2, 0, 1d);
			Mat an = new Mat(3, 1, CvType.CV_64FC1);
			Mat bn = new Mat(3, 1, CvType.CV_64FC1);
			Core.gemm(K.inv(), a, 1, new Mat(), 0, an);
			Core.gemm(K.inv(), b, 1, new Mat(), 0, bn);
			Mat li = an.cross(bn);
			Core.normalize(li, li);
			a.release();
			b.release();
			an.release();
			bn.release();
			return li;
		}

		public Ransac<Line> vanishingPointRansac(double width, double height) {
			int minimal_sample_set_dimension = 2;
			double maxError = (float) 0.01623 * 2;
			if (K == null) {
				K = new Mat(3, 3, CvType.CV_64FC1, new Scalar(0));
				K.put(0, 0, width);
				K.put(0, 2, width / 2);
				K.put(1, 1, height);
				K.put(1, 2, height / 2);
				K.put(2, 2, 1d);
			}
			return new Ransac<>(getLines(), getModelProvider(minimal_sample_set_dimension, maxError), minimal_sample_set_dimension, 100, maxError, Double.valueOf(Math.floor(this.size() * 0.7)).intValue());
		}

		private Function<Collection<Line>, Model<Line>> getModelProvider(int minimal_sample_set_dimension, double maxError) {
			return datas -> {
				Mat[] vp = new Mat[1];
				if (datas.size() == minimal_sample_set_dimension) {
					Iterator<Line> it = datas.iterator();
					vp[0] = getLineMat(it.next()).cross(getLineMat(it.next()));
					Core.normalize(vp[0], vp[0]);
				} else {
					// Extract the line segments corresponding to the indexes contained in the set
					Mat li_set = new Mat(3, datas.size(), CvType.CV_64FC1);
					Mat tau = new Mat(datas.size(), datas.size(), CvType.CV_64FC1, new Scalar(0, 0, 0));
					int i = 0;
					for (Line line : datas) {
						Mat li = getLineMat(line);
						li_set.put(0, i, li.get(0, 0));
						li_set.put(1, i, li.get(1, 0));
						li_set.put(2, i, li.get(2, 0));
						tau.put(i, i, line.size());
						i++;
					}

					// Least squares solution
					// Generate the matrix ATA (from LSS_set=A)
					Mat L = li_set.t();
					Mat ATA = new Mat(3, 3, CvType.CV_64FC1);
					Mat dst = new Mat();

					Core.gemm(L.t(), tau.t(), 1, new Mat(), 0, dst);
					Core.gemm(dst, tau, 1, new Mat(), 0, dst);
					Core.gemm(dst, L, 1, new Mat(), 0, ATA);

					// Obtain eigendecomposition
					Mat v = new Mat();
					Core.SVDecomp(ATA, new Mat(), v, new Mat());

					// Check eigenvecs after SVDecomp
					if (v.rows() < 3)
						throw new IllegalStateException();

					// Assign the result (the last column of v, corresponding to the eigenvector with lowest eigenvalue)
					vp[0] = new Mat(3, 1, CvType.CV_64FC1);
					vp[0].put(0, 0, v.get(0, 2));
					vp[0].put(1, 0, v.get(1, 2));
					vp[0].put(2, 0, v.get(2, 2));
					Core.normalize(vp[0], vp[0]);
					vp[0] = uncalibrate(vp[0]);
				}

				return new Model<Line>() {
					@Override
					public double computeError(Line line) {
						Mat lineMat = getLineMat(line);
						double di = vp[0].dot(lineMat);
						di /= (Core.norm(vp[0]) * Core.norm(lineMat));
						return di * di;
					}

					@Override
					public double computeGlobalError(List<Line> datas, Collection<Line> consensusDatas) {
						double globalError = 0;
						for (Line line : datas) {
							double error = computeError(line);
							if (error > maxError)
								error = maxError;
							globalError += error;
						}
						globalError = globalError / datas.size();
						return globalError;
					}

					@Override
					public Object[] getParams() {
						return new Object[] { vp[0] };
					}

				};
			};
		}
	}

	@Override
	public void stop() throws Exception {
		super.stop();
		timer.shutdown();
		timer.awaitTermination(5000, TimeUnit.MILLISECONDS);
		capture.release();
	}

}
