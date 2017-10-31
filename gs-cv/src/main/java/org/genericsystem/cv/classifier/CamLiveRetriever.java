package org.genericsystem.cv.classifier;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.Line;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.genericsystem.cv.utils.Ransac;
import org.genericsystem.cv.utils.Ransac.Model;
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
import org.opencv.videoio.VideoCapture;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

@SuppressWarnings({ "resource" })
public class CamLiveRetriever extends AbstractApp {

	static {
		NativeLibraryLoader.load();
	}

	static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private static long counter = 0;

	private static final int STABILIZATION_DELAY = 500;
	private static final int FRAME_DELAY = 100;

	private final ScheduledExecutorService timerFields = new ScheduledThreadPoolExecutor(1, new ThreadPoolExecutor.DiscardPolicy());
	private final VideoCapture capture = new VideoCapture(0);
	private final Fields fields = new Fields();

	private ImgDescriptor stabilizedImgDescriptor;
	private Mat frame = new Mat();
	private boolean stabilizationHasChanged = true;
	private int stabilizationErrors = 0;

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	public void stop() throws Exception {
		super.stop();
		timerFields.shutdown();
		timerFields.awaitTermination(5, TimeUnit.SECONDS);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {

		capture.read(frame);

		ImageView src0 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src0, 0, 0);

		ImageView src1 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src1, 1, 0);

		timerFields.scheduleAtFixedRate(() -> onSpace(), 0, STABILIZATION_DELAY, TimeUnit.MILLISECONDS);

		// Detect the rectangles
		timerFields.scheduleAtFixedRate(() -> {

			try {
				Stats.beginTask("frame");
				capture.read(frame);
				if (frame == null) {
					logger.warn("No frame !");
					return;
				}
				Mat deperspectivGraphy = computeFrameToDeperspectivedHomography(frame);
				if (deperspectivGraphy == null) {
					logger.warn("Unable to compute a valid deperspectivation");
					return;
				}
				if (stabilizationErrors > 20) {
					// TODO: clean fields
					fields.reset();
					stabilizationErrors = 0;
					stabilizedImgDescriptor = null;
				}
				if (stabilizedImgDescriptor == null) {
					stabilizedImgDescriptor = new ImgDescriptor(frame, deperspectivGraphy);
					return;
				}
				Stats.beginTask("newImgDescriptor");
				ImgDescriptor newImgDescriptor = new ImgDescriptor(frame, deperspectivGraphy);
				Stats.endTask("newImgDescriptor");
				Stats.beginTask("computeStabilizationGraphy");
				Mat stabilizationHomography = stabilizedImgDescriptor.computeStabilizationGraphy(newImgDescriptor);
				Stats.endTask("computeStabilizationGraphy");
				if (stabilizationHomography == null) {
					stabilizationErrors++;
					logger.warn("Unable to compute a valid stabilization ({} times)", stabilizationErrors);
					return;
				}
				Img stabilized = warpPerspective(frame, stabilizationHomography);
				if (stabilizationHasChanged) {
					Stats.beginTask("stabilizationHasChanged");
					Mat fieldsHomography = new Mat();
					stabilized = newImgDescriptor.getDeperspectivedImg();
					// Core.gemm(stabilizationHomography.inv(), deperspectivGraphy, 1, new Mat(), 0, fieldsHomography);
					Core.gemm(deperspectivGraphy, stabilizationHomography.inv(), 1, new Mat(), 0, fieldsHomography);
					fields.merge(detectRects(stabilized), fieldsHomography);
					Img stabilized_ = stabilized;
					fields.stream().forEach(f -> f.draw(stabilized_, f.getDeadCounter() == 0 ? new Scalar(0, 255, 0) : new Scalar(0, 0, 255)));
					stabilizedImgDescriptor = newImgDescriptor;
					stabilizationHomography = deperspectivGraphy;
					stabilizationHasChanged = false;
					Stats.endTask("stabilizationHasChanged");
				}
				Img display = new Img(frame, false);
				Stats.beginTask("consolidateOcr");
				fields.consolidateOcr(stabilized);
				Stats.endTask("consolidateOcr");
				fields.drawOcrPerspectiveInverse(display, stabilizationHomography.inv(), new Scalar(0, 255, 0), 1);
				src0.setImage(display.toJfxImage());
				src1.setImage(stabilized.toJfxImage());
				Stats.endTask("frame");

				Stats.resetCumulative("RANSAC re-compute");
				if (++counter % 10 == 0) {
					System.out.println(Stats.getStatsAndReset());
					counter = 0;
				}
			} catch (Throwable e) {
				logger.warn("Exception while computing layout.", e);
			}
		}, 50, FRAME_DELAY, TimeUnit.MILLISECONDS);

	}

	@Override
	protected void onSpace() {
		stabilizationHasChanged = true;
	}

	private List<Rect> detectRects(Img stabilized) {
		Img closed = stabilized.adaptativeGaussianInvThreshold(7, 3).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(9, 1));
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(closed.getSrc(), contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		double minArea = 200;
		List<Rect> res = contours.stream().filter(contour -> Imgproc.contourArea(contour) > minArea).map(c -> Imgproc.boundingRect(c)).collect(Collectors.toList());
		// res.forEach(rect -> Imgproc.rectangle(stabilized.getSrc(), rect.tl(), rect.br(), new Scalar(255, 200, 0)));
		return res;
	}

	static Img warpPerspective(Mat frame, Mat homography) {
		Mat dePerspectived = new Mat(frame.size(), CvType.CV_8UC3, Scalar.all(255));
		Imgproc.warpPerspective(frame, dePerspectived, homography, frame.size(), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(255));
		return new Img(dePerspectived, false);
	}

	private Lines houghlinesP(Mat frame) {
		Img grad = new Img(frame, false).morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_RECT, new Size(2, 2)).otsu();
		return new Lines(grad.houghLinesP(1, Math.PI / 180, 10, 100, 10));
	}

	private Mat computeFrameToDeperspectivedHomography(Mat frame) {
		Lines lines = houghlinesP(frame);

		if (lines.size() > 50)
			lines = lines.reduce(0.15, 50);

		if (lines.size() < 8) {
			logger.warn("Not enough lines to compute perspective transformation ({})", lines.size());
			return null;
		}
		Stats.beginTask("ransac");
		Ransac<Line> ransac = lines.vanishingPointRansac(frame.width(), frame.height());
		Stats.endTask("ransac");
		Mat vpMat = (Mat) ransac.getBestModel().getParams()[0];
		Point vp = new Point(vpMat.get(0, 0)[0], vpMat.get(1, 0)[0]);
		Point bary = new Point(frame.width() / 2, frame.height() / 2);
		return findHomography(vp, bary, frame.width(), frame.height());
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

		// System.out.println("vp : " + vp);
		// System.out.println("rotated vp : " + rotatedVp);
		// System.out.println("Alpha : " + alpha * 180 / Math.PI);
		// System.out.println();
		// System.out.println("A : " + A + " " + A_);
		// System.out.println("B : " + B + " " + B_);
		// System.out.println("C : " + C + " " + C_);
		// System.out.println("D : " + D + " " + D_);

		Mat src = new MatOfPoint2f(rotate(bary, -alpha, A_, B_, C_, D_));
		Mat dst = new MatOfPoint2f(A, B, C, D);
		return Imgproc.getPerspectiveTransform(src, dst);
	}

	private Point[] rotate(Point bary, double alpha, Point... p) {
		Mat matrix = Imgproc.getRotationMatrix2D(bary, alpha / Math.PI * 180, 1);
		MatOfPoint2f points = new MatOfPoint2f(p);
		MatOfPoint2f results = new MatOfPoint2f();
		Core.transform(points, results, matrix);
		return results.toArray();
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

		public Lines reduce(double factor, int threshold) {
			long target = Math.round(this.size() * factor);
			if (target < threshold)
				target = threshold;

			List<Line> newLines = new ArrayList<>();
			Set<Integer> indexes = new HashSet<>();
			while (indexes.size() < target) {
				int idx = ThreadLocalRandom.current().nextInt(this.size());
				if (indexes.add(idx))
					newLines.add(lines.get(idx));
			}
			return Lines.of(newLines);
		}

		private Mat getLineMat(Line line) {
			Mat a = new Mat(3, 1, CvType.CV_32F);
			Mat b = new Mat(3, 1, CvType.CV_32F);
			a.put(0, 0, new float[] { Double.valueOf(line.getX1()).floatValue() });
			a.put(1, 0, new float[] { Double.valueOf(line.getY1()).floatValue() });
			a.put(2, 0, new float[] { Double.valueOf(1d).floatValue() });
			b.put(0, 0, new float[] { Double.valueOf(line.getX2()).floatValue() });
			b.put(1, 0, new float[] { Double.valueOf(line.getY2()).floatValue() });
			b.put(2, 0, new float[] { Double.valueOf(1d).floatValue() });
			Mat an = new Mat(3, 1, CvType.CV_32F);
			Mat bn = new Mat(3, 1, CvType.CV_32F);
			Core.gemm(K.inv(), a, 1, new Mat(), 0, an);
			Core.gemm(K.inv(), b, 1, new Mat(), 0, bn);
			Mat li = an.cross(bn);
			Core.normalize(li, li);
			return li;
		}

		public Ransac<Line> vanishingPointRansac(double width, double height) {
			int minimal_sample_set_dimension = 2;
			double maxError = (float) 0.01623 * 2;
			if (K == null) {
				K = new Mat(3, 3, CvType.CV_32F, new Scalar(0));
				K.put(0, 0, new float[] { Double.valueOf(width).floatValue() });
				K.put(0, 2, new float[] { Double.valueOf(width / 2).floatValue() });
				K.put(1, 1, new float[] { Double.valueOf(height).floatValue() });
				K.put(1, 2, new float[] { Double.valueOf(height / 2).floatValue() });
				K.put(2, 2, new float[] { 1 });
			}
			return new Ransac<>(getLines(), getModelProvider(minimal_sample_set_dimension, maxError), minimal_sample_set_dimension, 100, maxError, Double.valueOf(Math.floor(this.size() * 0.7)).intValue());
		}

		private Function<Collection<Line>, Model<Line>> getModelProvider(int minimal_sample_set_dimension, double maxError) {
			return datas -> {
				Mat vp;

				if (datas.size() == minimal_sample_set_dimension) {
					Iterator<Line> it = datas.iterator();
					vp = getLineMat(it.next()).cross(getLineMat(it.next()));
					Core.normalize(vp, vp);
				} else {
					Stats.beginCumulative("RANSAC re-compute");
					// Extract the line segments corresponding to the indexes contained in the set
					Mat li_set = new Mat(3, datas.size(), CvType.CV_32F);
					Mat tau = new Mat(datas.size(), datas.size(), CvType.CV_32F, new Scalar(0, 0, 0));

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
					Mat ATA = new Mat(3, 3, CvType.CV_32F);
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
					vp = new Mat(3, 1, CvType.CV_32F);
					vp.put(0, 0, v.get(0, 2));
					vp.put(1, 0, v.get(1, 2));
					vp.put(2, 0, v.get(2, 2));

					Core.normalize(vp, vp);

					Core.gemm(K, vp, 1, new Mat(), 0, vp);

					if (vp.get(2, 0)[0] != 0) {
						vp.put(0, 0, new float[] { Double.valueOf(vp.get(0, 0)[0] / vp.get(2, 0)[0]).floatValue() });
						vp.put(1, 0, new float[] { Double.valueOf(vp.get(1, 0)[0] / vp.get(2, 0)[0]).floatValue() });
						vp.put(2, 0, new float[] { 1 });
					} else {
						// Since this is infinite, it is better to leave it calibrated
						Core.gemm(K.inv(), vp, 1, new Mat(), 0, vp);
					}
					Stats.endCumulative("RANSAC re-compute");
				}

				return new Model<Line>() {
					@Override
					public double computeError(Line line) {
						Mat lineMat = getLineMat(line);
						double di = vp.dot(lineMat);
						di /= (Core.norm(vp) * Core.norm(lineMat));
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
						return new Object[] { vp };
					}

				};
			};
		}
	}

}