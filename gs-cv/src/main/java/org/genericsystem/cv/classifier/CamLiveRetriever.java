package org.genericsystem.cv.classifier;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
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
import org.opencv.calib3d.Calib3d;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.DMatch;
import org.opencv.core.KeyPoint;
import org.opencv.core.Mat;
import org.opencv.core.MatOfDMatch;
import org.opencv.core.MatOfKeyPoint;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.features2d.DescriptorExtractor;
import org.opencv.features2d.DescriptorMatcher;
import org.opencv.imgproc.Imgproc;
import org.opencv.utils.Converters;
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

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private static final int OCR_DELAY = 150;
	private static final int STABILIZATION_DELAY = 110;
	private static final int FRAME_DELAY = 33;

	private final ScheduledExecutorService timerFields = Executors.newSingleThreadScheduledExecutor();
	private final ScheduledExecutorService timerOcr = Executors.newSingleThreadScheduledExecutor();
	private final VideoCapture capture = new VideoCapture(0);
	private final Fields fields = new Fields();

	private MatOfKeyPoint oldKeypoints;
	private MatOfKeyPoint newKeypoints;
	private Mat oldDescriptors;
	private Mat newDescriptors;
	private Mat homography;
	private Mat frame = new Mat();

	private Img stabilized;
	private boolean stabilizationHasChanged = true;

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	public void stop() throws Exception {
		super.stop();
		timerFields.shutdown();
		timerFields.awaitTermination(5, TimeUnit.SECONDS);
		timerOcr.shutdown();
		timerOcr.awaitTermination(5, TimeUnit.SECONDS);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		DescriptorExtractor extractor = DescriptorExtractor.create(DescriptorExtractor.ORB);
		DescriptorMatcher matcher = DescriptorMatcher.create(DescriptorMatcher.BRUTEFORCE_HAMMING);

		capture.read(frame);

		ImageView src0 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src0, 0, 0);

		ImageView src1 = new ImageView(Tools.mat2jfxImage(frame));
		mainGrid.add(src1, 1, 0);

		oldKeypoints = new MatOfKeyPoint();
		oldDescriptors = new Mat();

		// Perform the OCR
		timerOcr.scheduleWithFixedDelay(() -> consolidateOcr(), 500, OCR_DELAY, TimeUnit.MILLISECONDS);

		// Stabilize the image
		timerFields.scheduleWithFixedDelay(() -> onSpace(), 0, STABILIZATION_DELAY, TimeUnit.MILLISECONDS);

		// Detect the rectangles
		timerFields.scheduleWithFixedDelay(() -> {
			synchronized (this) {
				try {
					capture.read(frame);
					Img frameImg = new Img(frame, true);
					Img deskewed = perspectiveTransform(frameImg.getSrc());
					newKeypoints = detect(deskewed);
					newDescriptors = new Mat();
					extractor.compute(deskewed.getSrc(), newKeypoints, newDescriptors);
					stabilized = stabilize(frameImg, matcher);
					fields.storeCurrentHomography(homography);

					if (stabilized != null) {
						if (stabilizationHasChanged) {
							List<Rect> newRects = detectRects(stabilized);
							fields.merge(newRects);
							fields.draw(stabilized);
							stabilizationHasChanged = false;
						}
						Img display = new Img(frame, true);
						Img stabilizedDisplay = new Img(stabilized.getSrc(), true);

						// fields.drawRectsPerspective(display, homography.inv(), new Scalar(0, 0, 255), 1);
						// fields.drawRectsPerspective(stabilizedDisplay, homography, new Scalar(0, 255, 0), 1); // new Mat(3, 3, CvType.CV_64FC1)

						// fields.drawOcrPerspectiveInverse(display, homography.inv(), new Scalar(0, 64, 128), 1);
						// fields.drawConsolidated(stabilizedDisplay);

						src0.setImage(display.toJfxImage());
						src1.setImage(stabilizedDisplay.toJfxImage());
					}
				} catch (Throwable e) {
					logger.warn("Exception while computing layout.", e);
				}
			}
		}, 400, FRAME_DELAY, TimeUnit.MILLISECONDS);

	}

	private synchronized void consolidateOcr() {
		try {
			if (stabilized != null)
				fields.consolidateOcr(stabilized);
		} catch (Throwable e) {
			logger.warn("Exception while computing OCR", e);
		}
	}

	@Override
	protected synchronized void onSpace() {
		if (homography != null) {
			fields.storeCurrentHomography(homography);
			fields.storeLastHomographyInv(homography.inv());
		}
		oldKeypoints = newKeypoints;
		oldDescriptors = newDescriptors;
		stabilizationHasChanged = true;
	}

	private Img stabilize(Img frame, DescriptorMatcher matcher) {
		MatOfDMatch matches = new MatOfDMatch();
		if (oldDescriptors != null && !oldDescriptors.empty() && !newDescriptors.empty()) {
			Mat stabilizedMat = new Mat(); // frame.getSrc().clone();
			matcher.match(oldDescriptors, newDescriptors, matches);
			List<DMatch> goodMatches = new ArrayList<>();
			for (DMatch dMatch : matches.toArray()) {
				if (dMatch.distance <= 40) {
					goodMatches.add(dMatch);
				}
			}
			List<KeyPoint> newKeypoints_ = newKeypoints.toList();
			List<KeyPoint> oldKeypoints_ = oldKeypoints.toList();
			List<Point> goodNewKeypoints = new ArrayList<>();
			List<Point> goodOldKeypoints = new ArrayList<>();
			for (DMatch goodMatch : goodMatches) {
				goodNewKeypoints.add(newKeypoints_.get(goodMatch.trainIdx).pt);
				goodOldKeypoints.add(oldKeypoints_.get(goodMatch.queryIdx).pt);
			}

			if (goodMatches.size() > 30) {
				Mat goodNewPoints = Converters.vector_Point2f_to_Mat(goodNewKeypoints);
				MatOfPoint2f originalNewPoints = new MatOfPoint2f();
				Lines lines = getLines(frame.getSrc());
				Mat matrix = getHomography(frame.getSrc(), lines);
				Core.perspectiveTransform(goodNewPoints, originalNewPoints, matrix.inv());
				homography = Calib3d.findHomography(originalNewPoints, new MatOfPoint2f(goodOldKeypoints.stream().toArray(Point[]::new)), Calib3d.RANSAC, 10);
				Mat tmp = new Mat();
				Mat mask = new Mat(frame.size(), CvType.CV_8UC1, new Scalar(255));
				Mat maskWarpped = new Mat();
				Imgproc.warpPerspective(mask, maskWarpped, homography, frame.size());
				Imgproc.warpPerspective(frame.getSrc(), tmp, homography, frame.size(), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(255));
				tmp.copyTo(stabilizedMat, maskWarpped);
				return new Img(stabilizedMat, false);
			} else {
				System.out.println("Not enough matches (" + goodMatches.size() + ")");
				return null;
			}
		}
		System.out.println("No stabilized image");
		return null;
	}

	private List<Rect> detectRects(Img stabilized) {
		Img closed = stabilized.adaptativeGaussianInvThreshold(7, 3).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(9, 1));
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(closed.getSrc(), contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		double minArea = 200;
		List<Rect> res = contours.stream().filter(contour -> Imgproc.contourArea(contour) > minArea).map(c -> Imgproc.boundingRect(c)).collect(Collectors.toList());
		// res.forEach(rect -> Imgproc.rectangle(stabilized.getSrc(), rect.tl(), rect.br(), new Scalar(128, 0, 0)));
		return res;
	}

	private MatOfKeyPoint detect(Img frame) {
		Img closed = frame.adaptativeGaussianInvThreshold(17, 3).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(5, 5));
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(closed.getSrc(), contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		double minArea = 100;
		List<KeyPoint> keyPoints = new ArrayList<>();
		contours.stream().filter(contour -> Imgproc.contourArea(contour) > minArea).map(Imgproc::boundingRect).forEach(rect -> {
			keyPoints.add(new KeyPoint((float) rect.tl().x, (float) rect.tl().y, 6));
			keyPoints.add(new KeyPoint((float) rect.tl().x, (float) rect.br().y, 6));
			keyPoints.add(new KeyPoint((float) rect.br().x, (float) rect.tl().y, 6));
			keyPoints.add(new KeyPoint((float) rect.br().x, (float) rect.br().y, 6));
		});
		return new MatOfKeyPoint(keyPoints.stream().toArray(KeyPoint[]::new));
	}

	private Img perspectiveTransform(Mat frame) {
		Lines lines = getLines(frame);
		if (lines.size() > 10) {
			Mat homography = getHomography(frame, lines);
			Mat dePerspectived = new Mat(frame.size(), CvType.CV_8UC3, Scalar.all(255));
			Mat tmp = new Mat();
			Mat mask = new Mat(frame.size(), CvType.CV_8UC1, new Scalar(255));
			Mat maskWarpped = new Mat();
			Imgproc.warpPerspective(mask, maskWarpped, homography, frame.size());
			Imgproc.warpPerspective(frame, tmp, homography, frame.size(), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(255));
			tmp.copyTo(dePerspectived, maskWarpped);
			return new Img(dePerspectived, false);
		} else {
			logger.warn("Not enough lines to compute perspective transform");
			return null;
		}
	}

	private Lines getLines(Mat frame) {
		Img grad = new Img(frame, false).morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_RECT, new Size(2, 2)).otsu();
		return new Lines(grad.houghLinesP(1, Math.PI / 180, 10, 100, 10));
	}

	private Mat getHomography(Mat frame, Lines lines) {
		Ransac<Line> ransac = lines.vanishingPointRansac(frame.width(), frame.height());
		Mat vp_mat = (Mat) ransac.getBestModel().getParams()[0];
		Point vp = new Point(vp_mat.get(0, 0)[0], vp_mat.get(1, 0)[0]);
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

		public Ransac<Line> vanishingPointRansac(int width, int height) {
			int minimal_sample_set_dimension = 2;
			double maxError = (float) 0.01623 * 2;
			if (K == null) {
				K = new Mat(3, 3, CvType.CV_32F, new Scalar(0));
				K.put(0, 0, new float[] { width });
				K.put(0, 2, new float[] { width / 2 });
				K.put(1, 1, new float[] { height });
				K.put(1, 2, new float[] { height / 2 });
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