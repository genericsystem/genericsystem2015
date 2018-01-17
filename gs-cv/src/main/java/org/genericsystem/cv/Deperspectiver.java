package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.genericsystem.cv.Calibrated.AngleCalibrated;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.calib3d.Calib3d;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.DMatch;
import org.opencv.core.KeyPoint;
import org.opencv.core.Mat;
import org.opencv.core.MatOfDMatch;
import org.opencv.core.MatOfKeyPoint;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.features2d.BFMatcher;
import org.opencv.features2d.DescriptorMatcher;
import org.opencv.features2d.FastFeatureDetector;
import org.opencv.imgproc.Imgproc;
import org.opencv.utils.Converters;
import org.opencv.videoio.VideoCapture;
import org.opencv.xfeatures2d.BriefDescriptorExtractor;

import javafx.application.Platform;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class Deperspectiver extends AbstractApp {

	static {
		NativeLibraryLoader.load();
	}

	public static void main(String[] args) {
		launch(args);
	}

	private final double f = 6.053 / 0.009;
	private final VideoCapture capture = new VideoCapture(0);
	private SuperFrameImg superFrame = SuperFrameImg.create(capture, f);
	private ScheduledExecutorService timer = Executors.newSingleThreadScheduledExecutor();;

	private AngleCalibrated calibrated0;
	private Kalman kalmanZ = new Kalman();
	private ReferenceManager referenceManager = new ReferenceManager(superFrame.size());

	private boolean stabilizedMode = false;
	private boolean textsEnabledMode = false;

	@Override
	protected void fillGrid(GridPane mainGrid) {
		double displaySizeReduction = 2;

		ImageView view00 = new ImageView();
		ImageView view01 = new ImageView();
		ImageView view10 = new ImageView();
		ImageView view11 = new ImageView();

		mainGrid.add(view00, 0, 0);
		mainGrid.add(view01, 0, 1);
		mainGrid.add(view10, 1, 0);
		mainGrid.add(view11, 1, 1);

		view00.setFitWidth(superFrame.width() / displaySizeReduction);
		view00.setFitHeight(superFrame.height() / displaySizeReduction);
		view01.setFitWidth(superFrame.width() / displaySizeReduction);
		view01.setFitHeight(superFrame.height() / displaySizeReduction);
		view10.setFitWidth(superFrame.width() / displaySizeReduction);
		view10.setFitHeight(superFrame.height() / displaySizeReduction);
		view11.setFitWidth(superFrame.width() / displaySizeReduction);
		view11.setFitHeight(superFrame.height() / displaySizeReduction);

		double[] pp = superFrame.getPrincipalPoint();
		calibrated0 = new AngleCalibrated(0, Math.PI / 2);
		timer.scheduleAtFixedRate(() -> {
			try {
				Image[][] images = doWork(pp);
				if (images != null)
					Platform.runLater(() -> {
						view00.setImage(images[0][0]);
						view01.setImage(images[0][1]);
						view10.setImage(images[0][2]);
						view11.setImage(images[0][3]);
					});

			} catch (Throwable e) {
				e.printStackTrace();
			}
		}, 30, 30, TimeUnit.MILLISECONDS);

	}

	protected Image[][] doWork(double[] pp) {
		if (!stabilizedMode)
			superFrame = SuperFrameImg.create(capture, f);
		Lines lines = superFrame.detectLines();
		if (textsEnabledMode)
			lines.lines.addAll(superFrame.findTextOrientationLines());
		if (lines.size() > 4) {
			superFrame.draw(lines, new Scalar(0, 0, 255), 1);
			// calibrated0 = new AngleCalibrated(new double[] {0,Math.PI/2});
			calibrated0 = superFrame.findVanishingPoint(lines, calibrated0);

			AngleCalibrated[] calibratedVps = superFrame.findOtherVps(calibrated0, lines);

			superFrame.drawVanishingPointLines(lines, calibratedVps[0], new Scalar(0, 255, 0), 1);
			superFrame.drawVanishingPointLines(lines, calibratedVps[1], new Scalar(255, 0, 0), 1);

			double[] predictionZ = kalmanZ.predict();
			kalmanZ.correct(calibratedVps[2].uncalibrate(pp, f));
			calibratedVps[2] = new AngleCalibrated(new double[] { predictionZ[0], predictionZ[1], 1.0 }, pp, f);
			calibratedVps[1] = calibratedVps[0].getOrthoFromVps(calibratedVps[2]);

			superFrame.drawVpsArrows(calibratedVps, new double[] { 20, 20 }, new Scalar(0, 255, 0), 2);

			Image displayImage = superFrame.getDisplay().toJfxImage();

			Mat deperspectiveHomography = superFrame.findHomography(calibratedVps);
			SuperFrameImg superDeperspectived = superFrame.deperspective(deperspectiveHomography);
			List<Rect> detectedRects = superDeperspectived.detectRects();
			superDeperspectived.drawRects(superDeperspectived.detectRects(), new Scalar(255), -1);
			Image deperspectivedImage = superDeperspectived.getDisplay().toJfxImage();

			// Image grad = superFrame.getGradient().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(30, 30)).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(30, 30))
			// .morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(3, 3)).toJfxImage();
			// Image closed = superFrame.getBinarized().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(10, 10)).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(10, 10))
			// .morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(3, 3)).toJfxImage();
			// Image diff = superFrame.getDiffFrame().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(10, 10)).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(10, 10))
			// .morphologyEx(Imgproc.MORPH_GRADIENT, Imgproc.MORPH_ELLIPSE, new Size(3, 3)).toJfxImage();

			// Image text = superDeperspectived.getBinarized().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(5, 3)).toJfxImage();
			Image text = superDeperspectived.getDiffFrame().toJfxImage();
			// Template closedDeperspectived = new Template(superDeperspectived);
			// closedDeperspectived.drawRects(closedDeperspectived.detectRects(closedDeperspectived.getFrame().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, new Size(9, 5)), 100, 10000));
			// Image text2 = closedDeperspectived.getDisplay().toJfxImage();

			ImgDescriptor newImgDescriptor = new ImgDescriptor(superDeperspectived);
			referenceManager.submit(newImgDescriptor, detectedRects);
			SuperTemplate superTemplate = new SuperTemplate(referenceManager.getReference().getSuperFrame().getFrame().getSrc(), pp, f);
			List<Rect> referenceRects = referenceManager.getReferenceRects();
			superTemplate.drawRects(referenceRects, new Scalar(255), -1);
			Image superTemplateImg = superTemplate.getDisplay().toJfxImage();
			return new Image[][] { new Image[] { displayImage, deperspectivedImage, text, superTemplateImg } };

		} else {
			System.out.println("Not enough lines : " + lines.size());
			return null;
		}
	}

	private static class Reconciliation {
		private final Mat homography;
		private final List<Point> newPts;
		private final List<Point> referencePts;

		public Reconciliation(Mat homography, List<Point> newPts, List<Point> referencePts) {
			this.homography = homography;
			this.newPts = newPts;
			this.referencePts = referencePts;
		}

		public Mat getHomography() {
			return homography;
		}

		public List<Point> getPts() {
			return newPts;
		}

		public List<Point> getReferencePts() {
			return referencePts;
		}
	}

	public static class ImgDescriptor {
		private static final BriefDescriptorExtractor briefExtractor = BriefDescriptorExtractor.create(32, false);
		private static final FastFeatureDetector detector = FastFeatureDetector.create(10, true, FastFeatureDetector.TYPE_9_16);
		private static final DescriptorMatcher matcher = BFMatcher.create(DescriptorMatcher.BRUTEFORCE_HAMMING, true);

		private final SuperFrameImg superFrame;
		private final MatOfKeyPoint keypoints = new MatOfKeyPoint();
		private final Mat descriptors;
		private final long timeStamp;	

		public ImgDescriptor(SuperFrameImg superFrame) {
			this.superFrame = superFrame;
			detector.detect(superFrame.getFrame().getSrc(), keypoints);
			// keypoints = detect(deperspectivedImg);
			assert keypoints != null && !keypoints.empty();
			descriptors = new Mat();
			briefExtractor.compute(superFrame.getFrame().getSrc(), keypoints, descriptors);
			// EXTRACTOR.compute(deperspectivedImg.getSrc(), keypoints, descriptors);
			timeStamp = System.currentTimeMillis();
		}

		public SuperFrameImg getSuperFrame() {
			return superFrame;
		}

		public Mat getDescriptors() {
			return descriptors;
		}

		public MatOfKeyPoint getKeypoints() {
			return keypoints;
		}

		public long getTimeStamp(){
			return timeStamp;
		}

		public Reconciliation computeReconciliation(ImgDescriptor reference) {
			MatOfDMatch matches = new MatOfDMatch();
			// System.out.println(frameDescriptor.getDescriptors());
			matcher.match(getDescriptors(), reference.getDescriptors(), matches);

			List<KeyPoint> referenceKeyPoints = reference.getKeypoints().toList();
			List<KeyPoint> keyPoints = getKeypoints().toList();
			List<Point> referencePts = new ArrayList<>();
			List<Point> pts = new ArrayList<>();
			for (DMatch goodMatch : matches.toArray())
				if (goodMatch.distance <= 120) {
					referencePts.add(referenceKeyPoints.get(goodMatch.trainIdx).pt);
					pts.add(keyPoints.get(goodMatch.queryIdx).pt);
				}
			if (referencePts.size() > 40) {

				// List<Point[]> pairedPoints = new ArrayList<>();
				// for (int i = 0; i < goodNewKeypoints.size(); i++)
				// pairedPoints.add(new Point[] { goodOldKeypoints.get(i), goodNewKeypoints.get(i) });

				// double[] transScaleParams = new LevenbergImpl<>((points, params) -> distance(points, params), pairedPoints, new double[] { 1, 1, 0, 0 }).getParams();
				// System.out.println("params " + Arrays.toString(transScaleParams));
				// Mat result = getTSMat(transScaleParams);

				Mat result = Calib3d.findHomography(new MatOfPoint2f(pts.stream().toArray(Point[]::new)), new MatOfPoint2f(referencePts.stream().toArray(Point[]::new)), Calib3d.RANSAC, 1);
				if (result.size().empty()) {
					System.out.println("Stabilization homography is empty");
					return null;
				}
				if (!isValidHomography(result)) {
					System.out.println("Not a valid homography");
					return null;
				}
				return new Reconciliation(result, pts, referencePts);
			} else {
				System.out.println("Not enough matches (" + referencePts.size() + ")");
				return null;
			}
		}

		private boolean isValidHomography(Mat homography) {
			int w = superFrame.getFrame().width();
			int h = superFrame.getFrame().height();
			MatOfPoint2f original = new MatOfPoint2f(new Point[] { new Point(0, 0), new Point(w, 0), new Point(w, h), new Point(0, h) });
			MatOfPoint2f dst = new MatOfPoint2f();
			Core.perspectiveTransform(original, dst, homography);
			List<Point> targets = dst.toList();
			return isClockwise(targets.get(0), targets.get(1), targets.get(2));
		}

		private boolean isClockwise(Point a, Point b, Point c) {
			double areaSum = 0;
			areaSum += a.x * (b.y - c.y);
			areaSum += b.x * (c.y - a.y);
			areaSum += c.x * (a.y - b.y);
			return areaSum > 0;
		}


	}

	public static class ReferenceManager {
		private static final Mat IDENTITY_MAT = Mat.eye(new Size(3, 3), CvType.CV_64F);

		private TreeMap<ImgDescriptor, Mat> toReferenceGraphy = new TreeMap<>(new Comparator<ImgDescriptor>() {

			@Override
			public int compare(ImgDescriptor d1, ImgDescriptor d2) {
				return new Long(d1.getTimeStamp()).compareTo(new Long(d2.getTimeStamp()));
			}

		});

		private ImgDescriptor reference;
		private List<Rect> referenceRects = new ArrayList<>();
		private Size frameSize;

		public ReferenceManager(Size frameSize) {
			this.frameSize = frameSize;
		}

		public void submit(ImgDescriptor newImgDescriptor, List<Rect> detectedrects) {
			if (reference == null) {
				toReferenceGraphy.put(newImgDescriptor, IDENTITY_MAT);
				reference = newImgDescriptor;
				return;
			}

			int bestMatchingPointsCount = 0;
			ImgDescriptor bestImgDescriptor = null;
			Reconciliation bestReconciliation = null;

			Reconciliation reconciliationWithRef = newImgDescriptor.computeReconciliation(reference);
			if (reconciliationWithRef != null) {				
				bestReconciliation = reconciliationWithRef;
				bestImgDescriptor = reference;				
			}
			else{
				ImgDescriptor lastStored = toReferenceGraphy.lastKey();
				Reconciliation reconciliationWithlast = newImgDescriptor.computeReconciliation(lastStored);
				if (reconciliationWithlast != null){
					bestReconciliation = reconciliationWithlast;
					bestImgDescriptor = lastStored;
				}
				else{
					for (ImgDescriptor imgDescriptor : toReferenceGraphy.keySet()) {
						Reconciliation reconciliation = newImgDescriptor.computeReconciliation(imgDescriptor);
						if (reconciliation != null) {
							int matchingPointsCount = reconciliation.getPts().size();
							if (matchingPointsCount >= bestMatchingPointsCount) {
								bestMatchingPointsCount = matchingPointsCount;
								bestReconciliation = reconciliation;
								bestImgDescriptor = imgDescriptor;
							}
						}
					}
				}
			}
			if (bestReconciliation == null) {
				if (toReferenceGraphy.size() <= 1) {
					toReferenceGraphy.clear();
					toReferenceGraphy.put(newImgDescriptor, IDENTITY_MAT);
				}
				return;
			}
			Mat homographyToReference = new Mat();
			Core.gemm(bestReconciliation.getHomography(), toReferenceGraphy.get(bestImgDescriptor), 1, new Mat(), 0, homographyToReference);
			toReferenceGraphy.put(newImgDescriptor, homographyToReference);
			consolidate(shift(detectedrects, homographyToReference));
			updateReference();
			cleanReferenceNeighbours();
		}

		private void cleanReferenceNeighbours() {
			if (toReferenceGraphy.size() > 6) {
				double bestDistance = Double.MAX_VALUE;
				ImgDescriptor closestDescriptor = null;
				for (Entry<ImgDescriptor, Mat> entry : toReferenceGraphy.entrySet()) {
					if (!entry.getKey().equals(reference)) {
						double distance = distance(entry.getValue());
						if (distance < bestDistance) {
							bestDistance = distance;
							closestDescriptor = entry.getKey();
						}
					}
				}
				toReferenceGraphy.remove(closestDescriptor);
			}
		}

		private void updateReference() {
			ImgDescriptor consensualDescriptor = findConsensualDescriptor();
			if (reference != consensualDescriptor) {
				System.out.println("Change reference");
				Mat homoInv = toReferenceGraphy.get(consensualDescriptor).inv();
				for (Entry<ImgDescriptor, Mat> entry : toReferenceGraphy.entrySet()) {
					if (!entry.getKey().equals(consensualDescriptor)) {
						Mat result = new Mat();
						Core.gemm(entry.getValue(), homoInv, 1, new Mat(), 0, result);
						toReferenceGraphy.put(entry.getKey(), result);
					} else
						toReferenceGraphy.put(entry.getKey(), IDENTITY_MAT);
				}
				reference = consensualDescriptor;
			} else
				System.out.println("No change reference");
		}

		private ImgDescriptor findConsensualDescriptor() {
			double bestDistance = Double.MAX_VALUE;
			ImgDescriptor bestDescriptor = null;
			for (Entry<ImgDescriptor, Mat> entry : toReferenceGraphy.entrySet()) {
				double distance = 0;
				for (Entry<ImgDescriptor, Mat> entry2 : toReferenceGraphy.entrySet()) {
					if (!entry.getKey().equals(entry2.getKey())) {
						Mat betweenHomography = new Mat();
						Core.gemm(entry.getValue(), entry2.getValue().inv(), 1, new Mat(), 0, betweenHomography);
						distance += distance(betweenHomography);
					}
				}
				if (distance < bestDistance) {
					bestDistance = distance;
					bestDescriptor = entry.getKey();
				}
			}
			return bestDescriptor;
		}

		private void consolidate(List<Rect> shiftedRect) {
			referenceRects = shiftedRect;
		}

		public List<Rect> getReferenceRects() {
			return referenceRects;
		}

		private List<Rect> shift(List<Rect> detectedRects, Mat homography) {
			List<Point> pts = new ArrayList<>(2 * detectedRects.size());
			detectedRects.forEach(rect -> {
				pts.add(rect.tl());
				pts.add(rect.br());
			});
			List<Point> transform = transform(pts, homography);
			List<Rect> result = new ArrayList<>(detectedRects.size());
			for (int i = 0; i < transform.size(); i += 2)
				result.add(new Rect(transform.get(i), transform.get(i + 1)));
			return result;
		}

		private List<Point> transform(List<Point> originals, Mat homography) {
			Mat original = Converters.vector_Point2d_to_Mat(originals);
			Mat results = new Mat();
			Core.perspectiveTransform(original, results, homography);
			List<Point> res = new ArrayList<>();
			Converters.Mat_to_vector_Point2d(results, res);
			return res;
		}

		public Reconciliation computeHomography(ImgDescriptor newDescriptor) {
			return newDescriptor.computeReconciliation(getReference());
		}

		private double distance(Mat betweenHomography) {
			List<Point> originalPoints = Arrays.asList(new Point[] { new Point(0, 0), new Point(frameSize.width, 0), new Point(frameSize.width, frameSize.height), new Point(0, frameSize.height) });
			List<Point> points = transform(originalPoints, betweenHomography);
			return distance(points, originalPoints);
		}

		private double distance(List<Point> newPointList, List<Point> oldPointList) {
			double error = 0.0;
			for (int i = 0; i < oldPointList.size(); i++) {
				double deltaX = newPointList.get(i).x - oldPointList.get(i).x;
				double deltaY = newPointList.get(i).y - oldPointList.get(i).y;
				error += deltaX * deltaX + deltaY * deltaY;
			}
			return Math.sqrt(error) / oldPointList.size();
		}

		public ImgDescriptor getReference() {
			return reference;
		}
	}

	public static class SuperTemplate extends SuperFrameImg {

		public SuperTemplate(SuperFrameImg superFrame) {
			this(superFrame.getDisplay().getSrc(), superFrame.getPp(), superFrame.getF());
		}

		public SuperTemplate(Mat frameMat, double[] pp, double f) {
			super(frameMat, pp, f);
		}

		@Override
		protected Img buildDisplay() {
			return new Img(new Mat(size(), CvType.CV_8UC1, new Scalar(0)), false);
		}

	}

	@Override
	protected void onS() {

	}

	@Override
	protected void onT() {
		textsEnabledMode = !textsEnabledMode;
	}

	public static class Lines {

		final List<Line> lines;

		public Lines(Mat src) {
			lines = new ArrayList<>();
			for (int i = 0; i < src.rows(); i++) {
				double[] val = src.get(i, 0);
				Line line = new Line(val[0], val[1], val[2], val[3]);
				lines.add(line);
			}
		}

		public Lines filter(Predicate<Line> predicate) {
			return new Lines(lines.stream().filter(predicate).collect(Collectors.toList()));
		}

		public Lines reduce(int max) {
			if (lines.size() <= max)
				return this;

			Set<Line> newLines = new HashSet<>();
			while (newLines.size() < max)
				newLines.add(lines.get((int) (Math.random() * size())));
			return new Lines((newLines));
		}

		public Lines(Collection<Line> lines) {
			this.lines = new ArrayList<>(lines);
		}

		public Lines rotate(Mat matrix) {
			return new Lines(lines.stream().map(line -> line.transform(matrix)).collect(Collectors.toList()));
		}

		public Lines perspectivTransform(Mat matrix) {
			return new Lines(lines.stream().map(line -> line.perspectivTransform(matrix)).collect(Collectors.toList()));
		}

		public void draw(Mat frame, Scalar color, int thickness) {
			lines.forEach(line -> line.draw(frame, color, thickness));
		}

		public int size() {
			return lines.size();
		}
	}

	public static class Line {
		final double x1, y1, x2, y2;

		public Line(Point p1, Point p2) {
			this(p1.x, p1.y, p2.x, p2.y);
		}

		public Line(double x1, double y1, double x2, double y2) {
			this.x1 = x1;
			this.x2 = x2;
			this.y1 = y1;
			this.y2 = y2;
		}

		public double size() {
			return Math.sqrt(Math.pow(y2 - y1, 2) + Math.pow(x2 - x1, 2));
		}

		public Line transform(Mat rotationMatrix) {
			MatOfPoint2f results = new MatOfPoint2f();
			Core.transform(Converters.vector_Point2f_to_Mat(Arrays.asList(new Point(x1, y1), new Point(x2, y2))), results, rotationMatrix);
			Point[] targets = results.toArray();
			return new Line(targets[0].x, targets[0].y, targets[1].x, targets[1].y);
		}

		public Line perspectivTransform(Mat homography) {
			MatOfPoint2f results = new MatOfPoint2f();
			Core.perspectiveTransform(Converters.vector_Point2f_to_Mat(Arrays.asList(new Point(x1, y1), new Point(x2, y2))), results, homography);
			Point[] targets = results.toArray();
			return new Line(targets[0].x, targets[0].y, targets[1].x, targets[1].y);
		}

		public void draw(Mat frame, Scalar color, int thickness) {
			Imgproc.line(frame, new Point(x1, y1), new Point(x2, y2), color, thickness);
		}

	}

	static class Circle {
		public Circle(Point center, float radius) {
			this.center = center;
			this.radius = radius;
		}

		Point center;
		float radius;
	}

	@Override
	public void stop() throws Exception {
		super.stop();
		timer.shutdown();
		timer.awaitTermination(5000, TimeUnit.MILLISECONDS);
		capture.release();
	}

	@Override
	protected void onSpace() {
		stabilizedMode = !stabilizedMode;
	}

}
