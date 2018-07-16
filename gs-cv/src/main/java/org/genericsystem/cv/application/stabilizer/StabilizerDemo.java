package org.genericsystem.cv.application.stabilizer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.application.BoundedScheduledThreadPoolExecutor;
import org.genericsystem.cv.application.Config;
import org.genericsystem.cv.application.GSCapture;
import org.genericsystem.cv.application.GSVideoCapture;
import org.genericsystem.cv.application.fht.FHTManager;
import org.genericsystem.cv.application.mesh.Points;
import org.genericsystem.cv.utils.NativeLibraryLoader;
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

import javafx.application.Platform;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class StabilizerDemo extends AbstractApp {

	private final GSCapture gsCapture = new GSVideoCapture(0, GSVideoCapture.HD, GSVideoCapture.VGA);
	// private final GSCapture gsCapture = new GSVideoCapture("http://192.168.1.13:8080/video",GSVideoCapture.HD, GSVideoCapture.VGA);
	// private final GSCapture gsCapture = new GSPhotoCapture("resources/image.pdf");
	private Img frame;
	private ReferenceManager referenceManager;
	private Config config = new Config();
	private ScheduledExecutorService timer = new BoundedScheduledThreadPoolExecutor();
	private FHTManager fhtManager = new FHTManager(gsCapture.getResize());
	private ImageView[][] imageViews = new ImageView[][] { new ImageView[3], new ImageView[3], new ImageView[3] };
	private int frameCount = 0;

	public static void main(String[] args) {
		launch(args);
	}

	static {
		NativeLibraryLoader.load();
	}

	public StabilizerDemo() {
		frame = gsCapture.read();
		referenceManager = new ReferenceManager(gsCapture.getResize());
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		double displaySizeReduction = 1;
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
		}, 2000, 50, TimeUnit.MILLISECONDS);
	}

	public boolean contains(Rect rect, Rect shiftedRect) {
		return (rect.tl().x <= shiftedRect.tl().x && rect.tl().y <= shiftedRect.tl().y && rect.br().x >= shiftedRect.br().x && rect.br().y >= shiftedRect.br().y);
	}

	private Image[] doWork() {

		System.out.println("do work");
		if (!config.stabilizedMode) {
			frame = gsCapture.read();
			frameCount++;
		}
		long ref = System.currentTimeMillis();

		Image[] images = new Image[10];
		images[0] = frame.toJfxImage();

		if (frameCount < 30)
			return images;

		ImgDescriptor newImgDescriptor = new ImgDescriptor(frame);
		if (newImgDescriptor.getDescriptors().empty()) {
			System.out.println("Empty descriptors");
			return null;
		}
		if (!referenceManager.submit(newImgDescriptor)) {
			System.out.println("No reconciliation found");
			return null;
		}
		Mat homography = referenceManager.getHomography(newImgDescriptor);
		if (homography == null) {
			referenceManager = new ReferenceManager(frame.size());
			System.out.println("Refernce manager is full");
			return null;
		}

		Mat stabilized = referenceManager.dewarp(newImgDescriptor, homography);
		MatOfPoint trackedFeatures = new MatOfPoint();
		MatOfPoint corners = new MatOfPoint();// global
		Imgproc.goodFeaturesToTrack(new Img(stabilized, false).bgr2Gray().getSrc(), trackedFeatures, 300, 0.01, 10);
		Mat stabilizedDisplay = stabilized.clone();
		Arrays.stream(trackedFeatures.toArray()).forEach(corner -> Imgproc.circle(stabilizedDisplay, corner, 3, new Scalar(0, 255, 0)));

		// Mat gray = new Img(xxx, false).bgr2Gray().getSrc();
		// MatOfByte status = new MatOfByte();
		// MatOfFloat errors = new MatOfFloat();
		// Mat prevGray = gray;
		//
		// Video.calcOpticalFlowPyrLK(prevGray, gray, new MatOfPoint2f(trackedFeatures.toArray()), new MatOfPoint2f(corners.toArray()), status, errors, new Size(10, 10), 3, new TermCriteria(TermCriteria.COUNT + TermCriteria.EPS, 30, 0.01), 0, 0.0001);
		// // size 21*21 ?
		//
		// Mat newRigidTransform = estimateRigidTransform(trackedFeatures, corners, false);
		// Mat nrt33 = Mat.eye(3, 3, CvType.CV_32SC1);
		// newRigidTransform.copyTo(nrt33.rowRange(0, 2));
		// Mat rigidTransform = new Mat();// global var
		// Core.gemm(rigidTransform, nrt33, 1, new Mat(), 0, rigidTransform);
		//
		// Mat invTrans = rigidTransform.inv();// DECOMP_SVD
		// Imgproc.warpAffine(frame.getSrc(), stabilizedDisplay, invTrans.rowRange(0, 2), frame.getSrc().size());

		images[1] = new Img(stabilizedDisplay, false).toJfxImage();

		// Mat rvec1, tvec1;
		// solvePnP(objectPoints, corners1, cameraMatrix, distCoeffs, rvec1, tvec1);
		// Mat rvec2, tvec2;
		// solvePnP(objectPoints, corners2, cameraMatrix, distCoeffs, rvec2, tvec2);

		Mat binarized = new Img(stabilized, false).adaptativeGaussianInvThreshold(7, 5).getSrc();

		if (!fhtManager.isInitialized() || frameCount % 30 == 0)
			fhtManager.init(binarized);
		// images[2] = new Img(fhtManager.getMeshManager().drawReverse(stabilized, new Scalar(255, 0, 0), new Scalar(0, 255, 0)), false).toJfxImage();
		// if (!fhtManager.isInitialized() || frameCount % 30 == 0)
		fhtManager.init(binarized);

		images[1] = new Img(fhtManager.getMeshManager().drawReverse(stabilized, new Scalar(255, 0, 0), new Scalar(0, 255, 0)), false).toJfxImage();

		Img dewarped = new Img(fhtManager.getMeshManager().dewarpReverse(stabilized), false);
		images[2] = dewarped.toJfxImage();

		Img dewarpedBinary = dewarped.adaptativeGaussianInvThreshold(7, 5);
		Mat horizontalHisto = new Mat();
		Core.reduce(dewarpedBinary.getSrc(), horizontalHisto, 1, Core.REDUCE_SUM, CvType.CV_64FC1);
		Mat verticalHisto = new Mat();
		Core.reduce(dewarpedBinary.getSrc(), verticalHisto, 0, Core.REDUCE_SUM, CvType.CV_64FC1);

		// Core.divide(dewarpedBinary.width(), dst, dst);
		Core.normalize(horizontalHisto, horizontalHisto, 0, 255, Core.NORM_MINMAX);
		Mat transposedVerticalHisto = verticalHisto.t();
		Core.normalize(transposedVerticalHisto, transposedVerticalHisto, 0, 255, Core.NORM_MINMAX);

		Mat hHisto = Mat.zeros(new Size(255, dewarpedBinary.height()), CvType.CV_8UC1);
		// for (int row = 0; row < horizontalHisto.rows(); row++)
		// Imgproc.line(hHisto, new Point(0, row), new Point(horizontalHisto.get(row, 0)[0], row), new Scalar(255), 1);

		List<Mag> magnitudes = new ArrayList<>();
		for (int index = 0; index < horizontalHisto.rows(); index++)
			magnitudes.add(new Mag(index, horizontalHisto.get(index, 0)[0]));
		List<Mag[]> histoLines = getHistoLines(magnitudes, 0.3, 0.1);
		for (Mag[] mags : histoLines)
			Imgproc.rectangle(hHisto, new Rect(new Point(0, mags[0].index), new Point(255, mags[1].index)), new Scalar(255), -1);

		images[3] = new Img(hHisto, false).toJfxImage();

		Mat vHisto = Mat.zeros(new Size(dewarpedBinary.width(), 255), CvType.CV_8UC1);
		// for (int col = 0; col < transposedVerticalHisto.rows(); col++)
		// Imgproc.line(vHisto, new Point(col, 0), new Point(col, transposedVerticalHisto.get(col, 0)[0]), new Scalar(255), 1);

		List<Mag> vMagnitudes = new ArrayList<>();
		for (int index = 0; index < transposedVerticalHisto.rows(); index++)
			vMagnitudes.add(new Mag(index, transposedVerticalHisto.get(index, 0)[0]));
		histoLines = getHistoLines(vMagnitudes, 0.5, 0.3);
		for (Mag[] mags : histoLines)
			Imgproc.rectangle(vHisto, new Rect(new Point(mags[0].index, 0), new Point(mags[1].index, 255)), new Scalar(255), -1);

		images[4] = new Img(vHisto, false).toJfxImage();

		int nbrCell = 6;
		Points pts = fhtManager.getMeshManager().getReverseMesh().getPoints();
		Point[] basePts = new Point[] { pts.getPoint(-nbrCell, -nbrCell), pts.getPoint(-nbrCell, nbrCell), pts.getPoint(nbrCell, nbrCell), pts.getPoint(nbrCell, -nbrCell) };

		double width = stabilized.size().width / (2 * fhtManager.getHalfGridWidth().get());
		double height = stabilized.size().height / (2 * fhtManager.getHalfGridHeight().get());
		Point[] targetPts = new Point[] { new Point(stabilized.size().width / 2 - nbrCell * width, stabilized.size().height / 2 - nbrCell * height), new Point(stabilized.size().width / 2 + nbrCell * width, stabilized.size().height / 2 - nbrCell * height),
				new Point(stabilized.size().width / 2 + nbrCell * width, stabilized.size().height / 2 + nbrCell * height), new Point(stabilized.size().width / 2 - nbrCell * width, stabilized.size().height / 2 + nbrCell * height) };

		Mat homography2 = Imgproc.getPerspectiveTransform(new MatOfPoint2f(basePts), new MatOfPoint2f(targetPts));
		Mat stabilized2 = new Mat();
		Imgproc.warpPerspective(fhtManager.getMeshManager().buildEnlarged(stabilized), stabilized2, homography2, stabilized.size());

		Mat binarized2 = new Img(stabilized2, false).adaptativeGaussianInvThreshold(7, 5).getSrc();
		// if (!fhtManager.isInitialized() || frameCount % 30 == 0)
		fhtManager.init(binarized2);
		images[5] = new Img(fhtManager.getMeshManager().drawReverse(stabilized2, new Scalar(255, 0, 0), new Scalar(0, 255, 0)), false).toJfxImage();

		// Mesh reverseMesh = fhtManager.getMeshManager().getReverseMesh();
		// Mat result = reverseMesh.dewarp(stabilized, stabilized.size());
		images[6] = new Img(fhtManager.getMeshManager().dewarpReverse(stabilized2), false).toJfxImage();

		// images[3] = new Img(stabilized2, false).toJfxImage();
		// Calib3d.findHomography(new MatOfPoint2f(basePts), new MatOfPoint2f(targetPts), Calib3d.RANSAC, 1);

		// Mat patch = Mat.zeros(frame.size(), frame.type());
		// int deltaX = (int) (frame.size().width / 8);
		// int deltaY = (int) (frame.size().height / 4);
		// Iterator<ImgDescriptor> it = referenceManager.getToReferenceGraphy().keySet().iterator();
		// for (int row = 0; (row + deltaY) <= frame.size().height; row += deltaY)
		// for (int col = 0; (col + deltaX) <= frame.size().width; col += deltaX) {
		// Mat cell = new Mat(patch, new Range(row, row + deltaY), new Range(col, col + deltaX));
		// Mat resized = new Mat();
		// if (it.hasNext()) {
		// Imgproc.resize(it.next().getFrame().getSrc(), resized, new Size(deltaX, deltaY));
		// resized.copyTo(cell);
		// }
		//
		// }
		// images[2] = new Img(patch, false).toJfxImage();
		return images;
	}

	public static class Mag implements Comparable<Mag> {
		private final double magnitude;
		private final int index;

		public Mag(int index, double magnitude) {
			this.index = index;
			this.magnitude = magnitude;
		}

		@Override
		public int compareTo(Mag mag) {
			int result = Double.compare(mag.magnitude, magnitude);
			return result != 0 ? result : Double.compare(mag.index, index);
		}
	}

	public static List<Mag[]> getHistoLines(List<Mag> magnitudes, double localTheshold, double globalTheshold) {
		Set<Mag> alreadyComputed = new HashSet<>();
		double max = magnitudes.stream().mapToDouble(ts -> ts.magnitude).max().getAsDouble();
		// System.out.println("Max : " + max);
		List<Mag[]> result = new ArrayList<>();
		for (Mag mag : magnitudes.stream().sorted().collect(Collectors.toList())) {
			if (mag.magnitude < globalTheshold * max)
				break;
			if (!alreadyComputed.contains(mag)) {
				double tAlpha = localTheshold * mag.magnitude;

				int y1 = mag.index;
				while (y1 >= 0 && magnitudes.get(y1).magnitude >= tAlpha)
					y1--;
				if (y1 != 0)
					y1++;

				int y2 = mag.index;
				while (y2 < magnitudes.size() && magnitudes.get(y2).magnitude >= tAlpha)
					y2++;
				if (y2 != magnitudes.size() - 1)
					y2--;

				boolean alreadyVisited = false;
				for (int y = y1; y <= y2; y++)
					if (alreadyComputed.contains(magnitudes.get(y))) {
						alreadyVisited = true;
						break;
					}
				for (int y = y1; y <= y2; y++)
					alreadyComputed.add(magnitudes.get(y));
				if (!alreadyVisited)
					result.add(new Mag[] { magnitudes.get(y1), magnitudes.get(y2) });
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
	protected void onR() {
		timer.schedule(() -> referenceManager.clear(), 0, TimeUnit.MILLISECONDS);
	}

	List<Rect> detectRects(Mat mask, int minArea, int maxArea) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(mask, contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		Size size = mask.size();
		List<Rect> result = new ArrayList<>();
		for (MatOfPoint contour : contours) {
			double area = Imgproc.contourArea(contour);
			if (area >= minArea && area <= maxArea) {
				Rect rect = Imgproc.boundingRect(contour);
				// if (rect.tl().x != 0 && rect.tl().y != 0 && rect.br().x != size.width && rect.br().y != size.height)
				// if (getFillRatio(contour, rect) > fillRatio)
				result.add(rect);

			}
		}
		Collections.reverse(result);
		return result;
	}

	public double getFillRatio(MatOfPoint contour, Rect rect) {
		Mat mask = Mat.zeros(rect.size(), CvType.CV_8UC1);
		Imgproc.drawContours(mask, Arrays.asList(contour), 0, new Scalar(255), -1, Imgproc.LINE_8, new Mat(), Integer.MAX_VALUE, new Point(-rect.tl().x, -rect.tl().y));
		Mat mat = new Mat();
		Core.findNonZero(mask, mat);
		return mat.rows() / rect.area();
	}

	@Override
	public void stop() throws Exception {
		super.stop();
		timer.shutdown();
		timer.awaitTermination(5000, TimeUnit.MILLISECONDS);
		gsCapture.release();
	}
}
