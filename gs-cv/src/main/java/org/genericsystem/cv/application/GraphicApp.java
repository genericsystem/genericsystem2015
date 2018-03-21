package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Calibrated.AngleCalibrated;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.Lines;
import org.genericsystem.cv.application.SuperFrameImg.Span;
import org.genericsystem.cv.application.SuperFrameImg.SuperContour;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.genericsystem.layout.Layout;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;

import javafx.application.Platform;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class GraphicApp extends AbstractApp {

	private final double f = 6.053 / 0.009;
	private final GSCapture gsCapture = new GSVideoCapture(0, f, GSVideoCapture.HD, GSVideoCapture.VGA);
	// private final GSCapture gsCapture = new GSPhotoCapture("resources/image.pdf", f);
	private SuperFrameImg superFrame;
	private ReferenceManager referenceManager;
	private Config config = new Config();
	private Deperspectiver deperspectiver;
	private ScheduledExecutorService timer = new BoundedScheduledThreadPoolExecutor();
	ImageView[][] imageViews = new ImageView[][] { new ImageView[3], new ImageView[3], new ImageView[3] };

	public static void main(String[] args) {
		launch(args);
	}

	static {
		NativeLibraryLoader.load();
	}

	public GraphicApp() {
		superFrame = gsCapture.read();
		deperspectiver = new Deperspectiver(f, superFrame.getPp());
		referenceManager = new ReferenceManager(gsCapture.getResize());
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		double displaySizeReduction = 1.5;
		for (int col = 0; col < imageViews.length; col++)
			for (int row = 0; row < imageViews[col].length; row++) {
				ImageView imageView = new ImageView();
				imageViews[col][row] = imageView;
				mainGrid.add(imageViews[col][row], col, row);
				imageView.setFitWidth(superFrame.width() / displaySizeReduction);
				imageView.setFitHeight(superFrame.height() / displaySizeReduction);
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
		}, 30, 30, TimeUnit.MILLISECONDS);
	}

	private Image[] doWork() {

		System.out.println("do work");
		if (!config.stabilizedMode)
			superFrame = gsCapture.read();
		Image[] images = new Image[9];
		Lines lines = superFrame.detectLines();
		if (config.textsEnabledMode)
			lines.getLines().addAll(superFrame.findTextOrientationLines());
		AngleCalibrated[] calibratedVps = deperspectiver.computeCalibratedVps(superFrame, lines);
		if (calibratedVps == null)
			return null;
		Mat deperspectiveHomography = deperspectiver.findHomography(superFrame, calibratedVps);
		if (deperspectiveHomography == null)
			return null;

		superFrame.draw(lines, new Scalar(0, 0, 255), 1);
		superFrame.drawVanishingPointLines(lines, calibratedVps[0], new Scalar(0, 255, 0), 1);
		superFrame.drawVanishingPointLines(lines, calibratedVps[1], new Scalar(255, 0, 0), 1);
		superFrame.drawVpsArrows(calibratedVps, new double[] { 20, 20 }, new Scalar(0, 255, 0), 2);
		images[0] = superFrame.getDisplay().toJfxImage();

		SuperTemplate superDeperspectived = superFrame.deperspective(deperspectiveHomography);
		images[1] = superDeperspectived.getDiffFrame().toJfxImage();

		List<Rect> detectedRects = superDeperspectived.detectRects();
		superDeperspectived.drawRects(detectedRects, new Scalar(0, 255, 0), -1);

		SuperTemplate surfaceTemplate = new SuperTemplate(new SuperFrameImg(superDeperspectived.getDisplay().bgr2Gray().getSrc(), superFrame.getPp(), f), CvType.CV_8UC1, SuperFrameImg::getDisplay);
		Layout surfaceLayout = surfaceTemplate.layout();
		double surface = surfaceLayout.normalizedArea();

		superDeperspectived.putText(String.valueOf(surface));

		images[2] = superDeperspectived.getDisplay().toJfxImage();

		SuperTemplate superReferenceTemplate4 = new SuperTemplate(superFrame, CvType.CV_8UC3, SuperFrameImg::getFrame);

		List<SuperContour> detectedSuperContours = superReferenceTemplate4.detectSuperContours(20).stream().filter(sc -> Math.abs(sc.angle) < Math.PI / 4).collect(Collectors.toList());
		detectedSuperContours.stream().forEach(c -> Imgproc.line(superReferenceTemplate4.getDisplay().getSrc(), c.top, c.bottom, new Scalar(255, 255, 255), 1));
		detectedSuperContours.stream().forEach(c -> Imgproc.line(superReferenceTemplate4.getDisplay().getSrc(), c.left, c.right, new Scalar(255, 255, 255), 1));
		detectedSuperContours.stream().map(sc -> sc.center).forEach(pt -> Imgproc.circle(superReferenceTemplate4.getDisplay().getSrc(), pt, 3, new Scalar(255, 0, 0), -1));
		detectedSuperContours.stream().map(sc -> sc.left).forEach(pt -> Imgproc.circle(superReferenceTemplate4.getDisplay().getSrc(), pt, 3, new Scalar(0, 255, 0), -1));
		detectedSuperContours.stream().map(sc -> sc.right).forEach(pt -> Imgproc.circle(superReferenceTemplate4.getDisplay().getSrc(), pt, 3, new Scalar(0, 0, 255), -1));
		detectedSuperContours.stream().map(sc -> sc.top).forEach(pt -> Imgproc.circle(superReferenceTemplate4.getDisplay().getSrc(), pt, 3, new Scalar(0, 255, 255), -1));
		detectedSuperContours.stream().map(sc -> sc.bottom).forEach(pt -> Imgproc.circle(superReferenceTemplate4.getDisplay().getSrc(), pt, 3, new Scalar(255, 0, 255), -1));

		images[3] = superReferenceTemplate4.getDisplay().toJfxImage();

		SuperTemplate superReferenceTemplate5 = new SuperTemplate(superFrame, CvType.CV_8UC3, SuperFrameImg::getFrame) {
			@Override
			protected org.genericsystem.cv.Img buildDisplay() {
				return new Img(getFrame().getSrc(), true);
			};
		};

		List<SuperContour> filteredSuperContour = new ArrayList(
				TextOrientationLinesDetector.selectRandomObjects(superReferenceTemplate5.detectSuperContours(20).stream().filter(sc -> Math.abs(sc.angle) < Math.PI / 4 && sc.dx > 2 * sc.dy).collect(Collectors.toList()), 100));
		// filteredSuperContour = filteredSuperContour.stream().filter(sc -> Math.abs(sc.angle) < Math.PI/4 && sc.dx > 2*sc.dy).collect(Collectors.toList());

		DirectionalFilter df = new DirectionalFilter();
		int nBin = 64;
		Mat gray = superReferenceTemplate5.getGrayFrame().getSrc();
		Mat gx = df.gx(gray);
		Mat gy = df.gy(gray);
		Mat mag = new Mat();
		Mat ori = new Mat();
		Core.cartToPolar(gy, gx, mag, ori);

		Mat bin = df.bin(ori, 2 * nBin);
		filteredSuperContour.forEach(sc -> sc.computeHisto(mag, bin, nBin));

		Mat image = superReferenceTemplate5.getDisplay().getSrc();

		SuperContourInterpolator interpolator = new SuperContourInterpolator(filteredSuperContour, 2);
		MeshGrid meshGrid = new MeshGrid(20, image, interpolator, 20, 20);
		meshGrid.build();

		Img dewarped = new Img(meshGrid.dewarp(400), false);

		images[5] = dewarped.toJfxImage();

		filteredSuperContour.stream().forEach(c -> Imgproc.line(image, c.top, c.bottom, new Scalar(255, 255, 255), 1));
		filteredSuperContour.stream().forEach(c -> Imgproc.line(image, c.left, c.right, new Scalar(255, 255, 255), 1));
		/*
		 * detectedSuperContours2.stream().map(sc -> sc.center).forEach(pt -> Imgproc.circle(image, pt, 3, new Scalar(255, 0, 0), -1)); detectedSuperContours2.stream().map(sc -> sc.left).forEach(pt -> Imgproc.circle(image, pt, 3, new Scalar(0, 255, 0),
		 * -1)); detectedSuperContours2.stream().map(sc -> sc.right).forEach(pt -> Imgproc.circle(image, pt, 3, new Scalar(0, 0, 255), -1)); detectedSuperContours2.stream().map(sc -> sc.top).forEach(pt -> Imgproc.circle(image, pt, 3, new Scalar(0, 255,
		 * 255), -1)); detectedSuperContours2.stream().map(sc -> sc.bottom).forEach(pt -> Imgproc.circle(image, pt, 3, new Scalar(255, 0, 255), -1));
		 */

		meshGrid.draw(new Scalar(0, 255, 0));

		images[4] = superReferenceTemplate5.getDisplay().toJfxImage();

		// List<Point> detectedCenroids = superDeperspectived.detectCentroids();
		// SuperTemplate superReferenceTemplate = new SuperTemplate(superDeperspectived, CvType.CV_8UC1, SuperFrameImg::getFrame);
		// superReferenceTemplate.drawCentroids(detectedCenroids, new Scalar(255), -1, 2);
		// new Lines(superReferenceTemplate.getDisplay().houghLinesP(1, Math.PI / 180, 10, 50, 47)).filter(line -> Math.atan2(Math.abs(line.y2 - line.y1), Math.abs(line.x2 - line.x1)) < 5 * Math.PI / 180).draw(superReferenceTemplate.getDisplay().getSrc(),
		// new Scalar(255), 1);
		// images[4] = superReferenceTemplate.getDisplay().toJfxImage();

		SuperTemplate superReferenceTemplate2 = new SuperTemplate(superReferenceTemplate5, CvType.CV_8UC3, SuperFrameImg::getFrame);
		List<Span> spans = superReferenceTemplate2.assembleContours(filteredSuperContour, c -> true, 100, 5, 100);
		spans.forEach(sp -> {
			double a = Math.random() * 255;
			double b = Math.random() * 255;
			double c = Math.random() * 255;
			Scalar color = new Scalar(a, b, c);
			sp.getContours().forEach(ct -> Imgproc.drawContours(superReferenceTemplate2.getDisplay().getSrc(), Arrays.asList(ct.contour), 0, color, -1));
			if (!sp.getContours().isEmpty()) {
				Point[] pointer = new Point[] { sp.getContours().get(0).center };
				sp.getContours().forEach(ct -> {
					Imgproc.line(superReferenceTemplate2.getDisplay().getSrc(), pointer[0], ct.center, color, 1);
					pointer[0] = ct.center;
				});
			}
		});

		int rows = spans.size();
		int cols = (int) superReferenceTemplate2.size().width / 10;
		Double[][] table = new Double[rows][cols];
		for (int row = 0; row < rows; row++)
			for (int col = 0; col < cols; col++)
				table[row][col] = spans.get(row).getApprox().apply((double) col * 10);

		for (int row = 1; row < rows - 1; row++)
			for (int col = 0; col < cols; col++)
				if (table[row][col] == null) {
					if (col > 0 && table[row][col - 1] != null) {
						int topRow = row - 1;
						while (topRow >= 0 && (table[topRow][col - 1] == null || table[topRow][col] == null))
							topRow--;
						if (topRow >= 0) {
							int bottomRow = row + 1;
							while (bottomRow < rows && (table[bottomRow][col - 1] == null || table[bottomRow][col] == null))
								bottomRow++;
							if (bottomRow < rows) {
								double rapport = (table[row][col - 1] - table[topRow][col - 1]) / (table[bottomRow][col - 1] - table[topRow][col - 1]);
								table[row][col] = rapport * (table[bottomRow][col] - table[topRow][col]) + table[topRow][col];
							}
						}
					}
				}

		for (int row = 0; row < rows - 1; row++)
			for (int col = cols - 2; col >= 0; col--)
				if (table[row][col] == null) {
					if (col < cols - 1 && table[row][col + 1] != null) {
						int topRow = row - 1;
						while (topRow >= 0 && (table[topRow][col + 1] == null || table[topRow][col] == null))
							topRow--;
						if (topRow >= 0) {
							int bottomRow = row + 1;
							while (bottomRow < rows && (table[bottomRow][col + 1] == null || table[bottomRow][col] == null))
								bottomRow++;
							if (bottomRow < rows) {
								double rapport = (table[row][col + 1] - table[topRow][col + 1]) / (table[bottomRow][col + 1] - table[topRow][col + 1]);
								table[row][col] = rapport * (table[bottomRow][col] - table[topRow][col]) + table[topRow][col];
							}
						}
					}
				}

		for (int row = 0; row < rows; row++) {
			Point pointer = table[row][0] != null ? new Point(0, table[row][0]) : null;
			for (int col = 1; col < cols; col++) {
				Point tmp = table[row][col] != null ? new Point(col * 10, table[row][col]) : null;
				if (tmp != null && pointer != null)
					Imgproc.line(superReferenceTemplate2.getDisplay().getSrc(), pointer, tmp, new Scalar(0, 0, 255), 1);
				pointer = tmp;
			}
		}

		// Point pointer = new Point(0, approx.apply(0d));
		// for (double x = 10; x < superReferenceTemplate2.size().width; x += 10) {
		// Point tmp = new Point(x, approx.apply(x));
		// Imgproc.line(superReferenceTemplate2.getDisplay().getSrc(), pointer, tmp, new Scalar(0, 0, 255), 1);
		// pointer = tmp;
		// }

		// detectedrealCenroids.stream().map(sc -> sc.point1).forEach(pt -> Imgproc.circle(superReferenceTemplate2.getDisplay().getSrc(), pt, 3, new Scalar(0, 0, 255), -1));
		// new Lines(superReferenceTemplate2.getDisplay().houghLinesP(1, Math.PI / 180, 10, 50, 47)).filter(line -> Math.atan2(Math.abs(line.y2 - line.y1), Math.abs(line.x2 - line.x1)) < 5 * Math.PI /
		// 180).draw(superReferenceTemplate2.getDisplay().getSrc(),
		// new Scalar(255), 1);
		// images[5] = superReferenceTemplate2.getDisplay().toJfxImage();

		ImgDescriptor newImgDescriptor = new ImgDescriptor(superDeperspectived, surface);
		if (newImgDescriptor.getDescriptors().empty()) {
			System.out.println("Empty descriptors");
			return null;
		}
		referenceManager.submit(newImgDescriptor, detectedRects);
		List<Rect> referenceRects = referenceManager.getReferenceRects();
		SuperTemplate referenceTemplate = new SuperTemplate(referenceManager.getReference().getSuperFrame(), CvType.CV_8UC1, SuperFrameImg::getFrame);
		referenceTemplate.drawRects(referenceRects, new Scalar(255), -1);
		images[6] = referenceTemplate.getDisplay().toJfxImage();

		SuperTemplate superReferenceTemplate3 = new SuperTemplate(superFrame, CvType.CV_8UC1, SuperFrameImg::getFrame);
		superReferenceTemplate3.drawRects(referenceManager.getResizedFieldsRects(), new Scalar(255), -1);
		images[7] = superReferenceTemplate3.getDisplay().toJfxImage();

		SuperTemplate layoutTemplate = new SuperTemplate(referenceTemplate, CvType.CV_8UC3, SuperFrameImg::getDisplay);
		Layout layout = layoutTemplate.layout();
		layoutTemplate.drawLayout(layout);
		images[8] = layoutTemplate.getDisplay().toJfxImage();

		return images;
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

	@Override
	protected void onT() {
		config.textsEnabledMode = !config.textsEnabledMode;
	}

	@Override
	public void stop() throws Exception {
		super.stop();
		timer.shutdown();
		timer.awaitTermination(5000, TimeUnit.MILLISECONDS);
		gsCapture.release();
	}
}
