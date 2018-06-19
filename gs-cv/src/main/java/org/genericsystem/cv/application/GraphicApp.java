package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

import javafx.application.Platform;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class GraphicApp extends AbstractApp {

	private final GSCapture gsCapture = new GSVideoCapture(0, GSVideoCapture.HD, GSVideoCapture.VGA);
	// private final GSCapture gsCapture = new GSVideoCapture("http://192.168.1.13:8080/video",GSVideoCapture.HD, GSVideoCapture.VGA);
	// private final GSCapture gsCapture = new GSPhotoCapture("resources/image.pdf");
	private Img frame;
	private ReferenceManager referenceManager;
	private Config config = new Config();
	private ScheduledExecutorService timer = new BoundedScheduledThreadPoolExecutor();
	private FHTManager fhtManager = new FHTManager();
	ImageView[][] imageViews = new ImageView[][] { new ImageView[3], new ImageView[3], new ImageView[3], new ImageView[3] };

	public static void main(String[] args) {
		launch(args);
	}

	static {
		NativeLibraryLoader.load();
	}

	public GraphicApp() {
		frame = gsCapture.read();
		referenceManager = new ReferenceManager(gsCapture.getResize());
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {

		addIntegerSliderProperty("hBlurSize", fhtManager.gethBlurSize(), 0, 200);
		addIntegerSliderProperty("vBlurSize", fhtManager.getvBlurSize(), 0, 200);
		addDoubleSliderProperty("hNeighbourPenality", fhtManager.gethNeighbourPenality(), -5000, 0);
		addDoubleSliderProperty("vNeighbourPenality", fhtManager.getvNeighbourPenality(), -5000, 0);
		addDoubleSliderProperty("hAnglePenality", fhtManager.gethAnglePenality(), -1, 0);
		addDoubleSliderProperty("vAnglePenality", fhtManager.getvAnglePenality(), -1, 0);

		double displaySizeReduction = 1.5;
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
		}, 2000, 30, TimeUnit.MILLISECONDS);
	}

	private Image[] doWork() {

		System.out.println("do work");
		if (!config.stabilizedMode)
			frame = gsCapture.read();
		Image[] images = new Image[10];
		images[0] = frame.toJfxImage();

		Img binarized = frame.adaptativeGaussianInvThreshold(7, 5);
		Img flat = fhtManager.dewarp(frame.getSrc(), binarized.getSrc(), 0.75, 0.75);
		images[1] = flat.toJfxImage();

		// Img flatBinarized = flat.adaptativeGaussianInvThreshold(7, 5);
		Img gray = flat.bgr2Gray().gaussianBlur(new Size(3, 3));
		Core.absdiff(gray.getSrc(), new Scalar(100), gray.getSrc());
		Imgproc.adaptiveThreshold(gray.getSrc(), gray.getSrc(), 255, Imgproc.ADAPTIVE_THRESH_GAUSSIAN_C, Imgproc.THRESH_BINARY_INV, 7, 3);
		Img flatBinarized = new Img(gray.getSrc(), false);
		images[2] = flatBinarized.toJfxImage();

		List<Rect> detectedRects = detectRects(flatBinarized, 1, 10000, 0.);
		Img flatDisplay = new Img(flat.getSrc(), true);
		detectedRects.forEach(rect -> Imgproc.rectangle(flatDisplay.getSrc(), rect.tl(), rect.br(), new Scalar(0, 255, 0), -1));
		images[3] = flatDisplay.toJfxImage();

		// Layout surfaceLayout = flatBinarized.buildLayout(new Size(2, 0.4), new Size(0.04, 0.008), 8);
		// double surface = surfaceLayout.normalizedArea();
		// System.out.println("surface : " + surface + " " + surfaceLayout.area(flatBinarized) + " " + surfaceLayout.computeTotalSurface(flatBinarized));
		// Imgproc.putText(flatDisplay.getSrc(), String.valueOf(surface), new Point(flatDisplay.width() / 2, 20), Core.FONT_HERSHEY_PLAIN, 1, new Scalar(255, 255, 255), 1);

		// SuperTemplate superReferenceTemplate4 = new SuperTemplate(frame, CvType.CV_8UC3, SuperFrameImg::getFrame);
		//
		// List<SuperContour> detectedSuperContours = superReferenceTemplate4.detectSuperContours(20).stream().filter(sc -> Math.abs(sc.angle) < Math.PI / 4).collect(Collectors.toList());
		// detectedSuperContours.stream().forEach(c -> Imgproc.line(superReferenceTemplate4.getDisplay().getSrc(), c.top, c.bottom, new Scalar(255, 255, 255), 1));
		// detectedSuperContours.stream().forEach(c -> Imgproc.line(superReferenceTemplate4.getDisplay().getSrc(), c.left, c.right, new Scalar(255, 255, 255), 1));
		// detectedSuperContours.stream().map(sc -> sc.center).forEach(pt -> Imgproc.circle(superReferenceTemplate4.getDisplay().getSrc(), pt, 3, new Scalar(255, 0, 0), -1));
		// detectedSuperContours.stream().map(sc -> sc.left).forEach(pt -> Imgproc.circle(superReferenceTemplate4.getDisplay().getSrc(), pt, 3, new Scalar(0, 255, 0), -1));
		// detectedSuperContours.stream().map(sc -> sc.right).forEach(pt -> Imgproc.circle(superReferenceTemplate4.getDisplay().getSrc(), pt, 3, new Scalar(0, 0, 255), -1));
		// detectedSuperContours.stream().map(sc -> sc.top).forEach(pt -> Imgproc.circle(superReferenceTemplate4.getDisplay().getSrc(), pt, 3, new Scalar(0, 255, 255), -1));
		// detectedSuperContours.stream().map(sc -> sc.bottom).forEach(pt -> Imgproc.circle(superReferenceTemplate4.getDisplay().getSrc(), pt, 3, new Scalar(255, 0, 255), -1));
		//
		// images[3] = superReferenceTemplate4.getDisplay().toJfxImage();
		//
		// SuperTemplate superReferenceTemplate5 = new SuperTemplate(frame, CvType.CV_8UC3, SuperFrameImg::getFrame) {
		// @Override
		// protected org.genericsystem.cv.Img buildDisplay() {
		// return new Img(getFrame().getSrc(), true);
		// };
		// };
		//
		// List<SuperContour> filteredSuperContour = new ArrayList<>(
		// TextOrientationLinesDetector.selectRandomObjects(superReferenceTemplate5.detectSuperContours(20).stream().filter(sc -> Math.abs(sc.angle) < Math.PI / 4 && sc.dx > 2 * sc.dy).collect(Collectors.toList()), 200));
		//
		// DirectionalFilter df = new DirectionalFilter();
		// int nBin = 64;
		// Mat gray = superReferenceTemplate5.getFrame().bgr2Gray().getSrc();
		// Mat gx = df.gx(gray);
		// Core.subtract(Mat.zeros(gx.size(), gx.type()), gx, gx);
		// Mat gy = df.gy(gray);
		// Mat mag = new Mat();
		// Mat ori = new Mat();
		// Core.cartToPolar(gx, gy, mag, ori);
		//
		// int[][] bin = df.bin(ori, nBin);
		// List<Span> spans = superReferenceTemplate5.assembleContours(filteredSuperContour, c -> true, 100, 30, 70);
		// // filteredSuperContour = spans.stream().flatMap(span -> span.getContours().stream()).collect(Collectors.toList());
		// double regionSize = 100;
		// // filteredSuperContour = filteredSuperContour.stream()
		// // .filter(sc -> sc.center.x - regionSize > 0 && sc.center.x + regionSize / 2 < superReferenceTemplate5.getFrame().width() && sc.center.y - regionSize / 2 > 0 && sc.center.y + regionSize / 2 < superReferenceTemplate5.getFrame().height())
		// // .collect(Collectors.toList());
		//
		// filteredSuperContour.forEach(sc -> sc.computeHisto(mag, bin, nBin, df, 100));
		//
		// // Mat image = superReferenceTemplate5.getDisplay().getSrc();
		//
		// SuperContourInterpolator interpolator = new SuperContourInterpolator(filteredSuperContour, 2);
		// MeshManager meshGrid = new MeshManager(16, 9, interpolator, 20, 20, superReferenceTemplate5.getFrame().getSrc());
		//
		// Mat image = meshGrid.drawOnCopy(new Scalar(0, 255, 0), new Scalar(0, 0, 255));
		// Mat internal = new Mat(image, new Rect(new Point(20, 20), new Point(image.width() - 20, image.height() - 20)));
		// filteredSuperContour.stream().forEach(c -> Imgproc.line(internal, c.top, c.bottom, new Scalar(255, 255, 255), 1));
		// filteredSuperContour.stream().forEach(c -> Imgproc.line(internal, c.vBottom, c.vTop, new Scalar(0, 0, 255), 2));
		//
		// images[4] = new Img(internal).toJfxImage();
		//
		// images[5] = new Img(meshGrid.dewarp(), false).toJfxImage();
		//
		// SuperTemplate superReferenceTemplate2 = new SuperTemplate(superReferenceTemplate5, CvType.CV_8UC3, SuperFrameImg::getFrame);
		// // List<Span> spans = superReferenceTemplate2.assembleContours(filteredSuperContour, c -> true, 100, 30, 70);
		// spans.forEach(sp -> {
		// double a = Math.random() * 255;
		// double b = Math.random() * 255;
		// double c = Math.random() * 255;
		// Scalar color = new Scalar(a, b, c);
		// sp.getContours().forEach(ct -> Imgproc.drawContours(superReferenceTemplate2.getDisplay().getSrc(), Arrays.asList(ct.contour), 0, color, -1));
		// if (!sp.getContours().isEmpty()) {
		// Point[] pointer = new Point[] { sp.getContours().get(0).center };
		// sp.getContours().forEach(ct -> {
		// Imgproc.line(superReferenceTemplate2.getDisplay().getSrc(), pointer[0], ct.center, color, 1);
		// pointer[0] = ct.center;
		// });
		// }
		// });
		//
		// images[6] = superReferenceTemplate2.getDisplay().toJfxImage();

		ImgDescriptor newImgDescriptor = new ImgDescriptor(flat);
		if (newImgDescriptor.getDescriptors().empty()) {
			System.out.println("Empty descriptors");
			return null;
		}
		referenceManager.submit(newImgDescriptor, detectedRects);
		List<Rect> referenceRects = referenceManager.getReferenceRects();
		Mat referenceTemplate = Mat.zeros(flat.size(), CvType.CV_8UC1);// new SuperTemplate(referenceManager.getReference().getSuperFrame(), CvType.CV_8UC1, SuperFrameImg::getFrame);
		referenceRects.forEach(rect -> Imgproc.rectangle(referenceTemplate, rect, new Scalar(255), -1));
		images[4] = new Img(referenceTemplate, false).toJfxImage();

		Mat display = Mat.zeros(frame.size(), CvType.CV_8UC1);
		referenceManager.getResizedFieldsRects().forEach(rect -> Imgproc.rectangle(display, rect, new Scalar(255), -1));
		images[5] = new Img(display, false).toJfxImage();
		//
		// SuperTemplate layoutTemplate = new SuperTemplate(referenceTemplate, CvType.CV_8UC3, SuperFrameImg::getDisplay);
		// Layout layout = layoutTemplate.layout();
		// if (layout != null) {
		// layoutTemplate.drawLayout(layout);
		// images[9] = layoutTemplate.getDisplay().toJfxImage();
		// }

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

	List<Rect> detectRects(Img binarized, int minArea, int maxArea, double fillRatio) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(binarized.getSrc(), contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		Size size = binarized.size();
		List<Rect> result = new ArrayList<>();
		for (MatOfPoint contour : contours) {
			double area = Imgproc.contourArea(contour);
			if (area > minArea && area < maxArea) {
				Rect rect = Imgproc.boundingRect(contour);
				if (rect.tl().x != 0 && rect.tl().y != 0 && rect.br().x != size.width && rect.br().y != size.height)
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
