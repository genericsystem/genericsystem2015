package org.genericsystem.cv.application;

import java.text.Normalizer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.Ocr;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfDouble;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfRect;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.features2d.MSER;
import org.opencv.imgproc.Imgproc;
import org.opencv.utils.Converters;

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
	private FHTManager fhtManager = new FHTManager(gsCapture.getResize());
	private ImageView[][] imageViews = new ImageView[][] { new ImageView[3], new ImageView[3], new ImageView[3] };
	private int frameCount = 0;
	private final MSER detector = MSER.create(1, 6, 200, 0.25, 0.2, 200, 1.01, 0.03, 5);

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
		}, 2000, 30, TimeUnit.MILLISECONDS);
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

		Img binarized = frame.adaptativeGaussianInvThreshold(7, 5);
		Img flat = fhtManager.init(frame.getSrc(), binarized.getSrc()).getDewarp();

		images[1] = flat.toJfxImage();
		ref = trace("Dewarp", ref);
		// Img flatBinarized = flat.adaptativeGaussianInvThreshold(7, 5);
		// Img gray = flat.bgr2Gray().gaussianBlur(new Size(3, 3));
		// Core.absdiff(gray.getSrc(), new Scalar(100), gray.getSrc());
		// Imgproc.adaptiveThreshold(gray.getSrc(), gray.getSrc(), 255, Imgproc.ADAPTIVE_THRESH_GAUSSIAN_C, Imgproc.THRESH_BINARY_INV, 7, 3);
		// Img flatBinarized = new Img(gray.getSrc(), false);
		// images[2] = flatBinarized.toJfxImage();

		ArrayList<MatOfPoint> regions = new ArrayList<>();
		MatOfRect mor = new MatOfRect();
		detector.detectRegions(flat.bgr2Gray().getSrc(), regions, mor);
		List<Rect> rects = new ArrayList<>();
		Converters.Mat_to_vector_Rect(mor, rects);

		rects.removeIf(rect -> {
			for (Rect rect_ : rects)
				if (!rect_.equals(rect) && contains(rect_, rect))
					return true;
			return false;
		});

		Mat mask = Mat.zeros(flat.size(), CvType.CV_8UC1);
		rects.forEach(rect -> Imgproc.rectangle(mask, rect, new Scalar(255, 0, 0), -1));
		images[2] = new Img(mask, false).toJfxImage();
		ref = trace("Mask", ref);
		// RobustTextDetectorManager rbm = new RobustTextDetectorManager(flat.bgr2Gray().getSrc(), 1);
		// Img mserMask = new Img(rbm.getMserMask(), false);
		// images[2] = mserMask.toJfxImage();
		//
		Mat closed = new Mat();
		Imgproc.morphologyEx(mask, closed, Imgproc.MORPH_CLOSE, Imgproc.getStructuringElement(Imgproc.MORPH_RECT, new Size(5, 1)));
		List<Rect> detectedClosedRects = detectRects(closed, 8, 20000);
		Mat closedMask = Mat.zeros(flat.size(), CvType.CV_8UC1);
		detectedClosedRects.forEach(rect -> Imgproc.rectangle(closedMask, rect, new Scalar(255), -1));

		Mat flatDisplay = Mat.zeros(flat.size(), flat.type());
		flat.getSrc().copyTo(flatDisplay, closedMask);
		detectedClosedRects.forEach(rect -> Imgproc.rectangle(flatDisplay, rect.tl(), rect.br(), new Scalar(0, 255, 0), 1));
		images[3] = new Img(flatDisplay, false).toJfxImage();
		ref = trace("Close mask", ref);

		Mat flatDisplay2 = Mat.zeros(flat.size(), flat.type());
		Labels labels = new Labels(detectedClosedRects);
		labels.ocr(flat.getSrc(), 0, 1, 2, 2);
		labels.putOcr(flatDisplay2);
		Mat destination = new Mat();
		Mat matGray = new Mat();
		Imgproc.cvtColor(flat.getSrc(), matGray, Imgproc.COLOR_BGR2GRAY);
		Imgproc.Laplacian(matGray, destination, 3);
		MatOfDouble median = new MatOfDouble();
		MatOfDouble std = new MatOfDouble();
		Core.meanStdDev(destination, median, std);
		Imgproc.putText(flatDisplay2, "Quality : " + (int) Math.pow(std.get(0, 0)[0], 2), new Point(50, 50), Core.FONT_HERSHEY_PLAIN, 1, new Scalar(0, 0, 255), 1);

		System.out.println("Quality : " + Math.pow(std.get(0, 0)[0], 2));
		images[4] = new Img(flatDisplay2, false).toJfxImage();
		ref = trace("Ocr", ref);

		// Layout layout = mserMask.buildLayout(new Size(0, 0), new Size(0.08, 0.001), 8);
		// Img flatDisplay2 = new Img(flat.getSrc(), true);
		// layout.draw(flatDisplay2, new Scalar(255, 0, 0), new Scalar(0, 255, 0), 0, 1);
		// layout.ocrTree(flat, 0, 0);
		// layout.drawOcr(flatDisplay2);
		// images[5] = flatDisplay2.toJfxImage();
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

		ImgDescriptor newImgDescriptor = new ImgDescriptor(flat, labels);
		if (newImgDescriptor.getDescriptors().empty()) {
			System.out.println("Empty descriptors");
			return null;
		}
		referenceManager.submit(newImgDescriptor, detectedClosedRects);
		List<Rect> referenceRects = referenceManager.getReferenceRects();
		Mat referenceTemplate = Mat.zeros(flat.size(), CvType.CV_8UC1);// new SuperTemplate(referenceManager.getReference().getSuperFrame(), CvType.CV_8UC1, SuperFrameImg::getFrame);
		referenceRects.forEach(rect -> Imgproc.rectangle(referenceTemplate, rect, new Scalar(255), -1));
		images[6] = new Img(referenceTemplate, false).toJfxImage();

		Mat display = Mat.zeros(frame.size(), CvType.CV_8UC1);
		referenceManager.getResizedFieldsRects().forEach(rect -> Imgproc.rectangle(display, rect, new Scalar(255), -1));
		images[7] = new Img(display, false).toJfxImage();
		//
		// SuperTemplate layoutTemplate = new SuperTemplate(referenceTemplate, CvType.CV_8UC3, SuperFrameImg::getDisplay);
		// Layout layout = layoutTemplate.layout();
		// if (layout != null) {
		// layoutTemplate.drawLayout(layout);
		// images[9] = layoutTemplate.getDisplay().toJfxImage();
		// }

		return images;
	}

	public static class Label {
		private final Rect rect;
		private String label;

		public Label(Rect rect) {
			this.rect = rect;
		}

		public void ocr(Mat img, int confidence, int componentLevel, int dx, int dy) {
			double newTlx = rect.tl().x - dx >= 0 ? rect.tl().x - dx : 0;
			double newTly = rect.tl().y - dy >= 0 ? rect.tl().y - dy : 0;
			double newBrx = rect.br().x + dx <= img.width() ? rect.br().x + dx : img.width();
			double newBry = rect.br().y + dy <= img.height() ? rect.br().y + dy : img.height();
			label = Ocr.doWork(new Mat(img, new Rect(new Point(newTlx, newTly), new Point(newBrx, newBry))), confidence, componentLevel);
			System.out.println(label);
		}

		public void putOcr(Mat img) {
			String normalizedText = Normalizer.normalize(label, Normalizer.Form.NFD).replaceAll("[^\\p{ASCII}]", "");
			int[] baseLine = new int[1];
			Size size = Imgproc.getTextSize(normalizedText, Core.FONT_HERSHEY_PLAIN, 1, 1, baseLine);
			double scale = Math.min(rect.width / size.width, rect.height / size.height);

			if (scale < 0.3 && scale > 3)
				scale = 1;
			Imgproc.putText(img, normalizedText, new Point(rect.tl().x, rect.br().y), Core.FONT_HERSHEY_PLAIN, scale, new Scalar(0, 255, 0), 1);
		}

		public String getLabel() {
			return label;
		}

		public Rect getRect() {
			return rect;
		}
	}

	public static class Labels {
		private final List<Label> labels;

		public Labels(List<Rect> rects) {
			labels = rects.stream().map(Label::new).collect(Collectors.toList());
		}

		public Labels(Labels labels) {
			this.labels = new ArrayList<>(labels.getLabels());
		}

		public void putOcr(Mat img) {
			getLabels().forEach(field -> field.putOcr(img));
		}

		public void ocr(Mat img, int confidence, int componentLevel, int dx, int dy) {
			getLabels().forEach(field -> field.ocr(img, confidence, componentLevel, dx, dy));
		}

		public List<Label> getLabels() {
			return labels;
		}
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
