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
	// private final GSCapture gsCapture = new GSVideoCapture("http://192.168.1.13:8080/video", f, GSVideoCapture.HD, GSVideoCapture.VGA);
	// private final GSCapture gsCapture = new GSPhotoCapture("resources/image.pdf", f);
	private SuperFrameImg superFrame;
	private ReferenceManager referenceManager;
	private Config config = new Config();
	private Deperspectiver deperspectiver;
	private ScheduledExecutorService timer = new BoundedScheduledThreadPoolExecutor();
	ImageView[][] imageViews = new ImageView[][] { new ImageView[3], new ImageView[3], new ImageView[3], new ImageView[3] };

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
		}, 2000, 30, TimeUnit.MILLISECONDS);
	}

	private Image[] doWork() {

		System.out.println("do work");
		if (!config.stabilizedMode)
			superFrame = gsCapture.read();
		Image[] images = new Image[10];
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
		images[1] = superFrame.getDiffFrame().toJfxImage();

		List<Rect> detectedRects = superDeperspectived.detectRects();
		superDeperspectived.drawRects(detectedRects, new Scalar(0, 255, 0), -1);

		SuperTemplate surfaceTemplate = new SuperTemplate(new SuperFrameImg(superDeperspectived.getDisplay().bgr2Gray().getSrc(), superFrame.getPp(), f), CvType.CV_8UC1, SuperFrameImg::getDisplay);
		Layout surfaceLayout = surfaceTemplate.layout();
		double surface = surfaceLayout.normalizedArea();
		superDeperspectived.putText(String.valueOf(surface));

		images[2] = superDeperspectived.getFrame().toJfxImage();

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

		List<SuperContour> filteredSuperContour = new ArrayList<>(
				TextOrientationLinesDetector.selectRandomObjects(superReferenceTemplate5.detectSuperContours(20).stream().filter(sc -> Math.abs(sc.angle) < Math.PI / 4 && sc.dx > 2 * sc.dy).collect(Collectors.toList()), 200));

		DirectionalFilter df = new DirectionalFilter();
		int nBin = 64;
		Mat gray = superReferenceTemplate5.getFrame().bgr2Gray().getSrc();
		Mat gx = df.gx(gray);
		Core.subtract(Mat.zeros(gx.size(), gx.type()), gx, gx);
		Mat gy = df.gy(gray);
		Mat mag = new Mat();
		Mat ori = new Mat();
		Core.cartToPolar(gx, gy, mag, ori);

		int[][] bin = df.bin(ori, nBin);
		List<Span> spans = superReferenceTemplate5.assembleContours(filteredSuperContour, c -> true, 100, 30, 70);
		// filteredSuperContour = spans.stream().flatMap(span -> span.getContours().stream()).collect(Collectors.toList());
		double regionSize = 100;
		// filteredSuperContour = filteredSuperContour.stream()
		// .filter(sc -> sc.center.x - regionSize > 0 && sc.center.x + regionSize / 2 < superReferenceTemplate5.getFrame().width() && sc.center.y - regionSize / 2 > 0 && sc.center.y + regionSize / 2 < superReferenceTemplate5.getFrame().height())
		// .collect(Collectors.toList());

		filteredSuperContour.forEach(sc -> sc.computeHisto(mag, bin, nBin, df, 100));

		// Mat image = superReferenceTemplate5.getDisplay().getSrc();

		SuperContourInterpolator interpolator = new SuperContourInterpolator(filteredSuperContour, 2);
		MeshGrid meshGrid = new MeshGrid(16, 9, interpolator, 20, 20, superReferenceTemplate5.getFrame().getSrc());
		meshGrid.buildGrid();

		Mat image = meshGrid.drawOnCopy(new Scalar(0, 255, 0), new Scalar(0, 0, 255));
		Mat internal = new Mat(image, new Rect(new Point(20, 20), new Point(image.width() - 20, image.height() - 20)));
		filteredSuperContour.stream().forEach(c -> Imgproc.line(internal, c.top, c.bottom, new Scalar(255, 255, 255), 1));
		filteredSuperContour.stream().forEach(c -> Imgproc.line(internal, c.vBottom, c.vTop, new Scalar(0, 0, 255), 2));

		images[4] = new Img(internal).toJfxImage();

		images[5] = new Img(meshGrid.dewarp(), false).toJfxImage();

		SuperTemplate superReferenceTemplate2 = new SuperTemplate(superReferenceTemplate5, CvType.CV_8UC3, SuperFrameImg::getFrame);
		// List<Span> spans = superReferenceTemplate2.assembleContours(filteredSuperContour, c -> true, 100, 30, 70);
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

		images[6] = superReferenceTemplate2.getDisplay().toJfxImage();

		ImgDescriptor newImgDescriptor = new ImgDescriptor(superDeperspectived, surface);
		if (newImgDescriptor.getDescriptors().empty()) {
			System.out.println("Empty descriptors");
			return null;
		}
		referenceManager.submit(newImgDescriptor, detectedRects);
		List<Rect> referenceRects = referenceManager.getReferenceRects();
		SuperTemplate referenceTemplate = new SuperTemplate(referenceManager.getReference().getSuperFrame(), CvType.CV_8UC1, SuperFrameImg::getFrame);
		referenceTemplate.drawRects(referenceRects, new Scalar(255), -1);
		images[7] = referenceTemplate.getDisplay().toJfxImage();

		SuperTemplate superReferenceTemplate3 = new SuperTemplate(superFrame, CvType.CV_8UC1, SuperFrameImg::getFrame);
		superReferenceTemplate3.drawRects(referenceManager.getResizedFieldsRects(), new Scalar(255), -1);
		images[8] = superReferenceTemplate3.getDisplay().toJfxImage();

		SuperTemplate layoutTemplate = new SuperTemplate(referenceTemplate, CvType.CV_8UC3, SuperFrameImg::getDisplay);
		Layout layout = layoutTemplate.layout();
		if (layout != null) {
			layoutTemplate.drawLayout(layout);
			images[9] = layoutTemplate.getDisplay().toJfxImage();
		}

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
