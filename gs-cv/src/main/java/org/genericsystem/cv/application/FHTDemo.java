package org.genericsystem.cv.application;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction;
import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.application.mesh.MeshManager;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.genericsystem.layout.Layout;
import org.opencv.core.Mat;
import org.opencv.core.Scalar;
import org.opencv.core.Size;

import javafx.application.Platform;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class FHTDemo extends AbstractApp {

	public static void main(String[] args) {
		launch(args);
	}

	static {
		NativeLibraryLoader.load();
	}

	private GSCapture gsCapture = new GSVideoCapture(0, GSVideoCapture.HD, GSVideoCapture.VGA);
	private Img frame = gsCapture.read();
	private ScheduledExecutorService timer = new BoundedScheduledThreadPoolExecutor();
	private Config config = new Config();
	private final ImageView[][] imageViews = new ImageView[][] { new ImageView[3], new ImageView[3], new ImageView[3] };
	private int frameCount = 0;
	private FHTManager fhtManager = new FHTManager(gsCapture.getResize());

	public FHTDemo() {
		addIntegerSliderProperty("hBlurSize", fhtManager.gethBlurSize(), 0, 200);
		addIntegerSliderProperty("vBlurSize", fhtManager.getvBlurSize(), 0, 200);
		addDoubleSliderProperty("hNeighbourPenality", fhtManager.gethNeighbourPenality(), -5000, 0);
		addDoubleSliderProperty("vNeighbourPenality", fhtManager.getvNeighbourPenality(), -5000, 0);
		addDoubleSliderProperty("hAnglePenality", fhtManager.gethAnglePenality(), -1, 0);
		addDoubleSliderProperty("vAnglePenality", fhtManager.getvAnglePenality(), -1, 0);
		addDoubleSliderProperty("vRecover", fhtManager.getvRecover(), 0, 1);
		addDoubleSliderProperty("hRecover", fhtManager.gethRecover(), 0, 1);
		addIntegerSliderProperty("vStripsNumber", fhtManager.getvStripsNumber(), 1, 32);
		addIntegerSliderProperty("hStripsNumber", fhtManager.gethStripsNumber(), 1, 32);
		addDoubleSliderProperty("vLocalThreshold", fhtManager.getvLocalThreshold(), 0, 1);
		addDoubleSliderProperty("hLocalThreshold", fhtManager.gethLocalThreshold(), 0, 1);
		addDoubleSliderProperty("vGlobalThreshold", fhtManager.getvGlobalThreshold(), 0, 0.2);
		addDoubleSliderProperty("hGlobalThreshold", fhtManager.gethGlobalThreshold(), 0, 0.2);
		addDoubleSliderProperty("vMaxConnectDistance", fhtManager.getvMaxConnectDistance(), 0, 0.1);
		addDoubleSliderProperty("hMaxConnectDistance", fhtManager.gethMaxConnectDistance(), 0, 0.1);
		addDoubleSliderProperty("interpolatorPow", fhtManager.getInterpolatorPow(), 0, 10);
		addDoubleSliderProperty("interpolatorMinDist", fhtManager.getInterpolatorMinDist(), 0, 10);
		addIntegerSliderProperty("halfGridWidth", fhtManager.getHalfGridWidth(), 1, 32);
		addIntegerSliderProperty("halfGridHeight", fhtManager.getHalfGridHeight(), 1, 32);
		addIntegerSliderProperty("optimizationsCount", fhtManager.getOptimisationsCount(), 0, 32);
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
		}, 3000, 30, TimeUnit.MILLISECONDS);
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

	private Image[] doWork() {
		System.out.println("do work");
		if (!config.stabilizedMode) {
			frame = gsCapture.read();
			frameCount++;
		}
		Image[] images = new Image[20];
		long ref = System.currentTimeMillis();
		images[0] = frame.toJfxImage();

		Img binarized = frame.adaptativeGaussianInvThreshold(7, 5);// .dilate(Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(2, 2)));// .canny(20, 80);
		images[1] = binarized.toJfxImage();
		if (frameCount < 30)
			return images;

		fhtManager.init(frame.getSrc(), binarized.getSrc());
		ref = trace("FHT init", ref);

		double minVerticalAccuracy = 180 * (Math.atan(1 / (fhtManager.getStripWidth().get() - 1))) / Math.PI;
		System.out.println(fhtManager.getvStripsNumber().get() + " verticals strips with width : " + fhtManager.getStripWidth().get() + " each step : " + fhtManager.getvStep().get() + " min accuracy : " + minVerticalAccuracy);

		double minHorizontalAccuracy = 180 * (Math.atan(1 / (fhtManager.getStripHeight().get() - 1))) / Math.PI;
		System.out.println(fhtManager.gethStripsNumber().get() + " horizontal strips with height : " + fhtManager.getStripHeight().get() + " each step : " + fhtManager.gethStep().get() + " min accuracy : " + minHorizontalAccuracy);

		List<Mat> vStrips = fhtManager.getVStrips();
		List<Mat> hStrips = fhtManager.getHStrips();
		ref = trace("Extract strips", ref);

		List<Mat> vHoughs = fhtManager.getvHoughs();
		List<Mat> hHoughs = fhtManager.gethHoughs();
		ref = trace("Compute FHT", ref);

		List<List<TrajectStep>> vHoughTrajs = fhtManager.getvHoughTrajs();
		List<List<TrajectStep>> hHoughTrajs = fhtManager.gethHoughTrajs();
		ref = trace("Compute trajects", ref);

		vHoughTrajs = fhtManager.getOptimizedvHoughTrajs();// StripTractor.optimize(vHoughs, 81, -0.05, -100, vHoughTrajs, fhtManager.getvStep().get());
		hHoughTrajs = fhtManager.getOptimizedhHoughTrajs();// StripTractor.optimize(hHoughs, 81, -0.05, -100, hHoughTrajs, fhtManager.gethStep().get());
		ref = trace("Optimize trajects", ref);

		List<List<OrientedPoint>[]> fhtHorizontals = fhtManager.getFhtHorizontals();// ProjectionLines.toHorizontalsOrientedPoints(vHoughTrajs, fhtManager.getvStep().get(), 0.5, 0.05);
		List<List<OrientedPoint>[]> fhtVerticals = fhtManager.getFhtVerticals();// ProjectionLines.toVerticalsOrientedPoints(hHoughTrajs, fhtManager.gethStep().get(), 0.5, 0.05);

		List<List<Segment>>[] horizontalSegments = fhtManager.getHorizontalSegments();// Segment.connect(fhtHorizontals, fhtManager.getvStep().get(), 0.05, false);
		List<List<Segment>>[] verticalSegments = fhtManager.getVerticalSegments();// Segment.connect(fhtVerticals, fhtManager.gethStep().get(), 0.05, true);

		Img frameDisplayFHT = new Img(frame.getSrc().clone(), false);
		Segment.displayHorizontalOps(horizontalSegments[0], frameDisplayFHT.getSrc(), fhtManager.getvStep().get(), fhtManager.gethStep().get(), new Scalar(0, 255, 0));
		Segment.displayHorizontalOps(horizontalSegments[1], frameDisplayFHT.getSrc(), fhtManager.getvStep().get(), fhtManager.gethStep().get(), new Scalar(0, 0, 255));
		Segment.displayVerticalOps(verticalSegments[0], frameDisplayFHT.getSrc(), fhtManager.getvStep().get(), fhtManager.gethStep().get(), new Scalar(255, 255, 0));
		Segment.displayVerticalOps(verticalSegments[1], frameDisplayFHT.getSrc(), fhtManager.getvStep().get(), fhtManager.gethStep().get(), new Scalar(255, 0, 255));

		images[2] = frameDisplayFHT.toJfxImage();
		ref = trace("Display lines", ref);

		List<PolynomialSplineFunction>[] horizontalSplines = fhtManager.gethSplines();// Segment.toSplines(horizontalSegments, false);
		List<PolynomialSplineFunction>[] verticalSplines = fhtManager.getvSplines();// Segment.toSplines(verticalSegments, true);
		Img splineDisplay = new Img(frame.getSrc().clone(), false);
		FHT.displayHSplines(horizontalSplines[0], splineDisplay.getSrc(), 0, 255, 0);
		FHT.displayHSplines(horizontalSplines[1], splineDisplay.getSrc(), 0, 0, 255);
		FHT.displayVSplines(verticalSplines[0], splineDisplay.getSrc(), 255, 255, 0);
		FHT.displayVSplines(verticalSplines[1], splineDisplay.getSrc(), 255, 0, 255);
		images[3] = splineDisplay.toJfxImage();
		ref = trace("Display splines", ref);

		SplineInterpolator superInterpolator = fhtManager.getSuperInterpolator();
		ref = trace("Prepare interpolator", ref);

		MeshManager meshManager = fhtManager.getMeshManager();
		images[4] = new Img(meshManager.drawOnCopy(new Scalar(0, 255, 0), new Scalar(0, 0, 255)), false).toJfxImage();
		ref = trace("Build and draw mesh", ref);

		images[5] = new Img(meshManager.draw3Dsurface(new Scalar(0, 255, 0), new Scalar(0, 0, 255)), false).toJfxImage();
		ref = trace("3D surface / svd", ref);

		Img dewarpFHT3D = fhtManager.getDewarp();
		images[6] = dewarpFHT3D.toJfxImage();
		ref = trace("Dewarp 3D", ref);

		Img binarized3D = dewarpFHT3D.adaptativeGaussianInvThreshold(7, 5);// .canny(20, 80);
		images[7] = binarized3D.toJfxImage();
		ref = trace("Binarize dewarp 3D", ref);

		Layout layout3D = binarized3D.buildLayout(new Size(2, 0.0), new Size(0.001, 0.001), 8);
		layout3D.draw(dewarpFHT3D, new Scalar(255, 0, 0), new Scalar(0, 0, 255), 0, -1);
		images[8] = dewarpFHT3D.toJfxImage();
		ref = trace("Layout 3D", ref);

		return images;

	}

	@Override
	protected void onS() {
		config.stabilizedMode = !config.stabilizedMode;
	}

	@Override
	protected void onSpace() {
		if (config.isOn) {
			timer.shutdown();
			// gsCapture.release();
		} else {
			timer = new BoundedScheduledThreadPoolExecutor();
			// gsCapture = new GSVideoCapture(0, f, GSVideoCapture.HD, GSVideoCapture.VGA);
			startTimer();
		}
		config.isOn = !config.isOn;
	}

	@Override
	public void stop() throws Exception {
		super.stop();
		timer.shutdown();
		timer.awaitTermination(5000, TimeUnit.MILLISECONDS);
		gsCapture.release();
	}

}
