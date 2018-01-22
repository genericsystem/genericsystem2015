package org.genericsystem.cv.application;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Calibrated.AngleCalibrated;
import org.genericsystem.cv.Lines;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.genericsystem.layout.Layout;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.videoio.VideoCapture;

import javafx.application.Platform;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class GraphicApp extends AbstractApp {

	private final double f = 6.053 / 0.009;
	private final VideoCapture capture = new VideoCapture(0);
	private SuperFrameImg superFrame = SuperFrameImg.create(capture, f);
	private ReferenceManager referenceManager = new ReferenceManager(superFrame.size());
	private boolean stabilizedMode = false;
	private boolean textsEnabledMode = false;
	private Deperspectiver deperspectiver;
	private ScheduledExecutorService timer = new BoundedScheduledThreadPoolExecutor(1, (r, executor) -> {
	}, 2);

	public static void main(String[] args) {
		launch(args);
	}

	static {
		NativeLibraryLoader.load();
	}

	public final static double displayWidth = 400d;

	@Override
	protected void fillGrid(GridPane mainGrid) {
		double displaySizeReduction = 1;

		ImageView[][] imageViews = new ImageView[][] { new ImageView[2], new ImageView[2], new ImageView[2] };
		for (int col = 0; col < imageViews.length; col++)
			for (int row = 0; row < imageViews[col].length; row++) {
				ImageView imageView = new ImageView();
				imageViews[col][row] = imageView;
				mainGrid.add(imageViews[col][row], col, row);
				imageView.setFitWidth(superFrame.width() / displaySizeReduction);
				imageView.setFitHeight(superFrame.height() / displaySizeReduction);
			}

		double[] pp = superFrame.getPrincipalPoint();
		deperspectiver = new Deperspectiver(f, pp);
		timer.scheduleAtFixedRate(() -> {
			try {
				Image[] images = doWork(pp);
				if (images != null)
					Platform.runLater(() -> {
						Iterator<Image> it = Arrays.asList(images).iterator();
						for (int row = 0; row < imageViews.length; row++)
							for (int col = 0; col < imageViews[row].length; col++)
								imageViews[row][col].setImage(it.next());
					});
			} catch (Throwable e) {
				e.printStackTrace();
			}
		}, 30, 30, TimeUnit.MILLISECONDS);
	}

	private Image[] doWork(double[] pp) {

		if (!stabilizedMode)
			superFrame = SuperFrameImg.create(capture, f);
		Image[] images = new Image[6];
		Lines lines = superFrame.detectLines();
		AngleCalibrated[] calibratedVps = deperspectiver.computeCalibratedVps(superFrame, textsEnabledMode, lines);
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
		images[2] = superDeperspectived.getDisplay().toJfxImage();

		ImgDescriptor newImgDescriptor = new ImgDescriptor(superDeperspectived);
		if (newImgDescriptor.getDescriptors().empty()) {
			System.out.println("Empty descriptors");
			return null;
		}
		referenceManager.submit(newImgDescriptor, detectedRects);
		List<Rect> referenceRects = referenceManager.getReferenceRects();
		SuperTemplate referenceTemplate = new SuperTemplate(referenceManager.getReference().getSuperFrame(), CvType.CV_8UC1, SuperFrameImg::getFrame);
		referenceTemplate.drawRects(referenceRects, new Scalar(255), -1);
		images[3] = referenceTemplate.getDisplay().toJfxImage();

		SuperTemplate layoutTemplate = new SuperTemplate(referenceTemplate, CvType.CV_8UC3, SuperFrameImg::getDisplay);
		Layout layout = layoutTemplate.layout();
		layoutTemplate.drawLayout(layout);
		images[4] = layoutTemplate.getDisplay().toJfxImage();

		return images;
	}

	@Override
	protected void onS() {
		System.out.println("s pressed");
	}

	@Override
	protected void onSpace() {
		stabilizedMode = !stabilizedMode;
	}

	@Override
	protected void onR() {
		timer.schedule(() -> referenceManager.clear(), 0, TimeUnit.MILLISECONDS);
	}

	@Override
	protected void onT() {
		textsEnabledMode = !textsEnabledMode;
	}

	@Override
	public void stop() throws Exception {
		super.stop();
		timer.shutdown();
		timer.awaitTermination(5000, TimeUnit.MILLISECONDS);
		capture.release();
	}
}
