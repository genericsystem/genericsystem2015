package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.application.GeneralInterpolator.OrientedPoint;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

import javafx.application.Platform;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class RadonTransformDemo3 extends AbstractApp {

	public static void main(String[] args) {
		launch(args);
	}

	static {
		NativeLibraryLoader.load();
	}

	private final double f = 6.053 / 0.009;

	private GSCapture gsCapture = new GSVideoCapture(0, f, GSVideoCapture.HD, GSVideoCapture.VGA);
	private SuperFrameImg superFrame = gsCapture.read();
	private ScheduledExecutorService timer = new BoundedScheduledThreadPoolExecutor();
	private Config config = new Config();
	private final ImageView[][] imageViews = new ImageView[][] { new ImageView[3], new ImageView[3], new ImageView[3], new ImageView[3] };
	private int frameCount = 0;

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

	private Image[] doWork() {
		System.out.println("do work");
		if (!config.stabilizedMode) {
			superFrame = gsCapture.read();
			frameCount++;
		}
		Image[] images = new Image[20];

		long ref = System.currentTimeMillis();

		images[0] = superFrame.getFrame().toJfxImage();

		Img binarized = superFrame.getFrame().adaptativeGaussianInvThreshold(7, 5);// .canny(20, 80);

		// Mat frameClone = superFrame.getFrame().getSrc().clone();
		// DirectionalEnhancer.drawFilteredLines(binarized.getSrc(), DirectionalEnhancer.getLines(frameClone));

		images[1] = binarized.toJfxImage();

		if (frameCount < 30)
			return images;
		Img transposedBinarized = binarized.transpose();
		ref = trace("Binarization", ref);

		int stripWidth = 62;
		List<Mat> vStrips = RadonTransform.extractStrips(binarized.getSrc(), stripWidth);
		int stripHeight = 82;
		List<Mat> hStrips = RadonTransform.extractStrips(transposedBinarized.getSrc(), stripHeight);
		ref = trace("Extract strips", ref);

		List<Mat> vHoughs = vStrips.stream().map(strip -> RadonTransform.fastHoughTransform(strip)).collect(Collectors.toList());
		List<Mat> hHoughs = hStrips.stream().map(strip -> RadonTransform.fastHoughTransform(strip)).collect(Collectors.toList());
		vHoughs.stream().forEach(projectionMap -> Imgproc.resize(projectionMap, projectionMap, new Size(91, projectionMap.height()), 0, 0, Imgproc.INTER_LINEAR));
		hHoughs.stream().forEach(projectionMap -> Imgproc.resize(projectionMap, projectionMap, new Size(91, projectionMap.height()), 0, 0, Imgproc.INTER_LINEAR));
		vHoughs.stream().forEach(projectionMap -> Imgproc.morphologyEx(projectionMap, projectionMap, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(1, 2))));
		hHoughs.stream().forEach(projectionMap -> Imgproc.morphologyEx(projectionMap, projectionMap, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(1, 2))));
		vHoughs.stream().forEach(projectionMap -> Core.normalize(projectionMap, projectionMap, 0, 255, Core.NORM_MINMAX));
		hHoughs.stream().forEach(projectionMap -> Core.normalize(projectionMap, projectionMap, 0, 255, Core.NORM_MINMAX));
		ref = trace("Compute FHT", ref);

		List<TrajectStep[]> vHoughTrajs = vHoughs.stream().map(projectionMap -> RadonTransform.bestTraject(projectionMap, -40, 2)).collect(Collectors.toList());
		List<TrajectStep[]> hHoughTrajs = hHoughs.stream().map(projectionMap -> RadonTransform.bestTraject(projectionMap, -20, 2)).collect(Collectors.toList());
		ref = trace("Compute trajects", ref);

		for (TrajectStep[] houghVtraj : vHoughTrajs)
			for (int y = 0; y < houghVtraj.length; y++)
				houghVtraj[y].theta = (int) Math.round(Math.atan((double) (houghVtraj[y].theta - 45) / (45)) / Math.PI * 180 + 45);

		for (TrajectStep[] houghHtraj : hHoughTrajs)
			for (int y = 0; y < houghHtraj.length; y++)
				houghHtraj[y].theta = (int) Math.round(Math.atan((double) (houghHtraj[y].theta - 45) / (45)) / Math.PI * 180 + 45);

		ref = trace("Transform trajects", ref);

		List<Function<Double, Double>> approxVFHTFunctions = vHoughTrajs.stream().map(traj -> RadonTransform.approxTraject(traj)).collect(Collectors.toList());
		List<Function<Double, Double>> approxHFHTFunctions = hHoughTrajs.stream().map(traj -> RadonTransform.approxTraject(traj)).collect(Collectors.toList());

		ref = trace("Compute approxs", ref);

		int hStep = stripHeight / 2;
		int vStep = stripWidth / 2;

		List<OrientedPoint> fhtHorizontals = new ArrayList<>();
		for (int vStrip = 0; vStrip < approxVFHTFunctions.size(); vStrip++)
			fhtHorizontals.addAll(RadonTransform.toHorizontalOrientedPoints(approxVFHTFunctions.get(vStrip), (vStrip + 1) * vStep, binarized.height(), hStep));
		List<OrientedPoint> fhtVerticals = new ArrayList<>();
		for (int hStrip = 0; hStrip < approxHFHTFunctions.size(); hStrip++)
			fhtVerticals.addAll(RadonTransform.toVerticalOrientedPoints(approxHFHTFunctions.get(hStrip), (hStrip + 1) * hStep, binarized.width(), vStep));

		GeneralInterpolator interpolatorFHT = new GeneralInterpolator(fhtHorizontals, fhtVerticals, 3, 10);

		ref = trace("Prepare interpolator", ref);

		Img frameDisplayFHT = new Img(superFrame.getFrame().getSrc().clone(), false);
		for (OrientedPoint op : fhtVerticals) {
			Imgproc.line(frameDisplayFHT.getSrc(), new Point(op.center.x - Math.cos(op.angle) * stripWidth / 6, op.center.y - Math.sin(op.angle) * stripHeight / 6),
					new Point(op.center.x + Math.cos(op.angle) * stripWidth / 6, op.center.y + Math.sin(op.angle) * stripHeight / 6), new Scalar(0, 0, 255), 2);
			double angle = interpolatorFHT.interpolateVerticals(op.center.x, op.center.y);
			Imgproc.line(frameDisplayFHT.getSrc(), new Point(op.center.x - Math.cos(angle) * stripWidth / 6, op.center.y - Math.sin(angle) * stripHeight / 6),
					new Point(op.center.x + Math.cos(angle) * stripWidth / 6, op.center.y + Math.sin(angle) * stripHeight / 6), new Scalar(255, 0, 0), 2);
		}

		for (OrientedPoint op : fhtHorizontals) {
			Imgproc.line(frameDisplayFHT.getSrc(), new Point(op.center.x - Math.cos(op.angle) * stripWidth / 6, op.center.y - Math.sin(op.angle) * stripHeight / 6),
					new Point(op.center.x + Math.cos(op.angle) * stripWidth / 6, op.center.y + Math.sin(op.angle) * stripHeight / 6), new Scalar(0, 0, 255), 2);
			double angle = interpolatorFHT.interpolateHorizontals(op.center.x, op.center.y);
			Imgproc.line(frameDisplayFHT.getSrc(), new Point(op.center.x - Math.cos(angle) * stripWidth / 6, op.center.y - Math.sin(angle) * stripHeight / 6),
					new Point(op.center.x + Math.cos(angle) * stripWidth / 6, op.center.y + Math.sin(angle) * stripHeight / 6), new Scalar(255, 0, 0), 2);

		}
		images[2] = frameDisplayFHT.toJfxImage();
		ref = trace("Display lines", ref);

		MeshGrid meshGridFHT = new MeshGrid(new Size(16, 9), interpolatorFHT, 20, 20, superFrame.getFrame().getSrc());
		meshGridFHT.build();
		ref = trace("Build mesh", ref);

		images[3] = new Img(meshGridFHT.drawOnCopy(new Scalar(0, 255, 0)), false).toJfxImage();
		ref = trace("Draw mesh", ref);

		Img dewarpFHT = new Img(meshGridFHT.dewarp());
		images[4] = dewarpFHT.toJfxImage();
		ref = trace("Dewarp", ref);

		Img binarized2 = dewarpFHT.adaptativeGaussianInvThreshold(7, 5);// .canny(20, 80);
		images[5] = binarized2.toJfxImage();
		ref = trace("Binarize dewarp", ref);
		// ---------------------------------------------------------------------------

		//
		// Img transposedBinarized2 = binarized2.transpose();
		// ref = trace("Binarization2", ref);
		//
		// List<Mat> vStrips2 = RadonTransform.extractStrips(binarized2.getSrc(), stripWidth);
		// List<Mat> hStrips2 = RadonTransform.extractStrips(transposedBinarized2.getSrc(), stripHeight);
		// ref = trace("Extract strips2", ref);
		//
		// List<Mat> vHoughs2 = vStrips2.stream().map(strip -> RadonTransform.fastHoughTransform(strip)).collect(Collectors.toList());
		// List<Mat> hHoughs2 = hStrips2.stream().map(strip -> RadonTransform.fastHoughTransform(strip)).collect(Collectors.toList());
		// // vHoughs2.stream().forEach(projectionMap -> Imgproc.resize(projectionMap, projectionMap, new Size(91, projectionMap.height()), 0, 0, Imgproc.INTER_LINEAR));
		// // hHoughs2.stream().forEach(projectionMap -> Imgproc.resize(projectionMap, projectionMap, new Size(91, projectionMap.height()), 0, 0, Imgproc.INTER_LINEAR));
		// vHoughs2.stream().forEach(projectionMap -> Imgproc.morphologyEx(projectionMap, projectionMap, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(1, 2))));
		// hHoughs2.stream().forEach(projectionMap -> Imgproc.morphologyEx(projectionMap, projectionMap, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(1, 2))));
		// vHoughs2.stream().forEach(projectionMap -> Core.normalize(projectionMap, projectionMap, 0, 255, Core.NORM_MINMAX));
		// hHoughs2.stream().forEach(projectionMap -> Core.normalize(projectionMap, projectionMap, 0, 255, Core.NORM_MINMAX));
		// ref = trace("Compute FHT2", ref);
		//
		// List<TrajectStep[]> vHoughTrajs2 = vHoughs2.stream().map(projectionMap -> RadonTransform.bestTraject(projectionMap, -1000, 2)).collect(Collectors.toList());
		// List<TrajectStep[]> hHoughTrajs2 = hHoughs2.stream().map(projectionMap -> RadonTransform.bestTraject(projectionMap, -1000, 2)).collect(Collectors.toList());
		// ref = trace("Compute trajects2", ref);
		//
		// for (TrajectStep[] houghVtraj : vHoughTrajs2)
		// for (int y = 0; y < houghVtraj.length; y++)
		// houghVtraj[y].theta = (int) Math.round(Math.atan((double) (houghVtraj[y].theta - stripWidth + 1) / (stripWidth - 1)) / Math.PI * 180 + 45);
		//
		// for (TrajectStep[] houghHtraj : hHoughTrajs2)
		// for (int y = 0; y < houghHtraj.length; y++)
		// houghHtraj[y].theta = (int) Math.round(Math.atan((double) (houghHtraj[y].theta - stripHeight + 1) / (stripHeight - 1)) / Math.PI * 180 + 45);
		//
		// ref = trace("Transform trajects2", ref);
		//
		// List<Function<Double, Double>> approxVFHTFunctions2 = vHoughTrajs2.stream().map(traj -> RadonTransform.approxTraject(traj)).collect(Collectors.toList());
		// List<Function<Double, Double>> approxHFHTFunctions2 = hHoughTrajs2.stream().map(traj -> RadonTransform.approxTraject(traj)).collect(Collectors.toList());
		//
		// ref = trace("Compute approxs2", ref);
		//
		// List<OrientedPoint> fhtHorizontals2 = new ArrayList<>();
		// for (int vStrip = 0; vStrip < approxVFHTFunctions2.size(); vStrip++)
		// fhtHorizontals2.addAll(RadonTransform.toHorizontalOrientedPoints(approxVFHTFunctions2.get(vStrip), (vStrip + 1) * vStep, binarized2.height(), hStep));
		// List<OrientedPoint> fhtVerticals2 = new ArrayList<>();
		// for (int hStrip = 0; hStrip < approxHFHTFunctions2.size(); hStrip++)
		// fhtVerticals2.addAll(RadonTransform.toVerticalOrientedPoints(approxHFHTFunctions2.get(hStrip), (hStrip + 1) * hStep, binarized2.width(), vStep));
		//
		// GeneralInterpolator interpolatorFHT2 = new GeneralInterpolator(fhtHorizontals2, fhtVerticals2, 3, 10);
		//
		// ref = trace("Prepare interpolator2", ref);
		//
		// MeshGrid meshGridFHT2 = new MeshGrid(new Size(16, 9), interpolatorFHT2, 20, 20, dewarpFHT.getSrc());
		// meshGridFHT2.build();
		// ref = trace("Build mesh2", ref);
		//
		// images[6] = new Img(meshGridFHT2.drawOnCopy(new Scalar(0, 255, 0)), false).toJfxImage();
		// ref = trace("Draw mesh2", ref);
		//
		// Img dewarpFHT2 = new Img(meshGridFHT2.dewarp());
		// images[7] = dewarpFHT2.toJfxImage();
		// ref = trace("Dewarp2", ref);

		return images;

	}

	private long trace(String message, long ref) {
		long last = System.currentTimeMillis();
		System.out.println(message + " : " + (last - ref));
		return last;
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
