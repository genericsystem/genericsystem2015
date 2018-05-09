package org.genericsystem.cv.application;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.application.GeneralInterpolator.OrientedPoint;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.genericsystem.layout.Layout;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

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

		double vRecover = 0.5;
		int vStripsNumber = 16;
		double stripWidth = (binarized.width() / (vStripsNumber * (1 - vRecover) + vRecover));
		double vStep = ((1 - vRecover) * stripWidth);
		System.out.println(vStripsNumber + " verticals strips with width : " + stripWidth + " each step : " + vStep);

		double hRecover = 0.5;
		int hStripsNumber = 9;
		double stripHeight = (binarized.height() / (hStripsNumber * (1 - hRecover) + hRecover));
		double hStep = ((1 - hRecover) * stripHeight);
		System.out.println(hStripsNumber + " horizontal strips with width : " + stripHeight + " each step : " + hStep);

		List<Mat> vStrips = RadonTransform.extractStrips(binarized.getSrc(), vStripsNumber, stripWidth, vStep);
		List<Mat> hStrips = RadonTransform.extractStrips(transposedBinarized.getSrc(), hStripsNumber, stripHeight, hStep);
		ref = trace("Extract strips", ref);

		List<Mat> vHoughs = vStrips.stream().map(strip -> RadonTransform.fastHoughTransform(strip)).collect(Collectors.toList());
		List<Mat> hHoughs = hStrips.stream().map(strip -> RadonTransform.fastHoughTransform(strip)).collect(Collectors.toList());

		vHoughs.stream().forEach(projectionMap -> Imgproc.morphologyEx(projectionMap, projectionMap, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(1, 2))));
		hHoughs.stream().forEach(projectionMap -> Imgproc.morphologyEx(projectionMap, projectionMap, Imgproc.MORPH_GRADIENT, Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, new Size(1, 2))));

		vHoughs.stream().forEach(projectionMap -> Core.pow(projectionMap, 2, projectionMap));
		hHoughs.stream().forEach(projectionMap -> Core.pow(projectionMap, 2, projectionMap));

		vHoughs.stream().forEach(projectionMap -> projectionMap.row(0).setTo(new Scalar(0)));
		hHoughs.stream().forEach(projectionMap -> projectionMap.row(projectionMap.rows() - 1).setTo(new Scalar(0)));

		double maxV = vHoughs.stream().mapToDouble(projectionMap -> Core.minMaxLoc(projectionMap).maxVal).max().getAsDouble();
		double maxH = hHoughs.stream().mapToDouble(projectionMap -> Core.minMaxLoc(projectionMap).maxVal).max().getAsDouble();

		vHoughs.stream().forEach(projectionMap -> Core.divide(projectionMap, new Scalar(maxV), projectionMap));
		hHoughs.stream().forEach(projectionMap -> Core.divide(projectionMap, new Scalar(maxH), projectionMap));
		// vHoughs.stream().forEach(projectionMap -> Core.normalize(projectionMap, projectionMap, 0, 1, Core.NORM_MINMAX));
		// hHoughs.stream().forEach(projectionMap -> Core.normalize(projectionMap, projectionMap, 0, 1, Core.NORM_MINMAX));
		ref = trace("Compute FHT", ref);

		List<TrajectStep[]> vHoughTrajs = vHoughs.stream().map(projectionMap -> RadonTransform.bestTrajectFHT(projectionMap, -0.000001)).collect(Collectors.toList());
		List<TrajectStep[]> hHoughTrajs = hHoughs.stream().map(projectionMap -> RadonTransform.bestTrajectFHT(projectionMap, -0.000001)).collect(Collectors.toList());
		ref = trace("Compute trajects", ref);

		for (TrajectStep[] houghVtraj : vHoughTrajs) {
			for (int y = 0; y < houghVtraj.length; y++)
				houghVtraj[y].theta = (int) Math.round(Math.atan((houghVtraj[y].theta - stripWidth + 1) / (stripWidth - 1)) / Math.PI * 180 + 45);

			for (int y = 0; y < houghVtraj.length; y++) {
				if (houghVtraj[y].magnitude == 0)
					for (int end = y + 1; end < houghVtraj.length; end++) {
						if (houghVtraj[end].magnitude != 0) {
							for (int current = y; current < end; current++)
								houghVtraj[current].theta = houghVtraj[y == 0 ? 0 : y - 1].theta + (houghVtraj[end].theta - houghVtraj[y == 0 ? 0 : y - 1].theta) * (current - y) / (end - y + 1);
							y = end;
							break;
						}
					}
			}
		}

		for (TrajectStep[] houghHtraj : hHoughTrajs) {
			for (int y = 0; y < houghHtraj.length; y++)
				houghHtraj[y].theta = (int) Math.round(Math.atan((houghHtraj[y].theta - stripHeight + 1) / (stripHeight - 1)) / Math.PI * 180 + 45);

			for (int y = 0; y < houghHtraj.length; y++) {
				if (houghHtraj[y].magnitude == 0)
					for (int end = y + 1; end < houghHtraj.length; end++) {
						if (houghHtraj[end].magnitude != 0) {
							for (int current = y; current < end; current++)
								houghHtraj[current].theta = houghHtraj[y == 0 ? 0 : y - 1].theta + (houghHtraj[end].theta - houghHtraj[y == 0 ? 0 : y - 1].theta) * (current - y) / (end - y + 1);
							y = end;
							break;
						}
					}
			}
		}

		ref = trace("Transform trajects", ref);
		//
		// List<Function<Double, Double>> approxVFHTFunctions = vHoughTrajs.stream().map(traj -> RadonTransform.approxTraject(traj)).collect(Collectors.toList());
		// List<Function<Double, Double>> approxHFHTFunctions = hHoughTrajs.stream().map(traj -> RadonTransform.approxTraject(traj)).collect(Collectors.toList());
		//
		// ref = trace("Compute approxs", ref);

		// List<OrientedPoint> fhtHorizontals = new ArrayList<>();
		// for (int vStrip = 0; vStrip < approxVFHTFunctions.size(); vStrip++)
		// fhtHorizontals.addAll(RadonTransform.toHorizontalOrientedPoints(approxVFHTFunctions.get(vStrip), (vStrip + 1) * vStep, binarized.height(), hStep));
		// List<OrientedPoint> fhtVerticals = new ArrayList<>();
		// for (int hStrip = 0; hStrip < approxHFHTFunctions.size(); hStrip++)
		// fhtVerticals.addAll(RadonTransform.toVerticalOrientedPoints(approxHFHTFunctions.get(hStrip), (hStrip + 1) * hStep, binarized.width(), vStep));

		List<OrientedPoint> fhtHorizontals = new ArrayList<>();
		for (int vStripIndex = 0; vStripIndex < vHoughTrajs.size(); vStripIndex++)
			fhtHorizontals.addAll(RadonTransform.toHorizontalOrientedPoints(vHoughTrajs.get(vStripIndex), (vStripIndex + 1) * vStep));
		List<OrientedPoint> fhtVerticals = new ArrayList<>();
		for (int hStrip = 0; hStrip < hHoughTrajs.size(); hStrip++)
			fhtVerticals.addAll(RadonTransform.toVerticalOrientedPoints(hHoughTrajs.get(hStrip), (hStrip + 1) * hStep));

		GeneralInterpolator interpolatorFHT = new GeneralInterpolator(fhtHorizontals, fhtVerticals, 3, 0.000001);

		ref = trace("Prepare interpolator", ref);

		Img frameDisplayFHT = new Img(superFrame.getFrame().getSrc().clone(), false);
		for (OrientedPoint op : fhtVerticals) {
			double angle = op.angle + Math.PI / 2;
			Imgproc.line(frameDisplayFHT.getSrc(), new Point(op.center.x - Math.cos(angle) * stripWidth / 2 * op.strenght, op.center.y - Math.sin(angle) * stripHeight / 2 * op.strenght),
					new Point(op.center.x + Math.cos(angle) * stripWidth / 2 * op.strenght, op.center.y + Math.sin(angle) * stripHeight / 2 * op.strenght), new Scalar(0, 0, 255), 1);
			angle = interpolatorFHT.interpolateVerticals(op.center.x, op.center.y) + Math.PI / 2;
			Imgproc.line(frameDisplayFHT.getSrc(), new Point(op.center.x - Math.cos(angle) * stripWidth / 2 * op.strenght, op.center.y - Math.sin(angle) * stripHeight / 2 * op.strenght),
					new Point(op.center.x + Math.cos(angle) * stripWidth / 2 * op.strenght, op.center.y + Math.sin(angle) * stripHeight / 2 * op.strenght), new Scalar(255, 0, 0), 1);
		}

		for (OrientedPoint op : fhtHorizontals) {
			Imgproc.line(frameDisplayFHT.getSrc(), new Point(op.center.x - Math.cos(op.angle) * stripWidth / 2 * op.strenght, op.center.y - Math.sin(op.angle) * stripHeight / 2 * op.strenght),
					new Point(op.center.x + Math.cos(op.angle) * stripWidth / 2 * op.strenght, op.center.y + Math.sin(op.angle) * stripHeight / 2 * op.strenght), new Scalar(0, 0, 255), 1);
			double angle = interpolatorFHT.interpolateHorizontals(op.center.x, op.center.y);
			Imgproc.line(frameDisplayFHT.getSrc(), new Point(op.center.x - Math.cos(angle) * stripWidth / 2 * op.strenght, op.center.y - Math.sin(angle) * stripHeight / 2 * op.strenght),
					new Point(op.center.x + Math.cos(angle) * stripWidth / 2 * op.strenght, op.center.y + Math.sin(angle) * stripHeight / 2 * op.strenght), new Scalar(255, 0, 0), 1);

		}
		images[2] = frameDisplayFHT.toJfxImage();
		ref = trace("Display lines", ref);

		MeshGrid2 meshGridFHT = new MeshGrid2(8, 4, interpolatorFHT, 40, 40, superFrame.getFrame().getSrc());
		ref = trace("Build mesh", ref);

		images[3] = new Img(meshGridFHT.drawOnCopy(new Scalar(0, 255, 0), new Scalar(0, 0, 255)), false).toJfxImage();
		ref = trace("Draw mesh", ref);

		Img dewarpFHT = new Img(meshGridFHT.dewarp());
		images[4] = dewarpFHT.toJfxImage();
		ref = trace("Dewarp", ref);

		Img dewarpedBinarized2 = dewarpFHT.adaptativeGaussianInvThreshold(7, 5);// .canny(20, 80);
		images[5] = dewarpedBinarized2.toJfxImage();
		ref = trace("Binarize dewarp", ref);

		Layout layout = dewarpedBinarized2.buildLayout(new Size(0.01, 0.01), 3);
		layout.draw(dewarpFHT, new Scalar(255, 0, 0), new Scalar(0, 0, 255), 2, 1);
		images[6] = dewarpFHT.toJfxImage();
		ref = trace("Layout", ref);

		images[7] = new Img(meshGridFHT.draw3Dsurface(new Scalar(0, 255, 0), new Scalar(0, 0, 255))).toJfxImage();
		ref = trace("3D surface / svd", ref);

		Img dewarpFHT3D = new Img(meshGridFHT.dewarp3D());
		images[8] = dewarpFHT3D.toJfxImage();
		ref = trace("Dewarp 3D", ref);

		Img binarized3D = dewarpFHT3D.adaptativeGaussianInvThreshold(7, 5);// .canny(20, 80);
		images[9] = binarized3D.toJfxImage();
		ref = trace("Binarize dewarp 3D", ref);

		Layout layout3D = binarized3D.buildLayout(new Size(0.01, 0.01), 3);
		layout3D.draw(dewarpFHT3D, new Scalar(255, 0, 0), new Scalar(0, 0, 255), 2, 1);
		images[10] = dewarpFHT3D.toJfxImage();
		ref = trace("Layout 3D", ref);

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
