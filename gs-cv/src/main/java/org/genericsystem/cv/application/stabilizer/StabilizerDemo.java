package org.genericsystem.cv.application.stabilizer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.application.BoundedScheduledThreadPoolExecutor;
import org.genericsystem.cv.application.Config;
import org.genericsystem.cv.application.GSCapture;
import org.genericsystem.cv.application.GSVideoCapture;
import org.genericsystem.cv.application.fht.FHTManager;
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
		Mat binarized = new Img(stabilized, false).adaptativeGaussianInvThreshold(7, 5).getSrc();
		if (!fhtManager.isInitialized() || frameCount % 30 == 0)
			fhtManager.init(binarized);
		images[1] = new Img(fhtManager.getMeshManager().drawReverse(stabilized, new Scalar(255, 0, 0), new Scalar(0, 255, 0)), false).toJfxImage();

		// Mesh reverseMesh = fhtManager.getMeshManager().getReverseMesh();
		// Mat result = reverseMesh.dewarp(stabilized, stabilized.size());
		images[2] = new Img(fhtManager.getMeshManager().dewarpReverse(stabilized), false).toJfxImage();
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
