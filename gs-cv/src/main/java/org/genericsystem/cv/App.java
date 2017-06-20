package org.genericsystem.cv;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfByte;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;
import org.opencv.utils.Converters;

import javafx.application.Application;
import javafx.event.EventHandler;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextArea;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;

public class App extends Application {

	private final String sourceDirectoryPath = "pdf";
	private final String targetDirectoryPath = "png";
	private final String adjustedDirectoryPath = "ajusted";
	private final double displayWidth = 200d;
	private final double canyThreshold1 = 12;
	private final double canyThreshold2 = 90;
	private final double ratio = 2;
	private final boolean landscape = true;
	private final double[] zone = new double[] { 3.4d / 10.6d, 8d / 10.6d, 1.4d / 7.4d, 2d / 7.4d };
	// private final double resizeFactor = 4d / 3;

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	public void start(Stage stage) throws Exception {

		// SourceFilesConverter.doWork(sourceDirectoryPath, targetDirectoryPath);
		GridPane gridPane = new GridPane();

		int row = 0;
		for (Mat img : Tools.getClassMats(targetDirectoryPath)) {
			int column = 0;

			// Mat hsv = new Mat();
			// Imgproc.cvtColor(srcMat, hsv, Imgproc.COLOR_BGR2HSV);
			// Mat mask = new Mat();
			// Core.inRange(hsv, new Scalar(0, 0, 0), new Scalar(150, 255, 255), mask);
			// Mat result = new Mat();
			// Core.bitwise_and(hsv, hsv, result, mask);
			// Imgproc.cvtColor(result, srcMat, Imgproc.COLOR_HSV2BGR);
			gridPane.add(getImageViewFromMat(img), column++, row);

			Mat grayed = gray(img);
			gridPane.add(getImageViewFromMat(grayed), column++, row);

			Mat canny = canny(grayed, canyThreshold1);
			gridPane.add(getImageViewFromMat(canny), column++, row);

			Mat blured = canny.clone();// gaussianBlur(canny);
			Imgproc.dilate(blured, blured, Imgproc.getStructuringElement(Imgproc.MORPH_CROSS, new Size(5, 5)));
			gridPane.add(getImageViewFromMat(blured), column++, row);

			MatOfPoint2f contour = getContour(blured);

			Mat contoured = drawContour(colorize(blured), contour);
			gridPane.add(getImageViewFromMat(contoured), column++, row);

			Mat adjusted = transform(img, contour);
			gridPane.add(getImageViewFromMat(adjusted), column++, row);

			String hocrString = Ocr.doWork(adjusted);
			gridPane.add(new TextArea(hocrString), column++, row);

			Mat adjustedGrayed = gray(adjusted);
			// Imgproc.threshold(adjustedGrayed, adjustedGrayed, 220, 255, Imgproc.THRESH_TRUNC);
			gridPane.add(getImageViewFromMat(adjustedGrayed), column++, row);

			Mat adjustedCanny = canny(adjustedGrayed, canyThreshold2);
			Imgproc.dilate(adjustedCanny, adjustedCanny, Imgproc.getStructuringElement(Imgproc.MORPH_CROSS, new Size(12, 12)));
			gridPane.add(getImageViewFromMat(adjustedCanny), column++, row);

			Mat adjustedBlured = gaussianBlur(adjustedCanny);
			gridPane.add(getImageViewFromMat(adjustedCanny), column++, row);

			List<MatOfPoint> contours = new ArrayList<>();
			Imgproc.findContours(adjustedBlured, contours, new Mat(), Imgproc.RETR_LIST, Imgproc.CHAIN_APPROX_SIMPLE);
			Mat coutoured = adjusted.clone();
			for (int i = 0; i < contours.size(); i++) {
				if (contours.get(i).size().area() > 160) {
					MatOfPoint2f contour2f = new MatOfPoint2f(contours.get(i).toArray());
					MatOfPoint2f approxCurve = new MatOfPoint2f();
					Imgproc.approxPolyDP(contour2f, approxCurve, Imgproc.arcLength(contour2f, true) * 0.02, true);
					Rect rect = Imgproc.boundingRect(new MatOfPoint(approxCurve.toArray()));
					if (rect.height < 80 && rect.width > 120)
						Imgproc.rectangle(coutoured, rect.tl(), rect.br(), new Scalar(0, 255, 0), 5);
				}
			}
			gridPane.add(getImageViewFromMat(coutoured), column++, row);

			Point pt1 = new Point(zone[0] * adjusted.width(), zone[2] * adjusted.height());
			Point pt2 = new Point(zone[1] * adjusted.width(), zone[3] * adjusted.height());
			Rect rect = new Rect(pt1, pt2);

			Mat templated = adjusted.clone();
			Imgproc.rectangle(templated, rect.br(), rect.tl(), new Scalar(0, 255, 0), 20);
			gridPane.add(getImageViewFromMat(templated), column++, row);

			Mat ocrZone = new Mat(adjusted, rect);
			gridPane.add(getImageViewFromMat(ocrZone), column++, row);

			String zoneOcr = Ocr.doWork(ocrZone);
			gridPane.add(new TextArea(zoneOcr), column++, row);

			row++;
		}

		Scene scene = new Scene(new Group());
		stage.setTitle("Generic System OCR");
		ScrollPane scrollPane = new ScrollPane(gridPane);
		scrollPane.setFitToHeight(true);
		VBox root = new VBox(scrollPane);
		scene.setRoot(root);
		stage.setOnCloseRequest(new EventHandler<WindowEvent>() {
			@Override
			public void handle(WindowEvent event) {
				try {
					stop();
				} catch (Exception e) {
					throw new RuntimeException(e);
				}
			}
		});
		stage.setScene(scene);
		stage.show();
	}

	private Mat transform(Mat src, MatOfPoint2f contour2f) {
		Mat target = new Mat();
		List<Point> list = Arrays.asList(contour2f.toArray());
		double width = Math.max(Math.sqrt(Math.pow(list.get(0).x - list.get(1).x, 2) + Math.pow(list.get(0).y - list.get(1).y, 2)), Math.sqrt(Math.pow(list.get(2).x - list.get(3).x, 2) + Math.pow(list.get(2).y - list.get(3).y, 2)));
		double height = Math.max(Math.sqrt(Math.pow(list.get(1).x - list.get(2).x, 2) + Math.pow(list.get(1).y - list.get(2).y, 2)), Math.sqrt(Math.pow(list.get(3).x - list.get(0).x, 2) + Math.pow(list.get(3).y - list.get(0).y, 2)));
		boolean toReverse = landscape && width < height;
		if (toReverse) {
			System.out.println("inversion width height");
			double tmp = width;
			width = height;
			height = tmp;
		}
		List<Point> targets = new LinkedList<>(Arrays.asList(new Point(width, 0), new Point(0, 0), new Point(0, height), new Point(width, height)));
		if (toReverse) {
			Point first = targets.get(0);
			targets.remove(0);
			targets.add(first);
		}
		Imgproc.warpPerspective(src, target, Imgproc.getPerspectiveTransform(contour2f, Converters.vector_Point2f_to_Mat(targets)), new Size(width, height), Imgproc.INTER_CUBIC);
		return target;
	}

	private ImageView getImageViewFromMat(Mat src) {
		Mat target = new Mat();
		Imgproc.resize(src, target, new Size(displayWidth, Math.floor((displayWidth / src.width()) * src.height())));
		MatOfByte buffer = new MatOfByte();
		Imgcodecs.imencode(".png", target, buffer);
		ImageView imageView = new ImageView(new Image(new ByteArrayInputStream(buffer.toArray())));
		imageView.setPreserveRatio(true);
		imageView.setFitWidth(displayWidth);
		return imageView;
	}

	// private Mat resizeDisplay(Mat src) {
	// Mat target = new Mat();
	// Imgproc.resize(src, target, new Size(displayWidth, Math.floor((displayWidth / src.width()) * src.height())));
	// return target;
	// }

	static File createFileFromMat(Mat mat, String fileName, String directoryPath) {
		File destinationDirectory = new File(directoryPath);
		if (!destinationDirectory.exists())
			destinationDirectory.mkdir();
		File dest = new File(destinationDirectory, fileName);
		MatOfByte bytesBuffer = new MatOfByte();
		Imgcodecs.imencode(".png", mat, bytesBuffer);
		try (InputStream sourceFile = new ByteArrayInputStream(bytesBuffer.toArray()); OutputStream destinationFile = new FileOutputStream(dest)) {
			byte buffer[] = new byte[512 * 1024];
			int nbLecture;
			while ((nbLecture = sourceFile.read(buffer)) != -1) {
				destinationFile.write(buffer, 0, nbLecture);
			}
			destinationFile.flush();
			destinationFile.close();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		return dest;
	}

	private Mat read(File file) {
		return Imgcodecs.imread(file.getPath());
	}

	// @Deprecated
	// private Mat resize(Mat src) {
	// Mat target = new Mat();
	// // Imgproc.resize(src, target, new Size(src.width() * resizeFactor, src.height() * resizeFactor));//bad results
	// target = src.clone();
	// return target;
	// }

	private Mat gray(Mat src) {
		Mat target = new Mat();
		Imgproc.cvtColor(src, target, Imgproc.COLOR_BGR2GRAY);
		return target;
	}

	// Imgproc.threshold(target, target, 0, 255, Imgproc.THRESH_BINARY + Imgproc.THRESH_OTSU);
	// Imgproc.equalizeHist(target, target);
	// target.convertTo(target, -1, 1, -50); // decrease the brightness by 20 for each pixel
	// target.convertTo(target, -1, 2, 0); // increase the contrast (double)

	private Mat canny(Mat src, double thresHold) {
		Mat target = new Mat();
		Imgproc.Canny(src, target, thresHold, thresHold * ratio, 3, true);
		return target;
	}

	private Mat gaussianBlur(Mat src) {
		Mat target = new Mat();
		Imgproc.GaussianBlur(src, target, new Size(5, 5), 0);
		return target;
	}

	private MatOfPoint2f getContour(Mat src) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(src, contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		Collections.sort(contours, (c1, c2) -> Double.compare(Imgproc.contourArea(c2), Imgproc.contourArea(c1)));
		MatOfPoint2f approxCurve = new MatOfPoint2f();
		for (MatOfPoint contour : contours) {
			MatOfPoint2f contour2F = new MatOfPoint2f(contour.toArray());
			Imgproc.approxPolyDP(contour2F, approxCurve, Imgproc.arcLength(contour2F, true) * 0.02, true);
			if (approxCurve.total() == 4)
				break;
		}
		return approxCurve;
	}

	private Mat colorize(Mat src) {
		Mat target = new Mat();
		Imgproc.cvtColor(src, target, Imgproc.COLOR_GRAY2BGR);
		return target;

	}

	private Mat drawContour(Mat src, MatOfPoint2f contour2f) {
		Mat target = src.clone();
		MatOfPoint contour = new MatOfPoint();
		contour2f.convertTo(contour, CvType.CV_32S);
		Imgproc.drawContours(target, Arrays.asList(contour), -1, new Scalar(0, 255, 0), 20);
		return target;
	}

}
