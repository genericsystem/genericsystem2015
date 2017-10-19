package org.genericsystem.cv.utils;

import java.io.File;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;

import org.apache.commons.io.FilenameUtils;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.Ransac.Model;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.RotatedRect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgcodecs.Imgcodecs;
import org.opencv.imgproc.Imgproc;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class contains static methods that can be used to deskew an {@link Img}.
 * 
 * @author Nicolas Feybesse
 * @author Pierrik Lassalas
 */
public class Deskewer {

	static {
		NativeLibraryLoader.load();
	}

	public enum METHOD {
		ROTADED_RECTANGLES,
		HOUGH_LINES
	}

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private static final double ransacError = 0.1;
	private static final double closedImgSizeFactor = 2E-6;
	private static final double minAreaFactor = 3E-5;

	// Only for testing purposes
	public static void main(String[] args) {
		final String filename = System.getenv("HOME") + "/genericsystem/gs-ir-files/converted-png/image-4.png";
		Path imgPath = Paths.get(filename);
		Path temp = deskewAndSave(imgPath, METHOD.ROTADED_RECTANGLES);
		System.out.println(temp);
	}

	/**
	 * Deskew an image, and save it in the same folder as the original image.
	 * 
	 * @param imgPath - the Path to the image
	 * @return the path of the newly saved image
	 */
	public static Path deskewAndSave(final Path imgPath, METHOD method) {
		final String ext = FilenameUtils.getExtension(imgPath.getFileName().toString());
		final String filename = imgPath.toString().replace("." + ext, "") + "_deskewed." + ext;
		// TODO: save to a child folder containing only deskewed images?
		Path savedPath = imgPath.resolveSibling(filename);

		Img img = deskew(imgPath, METHOD.ROTADED_RECTANGLES);
		try {
			synchronized (Deskewer.class) {
				if (savedPath.toFile().exists()) {
					String[] fileNameParts = savedPath.getFileName().toString().split("\\.(?=[^\\.]+$)");
					savedPath = File.createTempFile(fileNameParts[0] + "-", "." + fileNameParts[1], imgPath.getParent().toFile()).toPath();
				}
			}
			Imgcodecs.imwrite(savedPath.toString(), img.getSrc());
			return savedPath;
		} catch (IOException e) {
			logger.error("An error has occured while saving file " + savedPath.toString(), e);
			return null;
		} finally {
			if (null != img)
				img.close();
		}
	}

	/**
	 * Deskew an image.
	 * 
	 * @param imgPath - the path to the image
	 * @return a new {@link Img}
	 */
	public static Img deskew(final Path imgPath, METHOD method) {
		if (!imgPath.toFile().exists())
			throw new IllegalStateException("No files were found at Path " + imgPath);
		Img img = new Img(imgPath.toString());
		Img deskewed = deskew(img, method);
		img.close();
		return deskewed;
	}

	/**
	 * Draw the Rotated rectangles used to calculate the deskew angle.
	 * 
	 * @param img - the source image
	 * @param scalar - the color used to draw the rectangles
	 * @param thickness - the thickness
	 * @return - an annotated Img
	 */
	public static Img getRotatedRectanglesDrawn(final Img img, Scalar scalar, int thickness) {
		Img imgCopy = new Img(img.getSrc(), true);
		Img closed = getClosedImg(imgCopy);
		List<RotatedRect> rectangles = getRotatedRects(closed.getSrc());
		rectangles.stream().forEach(rect -> drawSingleRotatedRectangle(imgCopy.getSrc(), rect, scalar, thickness));
		List<RotatedRect> filteredRectangles = getRansacInliersRects(rectangles, ransacError);
		filteredRectangles.stream().forEach(rect -> drawSingleRotatedRectangle(imgCopy.getSrc(), rect, new Scalar(0, 255, 0), thickness));
		closed.close();
		return imgCopy;
	}

	public static Img getLinesDrawn(final Img img, Scalar scalar, int thickness) {
		Img imgCopy = new Img(img.getSrc(), true);
		Img closed = getClosedImg(imgCopy);
		Lines lines = getLines(closed.getSrc());
		lines.stream().forEach(line -> drawSingleLine(imgCopy.getSrc(), line, scalar, thickness));
		Lines filteredLines = getRansacInliersLines(lines, ransacError);
		filteredLines.stream().forEach(line -> drawSingleLine(imgCopy.getSrc(), line, new Scalar(0, 255, 0), thickness));
		closed.close();
		return imgCopy;
	}

	/**
	 * Get the binary image used to compute the deskew angle.
	 * 
	 * @param img - the source image
	 * @return a binary image
	 */
	public static Img getBinary(final Img img) {
		return getClosedImg(img);
	}

	// This function modifies the Mat mat
	private static void drawSingleRotatedRectangle(Mat mat, final RotatedRect rect, final Scalar scalar, final int thickness) {
		Point points[] = new Point[4];
		rect.points(points);
		for (int i = 0; i < 4; ++i) {
			Imgproc.line(mat, points[i], points[(i + 1) % 4], scalar, thickness);
		}
	}

	private static void drawSingleLine(Mat mat, final Line line, final Scalar scalar, final int thickness) {
		line.draw(mat, scalar, thickness);
	}

	public static Img deskew(final Img img, METHOD method) {
		final Img closed = getClosedImg(img);
		final double angle = detectAngle(closed.getSrc(), method);
		logger.trace("Deskew angle = {}", angle);
		if (Double.isNaN(angle))
			return img;

		final Point center = new Point(img.width() / 2, img.height() / 2);
		// Rotation matrix
		Mat rotationMatrix = Imgproc.getRotationMatrix2D(center, angle, 1);

		// Get the bounding rectangle
		Rect bbox = new RotatedRect(center, img.size(), angle).boundingRect();
		// Adjust the transformation matrix to prevent image cropping
		double[] array = rotationMatrix.get(0, 2);
		array[0] += bbox.width / 2 - center.x;
		rotationMatrix.put(0, 2, array);
		array = rotationMatrix.get(1, 2);
		array[0] += bbox.height / 2 - center.y;
		rotationMatrix.put(1, 2, array);

		// Rotated Mat and empty Mat to apply the mask
		Mat rotated = new Mat(bbox.size(), CvType.CV_8UC3, Scalar.all(255));
		Mat rotatedMasked = new Mat();
		// New mask
		Mat mask = new Mat(img.size(), CvType.CV_8UC1, new Scalar(255));
		Mat warpedMask = new Mat();
		// Compute the rotation for the mask and the image
		Imgproc.warpAffine(mask, warpedMask, rotationMatrix, bbox.size());
		Imgproc.warpAffine(img.getSrc(), rotatedMasked, rotationMatrix, bbox.size(), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(255));
		// Apply the mask to the rotated Mat
		rotatedMasked.copyTo(rotated, warpedMask);
		// Release the matrices before return
		rotatedMasked.release();
		mask.release();
		warpedMask.release();
		rotationMatrix.release();
		closed.close();
		return new Img(rotated, false);
	}

	private static Img getClosedImg(final Img img) {
		double size = (closedImgSizeFactor * img.size().area());
		// Round the size factor to the nearest odd int
		size = 2 * (Math.floor(size / 2)) + 1;
		// return img.bilateralFilter(20, 80, 80).adaptativeGaussianInvThreshold(17, 9).morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(closedImgSizeFactor, closedImgSizeFactor));
		return img.bilateralFilter(20, 80, 80).bgr2Gray().grad(2.0d, 2.0d).thresHold(0, 255, Imgproc.THRESH_BINARY_INV + Imgproc.THRESH_OTSU).bitwise_not().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(size, size));
	}

	private static double detectAngle(final Mat dilated, METHOD method) {
		switch (method) {
		case HOUGH_LINES:
			Lines lines = getLines(dilated);
			return getRansacInliersLines(lines, ransacError).getMeanInDegree();
		default:
		case ROTADED_RECTANGLES:
			List<RotatedRect> rotatedRects = getRotatedRects(dilated);
			for (RotatedRect rotatedRect : rotatedRects) {
				if (rotatedRect.angle <= -45.0) {
					rotatedRect.angle += 90.0;
					double tmp = rotatedRect.size.width;
					rotatedRect.size.width = rotatedRect.size.height;
					rotatedRect.size.height = tmp;
				}
			}
			return getRansacInliersRects(rotatedRects, ransacError).stream().mapToDouble(i -> i.angle).average().orElse(rotatedRects.stream().mapToDouble(r -> r.angle).average().getAsDouble());
		}
	}

	private static List<RotatedRect> getInliers(final List<RotatedRect> data, final double confidence) {
		if (null == data)
			return null;
		double average = data.stream().mapToDouble(rect -> rect.angle).average().getAsDouble();
		double sd = Math.sqrt(data.stream().mapToDouble(rect -> Math.pow(rect.angle - average, 2)).average().getAsDouble());
		Collections.sort(data, (r1, r2) -> Double.compare(r1.angle, r2.angle));
		int middle = data.size() / 2;
		double median;
		if (middle % 2 == 1)
			median = data.get(middle).angle;
		else
			median = DoubleStream.of(data.get(middle).angle, data.get(middle - 1).angle).average().getAsDouble();
		return data.stream().filter(rect -> Math.abs(rect.angle - median) < confidence * sd).collect(Collectors.toList());
	}

	private static List<RotatedRect> getRansacInliersRects(final List<RotatedRect> data, final double error) {
		int n = 3; // number of random samples
		int k = 50; // number of iterations
		double t = error; // error margin
		int d = data.size() * 2 / 3; // number of minimum matches
		if (d < n) {
			if (d >= n - 1)
				n = 2;
			else
				return data;
		}

		Map<Integer, RotatedRect> bestFit = new HashMap<>();
		for (int i = 1, maxAttempts = 10; bestFit.size() <= 3 && i <= maxAttempts; ++i) {
			Ransac<RotatedRect> ransac = new Ransac<>(data, getModelProviderRects(), n, k * i, t, d);
			try {
				ransac.compute();
				bestFit = ransac.getBestDataSet();
				// bestFit.entrySet().forEach(entry -> logger.debug("key: {} | | value: {}", entry.getKey(), entry.getValue()));
			} catch (Exception e) {
				t *= 1.5;
				logger.trace("Can't get a good model. Increase the error margin to {}", t);
			}
		}
		return bestFit.values().stream().collect(Collectors.toList());
	}

	private static Lines getRansacInliersLines(final Lines data, final double error) {
		int n = 3; // number of random samples
		int k = 50; // number of iterations
		double t = error; // error margin
		int d = data.size() * 2 / 3; // number of minimum matches
		if (d < n) {
			if (d >= n - 1)
				n = 2;
			else
				return data;
		}

		Map<Integer, Line> bestFit = new HashMap<>();
		for (int i = 1, maxAttempts = 10; bestFit.size() <= 3 && i <= maxAttempts; ++i) {
			Ransac<Line> ransac = new Ransac<>(data.getLines(), getModelProviderLines(), n, k * i, t, d);
			try {
				ransac.compute();
				bestFit = ransac.getBestDataSet();
			} catch (Exception e) {
				t *= 1.5;
				logger.trace("Can't get a good model. Increase the error margin to {}", t);
			}
		}
		return new Lines(bestFit.values().stream().collect(Collectors.toList()));
	}

	private static Function<Collection<RotatedRect>, Model<RotatedRect>> getModelProviderRects() {
		return datas -> {
			double average = datas.stream().mapToDouble(rect -> rect.angle).average().getAsDouble();

			return new Model<RotatedRect>() {
				@Override
				public double computeError(RotatedRect data) {
					return Math.abs(data.angle - average);
				}

				@Override
				public double computeGlobalError(Collection<RotatedRect> datas) {
					double error = 0;
					for (RotatedRect rect : datas)
						error += Math.pow(computeError(rect), 2);
					return error;
				}

				@Override
				public Object[] getParams() {
					return new Object[] { average };
				}
			};
		};
	}

	private static Function<Collection<Line>, Model<Line>> getModelProviderLines() {
		return datas -> {
			double average = datas.stream().mapToDouble(line -> line.getAngle()).average().getAsDouble();

			return new Model<Line>() {
				@Override
				public double computeError(Line data) {
					return Math.abs(data.getAngle() - average);
				}

				@Override
				public double computeGlobalError(Collection<Line> datas) {
					double error = 0;
					for (Line line : datas)
						error += Math.pow(computeError(line), 2);
					return error;
				}

				@Override
				public Object[] getParams() {
					return new Object[] { average };
				}

			};
		};
	}

	private static List<RotatedRect> getRotatedRects(final Mat dilated) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(dilated, contours, new Mat(), Imgproc.RETR_LIST, Imgproc.CHAIN_APPROX_SIMPLE);
		double minArea = minAreaFactor * dilated.size().area();
		List<RotatedRect> rotatedRects = contours.stream().filter(contour -> Imgproc.contourArea(contour) > minArea).map(contour -> Imgproc.minAreaRect(new MatOfPoint2f(contour.toArray()))).collect(Collectors.toList());
		return rotatedRects;
	}

	private static Lines getLines(final Mat dilated) {
		Img binary = new Img(dilated);
		Lines lines = new Lines(binary.houghLinesP(1, Math.PI / 180, 100, 100, 10));
		binary.close();
		return lines;
	}
}
