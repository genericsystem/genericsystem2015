package org.genericsystem.cv;

import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.imageio.ImageIO;

import org.opencv.core.Mat;
import org.opencv.core.MatOfByte;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Size;
import org.opencv.imgcodecs.Imgcodecs;

import net.sourceforge.tess4j.Tesseract;
import net.sourceforge.tess4j.TesseractException;

public class Ocr {

	private static Tesseract instance;

	static {
		instance = new Tesseract();
		instance.setDatapath("/usr/share/tesseract-ocr/4.00/");
		instance.setLanguage("fra");
		instance.setHocr(false);
		instance.setPageSegMode(10);
		instance.setOcrEngineMode(1);
		instance.setTessVariable("preserve_interword_spaces", "0");
		instance.setTessVariable("textord_space_size_is_variable", "1");
		instance.setTessVariable("tessedit_char_whitelist", "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM0123456789.-,<'");
		instance.setTessVariable("tessedit_char_blacklist", "?{}_[]()À");
	}

	public static String doWork(File imageFile) {
		try {
			return instance.doOCR(imageFile);
		} catch (TesseractException e) {
			throw new RuntimeException(e);
		}

	}

	public static String doWork(Mat mat) {
		return doWork(mat2bufferedImage(mat));
	}

	public static String doWork(Mat mat, Rect rect) {
		return doWork(mat2bufferedImage(mat), new Rectangle(rect.x, rect.y, rect.width, rect.height));
	}

	public static String doWork(BufferedImage image) {
		try {
			return instance.doOCR(image);
		} catch (TesseractException e) {
			throw new RuntimeException(e);
		}
	}

	public static String doWork(BufferedImage image, Rectangle rectangle) {
		try {
			return instance.doOCR(image, rectangle);
		} catch (TesseractException e) {
			throw new RuntimeException(e);
		}

	}

	public static List<Rect> findBox(Mat mat) {

		try {
			return instance.getSegmentedRegions(Tools.mat2bufferedImage(mat), 2).stream().map(rectangle -> new Rect(new Point(rectangle.getX(), rectangle.getY()), new Size(rectangle.getWidth(), rectangle.getHeight()))).collect(Collectors.toList());
		} catch (TesseractException e) {
			throw new RuntimeException(e);
		}

	}

	public static List<Rect> findBox(File imageFile) {

		try {
			return instance.getSegmentedRegions(ImageIO.read(imageFile), 2).stream().map(rectangle -> new Rect(new Point(rectangle.getX(), rectangle.getY()), new Size(rectangle.getWidth(), rectangle.getHeight()))).collect(Collectors.toList());
		} catch (IOException | TesseractException e) {
			throw new RuntimeException(e);
		}

	}

	public static List<Rect> findBox(String hocrString) {
		Pattern pattern = Pattern.compile("title=\"bbox \\d+ \\d+ \\d+ \\d+");
		Matcher matcher = pattern.matcher(hocrString);

		List<Rect> result = new ArrayList<>();
		while (matcher.find()) {
			String s = matcher.group(0);
			s = s.replace("title=\"bbox ", "");
			String[] v = s.split(" ");
			List<Double> doubles = Arrays.stream(v).map(Double::valueOf).collect(Collectors.toList());
			result.add(new Rect(new Point(doubles.get(0), doubles.get(1)), new Point(doubles.get(2), doubles.get(3))));
		}
		Arrays.asList(result);
		return result;

	}

	public static BufferedImage mat2bufferedImage(Mat image) {
		MatOfByte bytemat = new MatOfByte();
		Imgcodecs.imencode(".png", image, bytemat);
		try {
			return ImageIO.read(new ByteArrayInputStream(bytemat.toArray()));
		} catch (IOException e) {
			throw new IllegalStateException(e);
		}
	}
	// public static BufferedImage mat2BufferedImage(Mat in) {
	// BufferedImage image = new BufferedImage(in.width(), in.height(), BufferedImage.TYPE_3BYTE_BGR);
	// WritableRaster raster = image.getRaster();
	// DataBufferByte dataBuffer = (DataBufferByte) raster.getDataBuffer();
	// byte[] data = dataBuffer.getData();
	// in.get(0, 0, data);
	// return image;
	// }

}
