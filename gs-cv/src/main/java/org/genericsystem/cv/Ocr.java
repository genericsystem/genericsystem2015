package org.genericsystem.cv;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.imageio.ImageIO;

import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Size;

import net.sourceforge.tess4j.Tesseract;
import net.sourceforge.tess4j.TesseractException;

public class Ocr {

	private static Tesseract instance;

	static {
		instance = new Tesseract();
		instance.setDatapath("/usr/share/tesseract-ocr/4.00/");
		instance.setLanguage("fra");
		instance.setHocr(false);
		// instance.setOcrEngineMode(2);
		instance.setTessVariable("tessedit_char_whitelist", "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM0123456789.-,;:?!=_<'()");
		instance.setTessVariable("tessedit_char_blacklist", "{}");
	}

	public static String doWork(File imageFile) {
		try {
			return instance.doOCR(imageFile);
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

}
