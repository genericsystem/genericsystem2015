package org.genericsystem.layout;

import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.stream.IntStream;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.Ocr;
import org.genericsystem.cv.utils.OCRPlasty;
import org.genericsystem.cv.utils.OCRPlasty.RANSAC;
import org.genericsystem.cv.utils.OCRPlasty.Tuple;
import org.opencv.core.Mat;
import org.opencv.core.Rect;

public class Field extends AbstractField {

	public Field(Rect rect) {
		super(rect);
	}

	public Field saveForLater(Rect newRect) {
		Field f = new Field(newRect);
		f.merge(this);
		return f;
	}

	@Override
	public void ocr(Img rootImg) {
		String ocr = Ocr.doWork(new Mat(rootImg.getSrc(), getLargeRect(rootImg, 0.03, 0.1)));
		if (!ocr.isEmpty()) {
			labels.merge(ocr, 1, Integer::sum);
			attempts++;
		}
		System.err.println("-> " + attempts);
		if (attempts <= 3 || attempts % 5 == 0)
			consolidateOcr();
	}

	@Override
	protected void consolidateOcr() {
		if (getLabelsSize() > 2) {
			List<String> strings = labels.entrySet().stream().sorted(Entry.<String, Integer>comparingByValue().reversed()).limit(20).collect(ArrayList<String>::new, (list, e) -> IntStream.range(0, e.getValue()).forEach(count -> list.add(e.getKey())),
					List::addAll);
			Tuple res = OCRPlasty.correctStringsAndGetOutliers(strings, RANSAC.NORM_LEVENSHTEIN);
			consolidated = res.getString(); // .orElse(labels.entrySet().stream().map(e -> e.getKey()).findFirst().orElse(null));
			if (getLabelsSize() > 10)
				res.getOutliers().forEach(outlier -> labels.remove(outlier));
		} else {
			consolidated = Optional.empty();
		}
	}

}