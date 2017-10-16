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

	public void merge(Field field) {
		field.getLabels().entrySet().forEach(entry -> labels.merge(entry.getKey(), entry.getValue(), Integer::sum));
		consolidated = field.getConsolidated();
		attempts = field.getAttempts();
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
		consolidateOcr();
	}

	@Override
	protected void consolidateOcr() {
		List<String> strings = labels.entrySet().stream().sorted(Entry.<String, Integer>comparingByValue().reversed()).limit(20).collect(ArrayList<String>::new, (list, e) -> IntStream.range(0, e.getValue()).forEach(count -> list.add(e.getKey())),
				List::addAll);
		if (getLabelsSize() > 2) {
			Tuple res = OCRPlasty.correctStringsAndGetOutliers(strings, RANSAC.NORM_LEVENSHTEIN);
			consolidated = res.getString(); // .orElse(labels.entrySet().stream().map(e -> e.getKey()).findFirst().orElse(null));
			if (getLabelsSize() > 10)
				res.getOutliers().forEach(outlier -> labels.remove(outlier));
		} else {
			consolidated = Optional.empty();
		}
	}

}