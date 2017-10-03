package org.genericsystem.cv.classifier;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.cv.Img;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;

public class DocFields implements Iterable<DocField> {
	private List<DocField> fields;

	public DocFields() {
		this.fields = new ArrayList<>();
	}

	public DocFields(List<DocField> fields) {
		this.fields = fields;
	}

	public static DocFields of(List<Rect> rects) {
		DocFields fields = new DocFields();
		fields.addFields(rects);
		return fields;
	}

	public void addFields(List<Rect> rects) {
		AtomicInteger counter = new AtomicInteger(0);
		fields = rects.stream().map(rect -> new DocField(counter.incrementAndGet(), rect)).collect(Collectors.toList());
	}

	public void drawOcrPerspectiveInverse(Img display, Scalar color, int thickness) {
		consolidatedFieldStream().forEach(field -> field.drawOcrPerspectiveInverse(display, color, thickness));
	}

	public void drawConsolidated(Img stabilizedDisplay) {
		consolidatedFieldStream().forEach(field -> field.draw(stabilizedDisplay));
	}

	public void consolidateOcr(Img rootImg) {
		fields.stream().filter(DocField::needOcr).forEach(f -> f.ocr(rootImg));
	}

	public Stream<DocField> consolidatedFieldStream() {
		return fields.stream().filter(f -> f.isConsolidated());
	}

	public int size() {
		return fields.size();
	}

	@Override
	public Iterator<DocField> iterator() {
		return fields.iterator();
	}

}