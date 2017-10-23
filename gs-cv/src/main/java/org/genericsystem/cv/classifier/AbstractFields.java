package org.genericsystem.cv.classifier;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.cv.Img;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Scalar;

public abstract class AbstractFields implements Iterable<AbstractField> {

	protected List<AbstractField> fields;
	protected static final double MIN_SIMILARITY = 0.90;
	protected static final double OVERLAP_THRESHOLD = 0.30;
	protected static final double OVERLAP_CONFIDENCE = 0.90;

	public AbstractFields() {
		this.fields = new ArrayList<>();
	}

	public AbstractFields(List<AbstractField> fields) {
		this.fields = fields;
	}

	protected abstract AbstractField getIntersection(AbstractField field1, AbstractField field2);

	protected abstract AbstractField getUnion(AbstractField field1, AbstractField field2);

	protected List<AbstractField> findMatchingFieldsWithConfidence(AbstractField field, double threshold) {
		return fields.stream().filter(f -> f.overlapsMoreThanThresh(field.getRect(), threshold)).collect(Collectors.toList());
	}

	protected List<AbstractField> findContainingFields(AbstractField field) {
		return stream().filter(f -> field.isIn(f)).collect(Collectors.toList());
	}

	protected List<AbstractField> findContainedFields(AbstractField field) {
		return stream().filter(f -> f.isIn(field)).collect(Collectors.toList());
	}

	protected AbstractField findNewField(Point pt) {
		return stream().filter(field -> field.contains(pt)).findFirst().orElse(null);
	}

	public void consolidateOcr(Img rootImg) {
		randomOcrStream().forEach(f -> f.ocr(rootImg));
	}

	public void drawOcrPerspectiveInverse(Img display, Mat homography, Scalar color, int thickness) {
		consolidatedFieldStream().forEach(field -> field.drawOcrPerspectiveInverse(display, homography, color, thickness));
	}

	public void drawConsolidated(Img stabilizedDisplay) {
		consolidatedFieldStream().forEach(field -> field.draw(stabilizedDisplay));
	}

	public Stream<AbstractField> randomOcrStream() {
		return stream().filter(AbstractField::needOcr);
	}

	public Stream<AbstractField> consolidatedFieldStream() {
		return stream().filter(f -> f.isConsolidated());
	}

	public Stream<AbstractField> stream() {
		return fields.stream();
	}

	public Stream<AbstractField> parallelStream() {
		return fields.parallelStream();
	}

	@Override
	public Iterator<AbstractField> iterator() {
		return fields.iterator();
	}

	public int size() {
		return fields.size();
	}

}
