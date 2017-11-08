package org.genericsystem.cv.retriever;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.cv.Img;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;

public abstract class AbstractFields<F extends AbstractField> implements Iterable<F> {

	protected List<F> fields;
	protected static final double MIN_SIMILARITY = 0.90;
	protected static final double OVERLAP_THRESHOLD = 0.30;
	protected static final double OVERLAP_CONFIDENCE = 0.90;

	public AbstractFields() {
		this.fields = new ArrayList<>();
	}

	public AbstractFields(List<F> fields) {
		this.fields = fields;
	}

	protected List<F> findMatchingFieldsWithConfidence(F field, double threshold) {
		return fields.stream().filter(f -> f.overlapsMoreThanThresh(field.getRect(), threshold)).collect(Collectors.toList());
	}

	protected List<F> findClusteredFields(F field, double epsilon) {
		return fields.stream().filter(f -> f.isClusteredWith(field.getRect(), epsilon)).collect(Collectors.toList());
	}

	protected List<F> findPossibleMatches(Rect rect, double epsilon) {
		return fields.stream().filter(f -> f.isClusteredWith(rect, epsilon)).collect(Collectors.toList());
	}

	// like findPossibleMatches(Rect, double) but will match only a number of sides (e.g., 3 instead of 4 sides)
	protected List<F> findPossibleMatches(Rect rect, double epsilon, int sides) {
		return fields.stream().filter(f -> f.isClusteredWith(rect, epsilon, sides)).collect(Collectors.toList());
	}

	protected List<F> findContainingFields(F field) {
		return stream().filter(f -> field.isIn(f)).collect(Collectors.toList());
	}

	protected List<F> findContainedFields(F field) {
		return stream().filter(f -> f.isIn(field)).collect(Collectors.toList());
	}

	protected F findNewField(Point pt) {
		return stream().filter(field -> field.contains(pt)).findFirst().orElse(null);
	}

	public abstract void performOcr(Img rootImg);

	public void drawOcrPerspectiveInverse(Img display, Mat homography, Scalar color, int thickness) {
		stream().forEach(field -> field.drawOcrPerspectiveInverse(display, homography, color, thickness));
	}

	public void drawFieldsOnStabilized(Img stabilized) {
		stream().forEach(f -> f.drawRect(stabilized, f.getDeadCounter() < 1 ? new Scalar(0, 255, 0) : new Scalar(0, 0, 255), 1));
	}

	public void drawConsolidated(Img stabilizedDisplay) {
		consolidatedFieldStream().forEach(field -> field.drawRect(stabilizedDisplay, new Scalar(0, 255, 0), 1));
	}

	public Stream<F> consolidatedFieldStream() {
		return stream().filter(f -> f.isConsolidated());
	}

	public Stream<F> stream() {
		return fields.stream();
	}

	@Override
	public Iterator<F> iterator() {
		return fields.iterator();
	}

	public int size() {
		return fields.size();
	}

}
