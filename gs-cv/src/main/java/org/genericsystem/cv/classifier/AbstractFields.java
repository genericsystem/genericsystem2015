package org.genericsystem.cv.classifier;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.cv.Img;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Scalar;

public abstract class AbstractFields implements Iterable<AbstractField> {

	private static ThreadLocalRandom rand = ThreadLocalRandom.current();
	protected List<AbstractField> fields;
	protected List<AbstractField> oldFields;
	protected static final double MIN_SIMILARITY = 0.90;
	protected static final double OVERLAP_THRESHOLD = 0.30;
	protected static final double OVERLAP_CONFIDENCE = 0.90;
	private long count = System.currentTimeMillis();

	public AbstractFields() {
		this.fields = new ArrayList<>();
		this.oldFields = new ArrayList<>();
	}

	public AbstractFields(List<AbstractField> fields) {
		this.fields = fields;
	}

	protected abstract AbstractField getIntersection(AbstractField field1, AbstractField field2);

	protected abstract AbstractField getUnion(AbstractField field1, AbstractField field2);

	protected List<AbstractField> findMatchingFieldsWithConfidence(AbstractField field, double threshold) {
		return oldFields.stream().filter(f -> f.overlapsMoreThanThresh(field.getRect(), threshold)).collect(Collectors.toList());
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
		// stream().filter(AbstractField::needOcr).filter(f -> f.canBeOCR(rootImg)).forEach(f -> f.ocr(rootImg));
		randomOcrStream().forEach(f -> f.ocr(rootImg));
	}

	public void drawOcrPerspectiveInverse(Img display, Mat homography, Scalar color, int thickness) {
		consolidatedFieldStream().forEach(field -> field.drawOcrPerspectiveInverse(display, homography, color, thickness));
	}

	public void drawConsolidated(Img stabilizedDisplay) {
		consolidatedFieldStream().forEach(field -> field.draw(stabilizedDisplay));
	}

	public Stream<AbstractField> randomOcrStream() {
		return stream().filter(f -> rand.nextBoolean());
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

	// public void mergeOverlaps() {
	// mergeOverlaps(OVERLAP_CONFIDENCE);
	// }
	//
	// public void mergeOverlaps(double confidence) {
	// ListIterator<AbstractField> it = fields.listIterator();
	// while (it.hasNext()) {
	// AbstractField field = it.next();
	// List<AbstractField> overlaps = findMatchingFieldsWithConfidence(field, confidence);
	// if (overlaps != null && !overlaps.isEmpty()) {
	// AbstractField newField = tryMerge(field, overlaps);
	// if (newField != null)
	// it.set(newField);
	// }
	// }
	// }
	//
	// // XXX pb: overlapping elements don't get deleted
	// public AbstractField tryMerge(AbstractField targetField, List<AbstractField> overlaps) {
	// if (targetField.getConsolidated().isPresent()) {
	// for (AbstractField field : overlaps) {
	// Optional<String> consolidatedTarget = targetField.getConsolidated();
	// Optional<String> consolidatedField = field.getConsolidated();
	// if (consolidatedField.isPresent()) {
	// String stringTarget = consolidatedTarget.get();
	// String stringCurrent = consolidatedField.get();
	// if (stringTarget.equals(stringCurrent)) {
	// AbstractField intersect = getIntersection(targetField, field);
	// return intersect;
	// } else if (StringCompare.compare(stringCurrent, stringTarget, SIMILARITY.COSINE_WORD) >= MIN_SIMILARITY) {
	// AbstractField union = getUnion(targetField, field);
	// return union;
	// } else {
	// // XXX add an else if() condition to check whether the text in the smaller box is contained within the one in the larger box (word inside sentence, etc.)
	// if (field.isIn(targetField)) {
	// if (StringCompare.containsSubstring(stringTarget, stringCurrent)) {
	// System.out.println(String.format("field.isIn(targetField), exact match (%s <=> %s)", stringTarget, stringCurrent));
	// } else if (StringCompare.containsSubstring(stringTarget, stringCurrent, MIN_SIMILARITY, SIMILARITY.COSINE_CHAR)) {
	// System.out.println(String.format("field.isIn(targetField), partial match (%s <=> %s)", stringTarget, stringCurrent));
	// }
	// } else if (targetField.isIn(field)) {
	// if (StringCompare.containsSubstring(stringTarget, stringCurrent)) {
	// System.out.println(String.format("targetField.isIn(field), exact match (%s <=> %s)", stringTarget, stringCurrent));
	// } else if (StringCompare.containsSubstring(stringTarget, stringCurrent, MIN_SIMILARITY, SIMILARITY.COSINE_CHAR)) {
	// System.out.println(String.format("targetField.isIn(field), partial match (%s <=> %s)", stringTarget, stringCurrent));
	// }
	// }
	// }
	// } // else do nothing, since the field was not consolidated
	// }
	// } // else do nothing, since the field was not consolidated
	// return null;
	// }

}
