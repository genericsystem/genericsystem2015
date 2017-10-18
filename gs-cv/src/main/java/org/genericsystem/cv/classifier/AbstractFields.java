package org.genericsystem.cv.classifier;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.OCRPlasty;
import org.genericsystem.cv.utils.RectangleTools;
import org.opencv.core.Mat;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;

public abstract class AbstractFields implements Iterable<AbstractField> {

	protected List<AbstractField> fields;
	protected static final double MIN_SIMILARITY = 0.90;
	protected static final double OVERLAP_THRESHOLD = 0.30;
	private long count = System.currentTimeMillis();

	public AbstractFields() {
		this.fields = new ArrayList<>();
	}

	public AbstractFields(List<AbstractField> fields) {
		this.fields = fields;
	}

	public void removeOverlaps() {
		Set<AbstractField> removes = new HashSet<>();
		for (AbstractField field : fields) {
			List<AbstractField> overlaps = getOverlaps(field);
			if (overlaps != null && !overlaps.isEmpty()) {
				removes.addAll(tryMerge(field, overlaps));
			}
		}
		// fields.removeAll(removes); // XXX: acts the same as emptying the list
		// TODO: find an alternative way to remove the fields that are not needed anymore
	}

	public List<AbstractField> getOverlaps(AbstractField targetField) {
		List<AbstractField> overlaps = new ArrayList<>();
		for (AbstractField field : fields) {
			if (targetField.overlapsMoreThanThresh(field, 0.5)) {
				overlaps.add(field);
			}
		}
		System.out.println("overlaps size: " + overlaps.size());
		return overlaps;
	}

	public List<AbstractField> tryMerge(AbstractField targetField, List<AbstractField> overlaps) {
		List<AbstractField> removes = new ArrayList<>();
		if (targetField.getConsolidated().isPresent()) {
			for (AbstractField field : overlaps) {
				Optional<String> consolidatedTarget = targetField.getConsolidated();
				Optional<String> consolidatedField = field.getConsolidated();
				if (consolidatedField.isPresent()) {
					if (consolidatedTarget.equals(consolidatedField)) {
						targetField = getIntersection(targetField, field);
						removes.add(field);
					} else if (OCRPlasty.similarity(Arrays.asList(consolidatedTarget.get(), consolidatedField.get())) >= MIN_SIMILARITY) {
						targetField = getUnion(targetField, field);
						removes.add(field);
					} else {
						// XXX add an else if() condition to check whether the text in the smaller box is contained within the one in the larger box (word inside sentence, etc.)
						// Do something?
					}
				} // else do nothing, since the field was not consolidated
			}
			return removes;
		} // else do nothing, since the field was not consolidated
		return Collections.emptyList();
	}

	protected AbstractField getIntersection(AbstractField field1, AbstractField field2) {
		Rect rect1 = field1.getRect();
		Rect rect2 = field2.getRect();
		Rect intersect = RectangleTools.getIntersection(rect1, rect2).orElseThrow(() -> new IllegalArgumentException("No intersecting rectangle was found"));
		Field intersection = new Field(intersect);
		intersection.merge(Arrays.asList(field1, field2));
		return intersection;
	}

	protected AbstractField getUnion(AbstractField field1, AbstractField field2) {
		Rect rect1 = field1.getRect();
		Rect rect2 = field2.getRect();
		Field union = new Field(RectangleTools.getUnion(rect1, rect2));
		union.merge(Arrays.asList(field1, field2));
		return union;
	}

	public void consolidateOcr(Img rootImg) {
		stream().filter(AbstractField::needOcr).filter(f -> f.canBeOCR(rootImg)).forEach(f -> f.ocr(rootImg));
		// if ((System.currentTimeMillis() - count) > 2_000) {
		// removeOverlaps();
		// count = System.currentTimeMillis();
		// }
	}

	public void drawOcrPerspectiveInverse(Img display, Mat homography, Scalar color, int thickness) {
		consolidatedFieldStream().forEach(field -> field.drawOcrPerspectiveInverse(display, homography, color, thickness));
	}

	public void drawConsolidated(Img stabilizedDisplay) {
		consolidatedFieldStream().forEach(field -> field.draw(stabilizedDisplay));
	}

	public Stream<AbstractField> consolidatedFieldStream() {
		return stream().filter(f -> f.isConsolidated());
	}

	public int size() {
		return fields.size();
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
}
