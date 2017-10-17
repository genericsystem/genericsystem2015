package org.genericsystem.cv.classifier;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
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

	public AbstractFields() {
		this.fields = new ArrayList<>();
	}

	public AbstractFields(List<AbstractField> fields) {
		this.fields = fields;
	}

	public void removeOverlaps() {
		for (AbstractField field : fields) {
			List<AbstractField> overlaps = getOverlaps(field);
			if (overlaps != null && !overlaps.isEmpty()) {
				tryMerge(field, overlaps);
			}
		}
	}

	public List<AbstractField> getOverlaps(AbstractField targetField) {
		// Look for the fields that overlaps targetField
		List<AbstractField> overlaps = new ArrayList<>();
		for (AbstractField field : fields) {
			if (targetField.isOverlapping(field)) {
				overlaps.add(field);
			}
		}
		return overlaps;
	}

	public void tryMerge(AbstractField targetField, List<AbstractField> overlaps) {
		// TODO Auto-generated method stub
		List<AbstractField> removes = new ArrayList<>();
		if (targetField.getConsolidated().isPresent()) {

			for (AbstractField field : overlaps) {
				Optional<String> consolidatedTarget = targetField.getConsolidated();
				Optional<String> consolidatedField = field.getConsolidated();
				if (consolidatedField.isPresent()) {
					if (consolidatedTarget.equals(consolidatedField)) {
						// merge the intersection to create a new field
						// add the original field to removes
						targetField = getIntersection(targetField, field);
						removes.add(field);
					} else if (OCRPlasty.similarity(Arrays.asList(consolidatedTarget.get(), consolidatedField.get())) >= MIN_SIMILARITY) {
						targetField = getUnion(targetField, field);
						removes.add(field);
					} else {
						// Do something, but what?
					}
				}
			}
		}
	}

	protected AbstractField getIntersection(AbstractField field1, AbstractField field2) {
		Rect rect1 = field1.getRect();
		Rect rect2 = field2.getRect();
		Field intersect = new Field(RectangleTools.getIntersection(rect1, rect2).get()); // Get the optional directly since one know that the rectangles intersect
		intersect.merge(Arrays.asList(field1, field2));
		return intersect;
	}

	protected AbstractField getUnion(AbstractField field1, AbstractField field2) {
		Rect rect1 = field1.getRect();
		Rect rect2 = field2.getRect();
		Field union = new Field(RectangleTools.getUnion(rect1, rect2));
		union.merge(Arrays.asList(field1, field2));
		return union;
	}

	public void consolidateOcr(Img rootImg) {
		stream().filter(AbstractField::needOcr).forEach(f -> f.ocr(rootImg));
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
