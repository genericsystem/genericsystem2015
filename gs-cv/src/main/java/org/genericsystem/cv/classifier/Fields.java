package org.genericsystem.cv.classifier;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.ParallelTasks;
import org.genericsystem.cv.utils.RectToolsMapper;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.utils.Converters;

@SuppressWarnings({ "unchecked", "rawtypes" })
public class Fields extends AbstractFields {

	private Mat lastHomographyInv;

	private static ThreadLocalRandom rand = ThreadLocalRandom.current();
	private static final int MAX_DELETE_UNMERGED = 5;
	private static final int OCR_TIMEOUT = 100;

	public void merge(List<Rect> newRects) {
		List<Field> oldFields = restabilizeFields();
		System.out.println("oldFields transformed (" + oldFields.size() + ")");

		fields = newRects.stream().map(Field::new).collect(Collectors.toList());

		if (lastHomographyInv != null) {
			ListIterator<Field> it = oldFields.listIterator();
			while (it.hasNext()) {
				Field currentOldField = it.next();
				// List<Field> matches = (List) findMatchingFieldsWithConfidence(currentOldField, 0.7);
				List<Field> matches = (List) findClusteredFields(currentOldField, 0.1);

				if (!matches.isEmpty()) {
					currentOldField.getConsolidated().ifPresent(s -> System.out.println("Merged: " + s));
					matches.forEach(f -> {
						f.merge(currentOldField);
						f.resetDeadCounter();
					});
					it.remove();
				} else {
					System.out.print(".");
				}
			}
			// Increment the deadCounter in old fields that were not merged
			oldFields.forEach(f -> f.incrementDeadCounter());
			oldFields.removeIf(f -> f.deadCounter >= MAX_DELETE_UNMERGED);
			// At this stage, add all the remaining fields still in oldFields
			fields.addAll(oldFields);
		}
	}

	@Override
	protected Field getIntersection(AbstractField field1, AbstractField field2) {
		Rect rect1 = field1.getRect();
		Rect rect2 = field2.getRect();
		Rect intersect = RectToolsMapper.getIntersection(rect1, rect2).orElseThrow(() -> new IllegalArgumentException("No intersecting rectangle was found"));
		Field intersection = new Field(intersect);
		Arrays.asList(field1, field2).forEach(f -> intersection.merge(f));
		return intersection;
	}

	@Override
	protected Field getUnion(AbstractField field1, AbstractField field2) {
		Rect rect1 = field1.getRect();
		Rect rect2 = field2.getRect();
		Field union = new Field(RectToolsMapper.getUnion(rect1, rect2));
		Arrays.asList(field1, field2).forEach(f -> union.merge(f));
		return union;
	}

	private List<Field> restabilizeFields() {
		// Apply the homography to the oldFields
		List<Rect> virtualRects = fields.stream().map(AbstractField::getRect).map(rect -> findNewRect(rect)).collect(Collectors.toList());
		return IntStream.range(0, fields.size()).mapToObj(i -> {
			Field f = new Field(virtualRects.get(i));
			f.merge(fields.get(i));
			return f;
		}).collect(Collectors.toList());
	}

	private Rect findNewRect(Rect rect) {
		List<Point> points = restabilize(Arrays.asList(rect.tl(), rect.br()));
		return new Rect(points.get(0), points.get(1));
	}

	private List<Point> restabilize(List<Point> originals) {
		Mat original = Converters.vector_Point2f_to_Mat(originals);
		MatOfPoint2f results = new MatOfPoint2f();
		Core.perspectiveTransform(original, results, lastHomographyInv);
		List<Point> res = results.toList();
		return res;
	}

	public void storeLastHomographyInv(Mat homographyInv) {
		this.lastHomographyInv = homographyInv;
	}

	@Override
	public void consolidateOcr(Img rootImg) {
		long TS = System.currentTimeMillis();
		while (System.currentTimeMillis() - TS <= OCR_TIMEOUT) {
			ParallelTasks tasks = new ParallelTasks();
			Set<Integer> indexes = new HashSet<>(tasks.getCounter());
			while (indexes.size() < tasks.getCounter()) {
				int idx = rand.nextInt(size());
				if (indexes.add(idx))
					tasks.add(() -> fields.get(idx).ocr(rootImg));
			}
			try {
				tasks.run();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	}

}