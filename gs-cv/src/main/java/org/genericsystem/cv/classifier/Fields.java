package org.genericsystem.cv.classifier;

import java.lang.invoke.MethodHandles;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Fields extends AbstractFields<Field> {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private static ThreadLocalRandom rand = ThreadLocalRandom.current();
	private static final int MAX_DELETE_UNMERGED = 5;
	private static final int OCR_TIMEOUT = 100;

	public void merge(List<Rect> newRects, Mat fieldsHomography) {
		List<Field> oldFields = restabilizeFields(fieldsHomography);
		logger.info("oldFields transformed ({})", oldFields.size());
		fields = newRects.stream().map(Field::new).collect(Collectors.toList());

		Iterator<Field> it = oldFields.iterator();
		while (it.hasNext()) {
			Field currentOldField = it.next();
			List<Field> matches = findMatchingFieldsWithConfidence(currentOldField, 0.7);
			// List<Field> matches = findClusteredFields(currentOldField, 0.1);
			if (!matches.isEmpty()) {
				currentOldField.getConsolidated().ifPresent(s -> logger.info("Merged: {}", s));
				if (matches.size() > 1)
					logger.error("Multiple matches: {}", matches.size());

				matches.forEach(f -> {
					double mergeArea = RectToolsMapper.inclusiveArea(f.getRect(), currentOldField.getRect());
					logger.info("Merging two fields with {} common area", String.format("%.3f", mergeArea));
					f.merge(currentOldField);
					f.resetDeadCounter();
				});
				it.remove();
			}
		}
		// Increment the deadCounter in old fields that were not merged
		oldFields.forEach(f -> f.incrementDeadCounter());
		oldFields.removeIf(f -> f.deadCounter >= MAX_DELETE_UNMERGED);
		// At this stage, add all the remaining fields still in oldFields
		fields.addAll(oldFields);
	}

	private List<Field> restabilizeFields(Mat homography) {
		List<Rect> virtualRects = fields.stream().map(AbstractField::getRect).map(rect -> findNewRect(rect, homography)).collect(Collectors.toList());
		return IntStream.range(0, fields.size()).mapToObj(i -> {
			Field f = new Field(virtualRects.get(i));
			f.merge(fields.get(i));
			return f;
		}).collect(Collectors.toList());
	}

	private Rect findNewRect(Rect rect, Mat homography) {
		List<Point> points = restabilize(Arrays.asList(rect.tl(), rect.br()), homography);
		return new Rect(points.get(0), points.get(1));
	}

	private List<Point> restabilize(List<Point> originals, Mat homography) {
		Mat original = Converters.vector_Point2f_to_Mat(originals);
		MatOfPoint2f results = new MatOfPoint2f();
		Core.perspectiveTransform(original, results, homography);
		List<Point> res = results.toList();
		return res;
	}

	@Override
	public void consolidateOcr(Img rootImg) {
		long TS = System.currentTimeMillis();
		while (System.currentTimeMillis() - TS <= OCR_TIMEOUT) {
			ParallelTasks tasks = new ParallelTasks();
			Set<Integer> indexes = new HashSet<>();
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