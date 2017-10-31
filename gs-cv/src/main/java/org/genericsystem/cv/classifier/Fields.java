package org.genericsystem.cv.classifier;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;

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
	private static final int OCR_TIMEOUT = 50;
	private static final double MIN_OVERLAP = 0.2;

	public void reset() {
		fields = new ArrayList<>();
	}

	public void merge(List<Rect> newRects) {
		List<Field> oldFields = fields;
		fields = buildNewFields(newRects, 0.5);

		Iterator<Field> it = oldFields.iterator();
		while (it.hasNext()) {
			Field currentOldField = it.next();
			List<Field> matches = findClusteredFields(currentOldField, 0.1);
			if (!matches.isEmpty()) {
				// Remove the false positives
				matches = matches.stream().filter(f -> RectToolsMapper.inclusiveArea(f.getRect(), currentOldField.getRect()) > MIN_OVERLAP / 10).collect(Collectors.toList());
				// If there is more than one match, select only the best
				if (matches.size() > 1) {
					logger.info("Multiple matches ({}), removing false positives", matches.size());
					// Remove the overlaps with less than 10% common area
					matches = matches.stream().filter(f -> RectToolsMapper.inclusiveArea(f.getRect(), currentOldField.getRect()) >= MIN_OVERLAP).collect(Collectors.toList());
					if (matches.size() > 1) {
						logger.warn("Still multiple matches ({}), selecting the maximum overlap", matches.size());
						matches = Arrays.asList(matches.stream().max((f1, f2) -> {
							double area1 = RectToolsMapper.inclusiveArea(f1.getRect(), currentOldField.getRect());
							double area2 = RectToolsMapper.inclusiveArea(f2.getRect(), currentOldField.getRect());
							return Double.compare(area1, area2);
						}).orElseThrow(IllegalStateException::new));
					}
				}
				matches.forEach(f -> {
					double mergeArea = RectToolsMapper.inclusiveArea(f.getRect(), currentOldField.getRect());
					currentOldField.getConsolidated().ifPresent(s -> logger.info("Merged: {}", s));
					logger.info("Merging fields with {}% common area", String.format("%.1f", mergeArea * 100));
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

	private List<Field> buildNewFields(List<Rect> rects, double thresholdFactor) {
		double meanArea = rects.stream().mapToDouble(r -> r.area()).average().getAsDouble();
		double sem = Math.sqrt(rects.stream().mapToDouble(r -> Math.pow(r.area() - meanArea, 2)).sum() / (rects.size() - 1));
		return rects.stream().filter(r -> Math.abs(r.area() - meanArea) <= (sem * thresholdFactor)).map(Field::new).collect(Collectors.toList());
	}

	public void restabilizeFields(Mat homography) {
		fields.forEach(field -> field.updateRect(findNewRect(field.getRect(), homography)));
		logger.info("Restabilized fields ({})", fields.size());
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
	public void performOcr(Img rootImg) {
		long TS = System.currentTimeMillis();
		while (System.currentTimeMillis() - TS <= OCR_TIMEOUT) {
			// runParallelOcr(rootImg);
			runSequentialOcr(rootImg);
		}
	}

	private void runSequentialOcr(Img rootImg) {
		int idx = rand.nextInt(size());
		fields.get(idx).ocr(rootImg);
	}

	private void runParallelOcr(Img rootImg) {
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