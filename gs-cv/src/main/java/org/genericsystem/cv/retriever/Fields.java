package org.genericsystem.cv.retriever;

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

	private static final int MAX_DELETE_UNMERGED = 5;
	private static final int OCR_TIMEOUT = 50;
	private static final double MIN_OVERLAP = 0.2;

	public void reset() {
		fields = new ArrayList<>();
	}

	public void removeOverlaps() {
		Iterator<Field> outerIt = fields.iterator();
		while (outerIt.hasNext()) {
			Field field1 = outerIt.next();
			Iterator<Field> innerIt = fields.iterator();
			while (innerIt.hasNext()) {
				Field field2 = innerIt.next();
				if (field1 != field2) {
					// Optional<Rect> optional = RectToolsMapper.getInsider(field1.getRect(), field2.getRect());
					double mergeArea = RectToolsMapper.inclusiveArea(field1.getRect(), field2.getRect());
					if (mergeArea != 0) {
						if (mergeArea <= 0.5) {
							// Not enough overlap => delete bigger rect
							logger.warn("Removing field with {} common area -> {}, {}", String.format("%.1f%%", mergeArea * 100), field1.getRect(), field1.getLabels());
						} else {
							// Enough overlap => merge fields
							logger.warn("Merging fields with {} common area -> {}, {}", String.format("%.1f%%", mergeArea * 100), field1.getLabels(), field2.getLabels());
							Rect mean = RectToolsMapper.getMean(Arrays.asList(field1.getRect(), field2.getRect()));
							field2.merge(field1);
							field2.updateRect(mean);
						}
						if (!field1.isLocked()) {
							outerIt.remove();
							break;
						} else
							System.err.println(" >>> unable to delete locked field");
					}
				}
			}
		}
	}

	public void drawLockedFields(Img display, Mat homography) {
		fields.forEach(field -> field.drawLockedField(display, homography));
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
					StringBuffer sb = new StringBuffer();
					sb.append(String.format("Merging fields with %.1f%% common area", mergeArea * 100));
					currentOldField.getConsolidated().ifPresent(s -> sb.append(String.format(" -> %s", s)));
					logger.info(sb.toString());
					f.merge(currentOldField);
					f.resetDeadCounter();
				});
				it.remove();
			} else
				logger.info("No match for : " + currentOldField.getLabels().keySet());

		}
		// Increment the deadCounter in old fields that were not merged
		oldFields.forEach(f -> f.incrementDeadCounter());
		oldFields.removeIf(f -> !f.isLocked() && f.deadCounter >= MAX_DELETE_UNMERGED);
		// At this stage, add all the remaining fields still in oldFields
		fields.addAll(oldFields);
	}

	private List<Field> buildNewFields(List<Rect> rects, double thresholdFactor) {
		double meanArea = rects.stream().mapToDouble(r -> r.area()).average().getAsDouble();
		double sem = Math.sqrt(rects.stream().mapToDouble(r -> Math.pow(r.area() - meanArea, 2)).sum() / (rects.size() - 1));
		return rects.stream().filter(r -> (r.area() - meanArea) <= (sem * thresholdFactor)).map(Field::new).collect(Collectors.toList());
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
			runParallelOcr(rootImg);
			// runSequentialOcr(rootImg);
		}
	}

	private void runSequentialOcr(Img rootImg) {
		int idx = ThreadLocalRandom.current().nextInt(size());
		Field f = fields.get(idx);
		if (!f.isLocked())
			f.ocr(rootImg);
	}

	private void runParallelOcr(Img rootImg) {
		ParallelTasks tasks = new ParallelTasks();
		Set<Integer> indexes = new HashSet<>();
		while (indexes.size() < tasks.getCounter()) {
			int idx = ThreadLocalRandom.current().nextInt(size());
			if (indexes.add(idx)) {
				Field f = fields.get(idx);
				if (!f.isLocked())
					tasks.add(() -> f.ocr(rootImg));
			}
		}
		try {
			tasks.run();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

}