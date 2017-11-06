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
		// Increment the dead counter of each field
		fields.forEach(f -> f.incrementDeadCounter());
		// Filter out the rectangles to remove the bigger ones
		List<Rect> rects = filterRects(newRects, 0.5);
		// Loop over all the rectangles and try to find any matching field
		for (Rect rect : rects) {
			List<Field> matches = findPossibleMatches(rect, 0.1);
			// Remove the false positives
			matches = matches.stream().filter(f -> RectToolsMapper.inclusiveArea(f.getRect(), rect) > MIN_OVERLAP / 10).collect(Collectors.toList());
			if (!matches.isEmpty()) {
				// If there is more than one match, select only the best
				if (matches.size() > 1) {
					logger.info("Multiple matches ({}), removing false positives", matches.size());
					// Remove the overlaps with less than 10% common area
					matches = matches.stream().filter(f -> RectToolsMapper.inclusiveArea(f.getRect(), rect) >= MIN_OVERLAP).collect(Collectors.toList());
					if (matches.size() > 1) {
						logger.warn("Still multiple matches ({}), selecting the maximum overlap", matches.size());
						matches = Arrays.asList(matches.stream().max((f1, f2) -> {
							double area1 = RectToolsMapper.inclusiveArea(f1.getRect(), rect);
							double area2 = RectToolsMapper.inclusiveArea(f2.getRect(), rect);
							return Double.compare(area1, area2);
						}).orElseThrow(IllegalStateException::new));
					}
				}
				matches.forEach(f -> {
					double mergeArea = RectToolsMapper.inclusiveArea(f.getRect(), rect);
					StringBuffer sb = new StringBuffer();
					sb.append(String.format("Merging fields with %.1f%% common area", mergeArea * 100));
					if (f.getConsolidated() != null)
						sb.append(String.format(" -> %s", f.getConsolidated()));
					logger.info(sb.toString());
					Field merged = new Field(f);
					merged.updateRect(rect);
					merged.resetDeadCounter();
					fields.add(merged);
					fields.remove(f);
				});
			} else {
				logger.info("No match for {}. Creating a new Field", rect);
				fields.add(new Field(rect));
			}
		}
		// Remove the rectangles that could not be merged too many times
		fields.removeIf(f -> !f.isLocked() && f.deadCounter >= MAX_DELETE_UNMERGED);
	}

	private List<Rect> filterRects(List<Rect> rects, double thresholdFactor) {
		double meanArea = rects.stream().mapToDouble(r -> r.area()).average().getAsDouble();
		double sem = Math.sqrt(rects.stream().mapToDouble(r -> Math.pow(r.area() - meanArea, 2)).sum() / (rects.size() - 1));
		return rects.stream().filter(r -> (r.area() - meanArea) <= (sem * thresholdFactor)).collect(Collectors.toList());
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
		if (size() <= 0)
			return;
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