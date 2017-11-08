package org.genericsystem.cv.retriever;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
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
import org.opencv.core.Scalar;
import org.opencv.utils.Converters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Fields extends AbstractFields<Field> {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private static final int MAX_DELETE_UNMERGED = 20;
	private static final int OCR_TIMEOUT = 50;
	private static final double MIN_OVERLAP = 0.2;

	public void reset() {
		fields = new ArrayList<>();
	}

	// TODO: compare the consolidated text before merging?
	public void removeOverlaps() {
		for (Field field : new ArrayList<>(fields)) {
			List<Field> matches = findPossibleMatches(field.getRect(), 0.1, 3).stream().filter(f -> !field.equals(f)).collect(Collectors.toList());
			if (!matches.isEmpty()) {
				logger.warn("Found {} matche(s) for {}", matches.size(), field.getRect());
				for (Field f : matches) {
					logger.warn("{}", f.getRect());
					if (field.getRect().area() < f.getRect().area()) {
						if (!f.isLocked()) {
							boolean ok = fields.remove(f);
							System.err.println("removed field (" + ok + ")" + "\n" + f);
						}
					}
				}
			}
			matches = findContainingFields(field).stream().filter(f -> !field.equals(f)).collect(Collectors.toList());
			if (!matches.isEmpty()) {
				logger.warn("Found {} inside {} other field(s)", field.getRect(), matches.size());
				for (Field f : matches) {
					logger.warn("{}", f.getRect());
					if (!f.isLocked()) {
						boolean ok = fields.remove(f);
						System.err.println("removed field (" + ok + ")" + "\n" + f);
					}
				}
			}
		}
	}

	public void drawLockedFields(Img display, Mat homography) {
		fields.forEach(field -> field.drawLockedField(display, homography));
		fields.stream().filter(field -> field.isChild()).forEach(field -> field.drawRect(display, field.getRectPointsWithHomography(homography), new Scalar(255, 128, 255), 1));
	}

	public void displayFieldsTree() {
		StringBuffer sb = new StringBuffer();
		sb.append("\n").append("--- FIELDS ---").append("\n");
		fields.forEach(field -> {
			sb.append(field.recursiveToString());
		});
		sb.append("\n").append("--- /FIELDS ---").append("\n");
		System.out.println(sb.toString());
	}

	public void mergeChildren(List<Rect> children) {
		List<Rect> fieldsRects = fields.stream().map(f -> f.getRect()).collect(Collectors.toList());
		for (int i = 0; i < fieldsRects.size(); ++i) {
			Field parent = fields.get(i);
			Rect rect = fieldsRects.get(i);

			List<Rect> possibleChildren = findChildren(children, rect);

			if (!possibleChildren.isEmpty()) {
				logger.warn("Found possible child(ren) for {}: {}", rect, possibleChildren);
				possibleChildren.forEach(child -> {
					List<Field> matches = findPossibleMatches(child, 0.1); // TODO: filter the matches to remove those that are too big?
					if (!matches.isEmpty()) {
						matches.forEach(f -> {
							double mergeArea = RectToolsMapper.inclusiveArea(f.getRect(), child);
							StringBuffer sb = new StringBuffer();
							sb.append(String.format("Merging %s with %s (%.1f%% common area)", f.getRect(), child, mergeArea * 100));
							if (f.getConsolidated() != null)
								sb.append(String.format(" -> %s", f.getConsolidated()));
							logger.warn(sb.toString());

							f.updateRect(child);
							f.setChild(true);

							// XXX shouldn't it be already set?
							f.setParent(parent);
							parent.addChild(f);

							f.resetDeadCounter();
						});
					} else {
						logger.warn("No match for child {}. Creating a new Field", child);
						Field f = new Field(child);
						f.setChild(true);

						f.setParent(parent);
						parent.addChild(f);

						fields.add(f);
					}
					children.remove(child);
				});
			}
		}
	}

	private List<Rect> findChildren(List<Rect> children, Rect putativeParent) {
		return children.stream().filter(child -> RectToolsMapper.commonArea(child, putativeParent)[0] > 0.9).collect(Collectors.toList());
	}

	public void merge(RectDetector rectDetector) {
		// Get the lists of rectangles
		List<Rect> rects = rectDetector.getFilteredRects(0.5);
		List<Rect> children = rectDetector.getFilteredRects2(0.5);

		// Remove the duplicates of rects in children
		rects.forEach(rect -> {
			Iterator<Rect> it = children.iterator();
			while (it.hasNext()) {
				if (RectToolsMapper.isInCluster(rect, it.next(), 0.1))
					it.remove();
			}
		});

		// Increment the dead counter of each field
		fields.forEach(f -> f.incrementDeadCounter());

		// Loop over all the rectangles and try to find any matching field
		for (Rect rect : rects) {
			List<Field> matches = findPossibleMatches(rect, 0.1);
			matches = cleanMatches(matches, rect);
			if (!matches.isEmpty()) {
				matches.forEach(f -> {
					double mergeArea = RectToolsMapper.inclusiveArea(f.getRect(), rect);
					StringBuffer sb = new StringBuffer();
					sb.append(String.format("Merging fields with %.1f%% common area", mergeArea * 100));
					if (f.getConsolidated() != null)
						sb.append(String.format(" -> %s", f.getConsolidated()));
					logger.info(sb.toString());
					f.updateRect(rect);
					f.resetDeadCounter();
				});
			} else {
				logger.info("No match for {}. Creating a new Field", rect);
				fields.add(new Field(rect));
			}
		}
		// Remove the rectangles that could not be merged too many times
		removeUnmergedFields();

		// Merge the potential children
		mergeChildren(children);
	}

	private void removeUnmergedFields() {
		// XXX elaborate a new strategy to deal with the parents/children
		// fields.removeIf(f -> !f.isLocked() && f.deadCounter >= MAX_DELETE_UNMERGED);
		Iterator<Field> it = fields.iterator();
		while (it.hasNext()) {
			Field f = it.next();
			if (!f.isLocked() && f.deadCounter >= MAX_DELETE_UNMERGED) {
				for (Field child : f.getChildren())
					child.setParent(null);
				if (f.getParent() != null)
					f.getParent().removeChild(f);
				it.remove();
			}
		}
	}

	private List<Field> cleanMatches(List<Field> matches, Rect rect) {
		List<Field> copy = new ArrayList<>(matches);
		// Remove the false positives
		copy = copy.stream().filter(f -> RectToolsMapper.inclusiveArea(f.getRect(), rect) > MIN_OVERLAP / 10).collect(Collectors.toList());
		if (copy.isEmpty()) {
			return Collections.emptyList();
		} else {
			// If there is more than one match, select only the best
			if (copy.size() > 1) {
				logger.info("Multiple matches ({}), removing false positives", copy.size());
				// Remove the overlaps with less than 10% common area
				copy = copy.stream().filter(f -> RectToolsMapper.inclusiveArea(f.getRect(), rect) >= MIN_OVERLAP).collect(Collectors.toList());
				if (copy.size() > 1) {
					logger.warn("Still multiple matches ({}), selecting the maximum overlap", copy.size());
					copy = Arrays.asList(copy.stream().max((f1, f2) -> {
						double area1 = RectToolsMapper.inclusiveArea(f1.getRect(), rect);
						double area2 = RectToolsMapper.inclusiveArea(f2.getRect(), rect);
						return Double.compare(area1, area2);
					}).orElseThrow(IllegalStateException::new));
				}
			}
			return copy;
		}
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
		int limit = tasks.getCounter() * 2;
		Set<Integer> indexes = new HashSet<>();
		while (indexes.size() < limit && indexes.size() < size()) {
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