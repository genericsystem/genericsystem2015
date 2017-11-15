package org.genericsystem.cv.retriever;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.ParallelTasks;
import org.genericsystem.cv.utils.RectToolsMapper;
import org.genericsystem.reinforcer.tools.GSPoint;
import org.genericsystem.reinforcer.tools.GSRect;
import org.genericsystem.reinforcer.tools.RectangleTools;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.utils.Converters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Fields extends AbstractFields<Field> {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private static final int MAX_DELETE_UNMERGED = 5;
	private static final int OCR_TIMEOUT = 50;
	private static final double MIN_OVERLAP = 0.2;

	public void reset() {
		displayFieldsTree();
		fields = new ArrayList<>();
	}

	@Override
	public void drawOcrPerspectiveInverse(Img display, Mat homography, Scalar color, int thickness) {
		// Only draw orphan fields
		stream().filter(field -> field.isOrphan()).forEach(field -> field.drawOcrPerspectiveInverse(display, homography, color, thickness));
	}

	public void drawLockedFields(Img display, Mat homography) {
		fields.forEach(field -> field.drawLockedField(display, homography));
		fields.stream().filter(field -> !field.isOrphan()).forEach(field -> field.drawRect(display, field.getRectPointsWithHomography(homography), new Scalar(255, 128, 255), 1));
	}

	public void drawTruncatedFields(Img display, Mat homography) {
		fields.stream().filter(f -> f.isTruncated()).forEach(field -> field.drawTruncatedField(display, homography));
		fields.stream().filter(field -> field.isTruncated()).forEach(field -> field.drawRect(display, field.getRectPointsWithHomography(homography), new Scalar(255, 128, 255), 1));
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

	public List<Field> getRoots() {
		return fields.stream().filter(field -> field.isOrphan()).collect(Collectors.toList());
	}

	private List<GSRect> identifyTruncated(List<GSRect> rects, int frameWidth, int frameHeight) {
		List<GSRect> truncatedList = rects.stream().filter(r -> r.isTruncatedRect(frameWidth, frameHeight)).collect(Collectors.toList());
		truncatedList.stream().forEach(r -> r.setTruncated(true));
		return truncatedList;
	}

	public void consolidate(Img img) {
		RectMerger rm = new RectMerger(img);
		List<GSRect> rects = rm.mergeRectsList();

		// Increment the dead counter of each field
		fields.forEach(Field::incrementDeadCounter);

		mergeRects(rects, img.width(), img.height());

		removeDeadTrees();
		cleanRelationships();
	}

	private static class RectMerger {
		private RectDetector rd;

		public RectMerger(Img img) {
			this.rd = new RectDetector(img);
		}

		public List<GSRect> mergeRectsList() {
			List<GSRect> rects = RectToolsMapper.rectToGSRect(rd.getRects(200, 11, 3, new Size(11, 3)));
			List<GSRect> children = RectToolsMapper.rectToGSRect(rd.getRects(40, 17, 3, new Size(7, 3)));
			// Remove the duplicates of rects in children
			children.removeIf(child -> rects.stream().anyMatch(parent -> RectangleTools.isInCluster(parent, child, 0.1)));
			rects.addAll(children);
			return rects;
		}
	}

	// TODO re-arrange the fields to match all the constraints (no overlap, children strictly contained in parents)
	private void cleanRelationships() {
		// // 1. Each tree must validate the constraints
		// for (Field parent : getRoots()) {
		// if (!parent.checkConstraints()) {
		// System.err.println(parent.recursiveToString());
		// parent.repairTree();
		// }
		// }
		// // 2. Two overlapping trees can't coexist and must be merged
	}

	private void mergeRects(List<GSRect> rects, int width, int height) {
		// List<GSRect> truncateds = identifyTruncated(rects, width, height);
		// mergeRect(truncateds);
		// rects.removeIf(rect -> truncateds.contains(rect));
		for (GSRect rect : rects) {
			Field match = findMatch(rect, 0.1);
			if (match != null)
				updateNode(rect, match);
			else
				createNode(rect, findPotentialParent(rect, getRoots()));
		}
	}

	private Field findPotentialParent(GSRect rect, List<Field> roots) {
		for (Field root : getRoots()) {
			Field parent = findPotentialParent(rect, root);
			if (parent != null)
				return parent;
		}
		return null;
	}

	private Field findPotentialParent(GSRect rect, Field root) {
		if (!rect.getInsider(root.getRect()).map(r -> r.equals(rect)).orElse(false))
			return null;
		for (Field child : root.getChildren()) {
			Field candidate = findPotentialParent(rect, child);
			if (candidate != null)
				return candidate;
		}
		return root;
	}

	private void createNode(GSRect rect, Field parent) {
		// truncated rects don't trigger field creation
		if (rect.isTruncated())
			return;
		logger.info("Creating a new node for {}", rect);
		Field f = new Field(rect);

		if (parent != null)
			f.setParent(parent);
		fields.add(f);
	}

	private void updateNode(GSRect rect, Field field) {
		logger.info("Updating node {} with {}", field.getRect(), rect);
		field.setTruncated(false);
		if (rect.isTruncated())
			field.setTruncated(true);
		field.updateRect(rect);
		field.resetDeadCounter();
	}

	private void removeNode(Field field) {
		logger.info("Removing node: {}", field.getRect());
		fields.remove(field);
	}

	private void removeDeadTrees() {
		Predicate<Field> predicate = f -> !f.isLocked() && f.getDeadCounter() >= MAX_DELETE_UNMERGED;
		getRoots().stream().filter(field -> isDeadTree(field, predicate)).flatMap(field -> listTree(field).stream()).forEach(this::removeNode);
	}

	private boolean isDeadTree(Field root, Predicate<Field> predicate) {
		if (!root.hasChildren()) // Single element in the tree, use the predicate
			return predicate.test(root);
		for (Field child : root.getChildren())
			if (!isDeadTree(child, predicate)) // Return false if one of the element does not match the predicate
				return false;
		return true; // If false was not returned at this stage, the tree is dead
	}

	private List<Field> listTree(Field root) {
		List<Field> res = new ArrayList<>();
		res.add(root);
		for (Field child : root.getChildren())
			res.addAll(listTree(child));
		return res;
	}

	private Field findMatch(GSRect rect, double eps) {
		List<Field> matches = findPossibleMatches(rect, eps);
		// Remove the false positives
		matches.removeIf(f -> f.getRect().inclusiveArea(rect) <= MIN_OVERLAP / 10);
		if (matches.isEmpty()) {
			return null;
		} else {
			// If there is more than one match, select only the best
			if (matches.size() > 1) {
				logger.debug("Multiple matches ({}), removing false positives", matches.size());
				// Remove the overlaps with less than 10% common area
				matches.removeIf(f -> f.getRect().inclusiveArea(rect) < MIN_OVERLAP);
				if (matches.size() > 1) {
					logger.warn("Still multiple matches ({}), selecting the maximum overlap", matches.size());
					matches = Arrays.asList(matches.stream().max((f1, f2) -> {
						double area1 = f1.getRect().inclusiveArea(rect);
						double area2 = f2.getRect().inclusiveArea(rect);
						return Double.compare(area1, area2);
					}).orElseThrow(IllegalStateException::new));
				}
			}
			return matches.isEmpty() ? null : matches.get(0);
		}
	}

	public void restabilizeFields(Mat homography) {
		long start = System.nanoTime();
		fields.forEach(field -> field.updateRect(findNewRect(field.getRect(), homography)));
		long stop = System.nanoTime();
		logger.info("Restabilized {} fields in {} ms", fields.size(), String.format("%.3f", ((double) (stop - start)) / 1_000_000));
	}

	private GSRect findNewRect(GSRect rect, Mat homography) {
		List<Point> originals = RectToolsMapper.gsPointToPoint(Arrays.asList(rect.tl(), rect.br()));
		List<GSPoint> points = RectToolsMapper.pointToGSPoint(restabilize(originals, homography));
		return new GSRect(points.get(0), points.get(1));
	}

	private List<Point> restabilize(List<Point> originals, Mat homography) {
		Mat original = Converters.vector_Point2d_to_Mat(originals);
		Mat results = new Mat();
		Core.perspectiveTransform(original, results, homography);
		List<Point> res = new ArrayList<>();
		Converters.Mat_to_vector_Point2d(results, res);
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