package org.genericsystem.cv.retriever;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.ParallelTasks;
import org.genericsystem.cv.utils.RectToolsMapper;
import org.genericsystem.reinforcer.tools.GSPoint;
import org.genericsystem.reinforcer.tools.GSRect;
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
		stream().forEach(field -> field.drawWithHomography(display, homography, color, thickness));
	}

	public void drawFieldsOnStabilized(Img stabilized) {
		stream().forEach(field -> field.draw(stabilized, 1));
	}

	public void displayFieldsTree() {
		StringBuffer sb = new StringBuffer();
		sb.append("\n").append("--- FIELDS ---").append("\n");
		fields.forEach(field -> sb.append(field.recursiveToString()));
		sb.append("\n").append("--- /FIELDS ---").append("\n");
		System.out.println(sb.toString());
	}

	public List<Field> getRoots() {
		return fields.stream().filter(field -> field.isOrphan()).collect(Collectors.toList());
	}

	public void consolidate(Img img) {
		fields.forEach(Field::incrementDeadCounter);
		mergeRects(img, 0.70);
		removeDeadTrees();
	}

	public List<GSRect> mergeRectsList(Img img) {
		RectDetector rd = new RectDetector(img);
		List<GSRect> rects = rd.getRects(200, 11, 3, new Size(11, 3));
		List<GSRect> children = rd.getRects(40, 17, 3, new Size(7, 3));
		return cleanList(rects, children, 0.70);
	}

	public List<GSRect> cleanList(List<GSRect> bigRects, List<GSRect> smallRects, double overlapThreshold) {
		smallRects.removeIf(smallRect -> bigRects.stream().anyMatch(bigRect -> smallRect.inclusiveArea(bigRect) > overlapThreshold));
		return Stream.concat(bigRects.stream().filter(bigRect -> smallRects.stream().filter(rect -> rect.isOverlapping(bigRect)).noneMatch(rect -> rect.isInsider(bigRect) == null)), smallRects.stream()).collect(Collectors.toList());
	}

	private void mergeRects(Img img, double overlapThreshold) {
		for (GSRect rect : mergeRectsList(img)) {
			Field match = findMatch(rect, overlapThreshold, img.width(), img.height());
			if (match != null)
				updateNode(rect, match, img.width(), img.height());
			else
				createNode(rect, findPotentialParent(rect));
		}
	}

	private Field findPotentialParent(GSRect rect) {
		for (Field root : getRoots()) {
			Field parent = findPotentialParent(rect, root);
			if (parent != null)
				return parent;
		}
		return null;
	}

	private Field findPotentialParent(GSRect rect, Field root) {
		GSRect insider = rect.isInsider(root.getRect());
		if (insider == null || rect == insider)
			return null;
		for (Field child : root.getChildren()) {
			Field candidate = findPotentialParent(rect, child);
			if (candidate != null)
				return candidate;
		}
		return root;
	}

	public void createNode(GSRect rect, Field parent) {
		if (checkOverlapConstraint(rect, null)) {
			logger.info("Creating a new node for {}", rect);
			Field f = new Field(rect);
			if (parent != null)
				f.setParent(parent);
			fields.add(f);
		}
	}

	public void updateNode(GSRect rect, Field field, int width, int height) {
		if (checkOverlapConstraint(rect, field)) {
			logger.info("Updating node {} with {}", field.getRect(), rect);
			field.updateRect(rect, width, height);
			field.resetDeadCounter();
		}
	}

	private boolean checkOverlapConstraint(GSRect rect, Field target) {
		for (Field field : fields)
			if (target == null || field != target)
				if (field.isOverlapping(rect))
					if (rect.isInsider(field.getRect()) == null)
						return false;
		return true;
	}

	public void removeNode(Field field) {
		logger.info("Removing node: {}", field.getRect());
		fields.remove(field);
	}

	private void removeDeadTrees() {
		getRoots().stream().filter(field -> isDeadTree(field, MAX_DELETE_UNMERGED)).flatMap(field -> listTree(field).stream()).forEach(this::removeNode);
	}

	private boolean isDeadTree(Field root, int maxDeadCount) {
		if (!root.hasChildren())
			return root.isDead(maxDeadCount);
		for (Field child : root.getChildren())
			if (!isDeadTree(child, maxDeadCount))
				return false;
		return true;
	}

	private List<Field> listTree(Field root) {
		List<Field> res = new ArrayList<>();
		res.add(root);
		for (Field child : root.getChildren())
			res.addAll(listTree(child));
		return res;
	}

	private Field findMatch(GSRect rect, double areaOverlap, int width, int height) {
		GSRect frameRect = new GSRect(0, 0, width, height);
		List<Field> matches = fields.stream().filter(f -> rect.inclusiveArea(f.getRect().getIntersection(frameRect)) > areaOverlap).collect(Collectors.toList());
		if (matches.isEmpty())
			return null;
		if (matches.size() > 1) {
			StringBuilder sb = new StringBuilder(matches.size() + " matches were detected.\n");
			for (Field field : matches)
				sb.append(field + "\n");
			logger.warn(sb.toString());
		}
		return matches.get(0);
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