package org.genericsystem.cv.retriever;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
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
import org.opencv.imgproc.Imgproc;
import org.opencv.utils.Converters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Fields extends AbstractFields<Field> {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private static final int MAX_DELETE_UNMERGED = 5;
	private static final int OCR_TIMEOUT = 50;

	public void reset() {
		displayFieldsTree();
		fields = new ArrayList<>();
	}

	@Override
	public void drawOcrPerspectiveInverse(Img display, Mat homography, int thickness) {
		stream().forEach(field -> field.drawWithHomography(display, homography, thickness));
	}

	public void drawFieldsOnStabilized(Img stabilized) {
		stream().forEach(field -> field.draw(stabilized, 1));
	}

	public void drawFieldsOnStabilizedDebug(Img stabilized) {
		for (GSRect rect : mergeRectsList(stabilized)) {
			Point[] targets = RectToolsMapper.gsPointToPoint(Arrays.asList(rect.decomposeClockwise())).toArray(new Point[0]);
			for (int i = 0; i < targets.length; ++i)
				Imgproc.line(stabilized.getSrc(), targets[i], targets[(i + 1) % targets.length], new Scalar(255, 0, 0), 1);
		}
	}

	public void displayFieldsTree() {
		StringBuffer sb = new StringBuffer();
		sb.append("\n").append("--- FIELDS ---").append("\n");
		getRoots().forEach(field -> sb.append(field.recursiveToString()));
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
		return Stream.concat(smallRects.stream().filter(smallRect -> bigRects.stream().filter(rect -> rect.isOverlapping(smallRect)).noneMatch(rect -> rect.getInsider(smallRect) == null)), bigRects.stream()).collect(Collectors.toList());
	}

	private void mergeRects(Img img, double overlapThreshold) {
		List<GSRect> rects = mergeRectsList(img);
		Collections.reverse(rects);
		for (GSRect rect : rects) {
			Field match = findMatch(rect, overlapThreshold, img.width(), img.height());
			if (match != null)
				updateNode(rect, match, img.width(), img.height());
			else if (!rect.isNearEdge(img.width(), img.height(), 10)) {
				Field parent = findPotentialParent(rect);
				if (parent == null) {
					List<Field> children = findPotentialChildren(rect);
					if (children.isEmpty())
						createNode(rect, null);
					else
						createNode2(rect, children);
				} else
					createNode(rect, parent);
			} else
				logger.warn("Rect {} was too close to the frame's edges", rect);
		}
	}

	private List<Field> findPotentialChildren(GSRect rect) {
		return getRoots().stream().filter(f -> f.getRect().isInside(rect)).collect(Collectors.toList());
	}

	private Field findPotentialParent(GSRect rect) {
		for (Field root : getRoots()) {
			Field parent = root.findPotentialParent(rect);
			if (parent != null)
				return parent;
		}
		return null;
	}

	public void createNode(GSRect rect, Field parent) {
		if (checkOverlapConstraint(rect)) {
			logger.info("Creating a new node for {}", rect);
			Field f = new Field(rect);
			if (parent != null)
				f.updateParent(parent);
			fields.add(f);
		} else
			logger.error("Unable to create node: " + rect);
	}

	public void createNode2(GSRect rect, List<Field> children) {
		if (checkOverlapConstraint(rect)) {
			logger.info("Creating a new node for {}", rect);
			Field f = new Field(rect);
			if (children != null)
				for (Field child : children)
					child.updateParent(f);
			fields.add(f);
		} else
			logger.error("Unable to create node: " + rect);
	}

	public void updateNode(GSRect rect, Field field, int width, int height) {
		logger.info("Updating node {} with {}", field.getRect(), rect);
		// field.updateRect(rect, width, height);
		field.resetParentsDeadCounter();
		field.resetChildrenDeadCounter();
	}

	private boolean checkOverlapConstraint(GSRect rect) {
		for (Field field : fields)
			if (rect.isOverlappingStrict(field.getRect()))
				if (rect.getInsider(field.getRect()) == null)
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
		if (matches.size() > 1)
			logger.warn(matches.size() + "matches were detected.");
		return matches.get(0);
	}

	public void restabilizeFields(Mat homography) {
		fields.forEach(field -> field.updateRect(findNewRect(field.getRect(), homography)));
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
		for (Set<Integer> indexes = new HashSet<>(); indexes.size() < limit && indexes.size() < size();) {
			int idx = ThreadLocalRandom.current().nextInt(size());
			if (indexes.add(idx)) {
				Field f = fields.get(idx);
				if (f.needOcr())
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