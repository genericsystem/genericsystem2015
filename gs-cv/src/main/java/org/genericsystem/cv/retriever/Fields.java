package org.genericsystem.cv.retriever;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.ParallelTasks;
import org.genericsystem.cv.utils.Ransac;
import org.genericsystem.cv.utils.Ransac.Model;
import org.genericsystem.cv.utils.RectToolsMapper;
import org.genericsystem.reinforcer.tools.GSPoint;
import org.genericsystem.reinforcer.tools.GSRect;
import org.genericsystem.reinforcer.tools.RectangleTools;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
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
		List<GSRect> truncatedList = rects.stream().filter(r -> isTruncatedRect(r, frameWidth, frameHeight)).collect(Collectors.toList());
		truncatedList.stream().forEach(r -> r.setTruncated(true));
		return truncatedList;

	}

	public void merge(RectDetector rectDetector, int frameWidth, int frameHeight) {
		// Get the lists of rectangles
		List<GSRect> rects = RectToolsMapper.rectToGSRect(rectDetector.getFilteredRects(1d));
		List<GSRect> children = RectToolsMapper.rectToGSRect(rectDetector.getFilteredRects2(1d));

		// Remove the duplicates of rects in children
		children.removeIf(child -> rects.stream().anyMatch(parent -> RectangleTools.isInCluster(parent, child, 0.1)));

		// Increment the dead counter of each field
		fields.forEach(f -> f.incrementDeadCounter());

		doWork(rects, frameWidth, frameHeight);
		// doWork(children, frameWidth, frameHeight);

		removeUnmergedFields();
		// adjustUnmergedParents(); // TODO remove?
		cleanRelationships();
	}

	// TODO re-arrange the fields to match all the constraints (no overlap, children strictly contained in parents)
	private void cleanRelationships() {
	}

	private void doWork(List<GSRect> rects, int width, int height) {
		List<GSRect> truncateds = identifyTruncated(rects, width, height);
		mergeRect(truncateds);
		rects.removeIf(rect -> truncateds.contains(rect));
		mergeRect(rects);
	}

	private void mergeRect(List<GSRect> rects) {
		// Loop over all the rectangles and try to find any matching field
		for (GSRect rect : rects) {
			placeRect(rect);
		}
	}

	private void placeRect(GSRect rect) {
		Field match = cleanMatches(rect, 0.1);
		if (match != null) {
			// We found a match, we can merge
			updateNode(rect, match);
		} else {
			for (Field root : getRoots()) {
				Field parent = findParentRecursive(rect, root);
				if (parent != null) {
					createNode(rect, parent);
					return;
				}
			}
			createNode(rect, null);
		}
	}

	private boolean isTruncatedRect(GSRect rect, int width, int height) {
		if (rect.tl().getX() <= 0d) {
			rect.setTruncateDirection(GSRect.RIGHT);
			return true;
		} else if (rect.tl().getY() <= 0d) {
			rect.setTruncateDirection(GSRect.UP);
			return true;
		} else if (rect.br().getX() >= width) {
			rect.setTruncateDirection(GSRect.LEFT);
			return true;
		} else if (rect.br().getY() >= height) {
			rect.setTruncateDirection(GSRect.BOTTOM);
			return true;
		}
		else if(rect.tl().getX() <= 0d && rect.tl().getY() <= 0d){
			rect.setTruncateDirection(GSRect.UP_RIGHT);
			return true;
		}		
		else if(rect.br().getX() >= width && rect.br().getY() >= height){
			rect.setTruncateDirection(GSRect.BOTTOM_LEFT);
			return true;
		}
		else if(rect.tl().getX() <= 0d && rect.br().getY() >= height){
			rect.setTruncateDirection(GSRect.BOTTOM_RIGHT);
			return true;
		}
		else if(rect.br().getX() >= width && rect.tl().getY() <= 0d){
			rect.setTruncateDirection(GSRect.UP_LEFT);
			return true;
		}		
			
		 else
			return false;
	}

	private Field findParentRecursive(GSRect rect, Field root) {
		if (rect.getInsider(root.getRect()).map(r -> r.equals(rect)).orElse(false)) {
			if (root.hasChildren()) {
				for (Field child : root.getChildren()) {
					Field candidate = findParentRecursive(rect, child);
					if (candidate != null)
						return candidate;
				}
				return root;
			} else
				return root;
		}
		return null;
	}

	private void createNode(GSRect rect, Field parent) {
		//truncated rects don't trigger field creation
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
		// logger.info(formatLog(field, rect));

		field.setTruncated(false);
		if (rect.isTruncated())
			field.setTruncated(true);

		field.registerShift(field.getRect().getShift(rect));
		field.updateRect(rect);
		field.resetDeadCounter();
		/*
		 * if(!f.isTruncated) f.resetDeadCounter();
		 */
	}

	private void removeNode(Field field, boolean removeRelations) {
		logger.info("Removing node: {}", field.getRect());
		if (removeRelations) {
			if (!field.isOrphan())
				field.getParent().removeChild(field);
			if (field.hasChildren())
				for (Field child : field.getChildren())
					child.setParent(null);
		}
		fields.remove(field);
	}

	private void adjustUnmergedParents() {
		fields.stream().filter(field -> field.hasChildren() && field.getDeadCounter() != 0).forEach(field -> {
			List<double[]> shifts = field.getShifts();
			if (!shifts.isEmpty()) {
				double[] mean = getMean(shifts);
				logger.debug("Mean before Ransac: {}", Arrays.toString(mean));
				if (shifts.size() > 3) {
					try {
						Ransac<double[]> ransac = new Ransac<>(shifts, getModelProvider(), 3, 50, 2, shifts.size() * 2 / 3);
						List<double[]> newShifts = ransac.getBestDataSet().values().stream().collect(Collectors.toList());
						mean = getMean(newShifts);
						logger.debug("Mean after Ransac: {}", Arrays.toString(mean));
					} catch (Exception e) {
						logger.info("Unable to compute ransac on shifts for {}", field.getRect());
					}
				}
				GSRect rect = field.getRect();
				GSPoint tl = new GSPoint(rect.tl().getX() - mean[0], rect.tl().getY() - mean[1]);
				GSPoint br = new GSPoint(rect.br().getX() - mean[2], rect.br().getY() - mean[3]);
				field.updateRect(new GSRect(tl, br));
				logger.info("Updated rect from {} to {}", rect, field.getRect());
				field.clearShifts();
			}
		});
	}

	private void removeUnmergedFields() {
		Predicate<Field> predicate = f -> !f.isLocked() && f.getDeadCounter() >= MAX_DELETE_UNMERGED;
		Set<Field> removes = new HashSet<>();

		// Delete each dead tree
		for (Field field : getRoots()) {
			if (deadTree(field, predicate))
				removes.addAll(killTree(field));
		}
		removes.forEach(field -> removeNode(field, false));

		// Clean the fields recursively from the 'root' of each tree
		getRoots().forEach(field -> removes.addAll(alternateDeleteRecursive(field, predicate)));
		removes.forEach(field -> removeNode(field, true));
	}

	private boolean deadTree(Field root, Predicate<Field> predicate) {
		if (!root.hasChildren()) // Single element in the tree, use the predicate
			return predicate.test(root);
		for (Field child : root.getChildren())
			if (!deadTree(child, predicate)) // Return false if one of the element does not match the predicate
				return false;
		return true; // If false was not returned at this stage, the tree is dead
	}

	private Set<Field> killTree(Field root) {
		Set<Field> res = new HashSet<>();
		res.add(root);
		if (root.hasChildren())
			for (Field child : root.getChildren())
				res.addAll(killTree(child));
		return res;
	}

	private Set<Field> deleteRecursive(Field field, Predicate<Field> predicate) {
		Set<Field> removes = new HashSet<>();
		if (predicate.test(field)) { // FIXME: this test should be moved downwards
			if (field.hasChildren()) {
				// Call the method recursively
				field.getChildren().forEach(f -> {
					removes.addAll(deleteRecursive(f, predicate));
				});
				if (!field.isOrphan()) {
					// attempt to merge text from child in parent (only if parent is not going to be deleted)
					// siblings?
				}
				// add remove here to delete parent?
			} else {
				if (!field.isOrphan()) {
					// attempt to merge text from child in parent (only if parent is not going to be deleted)
					if (!predicate.test(field.getParent())) {
						// check siblings to see if they need to be removed
						if (field.hasSiblings()) {
							// if all the siblings are removed, merge them all in the parent
							// Set<Field> siblings = field.getSiblings();
						} else {
							// attempt to merge directly in parent
						}
					} // else parent gets deleted, so we don't care
				}
			}
			removes.add(field);
		}
		return removes;
	}

	/**
	 * 1. If a field has children, it can't be removed <br>
	 * 2. If a field has a parent, it can't be removed <br>
	 * 3. A parent always contains its children <br>
	 * 4. Children are always enclosed in their parent <br>
	 */
	private Set<Field> alternateDeleteRecursive(Field field, Predicate<Field> predicate) {
		Set<Field> removes = new HashSet<>();

		if (field.hasChildren()) {
			field.getChildren().forEach(f -> {
				removes.addAll(alternateDeleteRecursive(f, predicate));
			});
			if (predicate.test(field)) {
				// Update the field's coordinates to encompass all the children
				List<GSRect> rects = field.getChildren().stream().map(f -> f.getRect()).collect(Collectors.toList());
				GSRect union = field.getRect().getUnion(rects.stream().reduce(rects.get(0), (r, total) -> total.getUnion(r)));
				logger.warn("Updating parent rect with union: {} -> {}", field.getRect(), union);
				field.updateRect(union);
			}
		} else {
			if (field.isOrphan()) {
				if (predicate.test(field))
					removes.add(field);
			} else {
				if (predicate.test(field)) {
					double[] area = RectangleTools.commonArea(field.getRect(), field.getParent().getRect());
					if (area[0] < 0.90) // Need to adjust the size of the child
						logger.error("area mismatch: {}", Arrays.toString(area));
					// else
					// logger.info("area ok: {}", Arrays.toString(area));
				}
			}
		}

		return removes;
	}

	private String formatLog(Field field, GSRect rect) {
		double mergeArea = field.getRect().inclusiveArea(rect);
		StringBuffer sb = new StringBuffer();
		sb.append(String.format("Merging %s with %s (%.1f%% common area)", field.getRect(), rect, mergeArea * 100));
		if (field.getConsolidated() != null)
			sb.append(String.format(" -> %s", field.getConsolidated()));
		return sb.toString();
	}

	private Field cleanMatches(GSRect rect, double eps) {
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

	private Function<Collection<double[]>, Model<double[]>> getModelProvider() {
		return datas -> {
			double[] mean = getMean(datas);

			return new Model<double[]>() {
				@Override
				public double computeError(double[] data) {
					double error = 0;
					for (int i = 0; i < mean.length; ++i)
						error += Math.pow(mean[i] - data[i], 2);
					return Math.sqrt(error);
				}

				@Override
				public double computeGlobalError(List<double[]> datas, Collection<double[]> consensusDatas) {
					double globalError = 0d;
					for (double[] data : datas)
						globalError += Math.pow(computeError(data), 2);
					return Math.sqrt(globalError) / datas.size();
				}

				@Override
				public Object[] getParams() {
					return new Object[] { mean };
				}
			};
		};
	}

	private double[] getMean(Collection<double[]> values) {
		if (values.isEmpty())
			return null;
		double[] mean = new double[values.stream().findFirst().get().length];
		for (double[] value : values)
			for (int i = 0; i < mean.length; ++i)
				mean[i] += value[i];
		for (int i = 0; i < mean.length; ++i)
			mean[i] /= values.size();
		return mean;
	}

}