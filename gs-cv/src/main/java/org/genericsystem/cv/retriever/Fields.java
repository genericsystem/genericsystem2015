package org.genericsystem.cv.retriever;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
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

	public void displayFieldsTree() {
		StringBuffer sb = new StringBuffer();
		sb.append("\n").append("--- FIELDS ---").append("\n");
		fields.forEach(field -> {
			sb.append(field.recursiveToString());
		});
		sb.append("\n").append("--- /FIELDS ---").append("\n");
		System.out.println(sb.toString());
	}

	public void merge(RectDetector rectDetector) {
		// Get the lists of rectangles
		List<GSRect> rects = RectToolsMapper.rectToGSRect(rectDetector.getFilteredRects(1d));
		List<GSRect> children = RectToolsMapper.rectToGSRect(rectDetector.getFilteredRects2(1d));

		// Remove the duplicates of rects in children
		rects.forEach(rect -> {
			Iterator<GSRect> it = children.iterator();
			while (it.hasNext()) {
				if (RectangleTools.isInCluster(rect, it.next(), 0.1))
					it.remove();
			}
		});

		// Increment the dead counter of each field
		fields.forEach(f -> f.incrementDeadCounter());

		// Loop over all the rectangles and try to find any matching field
		for (GSRect rect : rects) {
			List<Field> matches = findPossibleMatches(rect, 0.1);
			matches = cleanMatches(matches, rect);
			if (!matches.isEmpty()) {
				matches.forEach(f -> {
					logger.info(formatLog(f, rect));
					f.registerShift(RectangleTools.getShift(f.getRect(), rect));
					f.updateRect(rect);
					f.resetDeadCounter();
				});
			} else {
				logger.info("No match for {}. Creating a new Field", rect);
				Field f = new Field(rect);
				fields.add(f);
			}
		}
		// Remove the rectangles that could not be merged too many times
		removeUnmergedFields();

		// Merge the potential children
		mergeChildren(children);

		// Adjust the position of the parent's rects if no matches were found, based upon the mean shift of its children
		adjustUnmergedParents();
	}

	public void mergeChildren(List<GSRect> childrenRect) {
		List<GSRect> fieldsRects = fields.stream().map(f -> f.getRect()).collect(Collectors.toList());
		for (int i = 0; i < fieldsRects.size(); ++i) {
			Field parent = fields.get(i);
			GSRect rect = fieldsRects.get(i);

			List<GSRect> possibleChildrenRects = findChildrenRects(childrenRect, rect, 0.95);
			if (!possibleChildrenRects.isEmpty()) {
				logger.info("Found possible child(ren) for {}: {}", rect, possibleChildrenRects);
				possibleChildrenRects.forEach(childRect -> {
					List<Field> matches = findPossibleMatches(childRect, 0.1);
					if (!matches.isEmpty()) {
						matches.forEach(f -> {
							logger.info(formatLog(f, childRect));
							f.registerShift(RectangleTools.getShift(f.getRect(), childRect));
							setLinks(parent, f);
							f.updateRect(childRect);
							f.resetDeadCounter();
						});
					} else {
						logger.info("No match for child {}. Creating a new Field", childRect);
						Field f = new Field(childRect);
						setLinks(parent, f);
						fields.add(f);
					}
					childrenRect.remove(childRect);
				});
			}
		}
	}

	private List<GSRect> findChildrenRects(List<GSRect> children, GSRect putativeParent, double minArea) {
		return children.stream().filter(child -> RectangleTools.commonArea(child, putativeParent)[0] > minArea).collect(Collectors.toList());
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

		// Clean the fields recursively from the 'root' of each tree
		Set<Field> removes = new HashSet<>();
		fields.stream().filter(field -> field.isOrphan()).forEach(field -> removes.addAll(deleteRecursive(field, predicate)));
		removes.forEach(field -> {
			if (field.hasChildren())
				field.getChildren().forEach(child -> child.setParent(null));
			if (!field.isOrphan())
				field.getParent().removeChild(field);
			fields.remove(field);
		});
	}

	private Set<Field> deleteRecursive(Field field, Predicate<Field> predicate) {
		Set<Field> removes = new HashSet<>();
		if (predicate.test(field)) {
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
				removes.add(field);
			}
		} else
			System.out.print(". ");
		return removes;
	}

	private void setLinks(Field parent, Field child) {
		if (!child.isOrphan() && !child.getParent().equals(parent))
			logger.error("child's parent:\n" + child.getParent() + "\nparent:\n" + parent);
		child.setParent(parent);
		parent.addChildIfNotPresent(child);
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

	private String formatLog(Field field, GSRect rect) {
		double mergeArea = field.getRect().inclusiveArea(rect);
		StringBuffer sb = new StringBuffer();
		sb.append(String.format("Merging %s with %s (%.1f%% common area)", field.getRect(), rect, mergeArea * 100));
		if (field.getConsolidated() != null)
			sb.append(String.format(" -> %s", field.getConsolidated()));
		return sb.toString();
	}

	private List<Field> cleanMatches(List<Field> matches, GSRect rect) {
		List<Field> copy = new ArrayList<>(matches);
		// Remove the false positives
		copy = copy.stream().filter(f -> f.getRect().inclusiveArea(rect) > MIN_OVERLAP / 10).collect(Collectors.toList());
		if (copy.isEmpty()) {
			return Collections.emptyList();
		} else {
			// If there is more than one match, select only the best
			if (copy.size() > 1) {
				logger.debug("Multiple matches ({}), removing false positives", copy.size());
				// Remove the overlaps with less than 10% common area
				copy = copy.stream().filter(f -> f.getRect().inclusiveArea(rect) >= MIN_OVERLAP).collect(Collectors.toList());
				if (copy.size() > 1) {
					logger.warn("Still multiple matches ({}), selecting the maximum overlap", copy.size());
					copy = Arrays.asList(copy.stream().max((f1, f2) -> {
						double area1 = f1.getRect().inclusiveArea(rect);
						double area2 = f2.getRect().inclusiveArea(rect);
						return Double.compare(area1, area2);
					}).orElseThrow(IllegalStateException::new));
				}
			}
			return copy;
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