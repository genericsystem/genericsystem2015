package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map.Entry;
import java.util.Random;
import java.util.TreeMap;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Size;
import org.opencv.utils.Converters;

public class ReferenceManager {
	private static final Mat IDENTITY_MAT = Mat.eye(new Size(3, 3), CvType.CV_64F);

	private TreeMap<ImgDescriptor, Mat> toReferenceGraphy = new TreeMap<>(new Comparator<ImgDescriptor>() {

		@Override
		public int compare(ImgDescriptor d1, ImgDescriptor d2) {
			return new Long(d1.getTimeStamp()).compareTo(new Long(d2.getTimeStamp()));
		}

	});

	private ImgDescriptor reference;
	// private List<Rect> referenceRects = new ArrayList<>();
	private Fields fields = new Fields();
	private Size frameSize;

	public ReferenceManager(Size frameSize) {
		this.frameSize = frameSize;
	}

	public void submit(ImgDescriptor newImgDescriptor, List<Rect> detectedrects) {
		if (reference == null) {
			toReferenceGraphy.put(newImgDescriptor, IDENTITY_MAT);
			reference = newImgDescriptor;
			return;
		}

		int bestMatchingPointsCount = 0;
		ImgDescriptor bestImgDescriptor = null;
		Reconciliation bestReconciliation = null;

		ImgDescriptor lastStored = toReferenceGraphy.lastKey();
		Reconciliation reconciliationWithlast = newImgDescriptor.computeReconciliation(lastStored);
		if (reconciliationWithlast != null) {
			bestReconciliation = reconciliationWithlast;
			bestImgDescriptor = lastStored;
		} else {
			Reconciliation reconciliationWithRef = newImgDescriptor.computeReconciliation(reference);
			if (reconciliationWithRef != null) {
				bestReconciliation = reconciliationWithRef;
				bestImgDescriptor = reference;
			} else {
				int reconciliationTries = 0;
				List<ImgDescriptor> list = getRandomPool(lastStored, reference);
				Random randomGenerator = new Random();
				while (!list.isEmpty() && reconciliationTries < 5) {
					ImgDescriptor randomImgDescriptor = list.get(randomGenerator.nextInt(list.size()));
					Reconciliation reconciliation = newImgDescriptor.computeReconciliation(randomImgDescriptor);
					if (reconciliation != null) {
						int matchingPointsCount = reconciliation.getPts().size();
						if (matchingPointsCount >= bestMatchingPointsCount) {
							bestMatchingPointsCount = matchingPointsCount;
							bestReconciliation = reconciliation;
							bestImgDescriptor = randomImgDescriptor;
						}
					}
					reconciliationTries++;
				}
			}
		}
		if (bestReconciliation == null) {
			System.out.println("no reconciliation found");
			if (toReferenceGraphy.size() <= 1) {
				toReferenceGraphy.clear();
				toReferenceGraphy.put(newImgDescriptor, IDENTITY_MAT);
				reference = newImgDescriptor;
			}
			return;
		}
		Mat homographyToReference = new Mat();
		Core.gemm(bestReconciliation.getHomography(), toReferenceGraphy.get(bestImgDescriptor), 1, new Mat(), 0, homographyToReference);
		toReferenceGraphy.put(newImgDescriptor, homographyToReference);
		consolidate(shift(detectedrects, homographyToReference), frameSize);
		updateReference();
		cleanReferenceNeighbours();
	}

	private List<ImgDescriptor> getRandomPool(ImgDescriptor lastStored, ImgDescriptor reference) {
		List<ImgDescriptor> randomPool = new ArrayList<>(toReferenceGraphy.keySet());
		randomPool.remove(lastStored);
		randomPool.remove(reference);
		return randomPool;
	}

	private void cleanReferenceNeighbours() {
		if (toReferenceGraphy.size() > 30) {
			double bestDistance = Double.MAX_VALUE;
			ImgDescriptor closestDescriptor = null;
			for (Entry<ImgDescriptor, Mat> entry : toReferenceGraphy.entrySet()) {
				if (!entry.getKey().equals(reference)) {
					double distance = distance(entry.getValue());
					if (distance < bestDistance) {
						bestDistance = distance;
						closestDescriptor = entry.getKey();
					}
				}
			}
			toReferenceGraphy.remove(closestDescriptor);
		}
	}

	private void updateReference() {
		ImgDescriptor consensualDescriptor = findConsensualDescriptor();
		if (reference != consensualDescriptor) {
			System.out.println("Change reference");
			Mat homoInv = toReferenceGraphy.get(consensualDescriptor).inv();
			for (Entry<ImgDescriptor, Mat> entry : toReferenceGraphy.entrySet()) {
				if (!entry.getKey().equals(consensualDescriptor)) {
					Mat result = new Mat();
					Core.gemm(entry.getValue(), homoInv, 1, new Mat(), 0, result);
					toReferenceGraphy.put(entry.getKey(), result);
				} else
					toReferenceGraphy.put(entry.getKey(), IDENTITY_MAT);
			}
			reference = consensualDescriptor;
			fields.shift(homoInv);
		}
	}

	private ImgDescriptor findConsensualDescriptor() {
		double bestDistance = Double.MAX_VALUE;
		ImgDescriptor bestDescriptor = null;
		for (Entry<ImgDescriptor, Mat> entry : toReferenceGraphy.entrySet()) {
			double distance = 0;
			for (Entry<ImgDescriptor, Mat> entry2 : toReferenceGraphy.entrySet()) {
				if (!entry.getKey().equals(entry2.getKey())) {
					Mat betweenHomography = new Mat();
					Core.gemm(entry.getValue(), entry2.getValue().inv(), 1, new Mat(), 0, betweenHomography);
					distance += distance(betweenHomography);
				}
			}
			if (distance < bestDistance) {
				bestDistance = distance;
				bestDescriptor = entry.getKey();
			}
		}
		return bestDescriptor;

		// double minArea = Double.MAX_VALUE;
		// ImgDescriptor bestDescriptor = null;
		// for (Entry<ImgDescriptor, Mat> entry : toReferenceGraphy.entrySet()) {
		// double surface = entry.getKey().getSurface();
		// if (surface < minArea) {
		// minArea = surface;
		// bestDescriptor = entry.getKey();
		// }
		// }
		// return bestDescriptor;
	}

	public List<Rect> getResizedFieldsRects() {
		List<Rect> referenceRects = getReferenceRects();
		if (referenceRects.isEmpty())
			return Collections.emptyList();
		double minX = referenceRects.stream().mapToDouble(rect -> rect.tl().x).min().getAsDouble();
		double maxX = referenceRects.stream().mapToDouble(rect -> rect.br().x).max().getAsDouble();
		double minY = referenceRects.stream().mapToDouble(rect -> rect.tl().y).min().getAsDouble();
		double maxY = referenceRects.stream().mapToDouble(rect -> rect.br().y).max().getAsDouble();

		double horizontalRatio = frameSize.width / (maxX - minX) > 1 ? 1 : frameSize.width / (maxX - minX);
		double verticalRatio = frameSize.height / (maxY - minY) > 1 ? 1 : frameSize.height / (maxY - minY);
		return rescale(shift(referenceRects, minX, minY), Math.min(horizontalRatio, verticalRatio));
	}

	private List<Rect> rescale(List<Rect> rects, double ratio) {
		return rects.stream().map(r -> rescale(r, ratio)).collect(Collectors.toList());
	}

	public Rect rescale(Rect rect, double ratio) {
		return new Rect((int) (rect.x * ratio), (int) (rect.y * ratio), (int) (rect.width * ratio), (int) (rect.height * ratio));
	}

	private List<Rect> shift(List<Rect> rects, double minX, double minY) {
		return rects.stream().map(r -> shift(r, minX, minY)).collect(Collectors.toList());
	}

	private Rect shift(Rect rect, double minX, double minY) {
		return new Rect((int) (rect.x - minX), (int) (rect.y - minY), rect.width, rect.height);
	}

	private static class Field {

		private Rect rect;
		private int level = 0;

		Field(Rect rect) {
			this.rect = rect;
		}

		public int getLevel() {
			return level;
		}

		public void decrease() {
			level--;
		}

		public void increase() {
			level++;
		}

		public boolean isEnoughOverlapping(Rect shiftedRect, int pts) {
			return (Math.abs(rect.tl().x - shiftedRect.tl().x) < pts) && (Math.abs(rect.tl().y - shiftedRect.tl().y) < pts) && (Math.abs(rect.br().x - shiftedRect.br().x) < pts) && (Math.abs(rect.br().y - shiftedRect.br().y) < pts);
		}

		public boolean isOverlapping(Rect other) {
			return rect.tl().x <= other.br().x && other.tl().x <= rect.br().x && rect.tl().y <= other.br().y && other.tl().y <= rect.br().y;
		}

		public void dump(Rect shiftedRect, double dumpingSize) {
			this.rect = new Rect(new Point(Math.round(rect.tl().x * ((dumpingSize - 1) / dumpingSize) + shiftedRect.tl().x / dumpingSize), Math.round(rect.tl().y * ((dumpingSize - 1) / dumpingSize) + shiftedRect.tl().y / dumpingSize)),
					new Point(Math.round(rect.br().x * (dumpingSize - 1) / dumpingSize + shiftedRect.br().x / dumpingSize), Math.round(rect.br().y * (dumpingSize - 1) / dumpingSize + shiftedRect.br().y / dumpingSize)));
		}

		public Rect getRect() {
			return rect;
		}

		public void shift(Mat homography) {
			Mat original = Converters.vector_Point2d_to_Mat(Arrays.asList(rect.tl(), rect.br()));
			Mat results = new Mat();
			Core.perspectiveTransform(original, results, homography);
			List<Point> res = new ArrayList<>();
			Converters.Mat_to_vector_Point2d(results, res);
			rect = new Rect(res.get(0), res.get(1));
		}

		public boolean contains(Rect shiftedRect) {
			return (rect.tl().x <= shiftedRect.tl().x && rect.tl().y <= shiftedRect.tl().y && rect.br().x >= shiftedRect.br().x && rect.br().y >= shiftedRect.br().y);
		}

		public boolean isInner(Rect shiftedRect) {
			return (rect.tl().x >= shiftedRect.tl().x && rect.tl().y >= shiftedRect.tl().y && rect.br().x <= shiftedRect.br().x && rect.br().y <= shiftedRect.br().y);
		}

		public double getX() {
			return rect.x;
		}
	}

	private static class Fields {
		private List<Field> fieldsList = new ArrayList<>();

		public void clean(Predicate<Field> predicate) {
			fieldsList.removeIf(predicate);
		}

		// public double getMinX() {
		// double minX = Double.MAX_VALUE;
		// for (Field f : fieldsList) {
		// if (f.getRect().tl().x < minX)
		// minX = f.getRect().tl().x;
		// }
		// return minX;
		// }
		//
		// public double getMaxX() {
		// double maxX = 0.0;
		// for (Field f : fieldsList) {
		// if (f.getRect().br().x > maxX)
		// maxX = f.getRect().br().x;
		// }
		// return maxX;
		// }
		//
		// public double getMinY() {
		// double minY = Double.MAX_VALUE;
		// for (Field f : fieldsList) {
		// if (f.getRect().tl().y < minY)
		// minY = f.getRect().tl().y;
		// }
		// return minY;
		// }
		//
		// public double getMaxY() {
		// double maxY = 0.0;
		// for (Field f : fieldsList) {
		// if (f.getRect().br().y > maxY)
		// maxY = f.getRect().br().y;
		// }
		// return maxY;
		// }

		public void shift(Mat homoInv) {
			fieldsList.forEach(field -> field.shift(homoInv));
		}

		public List<Field> findOverlapingFields(Rect shiftedRect) {
			return fieldsList.stream().filter(field -> field.isOverlapping(shiftedRect)).collect(Collectors.toList());
		}

		public void add(Field field) {
			fieldsList.add(field);
		}

		public List<Rect> getPositiveLevelRects() {
			return fieldsList.stream().filter(field -> field.getLevel() >= 0).map(field -> field.getRect()).collect(Collectors.toList());
		}

		public void decreaseAll() {
			fieldsList.forEach(field -> field.decrease());
		}

		public List<Field> getFields() {
			return fieldsList;
		}
	}

	private void consolidate(List<Rect> shiftedRects, Size frameSize) {
		Rect frameRect = new Rect(0, 0, (int) frameSize.width, (int) frameSize.height);
		List<Field> displayedFields = fields.getFields().stream().filter(f -> f.isInner(frameRect)).collect(Collectors.toList());
		for (Rect shiftedRect : shiftedRects) {
			List<Field> targetFields = fields.findOverlapingFields(shiftedRect);
			boolean toAdd = true;
			for (Field targetField : targetFields) {
				if (targetField.isEnoughOverlapping(shiftedRect, 3)) {
					targetField.dump(shiftedRect, 3);
					targetField.increase();
					targetField.increase();
					toAdd = false;
				}
			}
			if (toAdd) {
				Field newField = new Field(shiftedRect);
				newField.increase();
				newField.increase();
				fields.add(newField);
			}
		}
		decreaseAll(displayedFields);
		fields.clean(targetField -> targetField.getLevel() < -5);
	}

	private void decreaseAll(List<Field> displayedFields) {
		displayedFields.forEach(f -> f.decrease());
	}

	public List<Rect> getReferenceRects() {
		return fields.getPositiveLevelRects();
	}

	private List<Rect> shift(List<Rect> detectedRects, Mat homography) {
		List<Point> pts = new ArrayList<>(2 * detectedRects.size());
		detectedRects.forEach(rect -> {
			pts.add(rect.tl());
			pts.add(rect.br());
		});
		List<Point> transform = transform(pts, homography);
		List<Rect> result = new ArrayList<>(detectedRects.size());
		for (int i = 0; i < transform.size(); i += 2)
			result.add(new Rect(transform.get(i), transform.get(i + 1)));
		return result;
	}

	private List<Point> transform(List<Point> originals, Mat homography) {
		Mat original = Converters.vector_Point2d_to_Mat(originals);
		Mat results = new Mat();
		Core.perspectiveTransform(original, results, homography);
		List<Point> res = new ArrayList<>();
		Converters.Mat_to_vector_Point2d(results, res);
		return res;
	}

	// public Reconciliation computeHomography(ImgDescriptor newDescriptor) {
	// return newDescriptor.computeReconciliation(getReference());
	// }

	private double distance(Mat betweenHomography) {
		List<Point> originalPoints = Arrays.asList(new Point[] { new Point(0, 0), new Point(frameSize.width, 0), new Point(frameSize.width, frameSize.height), new Point(0, frameSize.height) });
		List<Point> points = transform(originalPoints, betweenHomography);
		return distance(points, originalPoints);
	}

	private double distance(List<Point> newPointList, List<Point> oldPointList) {
		double error = 0.0;
		for (int i = 0; i < oldPointList.size(); i++) {
			double deltaX = newPointList.get(i).x - oldPointList.get(i).x;
			double deltaY = newPointList.get(i).y - oldPointList.get(i).y;
			error += deltaX * deltaX + deltaY * deltaY;
		}
		return Math.sqrt(error) / oldPointList.size();
	}

	public ImgDescriptor getReference() {
		return reference;
	}

	public void clear() {
		reference = null;
		toReferenceGraphy.clear();
		fields.clean(field -> true);
	}
}
