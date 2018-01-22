package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Map.Entry;
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

		Reconciliation reconciliationWithRef = newImgDescriptor.computeReconciliation(reference);
		if (reconciliationWithRef != null) {
			bestReconciliation = reconciliationWithRef;
			bestImgDescriptor = reference;
		} else {
			ImgDescriptor lastStored = toReferenceGraphy.lastKey();
			Reconciliation reconciliationWithlast = newImgDescriptor.computeReconciliation(lastStored);
			if (reconciliationWithlast != null) {
				bestReconciliation = reconciliationWithlast;
				bestImgDescriptor = lastStored;
			} else {
				for (ImgDescriptor imgDescriptor : toReferenceGraphy.keySet()) {
					Reconciliation reconciliation = newImgDescriptor.computeReconciliation(imgDescriptor);
					if (reconciliation != null) {
						int matchingPointsCount = reconciliation.getPts().size();
						if (matchingPointsCount >= bestMatchingPointsCount) {
							bestMatchingPointsCount = matchingPointsCount;
							bestReconciliation = reconciliation;
							bestImgDescriptor = imgDescriptor;
						}
					}
				}
			}
		}
		if (bestReconciliation == null) {
			// System.out.println("map size: " +toReferenceGraphy.size());
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
		consolidate(shift(detectedrects, homographyToReference));
		updateReference();
		cleanReferenceNeighbours();
	}

	private void cleanReferenceNeighbours() {
		if (toReferenceGraphy.size() > 6) {
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
		} else
			System.out.println("No change reference");
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
			this.rect = new Rect(new Point(rect.tl().x * ((dumpingSize - 1) / dumpingSize) + shiftedRect.tl().x / dumpingSize, rect.tl().y * ((dumpingSize - 1) / dumpingSize) + shiftedRect.tl().y / dumpingSize),
					new Point(rect.br().x * (dumpingSize - 1) / dumpingSize + shiftedRect.br().x / dumpingSize, rect.br().y * (dumpingSize - 1) / dumpingSize + shiftedRect.br().y / dumpingSize));
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
			return (rect.tl().x <= shiftedRect.tl().x && rect.tl().y <= shiftedRect.tl().y && rect.br().x >= shiftedRect.tl().x && rect.br().y >= shiftedRect.tl().y);
		}

		public boolean isInner(Rect shiftedRect) {
			return (rect.tl().x >= shiftedRect.tl().x && rect.tl().y >= shiftedRect.tl().y && rect.br().x <= shiftedRect.tl().x && rect.br().y <= shiftedRect.tl().y);
		}

	}

	private static class Fields {
		private List<Field> fieldsList = new ArrayList<>();

		public void clean(Predicate<Field> predicate) {
			fieldsList.removeIf(predicate);
		}

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
	}

	private void consolidate(List<Rect> shiftedRects) {
		for (Rect shiftedRect : shiftedRects) {
			List<Field> targetFields = fields.findOverlapingFields(shiftedRect);
			if (targetFields.isEmpty()) {
				Field newField = new Field(shiftedRect);
				newField.increase();
				newField.increase();
				fields.add(newField);
			} else {
				if (targetFields.size() == 1) {
					Field targetField = targetFields.iterator().next();
					if (targetField.isEnoughOverlapping(shiftedRect, 8)) {
						targetField.dump(shiftedRect, 3);
						targetField.increase();
						targetField.increase();
						targetField.increase();
					} else
						targetField.decrease();
				} else
					for (Field targetField : targetFields) {
						if ((!targetField.contains(shiftedRect) || !targetField.isInner(shiftedRect)))
							if (!targetField.isEnoughOverlapping(shiftedRect, 8))
								targetField.decrease();
					}
			}
		}
		fields.decreaseAll();
		fields.clean(targetField -> targetField.getLevel() < -10);
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

	public Reconciliation computeHomography(ImgDescriptor newDescriptor) {
		return newDescriptor.computeReconciliation(getReference());
	}

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
