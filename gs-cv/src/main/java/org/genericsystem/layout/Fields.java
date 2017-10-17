package org.genericsystem.layout;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.utils.Converters;

public class Fields extends AbstractFields {
	private Mat lastHomography;
	private Mat lastRotation;

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public void merge(List<Rect> rects) {
		List<Field> oldFields = (List) fields;
		fields = rects.stream().map(Field::new).collect(Collectors.toList());

		if (lastHomography != null) {
			List<Point> virtualCenters = restabilize(oldFields.stream().map(Field::getCenter).collect(Collectors.toList()));

			for (int index = 0; index < oldFields.size(); index++) {
				if (oldFields.get(index).isConsolidated()) {
					Field oldField = oldFields.get(index);
					Field field = findNewField(virtualCenters.get(index));
					if (field != null) {
						field.merge(oldField);
						System.out.println("Merge : " + oldField.getConsolidated());
						// System.out.println(newPoints.get(index) + " " + field.center());
					} else {
						System.out.println("Can 't merge : " + oldField.getConsolidated() + " ");
						Rect newRect = findNewRect(oldField.getRect());
						fields.add(oldField.saveForLater(newRect));
					}
				}
			}
		}
	}

	private Field findNewField(Point pt) {
		return (Field) stream().filter(field -> field.contains(pt)).findFirst().orElse(null);
	}

	private Rect findNewRect(Rect rect) {
		List<Point> points = restabilize(Arrays.asList(rect.tl(), rect.br()));
		return new Rect(points.get(0), points.get(1));
	}

	private List<Point> restabilize(List<Point> originals) {
		MatOfPoint2f results = new MatOfPoint2f();
		Core.perspectiveTransform(Converters.vector_Point2f_to_Mat(originals), results, lastHomography);
		MatOfPoint2f rotated = new MatOfPoint2f();
		Core.transform(results, rotated, lastRotation);
		return rotated.toList();
	}

	public void storeLastHomography(Mat homography) {
		this.lastHomography = homography;
	}

	public void storeLastRotation(Mat rotation) {
		this.lastRotation = rotation;
	}

	// @Override
	// public void consolidateOcr(Img rootImg) {
	// long TS = System.currentTimeMillis();
	// stream().filter(AbstractField::needOcr).filter(f -> System.currentTimeMillis() - TS <= 100).forEach(f -> f.ocr(rootImg));
	// }

}