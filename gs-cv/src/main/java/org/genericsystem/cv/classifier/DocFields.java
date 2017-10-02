package org.genericsystem.cv.classifier;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.cv.Img;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.utils.Converters;

public class DocFields {
	private List<DocField> fields;
	private Mat lastHomography;
	private Mat lastRotation;

	public DocFields() {
		this.fields = new ArrayList<>();
	}

	public DocFields(List<DocField> fields) {
		this.fields = fields;
	}

	public void merge(List<Rect> rects) {
		List<DocField> oldFields = fields;
		fields = rects.stream().map(rect -> new DocField(rect)).collect(Collectors.toList());
		if (lastHomography != null) {
			List<Point> newPoints = restabilize(oldFields.stream().map(f -> f.center()).collect(Collectors.toList()));
			for (int index = 0; index < oldFields.size(); index++)
				if (oldFields.get(index).isConsolidated()) {
					DocField field = findNewField(newPoints.get(index));
					if (field != null) {
						field.merge(oldFields.get(index));
						System.out.println("Merge : " + oldFields.get(index).getConsolidated());
						System.out.println(newPoints.get(index) + " " + field.center());
					} else
						System.out.println("Can't merge : " + oldFields.get(index).getConsolidated() + " ");
				}
		}
	}

	private DocField findNewField(Point pt) {
		for (DocField field : fields) {
			if (field.contains(pt))
				return field;
		}
		return null;
	}

	private List<Point> restabilize(List<Point> originals) {
		Mat src = Converters.vector_Point2f_to_Mat(originals);
		MatOfPoint2f results = new MatOfPoint2f();
		Core.perspectiveTransform(src, results, lastHomography);
		MatOfPoint2f rotated = new MatOfPoint2f();
		Core.transform(results, rotated, lastRotation);
		src.release();
		results.release();
		return rotated.toList();
	}

	public void storeLastHomography(Mat homography) {
		this.lastHomography = homography;
	}

	public void storeLastRotation(Mat rotation) {
		this.lastRotation = rotation;
	}

	public void drawOcrPerspectiveInverse(Img display, Scalar color, int thickness) {
		consolidatedFieldStream().forEach(field -> field.drawOcrPerspectiveInverse(display, color, thickness));
	}

	public void drawConsolidated(Img stabilizedDisplay) {
		consolidatedFieldStream().forEach(field -> field.draw(stabilizedDisplay));

	}

	public void consolidateOcr(Img rootImg) {
		fields.stream().filter(DocField::needOcr).forEach(f -> f.ocr(rootImg));
	}

	public Stream<DocField> consolidatedFieldStream() {
		return fields.stream().filter(f -> f.isConsolidated());
	}
}