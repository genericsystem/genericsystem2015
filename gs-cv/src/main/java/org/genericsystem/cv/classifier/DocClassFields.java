package org.genericsystem.cv.classifier;

import java.lang.invoke.MethodHandles;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.ClassifierUsingFields;
import org.opencv.core.Scalar;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Represent a class of documents. It contains the Fields, and all the descriptors needed to recognize a class.
 * 
 * @author Pierrik Lassalas
 */
public class DocClassFields {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private String classDirectory;
	private final DocFields fields;

	public DocClassFields() {
		this(new DocFields(), null);
	}

	public DocClassFields(DocFields fields) {
		this(fields, null);
	}

	public DocClassFields(DocFields fields, String classDirectory) {
		this.fields = fields;
		this.classDirectory = classDirectory;
	}

	public void consolidateOcr(Img img) {
		fields.consolidateOcr(img);
	}

	public void merge(Img img) {
		fields.buildFields(ClassifierUsingFields.detectRects(img));
	}

	public Img drawOcr(Img img, Scalar scalar, int thickness) {
		Img imgCopy = new Img(img.getSrc(), true);
		fields.drawOcrPerspectiveInverse(imgCopy, scalar, thickness);
		return imgCopy;
	}

	public String getClassDirectory() {
		return String.valueOf(classDirectory);
	}

	public void setClassDirectory(String classDirectory) {
		this.classDirectory = classDirectory;
	}
}
