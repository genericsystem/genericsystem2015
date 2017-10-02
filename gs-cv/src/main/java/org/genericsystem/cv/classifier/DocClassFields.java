package org.genericsystem.cv.classifier;

import java.lang.invoke.MethodHandles;

import org.genericsystem.cv.Img;
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

	private final DocFields fields;

	public DocClassFields() {
		this.fields = new DocFields();
	}

	public DocClassFields(DocFields fields) {
		this.fields = fields;
	}

	public void consolidateOcr(Img img) {
		fields.consolidateOcr(img);
	}

	public void merge(Img img) {
		fields.merge(ClassifierUsingFields.detectRects(img));
	}

	public Img drawOcr(Img img, Scalar scalar, int thickness) {
		Img imgCopy = new Img(img.getSrc(), true);
		fields.drawOcrPerspectiveInverse(imgCopy, scalar, thickness);
		return imgCopy;
	}
}
