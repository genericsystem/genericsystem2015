package org.genericsystem.cv.retriever;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.cv.Img;
import org.genericsystem.reinforcer.tools.GSRect;
import org.opencv.core.Mat;
import org.opencv.core.Scalar;

import io.vertx.core.json.JsonObject;

public abstract class AbstractFields<F extends AbstractField> implements Iterable<F> {

	protected List<F> fields;
	protected static final double MIN_SIMILARITY = 0.90;
	protected static final double OVERLAP_THRESHOLD = 0.30;
	protected static final double OVERLAP_CONFIDENCE = 0.90;
	protected static final String FIELDS = "fields";

	public AbstractFields() {
		this.fields = new ArrayList<>();
	}

	public AbstractFields(List<F> fields) {
		this.fields = fields;
	}

	protected List<F> findMatchingFieldsWithConfidence(F field, double threshold) {
		return fields.stream().filter(f -> f.overlapsMoreThanThresh(field.getRect(), threshold)).collect(Collectors.toList());
	}

	protected List<F> findPossibleMatches(GSRect rect, double epsilon) {
		return fields.stream().filter(f -> f.isClusteredWith(rect, epsilon)).collect(Collectors.toList());
	}

	public abstract void performOcr(Img rootImg);

	public void drawOcrPerspectiveInverse(Img display, Mat homography, int thickness) {
		stream().forEach(field -> field.drawOcrPerspectiveInverse(display, homography, thickness));
	}

	public void drawConsolidated(Img stabilizedDisplay) {
		consolidatedFieldStream().forEach(field -> field.drawRect(stabilizedDisplay, new Scalar(0, 255, 0), 1));
	}

	public Stream<F> consolidatedFieldStream() {
		return stream().filter(f -> f.isConsolidated());
	}

	public Stream<F> stream() {
		return fields.stream();
	}

	@Override
	public Iterator<F> iterator() {
		return fields.iterator();
	}

	public int size() {
		return fields.size();
	}

	public JsonObject toJsonObject() {
		return new JsonObject().put(FIELDS, fields);
	}

	public List<F> getFields(){
		return fields;
	}
}
