package org.genericsystem.cv.classifier;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.cv.Img;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.json.Json;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

public class DocFields implements Iterable<DocField> {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final String FIELDS = "fields";

	private List<DocField> fields;

	public DocFields() {
		this.fields = new ArrayList<>();
	}

	public DocFields(List<DocField> fields) {
		this.fields = fields;
	}

	public static DocFields of(List<Rect> rects) {
		DocFields fields = new DocFields();
		fields.buildFields(rects);
		return fields;
	}

	public static DocFields of(JsonObject jsonFields) {
		List<DocField> list = new ArrayList<>();
		JsonArray array = jsonFields.getJsonArray(FIELDS);
		array.forEach(field -> {
			try {
				list.add((DocField) field);
			} catch (Exception e) {
				logger.debug("Unable to cast {} as DocField ({}). Using Json.decodeValue instead.", field, e.getMessage());
				DocField f = Json.decodeValue(((JsonObject) field).encode(), DocField.class);
				list.add(f);
			}
		});
		DocFields fields = new DocFields(list);
		return fields;
	}

	public JsonObject toJsonObject() {
		return new JsonObject().put(FIELDS, fields);
	}

	public void buildFields(List<Rect> rects) {
		AtomicInteger counter = new AtomicInteger(0);
		fields = rects.stream().map(rect -> new DocField(counter.incrementAndGet(), rect)).collect(Collectors.toList());
	}

	public Img annotateImage(final Img img, final double fontScale, final Scalar color, final int thickness) {
		Img annotated = new Img(img.getSrc(), true);
		stream().forEach(field -> field.annotateImage(annotated, fontScale, color, thickness));
		return annotated;
	}

	public void drawOcrPerspectiveInverse(Img display, Scalar color, int thickness) {
		consolidatedFieldStream().forEach(field -> field.drawOcrPerspectiveInverse(display, color, thickness));
	}

	public void drawConsolidated(Img stabilizedDisplay) {
		consolidatedFieldStream().forEach(field -> field.drawRect(stabilizedDisplay, new Scalar(0, 0, 255), 2));
	}

	public void consolidateOcr(Img rootImg) {
		fields.stream().filter(DocField::needOcr).forEach(f -> f.ocr(rootImg));
	}

	public Stream<DocField> consolidatedFieldStream() {
		return fields.stream().filter(f -> f.isConsolidated());
	}

	public Stream<DocField> stream() {
		return fields.stream();
	}

	public Stream<DocField> parallelStream() {
		return fields.parallelStream();
	}

	public int size() {
		return fields.size();
	}

	@Override
	public Iterator<DocField> iterator() {
		return fields.iterator();
	}

}