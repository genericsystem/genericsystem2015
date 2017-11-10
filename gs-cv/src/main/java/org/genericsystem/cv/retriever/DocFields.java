package org.genericsystem.cv.retriever;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.ParallelTasks;
import org.genericsystem.reinforcer.tools.GSRect;
import org.opencv.core.Scalar;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.json.Json;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

public class DocFields extends AbstractFields<DocField> {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final String FIELDS = "fields";

	public DocFields() {
		super();
	}

	public DocFields(List<DocField> fields) {
		super(fields);
	}

	public static DocFields of(List<GSRect> rects) {
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

	public void buildFields(List<GSRect> rects) {
		int[] counter = new int[] { 0 };
		fields = rects.stream().map(rect -> new DocField(counter[0]++, rect)).collect(Collectors.toList());
	}

	public Img annotateImage(final Img img, final double fontScale, final Scalar color, final int thickness) {
		Img annotated = new Img(img.getSrc(), true);
		stream().forEach(field -> field.annotateImage(annotated, fontScale, color, thickness));
		return annotated;
	}

	@Override
	public void performOcr(Img rootImg) {
		// Need to process all fields for documents
		Iterator<DocField> it = fields.iterator();
		while (it.hasNext()) {
			ParallelTasks tasks = new ParallelTasks();
			for (int i = 0; i < tasks.getCounter(); ++i)
				if (it.hasNext())
					tasks.add(() -> it.next().ocr(rootImg));
			try {
				tasks.run();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	}

}