package org.genericsystem.reinforcer.tools;

import java.util.List;
import java.util.Map;

public class JsonLabel {

	public static class JsonLabels {
		private List<JsonLabel> fields;

		public List<JsonLabel> getFields() {
			return fields;
		}
	}

	private GSRect ocrRect;
	private Map<String, Integer> labels;
	private List<JsonLabel> children;

	public GSRect getOcrRect() {
		return ocrRect;
	}

	public Map<String, Integer> getLabels() {
		return labels;
	}

	public List<JsonLabel> getChildren() {
		return children;
	}
}
