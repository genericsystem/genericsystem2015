package org.genericsystem.reinforcer;

import java.util.List;

public class DetectedContent {
	private final String name;
	private final String content;

	public DetectedContent(String name, String content) {
		this.name = name;
		this.content = content;
	}

	public String getContent() {
		return content;
	}

	@Override
	public String toString() {
		return "{ " + name + ": " + content + " }";
	}

	public static boolean found(List<DetectedContent> found, String searched) {
		return found.stream().anyMatch(c -> c.getContent().equals(searched));
	}
}
