package org.genericsystem.cv.docPattern;

import java.util.List;
import java.util.stream.Collectors;

import org.opencv.core.Rect;

public class ZoneStream {

	private Rect rect;
	private List<String> labels;
	private String type;
	private List<String> authorizedValues;

	public static final String typeAttribute = "ATTRIBUTE";
	public static final String typeString = "STRING";
	public static final String typeFloat = "FLOAT";
	public static final String typeInteger = "INTEGER";

	public ZoneStream(Rect rect, List<String> labels) {
		super();
		this.rect = rect;
		this.labels = labels;
		this.init();
	}

	public Rect getRect() {
		return rect;
	}

	public void setRect(Rect rect) {
		this.rect = rect;
	}

	public List<String> getLabels() {
		return labels;
	}

	public void setLabels(List<String> labels) {
		this.labels = labels;
	}

	public String getType() {
		return this.type;
	}

	public List<String> getAuthorizedValues() {
		return this.authorizedValues;
	}

	public void init() {
		List<String> possibleValues = this.labels.stream().distinct().collect(Collectors.toList());
		if (possibleValues.size() == 1)
			this.type = typeAttribute;
		else {
			boolean isNumber = true;
			for (String s : possibleValues) {
				try {
					float f = Float.parseFloat(s);
				} catch (NumberFormatException e) {
					isNumber = false;
					break;
				}
			}
			boolean isInteger = false;
			if (isNumber) {
				isInteger = true;
				for (String s : possibleValues) {
					try {
						int i = Integer.parseInt(s);
					} catch (NumberFormatException e) {
						isInteger = false;
						break;
					}
				}
			}
			if (isInteger)
				this.type = typeInteger;
			else if (isNumber)
				this.type = typeFloat;
			else
				this.type = typeString;

		}
		if (possibleValues.size() < 0.5 * labels.size()) {
			this.authorizedValues = possibleValues;
		}
	}

	public boolean isLeft(ZoneStream zs) {
		if (this.rect.br().x < zs.rect.x)
			return true;
		else
			return false;
	}

	public boolean isAbove(ZoneStream zs) {
		if (this.rect.br().y < zs.rect.y)
			return true;
		else
			return false;
	}

	public double distance(ZoneStream zs) {
		double dx = zs.rect.x - this.rect.br().x;
		double dy = zs.rect.y - this.rect.br().y;
		return Math.pow(dx, 2) + Math.pow(dy, 2);
	}

	public String longestCommonPrefix(String a, String b) {
		int minLength = Math.min(a.length(), b.length());
		for (int i = 0; i < minLength; i++) {
			if (a.charAt(i) != b.charAt(i)) {
				return a.substring(0, i);
			}
		}
		return a.substring(0, minLength);
	}

	public String greatestCommonPrefix() {
		String longest = labels.get(0);
		for (int l = 1; l < labels.size() - 1; l++)
			longest = longestCommonPrefix(longest, labels.get(l));
		return longest;
	}

}
