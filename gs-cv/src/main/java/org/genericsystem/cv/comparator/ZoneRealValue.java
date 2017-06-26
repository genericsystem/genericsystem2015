package org.genericsystem.cv.comparator;

/**
 * Utility class to extract the real values for each zone from a json file.
 * 
 * @author Pierrik Lassalas
 *
 */
public class ZoneRealValue {
	private int num;
	private String text;

	public ZoneRealValue() {
	}

	public ZoneRealValue(int num, String text) {
		this.num = num;
		this.text = text;
	}

	public int getNum() {
		return num;
	}

	public void setNum(int num) {
		this.num = num;
	}

	public String getText() {
		return text;
	}

	public void setText(String text) {
		this.text = text;
	}

}
