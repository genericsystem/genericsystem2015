package org.genericsystem.cv.retriever;

import java.util.ArrayList;
import java.util.List;

import org.genericsystem.cv.Img;
import org.opencv.core.Mat;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;

public class Field extends AbstractField {

	private Field parent;
	private List<Field> children;

	private static final int LABELS_SIZE_THRESHOLD = 15;
	private static final double CONFIDENCE_THRESHOLD = 0.92;
	private boolean locked = false;

	private boolean child;

	public boolean isChild() {
		return child;
	}

	public void setChild(boolean child) {
		this.child = child;
	}

	public Field(Rect rect) {
		super(rect);
		this.parent = null;
		this.children = new ArrayList<>();
	}

	public Field(Field other) {
		super(other);
		if (other instanceof Field) {
			this.parent = other.parent;
			this.children = other.children;
			setFinal();
		}
	}

	@Override
	public void ocr(Img rootImg) {
		super.ocr(rootImg);
		if (attempts <= 3 || attempts % 5 == 0)
			consolidateOcr(false);
	}

	@Override
	public void merge(AbstractField field) {
		super.merge(field);
		if (field instanceof Field)
			setFinal();
	}

	@Override
	public void resetDeadCounter() {
		super.resetDeadCounter();
		setFinal();
	}

	public void drawLockedField(Img display, Mat homography) {
		if (locked)
			drawRect(display, getRectPointsWithHomography(homography), new Scalar(255, 172, 0), 2);
	}

	public void setFinal() {
		if (!locked)
			if (getLabelsSize() > LABELS_SIZE_THRESHOLD && getConfidence() > CONFIDENCE_THRESHOLD)
				this.locked = true;
	}

	public boolean isLocked() {
		return locked;
	}

	public void setChildren(List<Field> children) {
		this.children = children;
	}

	public void addChild(Field child) {
		children.add(child);
	}

	public void removeChild(Field child) {
		children.remove(child);
	}

	public boolean containsChild(Field field) {
		return children.contains(field);
	}

	public List<Field> getChildren() {
		return children;
	}

	public void addChildren(List<Field> children) {
		children.addAll(children);
	}

	public Field getParent() {
		return parent;
	}

	public void setParent(Field parent) {
		this.parent = parent;
	}

}