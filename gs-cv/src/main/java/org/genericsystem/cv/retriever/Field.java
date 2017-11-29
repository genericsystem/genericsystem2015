package org.genericsystem.cv.retriever;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.genericsystem.cv.Img;
import org.genericsystem.reinforcer.tools.GSRect;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;

public class Field extends AbstractField {

	private Field parent;
	private final List<Field> children;

	private static final int LABELS_SIZE_THRESHOLD = 10;
	private static final double CONFIDENCE_THRESHOLD = 0.92;
	private static final double LOCK_THRESHOLD = 0.90;

	private int deadCounter;
	private double locklLevel;

	public Field(GSRect rect) {
		super(rect);
		this.parent = null;
		this.children = new ArrayList<>();
		this.deadCounter = 0;
		this.locklLevel = 0;
	}

	public Field(GSRect rect, Field parent) {
		this(rect);
		updateParent(parent);
	}

	public String recursiveToString() {
		StringBuffer sb = new StringBuffer();
		this.recursiveToString(sb, 0);
		return sb.append("\n").toString();
	}

	public void recursiveToString(StringBuffer sb, int depth) {
		if (depth > 8)
			return;
		sb.append("depth: ").append(depth).append(": ").append(rect);
		if (isConsolidated())
			sb.append(" -> ").append(getConsolidated());
		if (!children.isEmpty()) {
			depth++;
			for (Field child : children) {
				sb.append("\n");
				for (int i = 0; i < depth; ++i)
					sb.append("  ");
				child.recursiveToString(sb, depth);
			}
		}
	}

	public Field findPotentialParent(GSRect rect) {
		if (!rect.isInside(getRect()))
			return null;
		for (Field child : children) {
			Field candidate = child.findPotentialParent(rect);
			if (candidate != null)
				return candidate;
		}
		return this;
	}

	@Override
	public String ocr(Img rootImg) {
		String ocr = super.ocr(rootImg);
		if (attempts <= 3 || attempts % 5 == 0) {
			Stats.beginTask("ocr plasty");
			consolidateOcr(false);
			Stats.endTask("ocr plasty");
		}
		return ocr;
	}

	@Override
	public void consolidateOcr(boolean force) {
		super.consolidateOcr(force);
		adjustLockLevel(getConfidence() > CONFIDENCE_THRESHOLD ? 1 : -0.5);
	}

	@Override
	public void drawDebugText(Img display, Point[] targets, Scalar color, int thickness) {
		String conf = String.format("%.3f", getLockLevel());
		Point topCenter = new Point((targets[0].x + targets[1].x) / 2, (targets[0].y + targets[1].y) / 2);
		double l = Math.sqrt(Math.pow(targets[0].x - topCenter.x, 2) + Math.pow(targets[0].y - topCenter.y, 2));
		Imgproc.putText(display.getSrc(), conf, new Point(topCenter.x - l, topCenter.y - 12), Core.FONT_HERSHEY_TRIPLEX, 0.35, color);
	}

	public void draw(Img display, int thickness) {
		if (needText(display))
			drawDebugText(display, new Scalar(0, 64, 255), thickness);
		drawRect(display, deadCounter == 0 ? new Scalar(0, 255, 0) : new Scalar(0, 0, 255), thickness);
	}

	public void drawWithHomography(Img display, Mat homography, int thickness) {
		if (needRect(display)) {
			drawRect(display, getRectPointsWithHomography(homography), drawAsLocked() ? new Scalar(0, 255, 0) : new Scalar(0, 0, 255), thickness);
			drawText(display, getRectPointsWithHomography(homography), new Scalar(0, 255, 0), thickness);
		}
	}

	private boolean drawAsLocked() {
		return isOrphan() || (!isOrphan() && parent.getDeadCounter() != 0) ? isLocked() : false;
	}

	private boolean needRect(Img display) {
		GSRect imgRect = new GSRect(0, 0, display.width(), display.height());
		return !isOrphan() && parent.getRect().isInside(imgRect) ? false : rect.isInside(imgRect);
	}

	private boolean needText(Img display) {
		return deadCounter == 0 && needRect(display);
	}

	public boolean isOnDisplay(Img display) {
		GSRect imgRect = new GSRect(0, 0, display.width(), display.height());
		return imgRect.isOverlapping(rect);
	}

	public boolean isLocked() {
		return getLockLevel() >= LOCK_THRESHOLD;
	}

	public double getLockLevel() {
		return Math.tanh(locklLevel);
	}

	public void adjustLockLevel(double step) {
		this.locklLevel += 0.2 * step;
	}

	public void resetDeadCounter() {
		deadCounter = 0;
	}

	public void incrementDeadCounter() {
		deadCounter++;
	}

	public int getDeadCounter() {
		return deadCounter;
	}

	public boolean addChildIfNotPresent(Field child) {
		return children.add(child);
	}

	public boolean removeChild(Field child) {
		return children.remove(child);
	}

	// public boolean containsChild(Field field) {
	// return children.stream().anyMatch(child -> child.getRect().inclusiveArea(field.getRect()) > 0.95);
	// }

	public List<Field> getChildren() {
		return children;
	}

	public Field getParent() {
		return parent;
	}

	public List<Field> getSiblings() {
		if (parent == null)
			return Collections.emptyList();
		return parent.getChildren().stream().filter(child -> this != child).collect(Collectors.toList());
	}

	public void setParent(Field parent) {
		this.parent = parent;
	}

	public void updateParent(Field parent) {
		setParent(parent);
		if (parent != null)
			this.parent.addChildIfNotPresent(this);
		else if (this.parent != null)
			this.parent.removeChild(this);
	}

	public boolean hasChildren() {
		return children != null && !children.isEmpty();
	}

	public boolean hasSiblings() {
		return !getSiblings().isEmpty();
	}

	public boolean isOrphan() {
		return parent == null;
	}

	public boolean isDead(int maxDeadCount) {
		return !isLocked() && deadCounter >= maxDeadCount;
	}

	public boolean needOcr() {
		return !isLocked();
	}

	public void resetChildrenDeadCounter() {
		for (Field child : children) {
			child.resetChildrenDeadCounter();
			child.resetDeadCounter();
		}
	}

	public void resetParentsDeadCounter() {
		resetDeadCounter();
		if (getParent() != null)
			getParent().resetParentsDeadCounter();
	}


	// void updateRect(GSRect rect, int width, int height) {
	// GSRect truncatedRect = getRect().getIntersection(new GSRect(0, 0, width, height));
	// double tlX = truncatedRect.getX();
	// double tlY = truncatedRect.getY();
	// double brX = tlX + getRect().getWidth();
	// double brY = tlY + getRect().getHeight();
	// GSRect updatedRect = new GSRect(new GSPoint(tlX <= 0 ? this.rect.tl().getX() : rect.tl().getX(), tlY <= 0 ? this.rect.tl().getY() : rect.tl().getY()),
	// new GSPoint(brX >= width ? this.rect.br().getX() : rect.br().getX(), brY >= height ? this.rect.br().getY() : rect.br().getY()));
	// this.rect = updatedRect;
	// }

	public Field findOldMatch(Fields oldFields) {		
		for(Field oldField : oldFields)
			if(getRect().inclusiveArea(oldField.getRect())>0.6)
				return oldField;
		return null;
	}


}