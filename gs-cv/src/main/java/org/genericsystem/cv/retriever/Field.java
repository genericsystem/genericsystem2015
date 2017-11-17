package org.genericsystem.cv.retriever;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.genericsystem.cv.Img;
import org.genericsystem.reinforcer.tools.GSPoint;
import org.genericsystem.reinforcer.tools.GSRect;
import org.opencv.core.Mat;
import org.opencv.core.Scalar;

public class Field extends AbstractField {

	private Field parent;
	private List<Field> children;

	private static final int LABELS_SIZE_THRESHOLD = 10;
	private static final double CONFIDENCE_THRESHOLD = 0.92;
	private boolean locked = false;

	public Field(GSRect rect) {
		super(rect);
		this.parent = null;
		this.children = new ArrayList<>();
	}

	public Field(GSRect rect, Field parent) {
		super(rect);
		setParent(parent);
		this.children = new ArrayList<>();
	}

	public String recursiveToString() {
		StringBuffer sb = new StringBuffer();
		this.recursiveToString(sb, 0);
		sb.append("\n");
		return sb.toString();
	}

	public void recursiveToString(StringBuffer sb, int depth) {
		if (depth > 8)
			return;
		sb.append("depth: ").append(depth).append(": ").append(rect);
		if (isConsolidated())
			sb.append(" -> ").append(getConsolidated());
		if (children.isEmpty()) {
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
		GSRect insider = rect.getInsider(this.getRect());
		if (insider == null || insider == this.getRect())
			return null;
		for (Field child : children) {
			Field candidate = child.findPotentialParent(rect);
			if (candidate != null)
				return candidate;
		}
		return this;
	}

	@Override
	public void ocr(Img rootImg) {
		super.ocr(rootImg);
		if (attempts <= 3 || attempts % 5 == 0)
			consolidateOcr(false);
	}

	@Override
	public void resetDeadCounter() {
		super.resetDeadCounter();
		tryLock();
	}

	public void draw(Img display, int thickness) {
		// if (needText())
		// drawText(display, new Scalar(0, 64, 255), thickness);
		drawRect(display, getColor(), thickness);
	}

	public Scalar getColor() {
		// if (drawAsLocked())
		// return new Scalar(255, 172, 0);
		if (deadCounter != 0)
			return new Scalar(0, 0, 255);
		else
			return new Scalar(0, 255, 0);
	}

	public void drawWithHomography(Img display, Mat homography, int thickness) {
		if (needRect())
			drawRect(display, getRectPointsWithHomography(homography), drawAsLocked() ? new Scalar(0, 255, 0) : new Scalar(0, 0, 255), thickness);
		if (needText())
			drawText(display, getRectPointsWithHomography(homography), new Scalar(0, 255, 0), thickness);
	}

	// private boolean drawAsChild() {
	// return !isOrphan() && parent.deadCounter != 0;
	// }

	private boolean drawAsLocked() {
		return isOrphan() || (!isOrphan() && parent.getDeadCounter() != 0) ? isLocked() : false;
	}

	private boolean needRect() {
		return !isOrphan() && parent.getDeadCounter() == 0 ? false : true /* isLocked() ? true : deadCounter == 0 */;
	}

	private boolean needText() {
		return deadCounter == 0 && needRect();
	}

	public void tryLock() {
		if (!locked && getLabelsSize() > LABELS_SIZE_THRESHOLD && getConfidence() > CONFIDENCE_THRESHOLD)
			this.locked = true;
	}

	public boolean isLocked() {
		return locked;
	}

	public boolean addChildIfNotPresent(Field child) {
		return children.add(child);
	}

	public boolean removeChild(Field child) {
		return children.remove(child);
	}

	public boolean containsChild(Field field) {
		return children.stream().anyMatch(child -> child.getRect().inclusiveArea(field.getRect()) > 0.95);
	}

	public boolean addChildren(Collection<Field> children) {
		return this.children.addAll(children);
	}

	public void setChildren(List<Field> children) {
		this.children = children;
	}

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
		this.parent.addChildIfNotPresent(this);
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
		return !locked && deadCounter >= maxDeadCount;
	}

	void updateRect(GSRect rect, int width, int height) {
		GSRect truncatedRect = getRect().getIntersection(new GSRect(0, 0, width, height));
		double tlX = truncatedRect.getX();
		double tlY = truncatedRect.getY();
		double brX = tlX + getRect().getWidth();
		double brY = tlY + getRect().getHeight();
		GSRect updatedRect = new GSRect(new GSPoint(tlX <= 0 ? this.rect.tl().getX() : rect.tl().getX(), tlY <= 0 ? this.rect.tl().getY() : rect.tl().getY()),
				new GSPoint(brX >= width ? this.rect.br().getX() : rect.br().getX(), brY >= height ? this.rect.br().getY() : rect.br().getY()));
		this.rect = updatedRect;
	}

	public boolean isVeryfyingConstraints() {

		return false;
	}

	public boolean needOcr() {
		return !isLocked() && deadCounter == 0;
	}

	public void resetChildrenDeadCounter() {
		List<Field> potentialChildren = getChildren();
		for (Field child : potentialChildren) {
			child.resetChildrenDeadCounter();
			child.resetDeadCounter();
		}
	}

	public void resetParentsDeadCounter() {
		resetDeadCounter();
		if (getParent() != null)
			getParent().resetParentsDeadCounter();
	}

}