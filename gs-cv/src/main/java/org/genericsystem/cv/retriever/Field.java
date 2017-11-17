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

	private static final int LABELS_SIZE_THRESHOLD = 15;
	private static final double CONFIDENCE_THRESHOLD = 0.92;
	private boolean locked = false;

	public Field(GSRect rect) {
		super(rect);
		this.parent = null;
		this.children = new ArrayList<>();
		// checkConstraints();
	}

	public String recursiveToString() {
		StringBuffer sb = new StringBuffer();
		recursiveToString(this, sb, 0);
		sb.append("\n");
		return sb.toString();
	}

	private void recursiveToString(Field field, StringBuffer sb, int depth) {
		if (depth > 8)
			return;
		sb.append("depth: ").append(depth).append(": ").append(field.getRect());
		if (field.isConsolidated())
			sb.append(" -> ").append(field.getConsolidated());
		if (!field.getChildren().isEmpty()) {
			depth++;
			for (Field child : field.getChildren()) {
				sb.append("\n");
				for (int i = 0; i < depth; ++i)
					sb.append("  ");
				recursiveToString(child, sb, depth);
			}
		}
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
		setFinal();
	}

	public void draw(Img display, Mat homography, Scalar color, int thickness) {
		Scalar scalar = selectColor(color);
		if (needRect())
			drawRect(display, getRectPointsWithHomography(homography), scalar, thickness);
		if (needText())
			drawText(display, getRectPointsWithHomography(homography), new Scalar(0, 64, 255), thickness);
	}

	private Scalar selectColor(Scalar defaultColor) {
		if (drawAsLocked())
			return new Scalar(255, 172, 0);
		if (drawAsChild())
			return new Scalar(255, 0, 0);
		return defaultColor;
	}

	private boolean drawAsChild() {
		return !isOrphan() && parent.deadCounter != 0;
	}

	private boolean drawAsLocked() {
		return isOrphan() || (!isOrphan() && parent.getDeadCounter() != 0) ? this.locked : false;
	}

	private boolean needRect() {
		return !isOrphan() && parent.getDeadCounter() == 0 ? false : isLocked() ? true : deadCounter == 0;
	}

	private boolean needText() {
		return deadCounter == 0 && needRect();
	}

	public void setFinal() {
		if (!locked)
			if (getLabelsSize() > LABELS_SIZE_THRESHOLD && getConfidence() > CONFIDENCE_THRESHOLD && isOrphan())
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
		if (parent != null)
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

	void updateRect(GSRect rect, int width, int height) {
		GSRect truncatedRect = getRect().getIntersection(new GSRect(0, 0, width, height));
		if (truncatedRect != this.rect) {
			double tlX = truncatedRect.getX();
			double tlY = truncatedRect.getY();
			double brX = tlX + truncatedRect.getWidth();
			double brY = tlY + truncatedRect.getHeight();

			this.rect = new GSRect(new GSPoint(tlX <= 0 ? this.rect.tl().getX() : rect.tl().getX(), tlY <= 0 ? this.rect.tl().getY() : rect.tl().getY()),
					new GSPoint(brX >= width ? this.rect.br().getX() : rect.br().getX(), brY >= height ? this.rect.br().getY() : rect.br().getY()));
		} else
			this.rect = rect;
	}

	// private boolean checkConstraints() {
	// boolean ok = isOrphan() ? this.checkConstraintsRecursive() : parent.checkConstraintsRecursive();
	// if (!ok)
	// logger.error("Invalid constraint for:\n{}Tree:\n{}", this, isOrphan() ? this.recursiveToString() : this.parent.recursiveToString());
	// return ok;
	// }
	//
	// private boolean checkConstraintsRecursive() {
	// // If the field is orphan and has no children, it validates the constraints
	// if (isOrphan() && !hasChildren())
	// return true;
	// // If the field has children, they must meet the constraint
	// if (hasChildren()) {
	// for (Field child : children)
	// if (child.isOutsideParent() || child.isOverlappingSiblings())
	// return false;
	//
	// // If false was not returned, apply the function to the children
	// for (Field child : children)
	// if (!child.checkConstraintsRecursive())
	// return false;
	// }
	// // At this stage, all the constraints should be verified
	// return true;
	// }
	//
	// private boolean isOutsideParent() {
	// return !rect.equals(rect.isInsider(getParent().getRect()));
	// }
	//
	// private boolean isOverlappingSiblings() {
	// return getSiblings().stream().anyMatch(sibling -> rect.isOverlapping(sibling.getRect()));
	// }

}