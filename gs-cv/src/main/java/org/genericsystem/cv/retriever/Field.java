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
	private boolean validated;

	public Field(GSRect rect) {
		super(rect);
		this.parent = null;
		this.children = new ArrayList<>();
		this.deadCounter = 0;
		this.locklLevel = 0;
		this.validated = false;
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
			if(isValidated() && isOrphan())
				drawRect(display, getRectPointsWithHomography(homography), new Scalar(255, 255, 0), thickness);
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

	public boolean isValidated() {
		return validated;
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

	public boolean addChild(Field child) {
		return children.add(child);
	}

	public boolean removeChild(Field child) {
		return children.remove(child);
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

	public void updateParent(Field parent) {
		this.parent = parent;
		this.parent.addChild(this);

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

	//	public boolean needOcr() {
	//		return !isLocked();
	//	}

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


	public Field findOldMatch(Fields oldFields) {		
		for(Field oldField : oldFields)
			if(getRect().inclusiveArea(oldField.getRect())>0.6)
				return oldField;
		return null;
	}

	public void consolidateLabelWithChildren() {
		if(children.isEmpty() || consolidated == null)
			return;
		StringBuilder consolidatedLabel = new StringBuilder(consolidated);
		for(Field child : children){	
			child.consolidateLabelWithChildren();
			if(child.getConsolidated()!=null && consolidatedLabel.toString().contains(child.getConsolidated())){
				int start = consolidatedLabel.indexOf(child.getConsolidated());
				consolidatedLabel.delete(start, start+child.getConsolidated().length());

			}		
		}
		consolidatedLabel = new StringBuilder(consolidatedLabel.toString().trim());
		if(consolidatedLabel.length() == 0)
			validated = true;
		//increasing confidence by 10% of difference to 1. Arbitrary choice
	}

	public boolean isInFrame(Img img) {		
		return (rect.tl().getX()>0 && rect.br().getX() < img.width()) && (rect.tl().getY() > 0 && rect.br().getY() < img.height());
	}

	//	public void consolidateLabelWithChildren() {
	//		if(children.isEmpty())
	//			return;
	//		children.sort(new Comparator<Field>(){
	//			@Override
	//			public int compare(Field f1, Field f2) {
	//				//return new Double(f1.getRect().tl().getX()).compareTo(new Double(f2.getRect().tl().getX()));
	//				return (int) (f1.getRect().tl().getX() - f2.getRect().tl().getX());
	//			}
	//		});
	//		String concatenated = "";
	//		for(Field child : children){	
	//			child.consolidateLabelWithChildren();
	//			concatenated+=child.getConsolidated()!=null?child.getConsolidated()+" ":"";
	//
	//		}
	//		System.out.println(concatenated + "\\/" + consolidated);
	//		if(consolidated!=null && consolidated.trim().equals(concatenated.trim())){
	//			System.out.println("All children matching, increasing confidence");
	//			//increasing confidence by 10% of difference to 1. Arbitrary choice
	//			confidence += (1 - confidence)*0.1;
	//		}
	//	}



}