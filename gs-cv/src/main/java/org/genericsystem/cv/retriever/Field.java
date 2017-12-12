package org.genericsystem.cv.retriever;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.RectToolsMapper;
import org.genericsystem.reinforcer.tools.GSRect;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.utils.Converters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Field {

	protected static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private static final double LOCK_THRESHOLD = 0.90;
	protected static final int MIN_SIZE_CONSOLIDATION = 5;

	private Field parent;
	private final List<Field> children;
	private GSRect rect;
	private GSRect ocrRect;
	private Map<String, Integer> labels;
	private String consolidated;
	private double confidence;
	private int attempts;
	private int deadCounter;
	private double locklLevel;

	public Field(GSRect rect) {		
		this.parent = null;
		this.children = new ArrayList<>();
		this.deadCounter = 0;
		this.locklLevel = 0;
		this.rect = rect;
		this.ocrRect = rect;
		this.labels = new HashMap<>();
		this.consolidated = null;
		this.attempts = 0;
		this.confidence = 0;
	}

	public Field(GSRect rect, Field parent) {
		this(rect);
		updateParent(parent);
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

	protected Point[] getRectPointsWithHomography(Mat homography) {
		List<Point> points = RectToolsMapper.gsPointToPoint(Arrays.asList(rect.decomposeClockwise()));
		MatOfPoint2f results = new MatOfPoint2f();
		Core.perspectiveTransform(Converters.vector_Point2f_to_Mat(points), results, homography);
		return results.toArray();
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
		adjustLockLevel(0.5);
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

	public boolean isConsolidated() {
		return consolidated != null;
	}

	public int getLabelsSize() {
		return labels.entrySet().stream().mapToInt(entry -> entry.getValue()).sum();
	}

	public Map<String, Integer> getLabels() {
		return labels;
	}

	public String getConsolidated() {
		return consolidated;
	}

	public int getAttempts() {
		return attempts;
	}

	public GSRect getRect() {
		return rect;
	}

	public GSRect getOcrRect() {
		return ocrRect;
	}

	public double getConfidence() {
		return confidence;
	}

	public void setConfidence(double confidence){
		this.confidence = confidence;
	}

	void updateRect(GSRect rect) {
		this.rect = rect;
	}

	void updateOcrRect(GSRect rect) {
		this.ocrRect = rect;
	}

	public void setConsolidated(String consolidated) {
		this.consolidated = consolidated;
	}

	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer();
		sb.append("AbstractField: ").append("\n").append(" -> rect: ").append(rect).append("\n").append(" -> labels size: ").append(getLabelsSize()).append("\n").append(" -> consolidated: ").append(consolidated).append("\n").append(" -> confidence: ")
		.append(confidence).append("\n");
		return sb.toString();
	}

	public void setAttempts(int attempts) {
		this.attempts = attempts;
	}
}