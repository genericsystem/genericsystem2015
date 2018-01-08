package org.genericsystem.cv.retriever;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.ParallelTasks;
import org.genericsystem.cv.utils.RectToolsMapper;
import org.genericsystem.reinforcer.tools.GSPoint;
import org.genericsystem.reinforcer.tools.GSRect;
import org.genericsystem.reinforcer.tools.Levenshtein;
import org.opencv.calib3d.Calib3d;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.opencv.utils.Converters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Fields extends AbstractFields<Field> {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private static final int MAX_DELETE_UNMERGED = 5;
	private static final int OCR_TIMEOUT = 100;


	public Fields(List<Field> fields) {
		super(fields);
	}

	public Fields() {
		super();
	}

	public void reset() {
		//displayFieldsTree();
		fields = new ArrayList<>();
	}

	@Override
	public void drawOcrPerspectiveInverse(Img display, Mat homography, int thickness) {
		stream().forEach(field -> field.drawWithHomography(display, homography, thickness));
	}

	public void drawFieldsOnStabilized(Img stabilized) {
		stream().forEach(field -> field.draw(stabilized, 1));
	}

	public void drawFieldsOnStabilizedDebug(Img stabilized) {
		for (GSRect rect : mergeRectsList(stabilized)) {
			Point[] targets = RectToolsMapper.gsPointToPoint(Arrays.asList(rect.decomposeClockwise())).toArray(new Point[0]);
			for (int i = 0; i < targets.length; ++i)
				Imgproc.line(stabilized.getSrc(), targets[i], targets[(i + 1) % targets.length], new Scalar(255, 0, 0), 1);
		}
	}

	public List<Field> getRoots() {
		return fields.stream().filter(field -> field.isOrphan()).collect(Collectors.toList());
	}

	public void consolidate(Img img) {
		fields.forEach(f -> {
			if(f.isInFrame(img)){
				f.incrementDeadCounter();
				f.adjustLockLevel(-0.5);
			}
		});
		mergeRects(img, 0.70);
		removeDeadTrees();
	}

	public List<GSRect> mergeRectsList(Img img) {
		RectDetector rd = new RectDetector(img);
		List<GSRect> rects = rd.getRects(200, 11, 3, new Size(11, 3));
		List<GSRect> children = rd.getRects(40, 17, 3, new Size(7, 3));
		return cleanList(rects, children, 0.70);
	}

	public List<GSRect> cleanList(List<GSRect> bigRects, List<GSRect> smallRects, double overlapThreshold) {
		smallRects.removeIf(smallRect -> bigRects.stream().anyMatch(bigRect -> smallRect.inclusiveArea(bigRect) > overlapThreshold));
		return Stream.concat(smallRects.stream().filter(smallRect -> bigRects.stream().filter(rect -> rect.isOverlapping(smallRect)).noneMatch(rect -> rect.getInsider(smallRect) == null)), bigRects.stream()).collect(Collectors.toList());
	}

	private void mergeRects(Img img, double overlapThreshold) {
		List<GSRect> rects = mergeRectsList(img);
		Collections.reverse(rects);
		for (GSRect rect : rects) {
			if (rect.isNearEdge(img.width(), img.height(), 10)) 
				logger.trace("Rect {} was too close to the frame's edges", rect);
			else{
				Field match = findMatch(rect, overlapThreshold, img.width(), img.height());
				if (match != null)
					updateNode(rect, match, img.width(), img.height());
				else{
					Field parent = findPotentialParent(rect);
					List<Field> children = parent==null?findPotentialChildren(getRoots(), rect):findPotentialChildren(parent.getChildren(), rect);					
					createNode(rect, parent, children);
				}
			}
		}
	}

	private List<Field> findPotentialChildren(List<Field> potentialChildren, GSRect rect) {
		return potentialChildren.stream().filter(f -> f.getRect().isInside(rect) && f.getRect().inclusiveArea(rect)<0.3).collect(Collectors.toList());
	}

	private Field findPotentialParent(GSRect rect) {
		for (Field root : getRoots()) {
			Field parent = root.findPotentialParent(rect);
			if (parent != null)
				return parent;
		}
		return null;
	}

	public void createNode(GSRect rect, Field parent, List<Field> children) {	
		if(!checkOverlapConstraint(rect))
			logger.trace("Rect {} was overlapping a field", rect);
		else{
			Field f = new Field(rect);
			if (children != null)
				for (Field child : children)
					child.updateParent(f);
			if (parent != null)
				f.updateParent(parent);
			fields.add(f);	
		}
	}

	public void updateNode(GSRect rect, Field field, int width, int height) {
		field.updateOcrRect(rect);
		field.adjustLockLevel(1.0);
		field.resetParentsDeadCounter();
		field.resetChildrenDeadCounter();
	}

	private boolean checkOverlapConstraint(GSRect rect) {
		for (Field field : fields)
			if (rect.isOverlappingStrict(field.getRect()))
				if (rect.getInsider(field.getRect()) == null){
					field.adjustLockLevel(-0.4);
					return false;
				}
		return true;
	}

	public void removeNode(Field field) {
		//logger.info("Removing node: {}", field.getRect());
		fields.remove(field);
	}

	private void removeDeadTrees() {
		getRoots().stream().filter(field -> isDeadTree(field, MAX_DELETE_UNMERGED)).flatMap(field -> listTree(field).stream()).forEach(this::removeNode);
	}

	private boolean isDeadTree(Field root, int maxDeadCount) {
		if (!root.hasChildren())
			return root.isDead(maxDeadCount);
		for (Field child : root.getChildren())
			if (!isDeadTree(child, maxDeadCount))
				return false;
		return true;
	}

	private List<Field> listTree(Field root) {
		List<Field> res = new ArrayList<>();
		res.add(root);
		for (Field child : root.getChildren())
			res.addAll(listTree(child));
		return res;
	}

	private Field findMatch(GSRect rect, double areaOverlap, int width, int height) {
		GSRect frameRect = new GSRect(0, 0, width, height);
		List<Field> matches = fields.stream().filter(f -> rect.inclusiveArea(f.getRect().getIntersection(frameRect)) > areaOverlap).collect(Collectors.toList());
		if (matches.isEmpty())
			return null;
		if (matches.size() > 1)
			logger.warn(matches.size() + " matches were detected.");
		return matches.get(0);
	}

	public void restabilizeFields(Mat homography) {		
		fields.forEach(field -> field.updateRect(findNewRect(field.getRect(), homography)));
	}

	private GSRect findNewRect(GSRect rect, Mat homography) {
		List<Point> originals = RectToolsMapper.gsPointToPoint(Arrays.asList(rect.tl(), rect.br()));
		List<GSPoint> points = RectToolsMapper.pointToGSPoint(restabilize(originals, homography));
		return new GSRect(points.get(0), points.get(1));
	}

	private List<Point> restabilize(List<Point> originals, Mat homography) {
		Mat original = Converters.vector_Point2d_to_Mat(originals);
		Mat results = new Mat();
		Core.perspectiveTransform(original, results, homography);
		List<Point> res = new ArrayList<>();
		Converters.Mat_to_vector_Point2d(results, res);
		return res;
	}

	@Override	
	public void performOcr(Img rootImg) {
		if (size() <= 0)
			return;
		long TS = System.currentTimeMillis();
		while (System.currentTimeMillis() - TS <= OCR_TIMEOUT) {
			runParallelOcr(rootImg);
			// runSequentialOcr(rootImg);
		}		
	}

	private void runSequentialOcr(Img rootImg) {
		int idx = ThreadLocalRandom.current().nextInt(size());
		Field f = fields.get(idx);
		//if (f.needOcr())
		f.ocr(rootImg);
	}

	private void runParallelOcr(Img rootImg) {
		ParallelTasks tasks = new ParallelTasks();
		int limit = tasks.getCounter() * 2;
		for (Set<Integer> indexes = new HashSet<>(); indexes.size() < limit && indexes.size() < size();) {
			int idx = ThreadLocalRandom.current().nextInt(size());
			if (indexes.add(idx)) {
				Field f = fields.get(idx);
				//if (f.needOcr())
				tasks.add(() -> f.ocr(rootImg));
			}
		}
		try {
			tasks.run();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}


	public Map<Field, Field> getLabelMatchesWithOldFields(Img rootImg, Fields oldFields) {
		if (size() <= 0)
			return null;
		long TS = System.currentTimeMillis();
		Map<Field, String> ocrs = new ConcurrentHashMap<Field, String>();
		while (System.currentTimeMillis() - TS <= OCR_TIMEOUT) {
			runParallelOcr(rootImg, ocrs);
		}
		return findRecoveringMatches(oldFields, ocrs);
	}

	private void runParallelOcr(Img rootImg, Map<Field, String> ocrs) {
		ParallelTasks tasks = new ParallelTasks();
		int limit = tasks.getCounter() * 2;
		for (Set<Integer> indexes = new HashSet<>(); indexes.size() < limit && indexes.size() < size();) {
			int idx = ThreadLocalRandom.current().nextInt(size());
			if (indexes.add(idx)) {
				Field f = fields.get(idx);
				tasks.add(() -> {
					String s = f.ocr(rootImg);
					if(s!=null)
						ocrs.put(f, s);
				});					
			}
		}
		try {
			tasks.run();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

	public void tryRecoveryfromOldFields(Map<Field, Field> labelMatches, Fields oldFields) {
		Mat homography = findRecoveringHomography(labelMatches);
		if(homography!=null ){
			oldFields.restabilizeFields(homography);
			transferHistoryFrom(oldFields);
		}		
	}

	private Map<Field, Field> findRecoveringMatches(Fields oldFields, Map<Field, String> ocrs){
		Map<Field, Field> matches = new HashMap<Field,Field>();
		for(Map.Entry<Field, String> entry : ocrs.entrySet()){
			if(entry.getValue().trim().length()<4)
				continue;
			for(Field oldField : oldFields){
				if(entry.getValue().equals(oldField.getConsolidated()) /*|| levenshteinDistance(entry.getValue(),oldField.getConsolidated())<2*/){
					if(matches.containsKey(entry.getKey()))
						matches.remove(entry.getKey());
					else
						matches.put(entry.getKey(), oldField);
				}				
			}			
		}
		System.out.println("returning "+matches);
		return matches;
	}

	private double levenshteinDistance(String ocr, String consolidatedLabel){
		return Levenshtein.normedDistance(ocr,consolidatedLabel);
	}

	private Mat findRecoveringHomography(Map<Field, Field> matches) {		
		List<Point> newPointList = new ArrayList<>();
		List<Point> oldPointList = new ArrayList<>();
		for(Map.Entry<Field, Field> entry : matches.entrySet()){
			newPointList.addAll(getSquarePoints(entry.getKey().getRect().decomposeClockwise()));
			oldPointList.addAll(getSquarePoints(entry.getValue().getRect().decomposeClockwise()));			

		}

		//
		//		List<Point[]> pairedPoints = new ArrayList<>();
		//		for(int i =0; i<newPointList.size(); i++)
		//			pairedPoints.add(new Point[]{oldPointList.get(i), newPointList.get(i)});
		//
		//
		//		double[] transScaleParams= new LMHostImpl<>((points, params) -> distance(points, params),pairedPoints , new double[] { 1, 1, 0, 0 }).getParams();
		//		Mat result = getTSMat(transScaleParams);
		//return result;

		Mat mask = new Mat();
		Mat homography = Calib3d.findHomography(new MatOfPoint2f(oldPointList.toArray(new Point[oldPointList.size()])), new MatOfPoint2f(newPointList.toArray(new Point[newPointList.size()])), 8, 2, mask, 2000, 0.995);
		for(int i=mask.rows()-1; i>=0 ;i--){
			if(mask.get(i, 0)[0]==0){
				newPointList.remove(i);
				oldPointList.remove(i);
			}
		}
		return evaluateHomographyError(newPointList, oldPointList, homography)<1?homography:null;
	}

	//	private double distance(Point[] points, double[] params){
	//		double p2x=points[1].x , p2y=points[1].y;
	//		double p1x = params[0]*points[0].x + params[0]*params[2];
	//		double p1y = params[1]*points[0].y + params[1]*params[3];
	//		double deltaX = p2x - p1x;
	//		double deltaY = p2y - p1y;
	//		double distance = Math.sqrt(deltaX*deltaX + deltaY*deltaY);
	//		//System.out.println("distance: "+distance);
	//		return distance < 5?distance:5;
	//	}
	//
	//	private Mat getTSMat(double[] transScaleParams) {
	//		Mat TSMat = new Mat(3, 3, CvType.CV_64FC1, new Scalar(0));
	//		TSMat.put(0, 0, transScaleParams[0]);
	//		TSMat.put(1, 1, transScaleParams[1]);
	//		TSMat.put(0, 2, transScaleParams[2]*transScaleParams[0]);
	//		TSMat.put(1, 2, transScaleParams[3]*transScaleParams[1]);
	//		TSMat.put(2, 2, 1d);
	//		return TSMat;
	//	}

	private double evaluateHomographyError(List<Point> newPointList, List<Point> oldPointList, Mat homography) {		
		double error = 0.0;
		List<Point> restabilizedPoints = restabilize(oldPointList, homography);
		for(int i = 0; i < restabilizedPoints.size(); i++){
			double deltaX = newPointList.get(i).x - restabilizedPoints.get(i).x;
			double deltaY = newPointList.get(i).y - restabilizedPoints.get(i).y;
			error+=deltaX*deltaX + deltaY*deltaY;
		}
		System.out.println("error found: "+ Math.sqrt(error)/restabilizedPoints.size());
		return Math.sqrt(error)/restabilizedPoints.size();
	}

	private List<Point> getSquarePoints(GSPoint[] GSpoints) {
		List<Point> points = new ArrayList<>();
		for(int i = 0; i < GSpoints.length; i++)
			points.add(new Point(GSpoints[i].getX(), GSpoints[i].getY()));
		return points;
	}


	public void transferHistoryFrom(Fields oldFields){

		for(Field newField : fields){
			Field match = newField.findOldMatch(oldFields);
			if(match!=null){
				newField.getLabels().putAll(match.getLabels());				
				newField.setConsolidated(match.getConsolidated());
			}			
		}
	}

	public void consolidateHierarchyLabels() {
		for(Field field : getRoots()){
			field.consolidateLabelWithChildren();
		}

	}




}