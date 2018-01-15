package org.genericsystem.cv.retriever;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.genericsystem.cv.Img;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Size;
import org.opencv.utils.Converters;

public class DescriptorManager {

	private Map<ImgDescriptor,Mat> descriptors = new HashMap<>();
	private ImgDescriptor reference; 
	private Map<ImgDescriptor,Double> distanceMap = new HashMap<>();
	private static final Mat IDENTITY_MAT = Mat.eye(new Size(3, 3), CvType.CV_64F);
	private Mat frame;

	public DescriptorManager(){

	}

	public void setFrame(Mat frame){
		this.frame = frame;
	}


	public Map<ImgDescriptor,Mat> getDescriptors() {
		return descriptors;
	}

	public void setReference(ImgDescriptor reference) {
		this.reference = reference;
	}


	public Map<ImgDescriptor,Double> getDistanceMap() {
		return distanceMap;
	}


	public Img add(ImgDescriptor deperspectivedImgDescriptor, Mat deperspectiveHomography) {

		if (descriptors.isEmpty()) {
			descriptors.put(deperspectivedImgDescriptor, IDENTITY_MAT);
			setReference(deperspectivedImgDescriptor);
			Mat stabilizationHomographyFromFrame = new Mat();
			Core.gemm(IDENTITY_MAT, deperspectiveHomography, 1, new Mat(), 0, stabilizationHomographyFromFrame);
			return CamLiveRetriever.warpPerspective(frame, stabilizationHomographyFromFrame);

		}
		Mat homographyToRef = IDENTITY_MAT;
		for (ImgDescriptor descriptor : descriptors.keySet()) {
			Mat joinHomography = deperspectivedImgDescriptor.computeStabilizationGraphy(descriptor);
			if (joinHomography != null) {
				homographyToRef = computeHomographyToRef(descriptor, joinHomography);
				descriptors.put(deperspectivedImgDescriptor, homographyToRef);
				break;
			}
			else
				return null;
		}
		updateReferenceDeperspectived(descriptors);		
		Mat stabilizationHomographyFromFrame = new Mat();
		Core.gemm(descriptors.get(deperspectivedImgDescriptor), deperspectiveHomography, 1, new Mat(), 0, stabilizationHomographyFromFrame);
		return CamLiveRetriever.warpPerspective(frame, stabilizationHomographyFromFrame);
	}

	private Mat computeHomographyToRef(ImgDescriptor descriptor, Mat joinHomography) {
		Mat homographyToRef = new Mat();
		Core.gemm(joinHomography, descriptors.get(descriptor), 1, new Mat(), 1, homographyToRef);
		return homographyToRef;
	}

	private ImgDescriptor updateReferenceDeperspectived(Map<ImgDescriptor, Mat> descriptorGroup) {
		ImgDescriptor bestDescriptor = computeBestDescriptor(descriptorGroup);
		if (bestDescriptor != reference){
			computeHomographiesToNewRef(bestDescriptor);
			setReference(bestDescriptor);
		}
		return bestDescriptor;
	}

	private void computeHomographiesToNewRef(ImgDescriptor newReference) {
		System.out.println(">>>>>>>>>>> CHANGE: Reference has changed, recomputing homographies to new ref");
		for (ImgDescriptor descriptor : descriptors.keySet()) {
			if (descriptor == newReference) {
				descriptors.put(reference, descriptors.get(descriptor).inv());
				descriptors.put(descriptor, IDENTITY_MAT);
			} else{
				Mat newHomographyToRef = new Mat();
				Core.gemm(descriptors.get(reference), descriptors.get(descriptor), 1, new Mat(), 1, newHomographyToRef);
				descriptors.put(descriptor, newHomographyToRef);
			}
		}		
	}

	private ImgDescriptor computeBestDescriptor(Map<ImgDescriptor, Mat> descriptorGroup) {
		Map<ImgDescriptor, Double> distanceMap = getDistanceMap();
		for (ImgDescriptor origin : descriptorGroup.keySet()) {
			double totalDistance = 0.0;
			for (ImgDescriptor target : descriptorGroup.keySet())
				totalDistance += origin == target ? 0.0 : computeDistanceBetweenDeperspectived(computeHomographyBetweenDeperspectived(origin, target));
			distanceMap.put(origin, totalDistance);
		}
		return Collections.min(distanceMap.entrySet(), Comparator.comparingDouble(Entry::getValue)).getKey();
	}

	private Mat computeHomographyBetweenDeperspectived(ImgDescriptor origin, ImgDescriptor target) {
		Mat homographyBetweenDeperspectived = new Mat();
		Core.gemm(descriptors.get(target).inv(), descriptors.get(origin), 1, new Mat(), 1, homographyBetweenDeperspectived);
		return homographyBetweenDeperspectived;
	}


	private double computeDistanceBetweenDeperspectived(Mat betweenDeperspectivedHomography) {
		List<Point> originalPoints = Arrays.asList(new Point[] { new Point(0, 0), new Point(frame.width(), 0), new Point(frame.width(), frame.height()), new Point(0, frame.height())});
		List<Point> points = restabilize(originalPoints, betweenDeperspectivedHomography);
		return evaluateDistanceBetweenStabilized(points, originalPoints);
	}

	private List<Point> restabilize(List<Point> originals, Mat homography) {
		Mat original = Converters.vector_Point2d_to_Mat(originals);
		Mat results = new Mat();
		Core.perspectiveTransform(original, results, homography);
		List<Point> res = new ArrayList<>();
		Converters.Mat_to_vector_Point2d(results, res);
		return res;
	}

	private double evaluateDistanceBetweenStabilized(List<Point> newPointList, List<Point> oldPointList) {
		double error = 0.0;
		for (int i = 0; i < oldPointList.size(); i++) {
			double deltaX = newPointList.get(i).x - oldPointList.get(i).x;
			double deltaY = newPointList.get(i).y - oldPointList.get(i).y;
			error += deltaX * deltaX + deltaY * deltaY;
		}
		return Math.sqrt(error) / oldPointList.size();
	}
}
