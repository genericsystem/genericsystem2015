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
import org.opencv.core.Scalar;
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
		List<Mat> possibleHomographies = new ArrayList<>();
		for (ImgDescriptor descriptor : descriptors.keySet()) {
			Mat joinHomography = deperspectivedImgDescriptor.computeStabilizationGraphy(descriptor);
			if (joinHomography != null) 				
				possibleHomographies.add(computeHomographyToRef(descriptor, joinHomography));
		}
		Mat pertinentHomography = possibleHomographies.size()==1?possibleHomographies.get(0):getPertinentHomography(possibleHomographies);
		if(pertinentHomography!=null)
			descriptors.put(deperspectivedImgDescriptor, pertinentHomography);
		else
			return null;
		updateReferenceDeperspectived(descriptors);		
		Mat stabilizationHomographyFromFrame = new Mat();
		Core.gemm(descriptors.get(deperspectivedImgDescriptor), deperspectiveHomography, 1, new Mat(), 0, stabilizationHomographyFromFrame);
		return CamLiveRetriever.warpPerspective(frame, stabilizationHomographyFromFrame);
	}

	private Mat getPertinentHomography(List<Mat> possibleHomographies) {
		return possibleHomographies.isEmpty()?null:getBestHomography(possibleHomographies);

	}

	private Mat getBestHomography(List<Mat> possibleHomographies){
		Map<Mat, Double> distancesByHomography = new HashMap<>();
		double totalDistance = 0.0;
		for(Mat h : possibleHomographies){
			double distance = computeDistanceBetweenDeperspectived(h);
			distancesByHomography.put(h, distance);
			totalDistance+=distance;
		}
		double meanDistance = totalDistance / distancesByHomography.size();
		double squareStandardDeviation = 0.0;
		for(Mat h : distancesByHomography.keySet()){
			double diff = meanDistance - distancesByHomography.get(h);
			squareStandardDeviation+=diff*diff;
			distancesByHomography.put(h, diff);
		}
		System.out.println("Standard deviation: "+Math.sqrt(squareStandardDeviation / distancesByHomography.size()));
		return (Math.sqrt(squareStandardDeviation / distancesByHomography.size())) > 10 ? null : Collections.min(distancesByHomography.entrySet(), Comparator.comparingDouble(Entry::getValue)).getKey();		
	}

	private Mat computeHomographyToRef(ImgDescriptor descriptor, Mat joinHomography) {
		return matrixProduct(joinHomography, descriptors.get(descriptor));
	}

	private boolean isReference(ImgDescriptor descriptor) {
		return descriptor == reference;
	}

	private Mat matrixProduct(Mat matrix1, Mat matrix2) {
		Mat result = new Mat(matrix1.cols(), matrix2.rows(), CvType.CV_64F, new Scalar(0));
		for (int i = 0; i < matrix1.rows(); i++) {
			for (int j = 0; j < matrix2.cols(); j++) {
				double sum = 0.0;
				for (int k = 0; k < matrix2.rows(); k++) {
					sum += matrix1.get(i, k)[0] * matrix2.get(k, j)[0];
				}
				result.put(i, j, sum);
			}
		}
		return result;
	}

	private ImgDescriptor updateReferenceDeperspectived(Map<ImgDescriptor, Mat> descriptorGroup) {
		ImgDescriptor bestDescriptor = computeBestDescriptor(descriptorGroup);
		if (isReference(bestDescriptor))
			System.out.println("Reference is still the best, doing nothing special for the moment.");
		else {
			System.out.println(">>>>>>>>>>> CHANGE: Reference has changed, recomputing homographies to new ref");
			computeHomographiesToNewRef(bestDescriptor);
			setReference(bestDescriptor);
		}
		return bestDescriptor;
	}

	private void computeHomographiesToNewRef(ImgDescriptor newReference) {
		ImgDescriptor oldReference = reference;
		for (ImgDescriptor descriptor : descriptors.keySet()) {
			if (descriptor == newReference) {
				descriptors.put(oldReference, descriptors.get(descriptor).inv());
				descriptors.put(descriptor, IDENTITY_MAT);
			} else
				descriptors.put(descriptor, matrixProduct(descriptors.get(oldReference), descriptors.get(descriptor)));
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
		if (isReference(origin))
			return descriptors.get(target).inv();
		else if (isReference(target))
			return descriptors.get(origin);
		else
			return matrixProduct(descriptors.get(target).inv(), descriptors.get(origin));
	}


	private double computeDistanceBetweenDeperspectived(Mat betweenStabilizedHomography) {
		List<Point> originalPoints = Arrays.asList(new Point[] { new Point(0, 0), new Point(640, 0), new Point(640, 480), new Point(0, 480) });
		List<Point> points = restabilize(originalPoints, betweenStabilizedHomography);
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
