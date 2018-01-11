package org.genericsystem.cv.retriever;

import java.util.HashMap;
import java.util.Map;

import org.opencv.core.Mat;

public class DescriptorManager {

	private Map<ImgDescriptor,Mat> descriptors = new HashMap<>();
	private ImgDescriptor reference; 
	private Map<ImgDescriptor,Double> distanceMap = new HashMap<>();

	public DescriptorManager(){

	}


	public Map<ImgDescriptor,Mat> getDescriptors() {
		return descriptors;
	}


	public ImgDescriptor getReference() {
		return reference;
	}


	public void setReference(ImgDescriptor reference) {
		this.reference = reference;
	}


	public Map<ImgDescriptor,Double> getDistanceMap() {
		return distanceMap;
	}


}
