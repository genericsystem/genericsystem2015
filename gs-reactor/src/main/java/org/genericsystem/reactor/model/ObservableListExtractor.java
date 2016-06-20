package org.genericsystem.reactor.model;

import java.util.Arrays;
import java.util.function.Function;
import java.util.stream.Collectors;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;

@FunctionalInterface
public interface ObservableListExtractor extends Function<Generic[], ObservableList<Generic>> {

	public static ObservableListExtractor from(Class<?>... genericClasses) {
		return gs -> FXCollections.observableArrayList(Arrays.stream(genericClasses).map(gs[0].getRoot()::<Generic> find).collect(Collectors.toList()));
	}

	public static final ObservableListExtractor INSTANCES = generics -> {
		System.out.println("INSTANCES : " + Arrays.toString(generics) + " " + generics[0].getObservableInstances());
		return generics[0].getObservableSubInstances();
	};

	public static final ObservableListExtractor SUBINSTANCES = generics -> {
		System.out.println("INSTANCES : " + Arrays.toString(generics) + " " + generics[0].getObservableSubInstances());
		return generics[0].getObservableSubInstances();
	};

	public static final ObservableListExtractor ATTRIBUTES_OF_TYPE = generics -> {
		System.out.println("ATTRIBUTES_OF_TYPE : " + Arrays.toString(generics) + " "
				+ generics[0].getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances(generics[0])));
		return generics[0].getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances(generics[0]));
	};

	public static final ObservableListExtractor ATTRIBUTES_OF_INSTANCES = generics -> {
		System.out.println("ATTRIBUTES_OF_INSTANCES : " + Arrays.toString(generics) + " "
				+ generics[1].getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances(generics[1])));
		return generics[1].getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances(generics[1]));
	};

	public static final ObservableListExtractor COMPONENTS = generics -> {
		System.out.println("COMPONENTS : " + Arrays.toString(generics) + " " + generics[0].getComponents());
		return FXCollections.observableList(generics[0].getComponents());
	};

	public static final ObservableListExtractor HOLDERS = generics -> {
		System.out.println("HOLDERS : " + Arrays.toString(generics) + " " + generics[1].getObservableHolders(generics[0]));
		return generics[1].getObservableHolders(generics[0]);
	};
}