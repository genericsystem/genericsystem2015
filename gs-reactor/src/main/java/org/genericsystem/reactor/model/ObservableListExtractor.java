package org.genericsystem.reactor.model;

import java.util.Arrays;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import org.genericsystem.common.Generic;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

@FunctionalInterface
public interface ObservableListExtractor extends Function<Generic[], ObservableList<Generic>> {

	public static ObservableListExtractor from(Class<?>... genericClasses) {
		return gs -> FXCollections.observableArrayList(Arrays.stream(genericClasses).map(gs[0].getRoot()::<Generic> find).collect(Collectors.toList()));
	}

	public static final ObservableListExtractor INSTANCES = generics -> {
		// System.out.println("INSTANCES : " + Arrays.toString(generics) + " " + generics[0].getObservableInstances());
		return generics[0].getObservableSubInstances();
	};

	public static final ObservableListExtractor SUBINSTANCES = generics -> {
		// System.out.println("INSTANCES : " + Arrays.toString(generics) + " " + generics[0].getObservableSubInstances());
		return generics[0].getObservableSubInstances();
	};

	public static final ObservableListExtractor SUBINSTANCES_OF_META = generics -> {
		return generics[0].getMeta().getObservableSubInstances();
	};

	public static final ObservableListExtractor ATTRIBUTES_OF_TYPE = generics -> {
		// System.out.println("ATTRIBUTES_OF_TYPE : " + Arrays.toString(generics) + " " + generics[0].getObservableAttributes().filtered(attribute ->
		// attribute.isCompositeForInstances(generics[0])));
		return generics[0].getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances(generics[0]));
	};

	public static final ObservableListExtractor ATTRIBUTES_OF_INSTANCES = generics -> {
		// System.out.println("ATTRIBUTES_OF_INSTANCES : " + Arrays.toString(generics) + " " + generics[1].getObservableAttributes().filtered(attribute ->
		// attribute.isCompositeForInstances(generics[1])));
		return generics[1].getObservableAttributes().filtered(attribute -> attribute.isCompositeForInstances(generics[1]));
	};

	public static final ObservableListExtractor COMPONENTS = generics -> {
		// System.out.println("COMPONENTS : " + Arrays.toString(generics) + " " + generics[0].getComponents());
		return FXCollections.observableList(generics[0].getComponents());
	};

	public static final ObservableListExtractor HOLDERS = generics -> {
		// System.out.println("HOLDERS : " + Arrays.toString(generics) + " " + generics[1].getObservableHolders(generics[0]));

		ObservableList<Generic> holders = generics[1].getObservableHolders(generics[0]);
		// holders.addListener((ListChangeListener) c -> System.out.println(c));
		return holders;
	};

	public static final ObservableListExtractor OTHER_COMPONENTS_1 = gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1]));

	public static final ObservableListExtractor OTHER_COMPONENTS_2 = gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2]));

	public static class ATTRIBUTES_OF_TYPE implements Supplier<ObservableListExtractor> {
		@Override
		public ObservableListExtractor get() {
			return ATTRIBUTES_OF_TYPE;
		}
	}

	public static class OTHER_COMPONENTS_1 implements Supplier<ObservableListExtractor> {
		@Override
		public ObservableListExtractor get() {
			return OTHER_COMPONENTS_1;
		}
	}

	public static class SUBINSTANCES implements Supplier<ObservableListExtractor> {
		@Override
		public ObservableListExtractor get() {
			return SUBINSTANCES;
		}
	}

	public static class ATTRIBUTES_OF_INSTANCES implements Supplier<ObservableListExtractor> {
		@Override
		public ObservableListExtractor get() {
			return ATTRIBUTES_OF_INSTANCES;
		}
	}

	public static class HOLDERS implements Supplier<ObservableListExtractor> {
		@Override
		public ObservableListExtractor get() {
			return HOLDERS;
		}
	}

	public static class OTHER_COMPONENTS_2 implements Supplier<ObservableListExtractor> {
		@Override
		public ObservableListExtractor get() {
			return OTHER_COMPONENTS_2;
		}
	}

}