package org.genericsystem.newjavafx;

import java.util.function.Function;

import javafx.beans.binding.ListBinding;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;

public class RootContext implements IContext {

	private final CocClientEngine engine;

	public ObjectProperty<Generic> rootProperty;

	public ObservableList<Generic> observableGenericList;

	public Function<Generic, ObservableList<Generic>> genericSubInstances;

	public RootContext(CocClientEngine cache) {
		this.engine = cache;
		rootProperty = new SimpleObjectProperty<Generic>(cache.getRoot());
		genericSubInstances = type -> rootProperty.getValue() != null ? cache.getCurrentCache().getInstancesObservableList(rootProperty.getValue()) : FXCollections.emptyObservableList();
		observableGenericList = new ListBinding<Generic>() {
			{
				super.bind(rootProperty);
			}

			@Override
			public void dispose() {
				super.unbind(rootProperty);
			}

			@Override
			protected ObservableList<Generic> computeValue() {
				System.out.println("Compute items for : " + rootProperty.getValue());
				return genericSubInstances.apply(rootProperty.getValue());
			}
		};
	}

	public CocClientEngine getEngine() {
		return engine;
	}

}
