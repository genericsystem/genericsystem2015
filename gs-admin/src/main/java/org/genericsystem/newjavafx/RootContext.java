package org.genericsystem.newjavafx;

import java.util.function.Function;

import javafx.beans.binding.ListBinding;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.HeavyClientEngine;

public class RootContext implements IContext {

	private final HeavyClientEngine cache;

	public ObjectProperty<Generic> rootProperty;

	public ObservableList<Generic> observableGenericList;

	public Function<Generic, ObservableList<Generic>> genericSubInstances;

	public RootContext(HeavyClientEngine cache) {

		this.cache = cache;

		rootProperty = new SimpleObjectProperty<Generic>(cache.getRoot());
		genericSubInstances = type -> rootProperty.getValue() != null ? this.cache.getCurrentCache().getObservableInstances(rootProperty.getValue()) : FXCollections.emptyObservableList();

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

	public HeavyClientEngine getCache() {
		return cache;
	}

}
