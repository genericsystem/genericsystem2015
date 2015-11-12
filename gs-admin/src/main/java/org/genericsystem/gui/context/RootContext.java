package org.genericsystem.gui.context;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.ListChangeListener.Change;
import javafx.collections.ObservableList;
import javafx.collections.transformation.TransformationList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.CocCache;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;

public class RootContext extends AbstractContext {

	public ObjectProperty<CocClientEngine> rootProperty = new SimpleObjectProperty<CocClientEngine>();
	public ObservableList<SubContext> observableSubContextList;

	public RootContext(CocClientEngine engine) {
		super(null);
		rootProperty.set(engine);
		observableSubContextList = transform(getCurrentCache().getInstancesObservableList(rootProperty.getValue()));
		for (SubContext sub : observableSubContextList) {
			System.out.println(sub.observableGeneric.getValue());
		}
	}

	private ObservableList<SubContext> transform(ObservableList<Generic> instancesObservableList) {
		return new TransformationList<SubContext, Generic>(instancesObservableList) {
			@Override
			protected void sourceChanged(Change<? extends Generic> change) {
				beginChange();
				while (change.next()) {
					for (int i = change.getFrom(); i < change.getTo(); i++) {
						if (change.wasAdded()) {
							add(i, get(i));
						} else if (change.wasRemoved()) {
							remove(i);
						}
					}
				}
				endChange();
			}

			@Override
			public int getSourceIndex(int index) {
				return index;
			}

			@Override
			public SubContext get(int index) {
				SubContext subContext = new SubContext(RootContext.this, index);
				subContext.observableGeneric = new ReadOnlyObjectWrapper<Generic>(instancesObservableList.get(index));
				return subContext;
			}

			@Override
			public int size() {
				return getSource().size();
			}

		};
	}

	@Override
	public CocCache getCurrentCache() {
		return rootProperty.getValue().getCurrentCache();
	}

}
