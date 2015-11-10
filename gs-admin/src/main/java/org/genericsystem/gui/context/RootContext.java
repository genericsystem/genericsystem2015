package org.genericsystem.gui.context;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.ListChangeListener.Change;
import javafx.collections.ObservableList;
import javafx.collections.transformation.TransformationList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.CocCache;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;

public class RootContext extends AbstractContext {

	public ObjectProperty<CocClientEngine> rootProperty = new SimpleObjectProperty<CocClientEngine>();
	public ObservableList<SubContext> observableGenericList;

	// public ObservableList<Generic> observableSubContext;

	public RootContext(CocClientEngine engine) {
		super(null);
		rootProperty.set(engine);
		observableGenericList = transform(getCurrentCache().getInstancesObservableList(rootProperty.getValue()));
		// observableSubContext = new;

	}

	private ObservableList<SubContext> transform(ObservableList<Generic> instancesObservableList) {
		// TODO Auto-generated method stub
		return new TransformationList<SubContext, Generic>(instancesObservableList) {
			@Override
			protected void sourceChanged(Change<? extends Generic> change) {
				beginChange();
				while (change.next()) {
					if (change.wasAdded()) {
						for (int i = change.getFrom(); i < change.getTo(); i++) {
							add(i, get(i));
						}
					} else
						for (int i = change.getFrom(); i < change.getTo(); i++) {
							remove(i);
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
				return new SubContext(RootContext.this, index);
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
