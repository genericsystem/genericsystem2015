package org.genericsystem.gui.context;

import java.util.function.Function;

import javafx.beans.binding.ListBinding;
import javafx.beans.property.ObjectProperty;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener.Change;
import javafx.collections.ObservableList;
import javafx.collections.transformation.TransformationList;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.DefaultCache;

public abstract class AbstractContext implements IContext {

	private final IContext parent;

	public AbstractContext(IContext parent) {
		this.parent = parent;
	}

	@Override
	public IContext getParent() {
		return this.parent;
	}

	@Override
	public DefaultCache<Generic> getCurrentCache() {
		return getParent().getCurrentCache();
	};

	protected ObservableList<SubContext> genericToSubContext(Function<Generic, SubContext> transfomation, ObjectProperty... rootProperty) {
		return new ListBinding<SubContext>() {
			{
				super.bind(rootProperty);
			}

			@Override
			protected ObservableList<SubContext> computeValue() {
				return transform(FXCollections.observableArrayList(getCurrentCache().getInstances((Generic) rootProperty[0].getValue()).toList()), transfomation);
				// return transform(getCurrentCache().getInstancesObservableList(((Generic) rootProperty[0].getValue())), transfomation);
			}
		};
	}

	protected ObservableList<SubContext> transform(ObservableList<Generic> instancesObservableList, Function<Generic, SubContext> transfomation) {
		return new TransformationList<SubContext, Generic>(instancesObservableList) {
			@Override
			protected void sourceChanged(Change<? extends Generic> change) {
				beginChange();
				while (change.next()) {
					if (change.wasPermutated()) {
						throw new UnsupportedOperationException();
					} else if (change.wasUpdated()) {
						throw new UnsupportedOperationException();
					} else {
						addRemove(change);
					}
				}
				endChange();
			}

			private void addRemove(Change<? extends Generic> c) {
				if (c.wasPermutated()) {
					throw new UnsupportedOperationException();
				} else {
					if (c.wasRemoved()) {
						nextRemove(c.getFrom(), (SubContext) null);
					}
					if (c.wasAdded()) {
						nextAdd(c.getFrom(), c.getFrom() + c.getAddedSubList().size());
					}
				}

			}

			@Override
			public int getSourceIndex(int index) {
				return index;
			}

			@Override
			public SubContext get(int index) {
				// SubContext subContext = new SubContext(AbstractContext.this);
				// subContext.observableGeneric = new ReadOnlyObjectWrapper<Generic>(instancesObservableList.get(index));
				return transfomation.apply(instancesObservableList.get(index));
			}

			@Override
			public int size() {
				return getSource().size();
			}

		};
	}
}
