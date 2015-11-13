package org.genericsystem.gui.context;

import java.util.function.Function;
import java.util.function.Supplier;

import javafx.beans.Observable;
import javafx.beans.binding.ListBinding;
import javafx.collections.ListChangeListener.Change;
import javafx.collections.ObservableList;
import javafx.collections.transformation.TransformationList;

import org.genericsystem.common.AbstractCache;

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
	public AbstractCache getCurrentCache() {
		return getParent().getCurrentCache();
	};

	public <T, U extends AbstractContext> ObservableList<U> getSubContextsObservableList(Function<T, U> extractor, Supplier<ObservableList<T>> originalListSupplier, Observable... propertiesToBind) {
		return new ListBinding<U>() {
			{
				super.bind(propertiesToBind);
			}

			@Override
			protected ObservableList<U> computeValue() {
				return transform(originalListSupplier.get(), extractor);
				// return transform(getCurrentCache().getInstancesObservableList(((Generic) rootProperty[0].getValue())), transfomation);
			}
		};
	}

	protected <T, U extends AbstractContext> ObservableList<U> transform(ObservableList<T> instancesObservableList, Function<T, U> transformation) {
		return new TransformationList<U, T>(instancesObservableList) {
			@Override
			protected void sourceChanged(Change<? extends T> change) {
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

			private void addRemove(Change<? extends T> c) {
				if (c.wasPermutated()) {
					throw new UnsupportedOperationException();
				} else {
					if (c.wasRemoved()) {
						nextRemove(c.getFrom(), (U) null);
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
			public U get(int index) {
				// SubContext subContext = new SubContext(AbstractContext.this);
				// subContext.observableGeneric = new ReadOnlyObjectWrapper<Generic>(instancesObservableList.get(index));
				return transformation.apply(instancesObservableList.get(index));
			}

			@Override
			public int size() {
				return getSource().size();
			}

		};
	}
}
