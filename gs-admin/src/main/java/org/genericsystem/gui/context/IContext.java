package org.genericsystem.gui.context;

import java.util.Collection;
import java.util.function.Function;

import javafx.beans.Observable;
import javafx.beans.binding.ListBinding;
import javafx.collections.ListChangeListener.Change;
import javafx.collections.ObservableList;
import javafx.collections.transformation.TransformationList;

import org.genericsystem.common.AbstractCache;

public interface IContext {

	IContext getParent();

	AbstractCache getCurrentCache();

	static <T, U extends IContext> ObservableList<U> getSubContextsObservableList(ObservableList<T> source, Function<T, U> extractor, Observable... propertiesToBind) {
		return new ListBinding<U>() {
			{
				super.bind(propertiesToBind);
			}

			@Override
			protected ObservableList<U> computeValue() {
				return transform(source, extractor);
			}
		};
	}

	static <T, U extends IContext> ObservableList<U> transform(ObservableList<T> source, Function<T, U> extractor) {
		return new TransformationList<U, T>(source) {
			@Override
			protected void sourceChanged(Change<? extends T> change) {
				beginChange();
				while (change.next()) {
					if (change.wasPermutated()) {
						throw new UnsupportedOperationException();
					} else if (change.wasUpdated()) {
						throw new UnsupportedOperationException();
					} else {
						if (change.wasRemoved()) {
							nextRemove(change.getFrom(), (U) null);
						}
						if (change.wasAdded()) {
							nextAdd(change.getFrom(), change.getFrom() + change.getAddedSubList().size());
						}
					}
				}
				endChange();
			}

			@Override
			public U get(int index) {
				return extractor.apply(source.get(index));
			}

			@Override
			public int size() {
				return getSource().size();
			}

			@Override
			public int getSourceIndex(int index) {
				return index;
			}

			@Override
			public boolean addAll(U... elements) {
				throw new UnsupportedOperationException();
			}

			@Override
			public boolean setAll(U... elements) {
				throw new UnsupportedOperationException();
			}

			@Override
			public boolean setAll(Collection<? extends U> col) {
				throw new UnsupportedOperationException();
			}

			@Override
			public boolean removeAll(U... elements) {
				throw new UnsupportedOperationException();
			}

			@Override
			public boolean retainAll(U... elements) {
				throw new UnsupportedOperationException();
			}

			@Override
			public void remove(int from, int to) {
				throw new UnsupportedOperationException();
			}
		};
	};
}
