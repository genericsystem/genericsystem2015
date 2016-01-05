package org.genericsystem.defaults.tools;

import java.util.ArrayList;
import java.util.List;

import javafx.beans.binding.ListBinding;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import com.sun.javafx.collections.ObservableListWrapper;

@SuppressWarnings("restriction")
public class ObservableListConcat<T> extends ListBinding<T> {

	private final List<ObservableList<T>> slaves = new ArrayList<>();

	public void addAllAndBind(ObservableList<T> list) {
		bind(list);
		slaves.add(list);
		invalidate();
	}

	@Override
	public void clear() {
		super.clear();
		for (ObservableList<T> slave : slaves)
			unbind(slave);
		slaves.clear();
	}

	@Override
	protected ObservableList<T> computeValue() {
		List<T> list = new ArrayList<>();
		for (ObservableList<T> slave : slaves)
			list.addAll(slave);
		return FXCollections.unmodifiableObservableList(new ObservableListWrapper<>(list));
	}

}
