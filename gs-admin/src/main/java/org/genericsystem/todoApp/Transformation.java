package org.genericsystem.todoApp;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import javafx.collections.ListChangeListener.Change;
import javafx.collections.ObservableList;
import javafx.collections.transformation.TransformationList;

public class Transformation<E, F> extends TransformationList<E, F> {

	private final Map<F, E> cache = new HashMap<>();
	private final Function<F, E> function;

	protected Transformation(ObservableList<? extends F> source, Function<F, E> function) {
		super(source);
		this.function = function;
	}

	@Override
	protected void sourceChanged(Change<? extends F> change) {
		while (change.next()) {
			beginChange();

			if (change.wasPermutated()) {
				nextRemove(change.getFrom(), IntStream.range(change.getFrom(), change.getTo()).mapToObj(this::get).collect(Collectors.toList()));
				nextAdd(change.getFrom(), change.getTo());
			} else {
				if (change.wasRemoved())
					nextRemove(change.getFrom(), change.getRemoved().stream().map(cache::get).collect(Collectors.toList()));
				if (change.wasAdded())
					nextAdd(change.getFrom(), change.getAddedSize());
			}

			endChange();

		}
	}

	@Override
	public int getSourceIndex(int index) {
		return index;
	}

	@Override
	public E get(int index) {
		F f = getSource().get(index);
		E e = cache.get(f);
		if (e == null)
			cache.put(f, e = function.apply(f));

		return e;
	}

	public E removeFromCache(int index) {
		return cache.remove(getSource().get(index));
	}

	@Override
	public int size() {
		return getSource().size();
	}

}
