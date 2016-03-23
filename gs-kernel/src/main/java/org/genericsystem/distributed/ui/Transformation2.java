package org.genericsystem.distributed.ui;

import java.util.ArrayList;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.WeakListChangeListener;

import com.sun.javafx.collections.ObservableListWrapper;

@SuppressWarnings("restriction")
public class Transformation2<E, F> extends ObservableListWrapper<F> {

	private final ObservableList<E> external;
	private final ListChangeListener<E> listener;
	private final Function<E, F> wrap;
	private final Function<F, E> unwrap;

	public Transformation2(ObservableList<E> external, Function<E, F> wrap, Function<F, E> unwrap) {
		super(new ArrayList<>());
		this.external = external;
		this.wrap = wrap;
		this.unwrap = unwrap;
		external.addListener(listener = new WeakListChangeListener<E>(change -> {
			while (change.next()) {
				beginChange();

				if (change.wasPermutated()) {
					assert false;
					nextRemove(change.getFrom(), IntStream.range(change.getFrom(), change.getTo()).mapToObj(this::remove).collect(Collectors.toList()));
					nextAdd(change.getFrom(), change.getTo());
				} else {
					if (change.wasRemoved()) {
						nextRemove(change.getFrom(), change.getRemoved().stream().map(cache::remove).collect(Collectors.toList()));
					}
					if (change.wasAdded())
						nextAdd(change.getFrom(), change.getAddedSize());
				}

				endChange();

			}
		}));
		beginChange();
		for (E element : external)
			add(wrap.apply(element));
		endChange();
	}
}
