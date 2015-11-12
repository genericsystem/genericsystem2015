package org.genericsystem.distributed.cacheonclient;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.WeakListChangeListener;

import org.genericsystem.common.Differential;
import org.genericsystem.common.Generic;

public class AsyncDifferential extends Differential implements AsyncIDifferential {

	private final ObservableList<Generic> addsObservableList = FXCollections.observableArrayList();
	private final ObservableList<Generic> removesObservableList = FXCollections.observableArrayList();
	private final Map<Generic, ObservableValue<List<Generic>>> dependenciesCache = new HashMap<>();

	public AsyncDifferential(AsyncIDifferential subCache) {
		super(subCache);
	}

	@Override
	public AsyncIDifferential getSubCache() {
		return (AsyncIDifferential) super.getSubCache();
	}

	@Override
	public ObservableValue<List<Generic>> getDependenciesObservableList(Generic generic) {
		ObservableValue<List<Generic>> observableValue = dependenciesCache.get(generic);
		if (observableValue != null)
			return observableValue;
		return new ObjectBinding<List<Generic>>() {
			{
				bind(addsObservableList, removesObservableList, getSubCache().getDependenciesObservableList(generic));
				dependenciesCache.put(generic, this);
			}

			@Override
			public void dispose() {
				unbind(addsObservableList, removesObservableList, getSubCache().getDependenciesObservableList(generic));
			}

			@Override
			protected List<Generic> computeValue() {
				return (Stream.concat(adds.contains(generic) ? Stream.empty() : getSubCache().getDependenciesObservableList(generic).getValue().stream().filter(x -> !removes.contains(x)), adds.stream().filter(x -> generic.isDirectAncestorOf(x)))
						.collect(Collectors.toList()));
			}
		};
	}

	// public ObservableList<Generic> getActualObservableList(Generic generic) {
	// return new ListBinding<Generic>() {
	// {
	// super.bind(getDependenciesObservableList(generic));
	// }
	//
	// @Override
	// protected ObservableList<Generic> computeValue() {
	// return FXCollections.observableArrayList(dependenciesCache.get(generic).getValue());
	// }
	//
	// };
	// }

	@Override
	protected Generic plug(Generic generic) {
		super.plug(generic);
		addsObservableList.add(generic);
		return generic;
	}

	@Override
	protected void unplug(Generic generic) {
		super.unplug(generic);
		if (!addsObservableList.remove(generic))
			removesObservableList.add(generic);
	}

	@Override
	public Wrappable<Generic> getWrappableDependencies(Generic generic) {
		return new AbstractWrappable<Generic>() {

			private Predicate<Generic> parent = x -> generic.isDirectAncestorOf(x);
			private Predicate<Generic> removed = x -> removesObservableList.contains(x);

			private ObservableList<Generic> filteredAdds = addsObservableList.filtered(parent);
			private ObservableList<Generic> subGenerics = getSubCache().getWrappableDependencies(generic).filtered(x -> !removed.test(x) && parent.test(x));

			private ListChangeListener<Generic> listenerOnAdds = new WeakListChangeListener<Generic>(c -> {
				beginChange();
				while (c.next())
					if (c.wasPermutated() || c.wasUpdated())
						throw new UnsupportedOperationException();
					else {
						int from = c.getFrom();
						int to = c.getTo();
						final int fpos = from;
						do {
							if (c.wasAdded()) {
								Generic tmp = c.getAddedSubList().get(from - fpos);
								if (parent.test(tmp))
									nextAdd(from, from + 1);
							} else if (c.wasRemoved()) {
								Generic tmp = c.getRemoved().get(from - fpos);
								if (parent.test(tmp))
									nextRemove(from, tmp);
							}
						} while (++from < to);
					}
				endChange();
			});

			private ListChangeListener<Generic> listenerOnRemoves = new WeakListChangeListener<Generic>(c -> {
				beginChange();
				while (c.next())
					if (c.wasPermutated() || c.wasUpdated())
						throw new UnsupportedOperationException();
					else {
						int from = c.getFrom();
						int to = c.getTo();
						final int fpos = from;
						do {
							if (c.wasAdded()) {
								Generic tmp = c.getAddedSubList().get(from - fpos);
								if (parent.test(tmp))
									nextRemove(from, tmp);
							} else if (c.wasRemoved()) { // is the if (c.wasRemoved()) necessary?
						Generic tmp = c.getRemoved().get(from - fpos);
						if (parent.test(tmp))
							nextAdd(from, from + 1);
					}
				} while (++from < to);
			}
		endChange();
	}		);

			private ListChangeListener<Generic> listenerOnSubGenerics = new WeakListChangeListener<Generic>(c -> {
				beginChange();
				while (c.next())
					if (c.wasPermutated() || c.wasUpdated())
						throw new UnsupportedOperationException();
					else {
						int from = c.getFrom();
						int to = c.getTo();
						final int fpos = from;
						do {
							if (c.wasAdded()) {
								Generic tmp = c.getAddedSubList().get(from - fpos);
								if (!removed.test(tmp))
									nextAdd(from, from + 1);
							} else if (c.wasRemoved()) { // is the if (c.wasRemoved()) necessary?
						Generic tmp = c.getRemoved().get(from - fpos);
						nextRemove(from, tmp);
					}
				} while (++from < to);
			}
		endChange();
	}		);

			{
				subGenerics.addListener(listenerOnSubGenerics);
				addsObservableList.addListener(listenerOnAdds);
				removesObservableList.addListener(listenerOnRemoves);
			}

			@Override
			public int size() {
				return (addsObservableList.contains(generic) ? 0 : subGenerics.size()) + filteredAdds.size();
			}

			@Override
			public Generic get(int index) {
				if (index < subGenerics.size())
					return subGenerics.get(index);
				return filteredAdds.get(index - subGenerics.size());
			}
		};
	}
}
