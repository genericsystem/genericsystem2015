package org.genericsystem.distributed.cacheonclient;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javafx.beans.binding.Bindings;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.WeakListChangeListener;

import org.genericsystem.common.Differential;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.observables.ObservableSnapshot;

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

	@Override
	protected Generic plug(Generic generic) {
		super.plug(generic);
		addsObservableList.add(generic);
		addsSnap.add(generic);
		return generic;
	}

	@Override
	protected void unplug(Generic generic) {
		super.unplug(generic);
		if (!addsObservableList.remove(generic))
			removesObservableList.add(generic);

		if (!addsSnap.remove(generic))
			removesSnap.add(generic);
	}

	@Override
	public ObservableSnapshot<Generic> getDependenciesObservableSnapshot(Generic generic) {
		ObservableValue<Predicate<Generic>> removePredicate = Bindings.<Predicate<Generic>> createObjectBinding(() -> t -> !removesSnap.contains(t), removesSnap);
		return getSubCache().getDependenciesObservableSnapshot(generic).filtered(removePredicate).concat(addsSnap.filtered(x -> generic.isDirectAncestorOf(x)));
	}

	@Override
	public ObservableList<Generic> getWrappableDependencies(Generic generic) {
		return new AbstractWrappable<Generic>() {
			private Predicate<Generic> parent = x -> generic.isDirectAncestorOf(x);
			private Predicate<Generic> removed = x -> removesObservableList.contains(x);
			private ObservableList<Generic> filteredRemoves = removesObservableList.filtered(parent);
			private ObservableList<Generic> filteredAdds = addsObservableList.filtered(parent);
			private ObservableList<Generic> subcacheGenerics = getSubCache().getWrappableDependencies(generic);
			private ListChangeListener<Generic> listenerOnAdds = new WeakListChangeListener<Generic>(change -> {
				// TODO check indices for all cases.
				// often, the sublist pointed at by indices change.getFrom(), change.getTo() doesn't match the actual list
				// it might be a misuse of nextAdd/nextRemove, and associated indices, or a problem of indices between addsObservableList and subcacheGenerics.
				// change here is an instance of SingleChange, so there's no real loop involved
					beginChange();
					while (change.next()) {
						if (change.wasPermutated() || change.wasUpdated())
							throw new UnsupportedOperationException();
						if (change.wasAdded()) {
							nextAdd(size() - change.getAddedSubList().size() - 1, size());
						}

						int currentIndex = change.getFrom();
						int changeEndIndex = change.getTo();
						final int changeStartIndex = currentIndex;
						do {
							if (change.wasAdded()) {
								Generic modifiedGeneric = change.getAddedSubList().get(currentIndex - changeStartIndex);
								if (parent.test(modifiedGeneric)) {
									System.out.println("Receive add with index : " + currentIndex);

									nextAdd(currentIndex, currentIndex + 1);
								}
							} else if (change.wasRemoved()) {
								Generic modifiedGeneric = change.getRemoved().get(currentIndex - changeStartIndex);
								if (parent.test(modifiedGeneric))
									nextRemove(currentIndex, modifiedGeneric);
							}
						} while (++currentIndex < changeEndIndex);
						endChange();
					}
				});
			private ListChangeListener<Generic> listenerOnRemoves = new WeakListChangeListener<Generic>(change -> {
				beginChange();
				while (change.next())
					if (change.wasPermutated() || change.wasUpdated())
						throw new UnsupportedOperationException();
					else {
						int currentIndex = change.getFrom();
						int changeEndIndex = change.getTo();
						final int changeStartIndex = currentIndex;
						do {
							if (change.wasAdded()) {
								Generic modifiedGeneric = change.getAddedSubList().get(currentIndex - changeStartIndex);
								if (parent.test(modifiedGeneric))
									nextRemove(currentIndex, modifiedGeneric);
							} else if (change.wasRemoved()) {
								Generic modifiedGeneric = change.getRemoved().get(currentIndex - changeStartIndex);
								if (parent.test(modifiedGeneric))
									nextAdd(currentIndex, currentIndex + 1);
							}
						} while (++currentIndex < changeEndIndex);
					}
				endChange();
			});
			private ListChangeListener<Generic> listenerOnSubcacheGenerics = new WeakListChangeListener<Generic>(change -> {
				beginChange();
				while (change.next())
					if (change.wasPermutated() || change.wasUpdated())
						throw new UnsupportedOperationException();
					else {
						int currentIndex = change.getFrom();
						int changeEndIndex = change.getTo();
						final int changeStartIndex = currentIndex;
						do {
							if (change.wasAdded()) {
								Generic modifiedGeneric = change.getAddedSubList().get(currentIndex);
								if (!removed.test(modifiedGeneric) && !contains(modifiedGeneric))
									nextAdd(currentIndex, currentIndex + 1);
							} else if (change.wasRemoved()) {
								Generic modifiedGeneric = change.getRemoved().get(currentIndex - changeStartIndex);
								nextRemove(currentIndex, modifiedGeneric);
							}
						} while (++currentIndex < changeEndIndex);
					}
				endChange();
			});
			{
				subcacheGenerics.addListener(listenerOnSubcacheGenerics);
				addsObservableList.addListener(listenerOnAdds);
				removesObservableList.addListener(listenerOnRemoves);
			}

			@Override
			public int size() {
				return (addsObservableList.contains(generic) ? 0 : subcacheGenerics.size() - filteredRemoves.size()) + filteredAdds.size();
			}

			@Override
			public Generic get(int index) {
				int subcacheGenericLength = subcacheGenerics.size(), filteredRemovesLength = filteredRemoves.size();
				if (index < subcacheGenericLength - filteredRemovesLength)
					for (int i = 0; i < subcacheGenericLength; i++) {
						if (filteredRemoves.contains(subcacheGenerics.get(i)))
							++index;
						if (i == index)
							return subcacheGenerics.get(i);
					}
				return filteredAdds.get(index - (subcacheGenericLength - filteredRemovesLength));
			}
		};
	}
}
