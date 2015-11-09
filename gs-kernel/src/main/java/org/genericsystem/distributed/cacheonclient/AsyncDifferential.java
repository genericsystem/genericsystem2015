package org.genericsystem.distributed.cacheonclient;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javafx.beans.binding.ListBinding;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

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

	public ObservableList<Generic> getActualObservableList(Generic generic) {
		return new ListBinding<Generic>() {
			{
				super.bind(getDependenciesObservableList(generic));
			}

			@Override
			protected ObservableList<Generic> computeValue() {
				return FXCollections.observableArrayList(dependenciesCache.get(generic).getValue());
			}

		};
	}

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
			removesObservableList.remove(generic);
	}

}
