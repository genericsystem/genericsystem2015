package org.genericsystem.distributed.cacheonclient;

import java.util.stream.Collectors;
import java.util.stream.Stream;

import javafx.beans.binding.ListBinding;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.common.Differential;
import org.genericsystem.common.Generic;

public class AsyncDifferential extends Differential implements AsyncIDifferential {

	private final ObservableList<Generic> addsObservableList = FXCollections.observableArrayList();
	private final ObservableList<Generic> removesObservableList = FXCollections.observableArrayList();

	public AsyncDifferential(AsyncIDifferential subCache) {
		super(subCache);
	}

	@Override
	public AsyncIDifferential getSubCache() {
		return (AsyncIDifferential) super.getSubCache();
	}

	@Override
	public ObservableList<Generic> getDependenciesObservableList(Generic generic) {
		return new ListBinding<Generic>() {
			{
				super.bind(addsObservableList, removesObservableList, getSubCache().getDependenciesObservableList(generic));
			}

			@Override
			public void dispose() {
				super.unbind(addsObservableList, removesObservableList, getSubCache().getDependenciesObservableList(generic));
			}

			@Override
			protected ObservableList<Generic> computeValue() {
				return FXCollections.observableArrayList(Stream.concat(adds.contains(generic) ? Stream.empty() : getSubCache().getDependenciesObservableList(generic).stream().filter(x -> !removes.contains(x)),
						adds.stream().filter(x -> generic.isDirectAncestorOf(x))).collect(Collectors.toList()));
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
