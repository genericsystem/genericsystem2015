package org.genericsystem.common;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Stream;

import org.genericsystem.api.core.IteratorSnapshot;

import com.sun.javafx.collections.ObservableListWrapper;

public class ObservableContainer extends ObservableListWrapper<Generic> implements IteratorSnapshot<Generic>, Consumer<List<Generic>> {

	public ObservableContainer() {
		super(new ArrayList<>());
	}

	@Override
	public Stream<Generic> stream() {
		return super.stream();
	}

	@Override
	public Generic get(Object o) {
		return contains(o) ? (Generic) o : null;
	}

	@Override
	public void accept(List<Generic> requestedValues) {
		addAll(requestedValues);
	}
}