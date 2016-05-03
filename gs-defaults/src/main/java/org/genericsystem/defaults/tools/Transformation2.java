package org.genericsystem.defaults.tools;

import java.util.ArrayList;
import java.util.function.Function;
import javafx.beans.Observable;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.WeakListChangeListener;
import javafx.util.Callback;
import com.sun.javafx.collections.ObservableListWrapper;

/**
 * @author Nicolas Feybesse
 *
 * @param <SOURCE>
 * @param <TARGET>
 */
@SuppressWarnings("restriction")
public class Transformation2<SOURCE, TARGET> extends ObservableListWrapper<TARGET> {

	private final ListChangeListener<SOURCE> listener = change -> {
		while (change.next()) {
			beginChange();
			if (change.wasPermutated()) {
				assert false;// Not tested after
				for (int i = change.getFrom(); i < change.getTo(); i++)
					removeSource(change.getFrom());
				int index = change.getFrom();
				for (SOURCE source : change.getList().subList(change.getFrom(), change.getTo()))
					addSource(index, source);
			} else {
				if (change.wasRemoved()) {
					for (int i = 0; i < change.getRemovedSize(); i++)
						removeSource(change.getFrom());
				}
				if (change.wasAdded()) {
					int index = change.getFrom();
					for (SOURCE source : change.getAddedSubList())
						addSource(index++, source);
				}
			}
			endChange();
		}
	};

	private final Function<SOURCE, TARGET> srcToTarget;
	private final ObservableList<SOURCE> external;

	public Transformation2(ObservableList<SOURCE> external, Function<SOURCE, TARGET> srcToTarget) {
		super(new ArrayList<>());
		this.external = external; // prevent of listener garbage collection
		this.srcToTarget = srcToTarget;
		external.addListener(new WeakListChangeListener<>(listener));
		beginChange();
		for (SOURCE element : external)
			add(srcToTarget.apply(element));
		endChange();
	}

	public Transformation2(ObservableList<SOURCE> external, Function<SOURCE, TARGET> srcToTarget, Callback<TARGET, Observable[]> extractor) {
		super(new ArrayList<>(), extractor);
		this.external = external; // prevents of listener garbage collection
		this.srcToTarget = srcToTarget;
		external.addListener(new WeakListChangeListener<>(listener));
		beginChange();
		for (SOURCE element : external)
			add(srcToTarget.apply(element));
		endChange();
	}

	public void addSource(int index, SOURCE element) {
		add(index, srcToTarget.apply(element));
	}

	public void removeSource(int index) {
		remove(index);
	}
}
