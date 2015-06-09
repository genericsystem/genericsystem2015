package org.genericsystem.javafx;

import java.util.function.Consumer;

import javafx.collections.ListChangeListener;

import org.genericsystem.api.core.Snapshot;

import com.sun.javafx.collections.ObservableListWrapper;

/**
 * @author Nicolas Feybesse
 *
 * @param <G>
 */
public class ObservableGenericList<G> extends ObservableListWrapper<G> {

	public ObservableGenericList(Snapshot<G> links, Consumer<G> removeConsumer) {
		super(links.toList());
		addListener((ListChangeListener<G>) e -> {
			while (e.next()) {
				e.getRemoved().forEach(removeConsumer);
			}
		});
	}
}
