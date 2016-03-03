package org.genericsystem.ui.components;

import java.util.List;
import java.util.function.Function;

import javafx.beans.value.ObservableValue;
import javafx.scene.layout.Pane;

import org.genericsystem.distributed.ui.Element;
import org.genericsystem.distributed.ui.Model;

public class GSPane<Component extends GSPane<Component, N>, N extends Pane> extends GSRegion<Component, N> {

	public GSPane(Element<?> parent, Class<N> paneClass) {
		super(parent, paneClass);
	}

	public <PARENTNODE> GSPane(Element<?> parent, Class<N> paneClass, Function<? super PARENTNODE, List<?>> getGraphicChildren) {
		super(parent, paneClass, getGraphicChildren);
	}

	@SuppressWarnings("unchecked")
	@Override
	public <M extends Model, T extends Model> Component select(Function<M, ObservableValue<T>> function) {
		super.select(function);
		return (Component) this;
	}

	// @SuppressWarnings("unchecked")
	// public Component include(Consumer<Element<N>> subModelInit) {
	// subModelInit.accept(this);
	// return (Component) this;
	// }

}
