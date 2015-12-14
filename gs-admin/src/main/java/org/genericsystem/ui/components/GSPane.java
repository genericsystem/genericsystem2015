package org.genericsystem.ui.components;

import java.util.function.Consumer;
import java.util.function.Function;
import javafx.beans.property.Property;
import javafx.collections.ObservableList;
import javafx.scene.layout.Pane;
import org.genericsystem.ui.Element;

public class GSPane<Component extends GSPane<Component, N>, N extends Pane> extends GSRegion<Component, N> {

	public GSPane(Element parent, Class<N> paneClass) {
		super(parent, paneClass);
	}

	public <PARENTNODE> GSPane(Element parent, Class<N> paneClass, Function<? super PARENTNODE, ObservableList<?>> getGraphicChildren) {
		super(parent, paneClass, getGraphicChildren);
	}

	@Override
	public <M, T> Component addForEachMetaBinding(Function<M, ObservableList<T>> function, Function<T, Property<M>> injectedProperty, Consumer<Element<N>> subModelInit) {
		super.addForEachMetaBinding(function, injectedProperty, subModelInit);
		return (Component) this;
	}

}
