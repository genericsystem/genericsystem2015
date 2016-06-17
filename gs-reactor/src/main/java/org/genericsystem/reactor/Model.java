package org.genericsystem.reactor;

import javafx.beans.property.Property;
import javafx.collections.ObservableMap;
import javafx.collections.ObservableSet;

//import org.genericsystem.kernel.Engine;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class Model {

	Model parent;
	Element<?> element;

	public Model getParent() {
		return parent;
	}

	public Element<?> getElement() {
		assert element != null;
		return element;
	}

	public void afterParentConstruct() {

	}

	public Property<String> getTextProperty(Element<?> element) {
		return getModelContext().getViewContext(element).getNode().getTextProperty();
	}

	public ObservableSet<String> getObservableStyleClasses(Element<?> element) {
		return getModelContext().getViewContext(element).getNode().getStyleClasses();
	}

	public ObservableMap<String, String> getObservableStyles(Element<?> element) {
		assert getModelContext() != null;
		assert getModelContext().getViewContext(element) != null;
		return getModelContext().getViewContext(element).getNode().getStyles();
	}

	ModelContext modelContext;

	ModelContext getModelContext() {
		return modelContext;
	}

}
