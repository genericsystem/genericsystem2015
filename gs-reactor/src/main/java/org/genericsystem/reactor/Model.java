package org.genericsystem.reactor;

import java.util.HashMap;
import java.util.Map;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
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

	private Map<Element<?>, Property<String>> observableTexts = new HashMap<Element<?>, Property<String>>() {
		private static final long serialVersionUID = -1827306835524845605L;

		@Override
		public Property<String> get(Object key) {
			Property<String> result = super.get(key);
			if (result == null)
				put((Element<?>) key, result = new SimpleStringProperty());
			return result;
		};
	};

	public Property<String> getTextProperty(Element<?> element) {
		return observableTexts.get(element);
	}

	private Map<Element<?>, ObservableSet<String>> observableStyleClasses = new HashMap<Element<?>, ObservableSet<String>>() {
		private static final long serialVersionUID = -1827306835524845605L;

		@Override
		public ObservableSet<String> get(Object key) {
			ObservableSet<String> result = super.get(key);
			if (result == null)
				put((Element<?>) key, result = FXCollections.observableSet());
			return result;
		};

	};

	public ObservableSet<String> getObservableStyleClasses(Element<?> element) {
		return observableStyleClasses.get(element);
	}

	private Map<Element<?>, ObservableMap<String, String>> observableStyles = new HashMap<Element<?>, ObservableMap<String, String>>() {
		private static final long serialVersionUID = -1827306835524845605L;

		@Override
		public ObservableMap<String, String> get(Object key) {
			ObservableMap<String, String> result = super.get(key);
			if (result == null)
				put((Element<?>) key, result = FXCollections.observableHashMap());
			return result;
		};
	};

	public ObservableMap<String, String> getObservableStyles(Element<?> element) {
		return observableStyles.get(element);
	}

}
