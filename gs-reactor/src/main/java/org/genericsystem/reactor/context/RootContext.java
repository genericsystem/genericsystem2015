package org.genericsystem.reactor.context;

import java.util.HashMap;
import java.util.Map;

import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;

import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 */
public class RootContext extends Context {

	private Map<Tag, ObservableList<Tag>> observableSubTags = new HashMap<Tag, ObservableList<Tag>>() {
		@Override
		public ObservableList<Tag> get(Object key) {
			ObservableList<Tag> result = super.get(key);
			if (result == null)
				put((Tag) key, result = build((Tag) key));
			return result;
		};
	};

	Map<Tag, ObservableList<Tag>> getObservableSubTags() {
		return this.observableSubTags;
	}

	// Method to override with a transformation of generic.getObservableInheritings() from GS an observablelist of Tags
	public ObservableList<Tag> build(Tag tag) {
		return tag.getObservableChildren();
	}

	public RootContext(Root engine) {
		super(null, new Generic[] { engine });
	}

	@Override
	public RootContext getRootContext() {
		return this;
	}

	public ObservableList<Tag> getObservableChildren(Tag tag) {
		return observableSubTags.get(tag);
	}

}
