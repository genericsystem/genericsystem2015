package org.genericsystem.reactor;

import java.util.HashMap;
import java.util.Map;

import org.genericsystem.reactor.context.TagSwitcher;

import com.sun.javafx.collections.ObservableListWrapper;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;

class FilteredChildren<T> {

	final Map<T, ObservableList<TagSwitcher>> selectorsByChild = new HashMap<>();// Prevents garbage collection
	final Map<T, Map<TagSwitcher, ObservableValue<Boolean>>> selectorsByChildAndSwitcher = new HashMap<T, Map<TagSwitcher, ObservableValue<Boolean>>>() {

		private static final long serialVersionUID = -5831485781427983238L;

		@Override
		public Map<TagSwitcher, ObservableValue<Boolean>> get(Object key) {
			Map<TagSwitcher, ObservableValue<Boolean>> result = super.get(key);
			if (result == null)
				put((T) key, result = new HashMap<>());
			return result;
		}
	};

	static class FilteredTagChildren extends FilteredChildren<Tag> {

		final ObservableList<Tag> filteredList;

		FilteredTagChildren(Tag tag, Context context) {
			filteredList = new FilteredList<>(new ObservableListWrapper<>(context.getRootContext().getObservableChildren(tag), child -> {
				if (child.getMetaBinding() != null)
					return new ObservableValue[] {};
				ObservableList<TagSwitcher> result = new ObservableListWrapper<>(child.getObservableSwitchers(), s -> {
					Property<Boolean> selector = new SimpleBooleanProperty();
					context.getHtmlDomNode(tag).getDisposables().add(context.getCache().safeSupply(() -> s.apply(context, child)).subscribe(bool -> selector.setValue(bool)));
					selectorsByChildAndSwitcher.get(child).put(s, selector);
					return new ObservableValue[] { selector };
				});
				selectorsByChild.put(child, result);
				return new ObservableList[] { result };
			}), child -> child.getMetaBinding() != null || selectorsByChildAndSwitcher.get(child).entrySet().stream().allMatch(entry -> !selectorsByChild.get(child).contains(entry.getKey()) || Boolean.TRUE.equals(entry.getValue().getValue())));
		}
	}
}