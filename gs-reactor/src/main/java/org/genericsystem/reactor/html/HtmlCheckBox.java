package org.genericsystem.reactor.html;

import java.util.function.BiConsumer;

import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;

import javafx.collections.MapChangeListener;
import javafx.collections.ObservableMap;
import javafx.collections.WeakMapChangeListener;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlCheckBox<M extends Model> extends Tag<M> {

	public HtmlCheckBox(Tag<?> parent) {
		super(parent, "input");
	}

	@Override
	protected InputCheckHtmlDomNode createNode(String parentId) {
		return new InputCheckHtmlDomNode(parentId, "checkbox");
	}

	public void bindAction(BiConsumer<M, String> consumer) {
		addPrefixBinding(model -> {
			ObservableMap<String, String> map = model.getObservableAttributes(this);

			MapChangeListener<String, String> attributesListener = change -> {
				if (change.getKey().equalsIgnoreCase(ReactorStatics.CHECKED))
					consumer.accept(model, change.getValueAdded());
			};
			map.addListener(new WeakMapChangeListener<>(attributesListener));
		});
	}

}
