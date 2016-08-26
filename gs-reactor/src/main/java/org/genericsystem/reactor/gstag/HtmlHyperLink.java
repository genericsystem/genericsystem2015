package org.genericsystem.reactor.gstag;

import java.util.function.Consumer;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.gs.GSTag;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.modelproperties.ActionDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;
import org.genericsystem.reactor.modelproperties.SwitchDefaults;

import io.vertx.core.json.JsonObject;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlHyperLink extends GSTag implements SelectionDefaults, SwitchDefaults, ActionDefaults<GenericModel> {

	public HtmlHyperLink(GSTag parent) {
		super(parent, "a");
	}

	public HtmlHyperLink(GSTag parent, String text) {
		super(parent, "a");
		setText(text);
	}

	public HtmlHyperLink(GSTag parent, String text, Consumer<GenericModel> action) {
		this(parent, text);
		bindAction(action);
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId) {

			@Override
			public void handleMessage(JsonObject json) {
				((ActionDefaults<?>) viewContext.getTag()).getAction(viewContext.getModelContext()).accept(new Object());
			}
		};
	}
}
