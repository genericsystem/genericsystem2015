package org.genericsystem.reactor.gstag;

import io.vertx.core.json.JsonObject;

import java.util.function.Consumer;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.modelproperties.ActionDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;
import org.genericsystem.reactor.modelproperties.SwitchDefaults;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlHyperLink extends Tag implements SelectionDefaults, SwitchDefaults, ActionDefaults {

	public HtmlHyperLink(Tag parent) {
		super(parent, "a");
	}

	public HtmlHyperLink(Tag parent, String text) {
		super(parent, "a");
		setText(text);
	}

	public HtmlHyperLink(Tag parent, String text, Consumer<Context> action) {
		this(parent, text);
		bindAction(action);
	}

	@Override
	protected HtmlDomNode createNode(HtmlDomNode parent, Context modelContext, Tag tag) {
		return new HtmlDomNode(parent, modelContext, tag) {

			@Override
			public void handleMessage(JsonObject json) {
				((ActionDefaults) getTag()).getAction(getModelContext()).accept(new Object());
			}
		};
	}
}
