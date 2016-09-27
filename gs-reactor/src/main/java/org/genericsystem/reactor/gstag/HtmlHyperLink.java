package org.genericsystem.reactor.gstag;

import java.util.function.Consumer;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.az.GSTagImpl;
import org.genericsystem.reactor.modelproperties.ActionDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;
import org.genericsystem.reactor.modelproperties.SwitchDefaults;

import io.vertx.core.json.JsonObject;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlHyperLink extends GSTagImpl implements SelectionDefaults, SwitchDefaults, ActionDefaults {

	public HtmlHyperLink() {
		super("a");
	}

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
	public HtmlDomNode createNode(HtmlDomNode parent, Context modelContext) {
		return new HtmlDomNode(parent, modelContext, this) {

			@Override
			public void handleMessage(JsonObject json) {
				((ActionDefaults) getTag()).getAction(getModelContext()).accept(new Object());
			}
		};
	}
}
