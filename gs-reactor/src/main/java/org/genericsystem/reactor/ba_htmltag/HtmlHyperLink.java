package org.genericsystem.reactor.ba_htmltag;

import java.util.function.Consumer;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.aa_modelproperties.ActionDefaults;
import org.genericsystem.reactor.aa_modelproperties.SelectionDefaults;
import org.genericsystem.reactor.aa_modelproperties.SwitchDefaults;
import org.genericsystem.reactor.ca_gscomponents.GSTagImpl;

import io.vertx.core.json.JsonObject;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlHyperLink extends GSTagImpl implements SelectionDefaults, SwitchDefaults, ActionDefaults {

	public HtmlHyperLink() {

	}

	public HtmlHyperLink(Tag parent) {
		super(parent);
	}

	public HtmlHyperLink(Tag parent, String text) {
		super(parent);
		setText(text);
	}

	@Override
	public String getTag() {
		return "a";
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
