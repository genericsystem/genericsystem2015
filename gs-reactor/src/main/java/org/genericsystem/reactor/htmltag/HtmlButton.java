package org.genericsystem.reactor.htmltag;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gscomponents.GSTagImpl;

import org.genericsystem.reactor.modelproperties.ActionDefaults;
import org.genericsystem.reactor.modelproperties.GSBuilderDefaults;

import io.vertx.core.json.JsonObject;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlButton extends GSTagImpl implements ActionDefaults, GSBuilderDefaults {

	public HtmlButton() {

	}

	@Override
	public String getTag() {
		return "button";
	}

	public HtmlButton(Tag parent) {
		super(parent);
	}

	@Override
	public HtmlDomNode createNode(HtmlDomNode parent, Context modelContext) {
		return new HtmlDomNode(parent, modelContext, this) {

			@Override
			public void handleMessage(JsonObject json) {
				((ActionDefaults) getTag()).getActionProperty(getModelContext()).getValue().accept(new Object());
			}
		};
	}
}
