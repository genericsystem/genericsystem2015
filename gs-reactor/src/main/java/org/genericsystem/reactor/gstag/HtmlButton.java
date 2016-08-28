package org.genericsystem.reactor.gstag;

import io.vertx.core.json.JsonObject;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.modelproperties.ActionDefaults;
import org.genericsystem.reactor.modelproperties.GSBuilderDefaults;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlButton extends Tag implements ActionDefaults, GSBuilderDefaults {

	public HtmlButton(Tag parent) {
		super(parent, "button");
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
