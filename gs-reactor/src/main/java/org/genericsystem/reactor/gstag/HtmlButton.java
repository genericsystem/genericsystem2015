package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gs.GSTagImpl;
import org.genericsystem.reactor.modelproperties.ActionDefaults;
import org.genericsystem.reactor.modelproperties.GSBuilderDefaults;

import io.vertx.core.json.JsonObject;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlButton extends GSTagImpl implements ActionDefaults, GSBuilderDefaults {

	public HtmlButton() {
		super("button");
	}

	public HtmlButton(Tag parent) {
		super(parent, "button");
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
