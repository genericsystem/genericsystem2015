package org.genericsystem.reactor.ba_htmltag;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.aa_modelproperties.ActionDefaults;
import org.genericsystem.reactor.aa_modelproperties.GSBuilderDefaults;
import org.genericsystem.reactor.ca_gscomponents.GSTagImpl;

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
				((ActionDefaults) getTag()).getAction(getModelContext()).accept(new Object());
			}
		};
	}
}
