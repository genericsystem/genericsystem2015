package org.genericsystem.reactor.htmltag;

import org.genericsystem.reactor.modelproperties.ActionDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;
import org.genericsystem.reactor.modelproperties.StepperDefaults;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.gscomponents.GSTagImpl;

import io.vertx.core.json.JsonObject;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlHyperLink extends GSTagImpl implements SelectionDefaults, StepperDefaults, ActionDefaults {

	@Override
	public String getTag() {
		return "a";
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
