package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gs.GSTag;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.modelproperties.ActionDefaults;
import org.genericsystem.reactor.modelproperties.GSBuilderDefaults;

import io.vertx.core.json.JsonObject;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlButton extends GSTag implements ActionDefaults<GenericModel>, GSBuilderDefaults {

	public HtmlButton(GSTag parent) {
		super(parent, "button");
	}

	@Override
	protected HtmlDomNode createNode(HtmlDomNode parent, Model modelContext, Tag tag) {
		return new HtmlDomNode(parent, modelContext, tag) {

			@Override
			public void handleMessage(JsonObject json) {
				((ActionDefaults<?>) getTag()).getAction(getModelContext()).accept(new Object());
			}
		};
	}
}
