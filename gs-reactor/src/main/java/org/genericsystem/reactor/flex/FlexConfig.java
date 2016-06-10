package org.genericsystem.reactor.flex;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.composite.CompositeModel;

import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

public class FlexConfig {

	private final String tag;
	private JsonObject json;

	public FlexConfig(String tag, String direction) {
		this.tag = tag;
		JsonObject styles = new JsonObject();
		styles.put("display", "flex");
		styles.put("tag", tag);
		styles.put("direction", direction);
		json.put("styles", styles);
		json.put("children", new JsonArray());
		json.put("metabinding", new JsonObject());
	}

	public JsonObject getJson() {
		return json;
	}

	public FlexConfig addStyle(String propertyName, String value) {
		json.getJsonObject("styles").put(propertyName, value);
		return this;
	}

	public FlexConfig addChild(FlexConfig subElementConfig) {
		json.getJsonArray("children").add(subElementConfig.getJson());
		return this;
	}

	public FlexConfig addMetaBinding() {
		// json.getJsonObject("metaBinding").put(propertyName, value);
		return this;
	}

	public <M extends CompositeModel> Element<M> createElement(Element<?> parent) {
		Element<M> element = new Element<M>(parent, tag) {
			{
				json.getJsonObject("styles").forEach(entry -> addStyle(entry.getKey(), (String) entry.getValue()));
				json.getJsonArray("children").forEach(child -> createElement(this));
			}

			@Override
			protected HtmlDomNode createNode(String parentId) {
				return new HtmlDomNode(parentId);
			}
		};

		return element;

	}
}
