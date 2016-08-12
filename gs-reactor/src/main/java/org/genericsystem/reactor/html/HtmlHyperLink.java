package org.genericsystem.reactor.html;

import java.util.function.Consumer;

import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlHyperLink<M extends Model> extends Tag<M> {

	public HtmlHyperLink(Tag<?> parent) {
		super(parent, "a");
	}

	public HtmlHyperLink(Tag<?> parent, String text) {
		super(parent, "a");
		setText(this, text);
	}

	public HtmlHyperLink(Tag<?> parent, String text, Consumer<M> action) {
		this(parent, text);
		bindAction(action);
	}

	public void bindAction(Consumer<M> consumer) {
		addActionBinding(ActionHtmlNode::getActionProperty, consumer);
	}

	@Override
	protected ActionHtmlNode createNode(String parentId) {
		return new ActionHtmlNode(parentId);
	}

}
