package org.genericsystem.reactor.components;

import java.util.function.Consumer;
import javafx.event.Event;
import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.HtmlElement.ActionHtmlNode;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlHyperLink<M extends Model> extends HtmlElement<M, HtmlHyperLink<M>, ActionHtmlNode> {

	public HtmlHyperLink(HtmlElement<?, ?, ?> parent, String text) {
		super(parent, ActionHtmlNode.class);
		addBoot(HtmlDomNode::getText, text);
	}

	public HtmlHyperLink(HtmlElement<?, ?, ?> parent, String text, Consumer<M> action) {
		this(parent, text);
		setAction(action);
	}

	public <T extends Event> HtmlHyperLink<M> setAction(Consumer<M> consumer) {
		addActionBinding(ActionHtmlNode::getActionProperty, consumer);
		return this;
	}

	@Override
	protected ActionHtmlNode createNode(Object parent) {
		return new ActionHtmlNode("a");
	}

}
