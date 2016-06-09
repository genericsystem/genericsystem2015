package org.genericsystem.reactor.html;

import java.util.function.Consumer;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.Model;

import javafx.event.Event;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlHyperLink<M extends Model> extends Element<M> {

	public HtmlHyperLink(Element<?> parent, String text) {
		super(parent, "a");
		addBoot(HtmlDomNode::getText, text);
	}

	public HtmlHyperLink(Element<?> parent, String text, Consumer<M> action) {
		this(parent, text);
		setAction(action);
	}

	public <T extends Event> HtmlHyperLink<M> setAction(Consumer<M> consumer) {
		addActionBinding(ActionHtmlNode::getActionProperty, consumer);
		return this;
	}

	@Override
	protected ActionHtmlNode createNode(String parentId) {
		return new ActionHtmlNode(parentId);
	}

}
