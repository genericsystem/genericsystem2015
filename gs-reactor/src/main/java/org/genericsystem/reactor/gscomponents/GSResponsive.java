package org.genericsystem.reactor.gscomponents;

import java.util.function.UnaryOperator;

import org.genericsystem.reactor.Tag;

public class GSResponsive extends GSDiv {

	@SafeVarargs
	public GSResponsive(Tag parent, FlexDirection direction, UnaryOperator<Tag>... contentTagConsumers) {
		super(parent, FlexDirection.ROW);
		addStyle("flex-wrap", "wrap");
		for (UnaryOperator<Tag> contentTagConsumer : contentTagConsumers) {
			Tag tag = contentTagConsumer.apply(this);
			tag.addStyle("margin", "10px");
			tag.addStyle("padding", "10px");
			tag.addStyle("border-radius", "10px");
			tag.addStyle("background-color", "white");
			tag.addStyle("min-width", "10cm");
		}
	}
}
