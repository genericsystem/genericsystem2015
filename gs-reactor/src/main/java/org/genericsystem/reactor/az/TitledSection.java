package org.genericsystem.reactor.az;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gstag.HtmlSection;

public class TitledSection extends HtmlSection {

	protected final FlexDirection flexDirection; // For the content’s direction.

	public TitledSection(Tag parent, FlexDirection flexDirection) {
		super(parent);
		addStyle("display", "flex");
		addStyle("flex-direction", FlexDirection.COLUMN.toString());
		addStyle("flex-wrap", "nowrap");
		this.flexDirection = flexDirection;
		titleHeader();
		content();
	}

	protected void titleHeader() {
	}

	protected void content() {
	}
}