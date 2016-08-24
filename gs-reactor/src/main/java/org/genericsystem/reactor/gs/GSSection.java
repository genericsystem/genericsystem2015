package org.genericsystem.reactor.gs;

import java.util.function.BiConsumer;
import java.util.function.Consumer;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.gstag.HtmlH1;
import org.genericsystem.reactor.gstag.HtmlH2;

public class GSSection extends GSTag {
	private final FlexDirection direction;

	public GSSection(GSTag parent) {
		this(parent, FlexDirection.COLUMN);
	}

	public GSSection(GSTag parent, FlexDirection direction) {
		super(parent, "section");
		this.direction = direction;
		addStyle("display", "flex");
		addStyle("flex-direction", direction.toString());
		addStyle("flex-wrap", "nowrap");
	}

	public FlexDirection getDirection() {
		return direction;
	}

	public FlexDirection getReverseDirection() {
		return getDirection().reverse();
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

	public static class GenericColumn extends GSSection {
		public GenericColumn(GSTag parent) {
			super(parent, FlexDirection.COLUMN);
		}
	}

	public static class GenericRow extends GSSection {
		public GenericRow(GSTag parent) {
			super(parent, FlexDirection.ROW);
		}
	}

	public static class GenericRowWrapper extends GSSection {
		public GenericRowWrapper(GSTag parent, FlexDirection direction, Consumer<GSTag> consumer) {
			super(parent, direction);
			addStyle("justify-content", "center");
			consumer.accept(this);
		}

		public <T> GenericRowWrapper(GSTag parent, FlexDirection direction, BiConsumer<GSTag, T> consumer, T arg) {
			super(parent, direction);
			addStyle("justify-content", "center");
			consumer.accept(this, arg);
		}
	}

	public static class GenericH1Section extends GenericRowWrapper {
		public GenericH1Section(GSTag parent, String text) {
			super(parent, FlexDirection.ROW, HtmlH1::new, text);
		}
	}

	public static class GenericH2Section extends GenericRowWrapper {
		public GenericH2Section(GSTag parent, String text) {
			super(parent, FlexDirection.ROW, HtmlH2::new, text);
		}
	}
}