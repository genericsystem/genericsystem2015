package org.genericsystem.reactor.gscomponents;

import java.util.function.BiConsumer;
import java.util.function.Consumer;

import org.genericsystem.reactor.Tag;

import org.genericsystem.reactor.htmltag.HtmlDiv;
import org.genericsystem.reactor.htmltag.HtmlH1;
import org.genericsystem.reactor.htmltag.HtmlH2;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;

public class GSDiv extends HtmlDiv {
	private final Property<FlexDirection> direction = new SimpleObjectProperty<>();

	public GSDiv() {
		this(FlexDirection.COLUMN);
	}

	public GSDiv(Tag parent) {
		this(parent, FlexDirection.COLUMN);
	}

	public GSDiv(FlexDirection direction) {
		setDirection(direction);
		addStyle("display", "flex");
		addStyle("flex-wrap", "nowrap");
	}

	public GSDiv(Tag parent, FlexDirection direction) {
		super(parent);
		setDirection(direction);
		addStyle("display", "flex");
		addStyle("flex-wrap", "nowrap");
	}

	public void setDirection(FlexDirection direction) {
		this.direction.setValue(direction);
		addStyle("flex-direction", direction.toString());
	}

	public FlexDirection getDirection() {
		return direction.getValue();
	}

	public FlexDirection getReverseDirection() {
		return direction.getValue().reverse();
	}

	public Property<FlexDirection> getDirectionProperty() {
		return direction;
	}

	public void reverseDirection() {
		if (GSDiv.class.isAssignableFrom(getParent().getClass())) {
			Property<FlexDirection> parentDirection = ((GSDiv) getParent()).getDirectionProperty();
			setDirection(parentDirection.getValue().reverse());
			parentDirection.addListener((o, v, nv) -> setDirection(nv.reverse()));
		} else
			throw new IllegalStateException("The class of the parent must extend GSDiv when reverseDirection is used.");
	}

	public void keepDirection() {
		if (GSDiv.class.isAssignableFrom(getParent().getClass())) {
			Property<FlexDirection> parentDirection = ((GSDiv) getParent()).getDirectionProperty();
			setDirection(parentDirection.getValue());
			parentDirection.addListener((o, v, nv) -> setDirection(nv));
		} else
			throw new IllegalStateException("The class of the parent must extend GSDiv when keepDirection is used.");
	}

	public static class GenericColumn extends GSDiv {
		public GenericColumn() {
			super(FlexDirection.COLUMN);
		}

		public GenericColumn(Tag parent) {
			super(parent, FlexDirection.COLUMN);
		}
	}

	public static class GenericRow extends GSDiv {
		public GenericRow(Tag parent) {
			super(parent, FlexDirection.ROW);
		}
	}

	public static class GenericRowWrapper extends GSDiv {
		public GenericRowWrapper(Tag parent, FlexDirection direction, Consumer<Tag> consumer) {
			super(parent, direction);
			addStyle("justify-content", "center");
			consumer.accept(this);
		}

		public <T> GenericRowWrapper(Tag parent, FlexDirection direction, BiConsumer<Tag, T> consumer, T arg) {
			super(parent, direction);
			addStyle("justify-content", "center");
			consumer.accept(this, arg);
		}
	}

	public static class GenericH1Section extends GenericRowWrapper {
		public GenericH1Section(Tag parent, String text) {
			super(parent, FlexDirection.ROW, HtmlH1::new, text);
		}
	}

	public static class GenericH2Section extends GenericRowWrapper {
		public GenericH2Section(Tag parent, String text) {
			super(parent, FlexDirection.ROW, HtmlH2::new, text);
		}
	}
}