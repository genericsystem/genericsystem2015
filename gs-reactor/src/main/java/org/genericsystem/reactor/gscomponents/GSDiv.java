package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.htmltag.HtmlDiv;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;

public class GSDiv extends HtmlDiv {
	private final Property<FlexDirection> direction = new SimpleObjectProperty<>();

	public GSDiv() {
		this(FlexDirection.COLUMN);
	}

	public GSDiv(FlexDirection direction) {
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
	}

	public static class GenericRow extends GSDiv {
		public GenericRow() {
			super(FlexDirection.ROW);
		}
	}
}