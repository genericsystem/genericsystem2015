package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;

public class FlexDiv extends HtmlDiv {
	private final Property<FlexDirection> direction = new SimpleObjectProperty<>();

	public FlexDiv() {
		this(FlexDirection.COLUMN);
	}

	public FlexDiv(FlexDirection direction) {
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
		if (FlexDiv.class.isAssignableFrom(getParent().getClass())) {
			Property<FlexDirection> parentDirection = ((FlexDiv) getParent()).getDirectionProperty();
			setDirection(parentDirection.getValue().reverse());
			parentDirection.addListener((o, v, nv) -> setDirection(nv.reverse()));
		} else
			throw new IllegalStateException("The class of the parent must extend GSDiv when reverseDirection is used.");
	}

	public void keepDirection() {
		if (FlexDiv.class.isAssignableFrom(getParent().getClass())) {
			Property<FlexDirection> parentDirection = ((FlexDiv) getParent()).getDirectionProperty();
			setDirection(parentDirection.getValue());
			parentDirection.addListener((o, v, nv) -> setDirection(nv));
		} else
			throw new IllegalStateException("The class of the parent must extend GSDiv when keepDirection is used.");
	}

	public static class FlexColumn extends FlexDiv {
		public FlexColumn() {
			super(FlexDirection.COLUMN);
		}
	}

	public static class FlexRow extends FlexDiv {
		public FlexRow() {
			super(FlexDirection.ROW);
		}
	}
}