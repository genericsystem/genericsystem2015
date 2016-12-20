package org.genericsystem.reactor.contextproperties;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDiv;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;

public interface FlexDirectionDefaults extends ContextProperty {

	public static final String FLEX_DIRECTION = "flexDirection";

	default Property<FlexDirection> getDirectionProperty(Context context) {
		if (!context.containsProperty((Tag) this, FLEX_DIRECTION))
			storeProperty(FLEX_DIRECTION, context, c -> {
				Property<FlexDirection> direction = new SimpleObjectProperty<>();
				direction.addListener((o, v, nv) -> addStyle(context, "flex-direction", nv != null ? nv.toString() : "column"));
				direction.setValue(FlexDirection.COLUMN);
				return direction;
			});
		return getProperty(FLEX_DIRECTION, context);
	}

	default void setDirection(Context context, FlexDirection direction) {
		getDirectionProperty(context).setValue(direction);
	}

	default void setDirection(FlexDirection direction) {
		addPrefixBinding(context -> setDirection(context, direction));
	}

	default void reverseDirection(Context context) {
		if (FlexDirectionDefaults.class.isAssignableFrom(((Tag) this).getParent().getClass())) {
			Context parentContext = ((Tag) this).getMetaBinding() != null ? context.getParent() : context;
			Property<FlexDirection> parentDirection = ((FlexDirectionDefaults) ((Tag) this).getParent()).getDirectionProperty(parentContext);
			setDirection(context, parentDirection.getValue() != null ? parentDirection.getValue().reverse() : FlexDirection.ROW);
			parentDirection.addListener((o, v, nv) -> setDirection(context, nv.reverse()));
		} else
			throw new IllegalStateException("The class of the parent must implement FlexDirectionDefaults when reverseDirection is used.");
	}

	default void reverseDirection() {
		addPrefixBinding(context -> reverseDirection(context));
	}

	default void keepDirection(Context context) {
		if (FlexDirectionDefaults.class.isAssignableFrom(((Tag) this).getParent().getClass())) {
			Context parentContext = ((Tag) this).getMetaBinding() != null ? context.getParent() : context;
			Property<FlexDirection> parentDirection = ((FlexDiv) ((Tag) this).getParent()).getDirectionProperty(parentContext);
			setDirection(context, parentDirection.getValue());
			parentDirection.addListener((o, v, nv) -> setDirection(context, nv));
		} else
			throw new IllegalStateException("The class of the parent must implement FlexDirectionDefaults when keepDirection is used.");
	}

	default void keepDirection() {
		addPrefixBinding(context -> keepDirection(context));
	}
}
