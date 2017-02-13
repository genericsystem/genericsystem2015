package org.genericsystem.reactor.contextproperties;

import java.util.function.Function;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gscomponents.FlexDirection;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ChangeListener;

public interface FlexDirectionDefaults extends ContextProperty {

	public static final String FLEX_DIRECTION = "flexDirection";
	public static final String PARENT_DIRECTION_LISTENER = "parentDirectionListener";

	default Property<FlexDirection> getDirectionProperty(Context context) {
		if (!context.containsAttribute((Tag) this, FLEX_DIRECTION)) {
			Property<FlexDirection> direction = new SimpleObjectProperty<>();
			direction.addListener((o, v, nv) -> addStyle(context, "flex-direction", nv != null ? nv.toString() : "column"));
			direction.setValue(FlexDirection.COLUMN);
			addContextAttribute(FLEX_DIRECTION, context, direction);
		}
		return getContextProperty(FLEX_DIRECTION, context);
	}

	default Property<ChangeListener<FlexDirection>> getDirectionListenerProperty(Context context) {
		return getContextProperty(PARENT_DIRECTION_LISTENER, context);
	}

	default void setDirection(Context context, FlexDirection direction) {
		getDirectionProperty(context).setValue(direction);
	}

	default void setDirection(FlexDirection direction) {
		addPrefixBinding(context -> setDirection(context, direction));
	}

	default void reverseDirection(Context context) {
		createNewInitializedProperty(PARENT_DIRECTION_LISTENER, context, (ChangeListener<FlexDirection>) ((o, v, nv) -> setDirection(context, nv.reverse())));
		startTrackingDirection(context, direction -> direction != null ? direction.reverse() : FlexDirection.ROW);
	}

	default void reverseDirection() {
		addPrefixBinding(context -> reverseDirection(context));
	}

	default void keepDirection(Context context) {
		createNewInitializedProperty(PARENT_DIRECTION_LISTENER, context, (ChangeListener<FlexDirection>) ((o, v, nv) -> setDirection(context, nv)));
		startTrackingDirection(context, direction -> direction);
	}

	default void keepDirection() {
		addPrefixBinding(context -> keepDirection(context));
	}

	default void startTrackingDirection(Context context, Function<FlexDirection, FlexDirection> getInitialDirection) {
		if (FlexDirectionDefaults.class.isAssignableFrom(((Tag) this).getParent().getClass())) {
			Context parentContext = ((Tag) this).getMetaBinding() != null ? context.getParent() : context;
			Property<FlexDirection> parentDirection = ((FlexDirectionDefaults) ((Tag) this).getParent()).getDirectionProperty(parentContext);
			setDirection(context, getInitialDirection.apply(parentDirection.getValue()));
			parentDirection.addListener(getDirectionListenerProperty(context).getValue());
		} else
			throw new IllegalStateException("The class of the parent must implement FlexDirectionDefaults when reverseDirection or keepDirection is used.");
	}

	default void stopTrackingDirection(Context context) {
		Context parentContext = ((Tag) this).getMetaBinding() != null ? context.getParent() : context;
		Property<FlexDirection> parentDirection = ((FlexDirectionDefaults) ((Tag) this).getParent()).getDirectionProperty(parentContext);
		parentDirection.removeListener(getDirectionListenerProperty(context).getValue());
		context.getAttributes((Tag) this).remove(PARENT_DIRECTION_LISTENER);
	}
}
