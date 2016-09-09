package org.genericsystem.reactor.gs3;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gs.FlexDirection;
import org.genericsystem.reactor.model.StringExtractor;
import org.genericsystem.reactor.modelproperties.GenericStringDefaults;

import javafx.beans.property.Property;

public interface FlexStyle extends Tag {

	default FlexDirection getDirection() {
		return null;
	}

	default void setDirection(FlexDirection direction) {
	}

	default void reverseDirection() {
	}

	default void keepDirection() {
	}

	default Property<FlexDirection> getDirectionProperty() {
		return null;
	}

	@Override
	default void style() {
		addStyle("flex", "1");
	}

	public static interface RowFlexStyle extends FlexStyle {

		@Override
		default void style() {
			addStyle("flex", "1");
			setDirection(FlexDirection.ROW);
		}
	}

	public static interface ColumnFlexStyle extends FlexStyle {

		@Override
		default void style() {
			addStyle("flex", "1");
			setDirection(FlexDirection.COLUMN);
		}
	}

	public static interface RowStyle extends FlexStyle {

		@Override
		default void style() {
			addStyle("flex", "1");
			reverseDirection();
		}
	}

	public static interface RowNameStyle extends FlexStyle {

		@Override
		default void style() {
			addStyle("flex", "1");
			addStyle("color", "White");
			addStyle("margin-right", "1px");
			addStyle("margin-bottom", "1px");
			addPrefixBinding(modelContext -> addStyle(modelContext, "background-color",
					"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(modelContext.getGeneric().getMeta())) ? ((GenericStringDefaults) this).getGenericStringProperty(modelContext).getValue() : "#3393FF"));
		}
	}

	public static interface CellStyle extends FlexStyle {

		@Override
		default void style() {
			addStyle("flex", "1");
			addStyle("overflow", "hidden");
		}
	}

	public static interface TitleStyle extends FlexStyle {

		@Override
		default void style() {
			addStyle("background-color", "#EA4500");
			addStyle("margin-right", "1px");
			addStyle("margin-bottom", "1px");
			addStyle("color", "White");
			addStyle("justify-content", "center");
			addStyle("align-items", "center");
		}
	}

	public static interface TitleLineCellStyle extends FlexStyle {

		@Override
		default void style() {
			addStyle("flex", "1");
			addStyle("justify-content", "center");
			addStyle("align-items", "center");
			addStyle("margin-right", "1px");
			addStyle("margin-bottom", "1px");
			addStyle("color", "#ffffff");
			addStyle("background-color", "#ea0084");
		}
	}

	public static interface RemoveStyle extends FlexStyle {

		@Override
		default void style() {
			keepDirection();
			if (FlexDirection.ROW.equals(getDirection())) {
				addStyle("flex", "0");
				addStyle("min-width", "100px");
			} else {
				addStyle("flex", "1");
			}
			getDirectionProperty().addListener((o, v, nv) -> {
				if (FlexDirection.ROW.equals(nv)) {
					addStyle("flex", "0");
					addStyle("min-width", "100px");
				} else {
					addStyle("flex", "1");
				}
			});
			addStyle("background-color", "#ea0084");
			addStyle("margin-right", "1px");
			addStyle("margin-bottom", "1px");
		}
	}

	public static interface SubCellStyle extends FlexStyle {

		@Override
		default void style() {
			setDirection(FlexDirection.ROW);
			addStyle("flex", "1");
			addStyle("justify-content", "center");
			addStyle("align-items", "center");
			addStyle("margin-bottom", "1px");
			addStyle("margin-right", "1px");
			addPrefixBinding(modelContext -> addStyle(modelContext, "background-color",
					"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(modelContext.getGeneric().getMeta())) ? ((GenericStringDefaults) this).getGenericStringProperty(modelContext).getValue() : "#e5ed00"));
		}
	}

	public static interface FullSizeStyle extends FlexStyle {

		@Override
		default void style() {
			addStyle("flex", "1 1 100%");
			addStyle("height", "100%");
			addStyle("width", "100%");
		}
	}

	public static interface EditorContentStyle extends FlexStyle {

		@Override
		default void style() {
			keepDirection();
			addStyle("flex", "1");
			addStyle("height", "100%");
		}
	}

	public static interface LinkTitlesStyle extends FlexStyle {

		@Override
		default void style() {
			reverseDirection();
			addStyle("flex", "0.3");
		}
	}

	public static interface ReversedFlexStyle extends FlexStyle {

		@Override
		default void style() {
			reverseDirection();
			addStyle("flex", "1");
		}
	}

	public static interface SubCellEditorStyle extends FlexStyle {

		@Override
		default void style() {
			setDirection(FlexDirection.ROW);
			addStyle("flex", "1");
			addStyle("margin-right", "1px");
			addStyle("margin-bottom", "1px");
			addStyle("color", "#ffffff");
		}
	}

	public static interface ComponentEditorStyle extends SubCellEditorStyle {

		@Override
		default void style() {
			SubCellEditorStyle.super.style();
			addStyle("justify-content", "center");
			addStyle("align-items", "center");
		}
	}

	public static interface AttributeEditorStyle extends FlexStyle {

		@Override
		default void style() {
			addStyle("flex", "1");
			addStyle("flex-direction", "column");
			addStyle("flex-wrap", "wrap");
			addStyle("overflow", "auto");
		}
	}

	public static interface MultiCheckboxStyle extends FlexStyle {

		@Override
		default void style() {
			addStyle("flex", "1");
			setDirection(FlexDirection.COLUMN);
			addStyle("flex-wrap", "wrap");
			addStyle("overflow", "auto");
		}
	}

	public static interface CheckboxLabelStyle extends FlexStyle {

		@Override
		default void style() {
			addStyle("flex", "1 0 auto");
			addStyle("justify-content", "center");
			addStyle("align-items", "center");
			addStyle("text-align", "center");
			addPrefixBinding(model -> {
				if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric().getMeta())))
					addStyle(model, "background-color", getGenericStringProperty(model).getValue());
			});
		}
	}

	public static interface CheckboxStyle extends FlexStyle {

		@Override
		default void style() {
			addStyle("float", "left");
			addStyle("vertical-align", "middle");
			addStyle("margin", "4px");
		}
	}

	public static interface AttributeEditionColumnStyle extends FlexStyle {

		@Override
		default void style() {
			addStyle("flex", "1");
			addStyle("flex-wrap", "wrap");
		}
	}

	public static interface SubcellEditorContainerStyle extends FlexStyle {

		@Override
		default void style() {
			setDirection(FlexDirection.ROW);
			addStyle("flex", "1 0 auto");
		}
	}

	public static interface CenteredFlex extends FlexStyle {

		@Override
		default void style() {
			addStyle("flex", "1");
			addStyle("justify-content", "center");
			addStyle("align-items", "center");
		}
	}

	public static interface ActionLinkStyle extends FlexStyle {

		@Override
		default void style() {
			addStyle("justify-content", "center");
			addStyle("text-decoration", "none");
			addStyle("height", "100%");
		}
	}
}
