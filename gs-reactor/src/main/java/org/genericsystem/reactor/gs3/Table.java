package org.genericsystem.reactor.gs3;

import org.genericsystem.reactor.annotations.Parent;
import org.genericsystem.reactor.gs.FlexDirection;
import org.genericsystem.reactor.gs.GSCheckBoxWithValue.GSCheckBoxDisplayer;
import org.genericsystem.reactor.gs.GSDiv;
import org.genericsystem.reactor.gstag.HtmlH2;
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

public class Table extends GSDiv {

	@Override
	public void init() {
		addStyle("flex", "1");
	}

	public static class HorizontalTable extends Table {

		@Override
		public void init() {
			super.init();
			setDirection(FlexDirection.ROW);
		}
	}

	public static class Title extends GSDiv {

		@Override
		public void init() {
			addStyle("background-color", "#EA4500");
			addStyle("margin-right", "1px");
			addStyle("margin-bottom", "1px");
			addStyle("color", "White");
			addStyle("justify-content", "center");
			addStyle("align-items", "center");
		}
	}

	@Parent(Title.class)
	public static class TitleContent extends HtmlH2 {

		@Override
		public void init() {
			setStringExtractor(StringExtractor.MANAGEMENT);
			bindText();
		}
	}

	@Parent(Table.class)
	public static class Row extends GSDiv {

		@Override
		public void init() {
			forEach(ObservableListExtractor.SUBINSTANCES);
			reverseDirection();
			addStyle("flex", "1");
		}
	}

	@Parent(Row.class)
	public static class RowName extends GSDiv {

		@Override
		public void init() {
			keepDirection();
			addStyle("flex", "1");
			addStyle("margin-right", "1px");
			addStyle("margin-bottom", "1px");
			addStyle("overflow", "hidden");
			addPrefixBinding(modelContext -> addStyle(modelContext, "background-color", "Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(modelContext.getGeneric().getMeta())) ? getGenericStringProperty(modelContext).getValue() : "#3393FF"));
		}
	}

	@Parent(RowName.class)
	public static class RowNameDisplayer extends HtmlHyperLink {

		@Override
		public void init() {
			addStyle("color", "White");
			bindText();
			bindAction(model -> getSelectionProperty(model).setValue(model));
		}
	}

	@Parent(Row.class)
	public static class Cell extends GSDiv {

		@Override
		public void init() {
			forEach(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
			addStyle("flex", "1");
			addStyle("margin-bottom", "1px");
			addStyle("margin-right", "1px");
		}
	}

	@Parent(Cell.class)
	public static class SubCell extends GSDiv {

		@Override
		public void init() {
			forEach(ObservableListExtractor.HOLDERS);
			setDirection(FlexDirection.ROW);
			addStyle("flex", "1");
		}
	}

	@Parent(Cell.class)
	public static class SubCell2 extends SubCell {

		@Override
		public void init() {
			super.init();
			addStyle("border", "1px solid red");
		}
	}

	@Parent(SubCell.class)
	public static class ComponentSubCell extends GSDiv {

		@Override
		public void init() {
			forEach(gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
			setDirection(FlexDirection.ROW);
			addStyle("flex", "1");
			addStyle("justify-content", "center");
			addStyle("align-items", "center");
			addStyle("margin-bottom", "1px");
			addStyle("margin-right", "1px");
			addPrefixBinding(modelContext -> addStyle(modelContext, "background-color", "Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(modelContext.getGeneric().getMeta())) ? getGenericStringProperty(modelContext).getValue() : "#e5ed00"));
		}
	}

	@Parent(ComponentSubCell.class)
	public static class ComponentLabel extends GSLabelDisplayer {
	}

	@Parent(SubCell.class)
	public static class BooleanValueSubCell extends GSDiv {

		@Override
		public void init() {
			select(gs -> gs[1].getComponents().size() == 1 && Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
			setDirection(FlexDirection.ROW);
			addStyle("justify-content", "center");
			addStyle("align-items", "center");
			addStyle("flex", "1");
			addStyle("background-color", "#e5ed00");
		}
	}

	@Parent(BooleanValueSubCell.class)
	public static class BooleanDisplayer extends GSCheckBoxDisplayer {
	}

	@Parent(SubCell.class)
	public static class ValueSubCell extends GSDiv {

		@Override
		public void init() {
			select(gs -> gs[1].getComponents().size() == 1 && !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
			setDirection(FlexDirection.ROW);
			addStyle("justify-content", "center");
			addStyle("align-items", "center");
			addStyle("flex", "1");
			addStyle("background-color", "#e5ed00");
		}
	}

	@Parent(ValueSubCell.class)
	public static class ValueDisplayer extends GSLabelDisplayer {
	}
}
