package org.genericsystem.reactor.gs3;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.annotations.Parent;
import org.genericsystem.reactor.gs.GSCheckBoxWithValue.GSCheckBoxDisplayer;
import org.genericsystem.reactor.gs.GSDiv;
import org.genericsystem.reactor.gstag.HtmlButton;
import org.genericsystem.reactor.gstag.HtmlH2;
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

public class Table extends GSDiv implements FlexStyle, SelectionDefaults {

	public static class HorizontalTable extends Table implements RowFlexStyle {
	}

	public static class Title extends GSDiv implements TitleStyle {
	}

	@Override
	public void postfix() {
		bindSelection(find(Row.class));
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
	public static class TitleRow extends GSDiv implements RowStyle {
	}

	@Parent(TitleRow.class)
	public static class TypeName extends GSDiv implements TitleLineCellStyle {
	}

	@Parent(TypeName.class)
	public static class TypeNameDisplayer extends GSLabelDisplayer {
	}

	@Parent(TitleRow.class)
	public static class TypeAttribute extends GSDiv implements RowFlexStyle {

		@Override
		public void init() {
			forEach(ObservableListExtractor.ATTRIBUTES_OF_TYPE);
		}
	}

	@Parent(TypeAttribute.class)
	public static class AttributeName extends GSDiv implements TitleLineCellStyle {

		@Override
		public void init() {
			select(gs -> gs[0].getComponents().size() < 2 ? gs[0] : null);
		}
	}

	@Parent(AttributeName.class)
	public static class AttributeNameDisplayer extends GSLabelDisplayer {
	}

	@Parent(TypeAttribute.class)
	public static class RelationName extends GSDiv implements RowFlexStyle {

		@Override
		public void init() {
			select(gs -> gs[0].getComponents().size() >= 2 ? gs[0] : null);
		}
	}

	@Parent(RelationName.class)
	public static class ComponentName extends GSDiv implements TitleLineCellStyle {

		@Override
		public void init() {
			forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[1])));
		}
	}

	@Parent(ComponentName.class)
	public static class ComponentNameDisplayer extends GSLabelDisplayer {
	}

	@Parent(TitleRow.class)
	public static class EmptyCell extends GSDiv implements ButtonStyle {
	}

	@Parent(Table.class)
	public static class Row extends GSDiv implements RowStyle {

		@Override
		public void init() {
			forEach(ObservableListExtractor.SUBINSTANCES);
		}
	}

	@Parent(Row.class)
	public static class RowName extends GSDiv implements CellStyle {
	}

	@Parent(RowName.class)
	public static class RowNameDisplayer extends HtmlHyperLink implements RowNameStyle {

		@Override
		public void init() {
			bindText();
			bindAction(model -> getSelectionProperty(model).setValue(model));
		}
	}

	@Parent(Row.class)
	public static class Cell extends GSDiv implements CellStyle {

		@Override
		public void init() {
			forEach(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
		}
	}

	@Parent(Cell.class)
	public static class SubCell extends GSDiv implements RowFlexStyle {

		@Override
		public void init() {
			forEach(ObservableListExtractor.HOLDERS);
		}
	}

	@Parent(SubCell.class)
	public static class ComponentSubCell extends GSDiv implements SubCellStyle {

		@Override
		public void init() {
			forEach(gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
		}
	}

	@Parent(ComponentSubCell.class)
	public static class ComponentLabel extends GSLabelDisplayer {
	}

	@Parent(SubCell.class)
	public static class BooleanValueSubCell extends GSDiv implements SubCellStyle {

		@Override
		public void init() {
			select(gs -> gs[1].getComponents().size() == 1 && Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
		}
	}

	@Parent(BooleanValueSubCell.class)
	public static class BooleanDisplayer extends GSCheckBoxDisplayer {
	}

	@Parent(SubCell.class)
	public static class ValueSubCell extends GSDiv implements SubCellStyle {

		@Override
		public void init() {
			select(gs -> gs[1].getComponents().size() == 1 && !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
		}
	}

	@Parent(ValueSubCell.class)
	public static class ValueDisplayer extends GSLabelDisplayer {
	}

	@Parent(Row.class)
	public static class RemoveButtonDiv extends GSDiv implements ButtonStyle {
	}

	@Parent(RemoveButtonDiv.class)
	public static class RemoveButton extends HtmlButton implements FullSizeStyle {

		@Override
		public void init() {
			setText("Remove");
			bindAction(Context::remove);
		}
	}
}
