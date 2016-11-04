package org.genericsystem.reactor.gscomponents2;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.SetStringExtractor;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection;
import org.genericsystem.reactor.gscomponents.CheckBoxWithValue.CheckBoxDisplayer;
import org.genericsystem.reactor.gscomponents.DivWithTitle.TitleDiv;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH2;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel.GSLabelDisplayer;
import org.genericsystem.reactor.gscomponents.InstancesTable;
import org.genericsystem.reactor.gscomponents2.CellDiv.TitleLineCellDiv;
import org.genericsystem.reactor.gscomponents2.Table2.Row.Cell;
import org.genericsystem.reactor.gscomponents2.Table2.Row.Cell.SubCell;
import org.genericsystem.reactor.gscomponents2.Table2.Row.Cell.SubCell.BooleanValueSubCell;
import org.genericsystem.reactor.gscomponents2.Table2.Row.Cell.SubCell.ComponentSubCell;
import org.genericsystem.reactor.gscomponents2.Table2.Row.Cell.SubCell.ValueSubCell;
import org.genericsystem.reactor.gscomponents2.Table2.Row.RemoveButtonDiv;
import org.genericsystem.reactor.gscomponents2.Table2.Row.RemoveButtonDiv.RemoveButton;
import org.genericsystem.reactor.gscomponents2.Table2.Row.RowName;
import org.genericsystem.reactor.gscomponents2.Table2.Row.RowName.RowNameDisplayer;
import org.genericsystem.reactor.gscomponents2.Table2.TableContent;
import org.genericsystem.reactor.gscomponents2.Table2.TableTitle;
import org.genericsystem.reactor.gscomponents2.Table2.TableTitle.TableTitleContent;
import org.genericsystem.reactor.gscomponents2.Table2.TitleRow.EmptyCell;
import org.genericsystem.reactor.gscomponents2.Table2.TitleRow.TypeAttribute;
import org.genericsystem.reactor.gscomponents2.Table2.TitleRow.TypeAttribute.AttributeName;
import org.genericsystem.reactor.gscomponents2.Table2.TitleRow.TypeAttribute.RelationName;
import org.genericsystem.reactor.gscomponents2.Table2.TitleRow.TypeAttribute.RelationName.ComponentName;
import org.genericsystem.reactor.gscomponents2.Table2.TitleRow.TypeName;
import org.genericsystem.reactor.model.ContextAction.REMOVE;
import org.genericsystem.reactor.model.ContextAction.SET_SELECTION;
import org.genericsystem.reactor.model.ObservableListExtractor.ATTRIBUTES_OF_INSTANCES;
import org.genericsystem.reactor.model.ObservableListExtractor.ATTRIBUTES_OF_TYPE;
import org.genericsystem.reactor.model.ObservableListExtractor.HOLDERS;
import org.genericsystem.reactor.model.ObservableListExtractor.OTHER_COMPONENTS_1;
import org.genericsystem.reactor.model.ObservableListExtractor.OTHER_COMPONENTS_2;
import org.genericsystem.reactor.model.ObservableListExtractor.SUBINSTANCES;
import org.genericsystem.reactor.model.ObservableValueSelector.CHECK_BOX_DISPLAYER;
import org.genericsystem.reactor.model.ObservableValueSelector.LABEL_DISPLAYER;
import org.genericsystem.reactor.model.ObservableValueSelector.RELATION_SELECTOR;
import org.genericsystem.reactor.model.ObservableValueSelector.STRICT_ATTRIBUTE_SELECTOR;
import org.genericsystem.reactor.model.StringExtractor;

@Children({ TableTitle.class, TableContent.class })
@Style(name = "flex", value = "1")
public class Table2 extends FlexDiv {

	@FlexDirectionStyle(path = TableContent.class, value = FlexDirection.ROW)
	public static class HorizontalTable extends Table2 {
	}

	@Children(TableTitleContent.class)
	public static class TableTitle extends TitleDiv {

		@SetStringExtractor(StringExtractor.MANAGEMENT.class)
		@BindText
		public static class TableTitleContent extends HtmlH2 {
		}
	}

	@Children({ TitleRow.class, InstanceBuilder2.class, Row.class })
	public static class TableContent extends FlexDiv implements SelectionDefaults {
		@Override
		public void init() {
			bindSelection(find(Row.class));
		}
	}

	@ReverseFlexDirection
	@Style(name = "flex", value = "1")
	@Children({ TypeName.class, TypeAttribute.class, EmptyCell.class })
	public static class TitleRow extends FlexDiv {
		@Children(GSLabelDisplayer.class)
		public static class TypeName extends TitleLineCellDiv {
		}

		@ForEach(ATTRIBUTES_OF_TYPE.class)
		@Style(name = "flex", value = "1")
		@FlexDirectionStyle(FlexDirection.ROW)
		@Children({ AttributeName.class, RelationName.class })
		public static class TypeAttribute extends FlexDiv {

			@Select(STRICT_ATTRIBUTE_SELECTOR.class)
			@Children(GSLabelDisplayer.class)
			public static class AttributeName extends TitleLineCellDiv {
			}

			@Select(RELATION_SELECTOR.class)
			@Style(name = "flex", value = "1")
			@FlexDirectionStyle(FlexDirection.ROW)
			@Children(ComponentName.class)
			public static class RelationName extends FlexDiv {

				@ForEach(OTHER_COMPONENTS_1.class)
				@Children(GSLabelDisplayer.class)
				public static class ComponentName extends TitleLineCellDiv {
				}
			}
		}

		// EmptyCell corresponding to the “Remove” or “Add” button of other lines.
		@Style(name = "flex", value = "1")
		public static class EmptyCell extends InstancesTable.ButtonDiv {
		}
	}

	@ForEach(SUBINSTANCES.class)
	@ReverseFlexDirection
	@Style(name = "flex", value = "1")
	@Children({ RowName.class, Cell.class, RemoveButtonDiv.class })
	public static class Row extends FlexDiv {

		@Children(RowNameDisplayer.class)
		public static class RowName extends CellDiv {

			@Style(name = "flex", value = "1")
			@Style(name = "color", value = "white")
			@Style(name = "margin-right", value = "1px")
			@Style(name = "margin-bottom", value = "1px")
			@GenericValueBackgroundColor("#3393ff")
			@BindText
			@BindAction(SET_SELECTION.class)
			public static class RowNameDisplayer extends HtmlHyperLink {
			}
		}

		@ForEach(ATTRIBUTES_OF_INSTANCES.class)
		@Children(SubCell.class)
		public static class Cell extends CellDiv {

			@ForEach(HOLDERS.class)
			@Style(name = "flex", value = "1")
			@FlexDirectionStyle(FlexDirection.ROW)
			@Children({ ComponentSubCell.class, BooleanValueSubCell.class, ValueSubCell.class })
			public static class SubCell extends FlexDiv {

				@ForEach(OTHER_COMPONENTS_2.class)
				@Children(GSLabelDisplayer.class)
				public static class ComponentSubCell extends SubcellDiv {
				}

				@Select(CHECK_BOX_DISPLAYER.class)
				@Children(CheckBoxDisplayer.class)
				public static class BooleanValueSubCell extends SubcellDiv {
				}

				@Select(LABEL_DISPLAYER.class)
				@Children(GSLabelDisplayer.class)
				public static class ValueSubCell extends SubcellDiv {
				}
			}
		}

		// Button to delete the instance.
		@Children(RemoveButton.class)
		public static class RemoveButtonDiv extends InstancesTable.ButtonDiv {
			@Style(name = "flex", value = "1")
			@Style(name = "height", value = "100%")
			@Style(name = "width", value = "100%")
			@SetText("Remove")
			@BindAction(REMOVE.class)
			public static class RemoveButton extends HtmlButton {
			}
		}
	}
}
