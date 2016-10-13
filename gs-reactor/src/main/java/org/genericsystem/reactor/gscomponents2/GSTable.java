package org.genericsystem.reactor.gscomponents2;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import org.genericsystem.reactor.htmltag.HtmlButton;
import org.genericsystem.reactor.htmltag.HtmlH2;
import org.genericsystem.reactor.htmltag.HtmlHyperLink;
import org.genericsystem.reactor.htmltag.HtmlLabel.GSLabelDisplayer;

import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.SetStringExtractor;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.GSCheckBoxWithValue.GSCheckBoxDisplayer;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.gscomponents2.GSCellDiv.GSTitleLineCellDiv;
import org.genericsystem.reactor.gscomponents2.GSTable.Row.Cell;
import org.genericsystem.reactor.gscomponents2.GSTable.Row.Cell.SubCell;
import org.genericsystem.reactor.gscomponents2.GSTable.Row.Cell.SubCell.BooleanValueSubCell;
import org.genericsystem.reactor.gscomponents2.GSTable.Row.Cell.SubCell.ComponentSubCell;
import org.genericsystem.reactor.gscomponents2.GSTable.Row.Cell.SubCell.ValueSubCell;
import org.genericsystem.reactor.gscomponents2.GSTable.Row.RemoveButtonDiv;
import org.genericsystem.reactor.gscomponents2.GSTable.Row.RemoveButtonDiv.RemoveButton;
import org.genericsystem.reactor.gscomponents2.GSTable.Row.RowName;
import org.genericsystem.reactor.gscomponents2.GSTable.Row.RowName.RowNameDisplayer;
import org.genericsystem.reactor.gscomponents2.GSTable.TableContent;
import org.genericsystem.reactor.gscomponents2.GSTable.TableTitle;
import org.genericsystem.reactor.gscomponents2.GSTable.TableTitle.TableTitleContent;
import org.genericsystem.reactor.gscomponents2.GSTable.TitleRow.EmptyCell;
import org.genericsystem.reactor.gscomponents2.GSTable.TitleRow.TypeAttribute;
import org.genericsystem.reactor.gscomponents2.GSTable.TitleRow.TypeAttribute.AttributeName;
import org.genericsystem.reactor.gscomponents2.GSTable.TitleRow.TypeAttribute.RelationName;
import org.genericsystem.reactor.gscomponents2.GSTable.TitleRow.TypeAttribute.RelationName.ComponentName;
import org.genericsystem.reactor.gscomponents2.GSTable.TitleRow.TypeName;
import org.genericsystem.reactor.gscomponents3.DivWithTitle.GSTitleDiv;
import org.genericsystem.reactor.gscomponents3.InstancesTable;
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

@ReactorDependencies({ TableTitle.class, TableContent.class })
@Style(name = "flex", value = "1")
public class GSTable extends GSDiv {

	@FlexDirectionStyle(path = TableContent.class, value = FlexDirection.ROW)
	public static class HorizontalTable extends GSTable {
	}

	@ReactorDependencies(TableTitleContent.class)
	public static class TableTitle extends GSTitleDiv {

		@SetStringExtractor(StringExtractor.MANAGEMENT.class)
		@BindText
		public static class TableTitleContent extends HtmlH2 {
		}
	}

	@ReactorDependencies({ TitleRow.class, GSInstanceBuilder.class, Row.class })
	public static class TableContent extends GSDiv implements SelectionDefaults {
		@Override
		public void init() {
			bindSelection(find(Row.class));
		}
	}

	@ReverseFlexDirection
	@Style(name = "flex", value = "1")
	@ReactorDependencies({ TypeName.class, TypeAttribute.class, EmptyCell.class })
	public static class TitleRow extends GSDiv {
		@ReactorDependencies(GSLabelDisplayer.class)
		public static class TypeName extends GSTitleLineCellDiv {
		}

		@ForEach(ATTRIBUTES_OF_TYPE.class)
		@Style(name = "flex", value = "1")
		@FlexDirectionStyle(FlexDirection.ROW)
		@ReactorDependencies({ AttributeName.class, RelationName.class })
		public static class TypeAttribute extends GSDiv {

			@Select(STRICT_ATTRIBUTE_SELECTOR.class)
			@ReactorDependencies(GSLabelDisplayer.class)
			public static class AttributeName extends GSTitleLineCellDiv {
			}

			@Select(RELATION_SELECTOR.class)
			@Style(name = "flex", value = "1")
			@FlexDirectionStyle(FlexDirection.ROW)
			@ReactorDependencies(ComponentName.class)
			public static class RelationName extends GSDiv {

				@ForEach(OTHER_COMPONENTS_1.class)
				@ReactorDependencies(GSLabelDisplayer.class)
				public static class ComponentName extends GSTitleLineCellDiv {
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
	@ReactorDependencies({ RowName.class, Cell.class, RemoveButtonDiv.class })
	public static class Row extends GSDiv {

		@ReactorDependencies(RowNameDisplayer.class)
		public static class RowName extends GSCellDiv {

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
		@ReactorDependencies(SubCell.class)
		public static class Cell extends GSCellDiv {

			@ForEach(HOLDERS.class)
			@Style(name = "flex", value = "1")
			@FlexDirectionStyle(FlexDirection.ROW)
			@ReactorDependencies({ ComponentSubCell.class, BooleanValueSubCell.class, ValueSubCell.class })
			public static class SubCell extends GSDiv {

				@ForEach(OTHER_COMPONENTS_2.class)
				@ReactorDependencies(GSLabelDisplayer.class)
				public static class ComponentSubCell extends GSSubcellDiv {
				}

				@Select(CHECK_BOX_DISPLAYER.class)
				@ReactorDependencies(GSCheckBoxDisplayer.class)
				public static class BooleanValueSubCell extends GSSubcellDiv {
				}

				@Select(LABEL_DISPLAYER.class)
				@ReactorDependencies(GSLabelDisplayer.class)
				public static class ValueSubCell extends GSSubcellDiv {
				}
			}
		}

		// Button to delete the instance.
		@ReactorDependencies(RemoveButton.class)
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
