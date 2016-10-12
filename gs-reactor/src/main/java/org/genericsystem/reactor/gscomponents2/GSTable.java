package org.genericsystem.reactor.gscomponents2;

import org.genericsystem.reactor.modelproperties.GenericStringDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import org.genericsystem.reactor.htmltag.HtmlButton;
import org.genericsystem.reactor.htmltag.HtmlH2;
import org.genericsystem.reactor.htmltag.HtmlHyperLink;
import org.genericsystem.reactor.htmltag.HtmlLabel.GSLabelDisplayer;

import org.genericsystem.reactor.gscomponents3.DivWithTitle.GSTitleDiv;
import org.genericsystem.reactor.gscomponents3.InstancesTable;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.GSCheckBoxWithValue.GSCheckBoxDisplayer;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.gscomponents2.GSCellDiv.GSTitleLineCellDiv;
import org.genericsystem.reactor.gscomponents2.GSTable.Row.Cell.SubCell.BooleanValueSubCell.BooleanDisplayer;
import org.genericsystem.reactor.gscomponents2.GSTable.Row.Cell.SubCell.ComponentSubCell.ComponentLabel;
import org.genericsystem.reactor.gscomponents2.GSTable.Row.Cell.SubCell.ValueSubCell.ValueDisplayer;
import org.genericsystem.reactor.gscomponents2.GSTable.Row.RemoveButtonDiv.RemoveButton;
import org.genericsystem.reactor.gscomponents2.GSTable.Row.RowName.RowNameDisplayer;
import org.genericsystem.reactor.gscomponents2.GSTable.TableTitle.TableTitleContent;
import org.genericsystem.reactor.gscomponents2.GSTable.TitleRow.EmptyCell;
import org.genericsystem.reactor.gscomponents2.GSTable.TitleRow.TypeAttribute.AttributeName.AttributeNameDisplayer;
import org.genericsystem.reactor.gscomponents2.GSTable.TitleRow.TypeAttribute.RelationName.ComponentName.ComponentNameDisplayer;
import org.genericsystem.reactor.gscomponents2.GSTable.TitleRow.TypeName.TypeNameDisplayer;
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

@ReactorDependencies({ TableTitleContent.class, TypeNameDisplayer.class, AttributeNameDisplayer.class, ComponentNameDisplayer.class, GSInstanceBuilder.class, RowNameDisplayer.class, ComponentLabel.class, EmptyCell.class, BooleanDisplayer.class,
		ValueDisplayer.class, RemoveButton.class })
@Style(name = "flex", value = "1")
public class GSTable extends GSDiv implements SelectionDefaults, Tag {

	public GSTable() {
		super();
	}

	public GSTable(Tag parent) {
		super(parent);
	}

	// No automatic enclosing class parent for this GSTable extention
	@FlexDirectionStyle(FlexDirection.ROW)
	public static class HorizontalTable extends GSTable {

		public HorizontalTable(Tag parent) {
			super(parent);
		}
	}

	@Override
	public void postfix() {
		bindSelection(find(Row.class));
	}

	public static class TableTitle extends GSTitleDiv {

		public static class TableTitleContent extends HtmlH2 {
			@Override
			public void init() {
				setStringExtractor(StringExtractor.MANAGEMENT);
				bindText();
			}
		}
	}

	@ReverseFlexDirection
	@Style(name = "flex", value = "1")
	public static class TitleRow extends GSDiv {
		public static class TypeName extends GSTitleLineCellDiv {
			public static class TypeNameDisplayer extends GSLabelDisplayer {
			}
		}

		@ForEach(ATTRIBUTES_OF_TYPE.class)
		@Style(name = "flex", value = "1")
		@FlexDirectionStyle(FlexDirection.ROW)
		public static class TypeAttribute extends GSDiv {

			@Select(STRICT_ATTRIBUTE_SELECTOR.class)
			public static class AttributeName extends GSTitleLineCellDiv {

				public static class AttributeNameDisplayer extends GSLabelDisplayer {

				}
			}

			@Select(RELATION_SELECTOR.class)
			@Style(name = "flex", value = "1")
			@FlexDirectionStyle(FlexDirection.ROW)
			public static class RelationName extends GSDiv {

				@ForEach(OTHER_COMPONENTS_1.class)
				public static class ComponentName extends GSTitleLineCellDiv {

					public static class ComponentNameDisplayer extends GSLabelDisplayer {
					}
				}

				@ForEach(OTHER_COMPONENTS_2.class)
				public static class InstanceComponentName extends ComponentName {

				}
			}
		}

		// EmptyCell corresponding to the “Remove” or “Add” button of other lines.
		public static class EmptyCell extends InstancesTable.ButtonDiv {
		}
	}

	@ForEach(SUBINSTANCES.class)
	@ReverseFlexDirection
	@Style(name = "flex", value = "1")
	public static class Row extends GSDiv {

		public static class RowName extends GSCellDiv {

			@Style(name = "flex", value = "1")
			@Style(name = "color", value = "white")
			@Style(name = "margin-right", value = "1px")
			@Style(name = "margin-bottom", value = "1px")
			public static class RowNameDisplayer extends HtmlHyperLink {

				@Override
				public void init() {
					bindText();
					bindAction(model -> getSelectionProperty(model).setValue(model));
					addPrefixBinding(modelContext -> addStyle(modelContext, "background-color",
							"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(modelContext.getGeneric().getMeta())) ? ((GenericStringDefaults) this).getGenericStringProperty(modelContext).getValue() : "#3393FF"));
				}
			}
		}

		@ForEach(ATTRIBUTES_OF_INSTANCES.class)
		public static class Cell extends GSCellDiv {

			@ForEach(HOLDERS.class)
			@Style(name = "flex", value = "1")
			@FlexDirectionStyle(FlexDirection.ROW)
			public static class SubCell extends GSDiv {

				@ForEach(OTHER_COMPONENTS_2.class)
				public static class ComponentSubCell extends GSSubcellDiv {

					public static class ComponentLabel extends GSLabelDisplayer {
					}
				}

				@Select(CHECK_BOX_DISPLAYER.class)
				public static class BooleanValueSubCell extends GSSubcellDiv {

					public static class BooleanDisplayer extends GSCheckBoxDisplayer {
					}
				}

				@Select(LABEL_DISPLAYER.class)
				public static class ValueSubCell extends GSSubcellDiv {

					public static class ValueDisplayer extends GSLabelDisplayer {
					}
				}

			}
		}

		// Button to delete the instance.
		public static class RemoveButtonDiv extends InstancesTable.ButtonDiv {
			@Style(name = "flex", value = "1")
			@Style(name = "height", value = "100%")
			@Style(name = "width", value = "100%")
			public static class RemoveButton extends HtmlButton {

				@Override
				public void init() {
					setText("Remove");
					bindAction(Context::remove);
				}
			}
		}
	}
}
