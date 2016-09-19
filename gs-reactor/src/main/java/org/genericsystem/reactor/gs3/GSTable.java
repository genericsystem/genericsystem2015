package org.genericsystem.reactor.gs3;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Parent;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.gs.GSCheckBoxWithValue.GSCheckBoxDisplayer;
import org.genericsystem.reactor.gs.GSDiv;
//import org.genericsystem.reactor.gs3.GSInstanceBuilder.AddButtonDiv.AddButton;
//import org.genericsystem.reactor.gs3.GSInstanceBuilder.BuilderCell.BooleanHolderBuilder.CheckboxContainerBuildDiv.BooleanHolderBuilderInput;
//import org.genericsystem.reactor.gs3.GSInstanceBuilder.BuilderCell.HolderBuilder.HolderBuilderInput;
//import org.genericsystem.reactor.gs3.GSInstanceBuilder.BuilderCell.LinkBuilder.ComponentBuilder.ComponentBuilderSelect;
//import org.genericsystem.reactor.gs3.GSInstanceBuilder.InstanceNameBuilder.InstanceNameBuilderInput;
import org.genericsystem.reactor.gs3.GSTable.Row.Cell.SubCell.BooleanValueSubCell.BooleanDisplayer;
import org.genericsystem.reactor.gs3.GSTable.Row.Cell.SubCell.ComponentSubCell.ComponentLabel;
import org.genericsystem.reactor.gs3.GSTable.Row.Cell.SubCell.ValueSubCell.ValueDisplayer;
import org.genericsystem.reactor.gs3.GSTable.Row.RemoveButtonDiv.RemoveButton;
import org.genericsystem.reactor.gs3.GSTable.Row.RowName.RowNameDisplayer;
import org.genericsystem.reactor.gs3.GSTable.TableTitle.TableTitleContent;
import org.genericsystem.reactor.gs3.GSTable.TitleRow.EmptyCell;
import org.genericsystem.reactor.gs3.GSTable.TitleRow.TypeAttribute.AttributeName.AttributeNameDisplayer;
import org.genericsystem.reactor.gs3.GSTable.TitleRow.TypeAttribute.RelationName.ComponentName.ComponentNameDisplayer;
import org.genericsystem.reactor.gs3.GSTable.TitleRow.TypeName.TypeNameDisplayer;
import org.genericsystem.reactor.gstag.HtmlButton;
import org.genericsystem.reactor.gstag.HtmlH2;
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;
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
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

@ReactorDependencies({ TableTitleContent.class, GSInstanceBuilder.class, TypeNameDisplayer.class, AttributeNameDisplayer.class, ComponentNameDisplayer.class, RowNameDisplayer.class, ComponentLabel.class, EmptyCell.class, BooleanDisplayer.class,
		ValueDisplayer.class, RemoveButton.class })
public class GSTable extends CompositeTagImpl implements FlexStyle, SelectionDefaults, Tag {

	public GSTable() {
		super();
	}

	public GSTable(Tag parent) {
		super(parent);
	}

	// No automatic enclosing class parent for this GSTable extention
	public static class HorizontalTable extends GSTable implements RowFlexStyle {

		public HorizontalTable(Tag parent) {
			super(parent);
		}
	}

	@Override
	public void postfix() {
		bindSelection(find(Row.class));
	}

	public static class TableTitle extends GSDiv implements FlexStyle.TitleStyle {

		public static class TableTitleContent extends HtmlH2 {
			@Override
			public void init() {
				setStringExtractor(StringExtractor.MANAGEMENT);
				bindText();
			}
		}
	}

	public static class TitleRow extends GSDiv implements FlexStyle.RowStyle {
		public static class TypeName extends GSDiv implements FlexStyle.TitleLineCellStyle {
			public static class TypeNameDisplayer extends GSLabelDisplayer {
			}
		}

		@ForEach(ATTRIBUTES_OF_TYPE.class)
		public static class TypeAttribute extends GSDiv implements FlexStyle.RowFlexStyle {

			@Select(STRICT_ATTRIBUTE_SELECTOR.class)
			public static class AttributeName extends GSDiv implements FlexStyle.TitleLineCellStyle {

				public static class AttributeNameDisplayer extends GSLabelDisplayer {

				}
			}

			@Select(RELATION_SELECTOR.class)
			public static class RelationName extends GSDiv implements FlexStyle.RowFlexStyle {

				@ForEach(OTHER_COMPONENTS_1.class)
				public static class ComponentName extends GSDiv implements FlexStyle.TitleLineCellStyle {

					public static class ComponentNameDisplayer extends GSLabelDisplayer {
					}
				}

				@Parent(RelationName.class)
				@ForEach(OTHER_COMPONENTS_2.class)
				public static class InstanceComponentName extends ComponentName {

				}
			}
		}

		// EmptyCell corresponding to the “Remove” or “Add” button of other lines.
		public static class EmptyCell extends GSDiv implements ButtonStyle {
		}
	}

	@ForEach(SUBINSTANCES.class)
	public static class Row extends GSDiv implements FlexStyle.RowStyle {

		public static class RowName extends GSDiv implements FlexStyle.CellStyle {

			public static class RowNameDisplayer extends HtmlHyperLink implements RowNameStyle {

				@Override
				public void init() {
					bindText();
					bindAction(model -> getSelectionProperty(model).setValue(model));
				}
			}
		}

		@ForEach(ATTRIBUTES_OF_INSTANCES.class)
		public static class Cell extends GSDiv implements FlexStyle.CellStyle {

			@ForEach(HOLDERS.class)
			public static class SubCell extends GSDiv implements FlexStyle.RowFlexStyle {

				@ForEach(OTHER_COMPONENTS_2.class)
				public static class ComponentSubCell extends GSDiv implements FlexStyle.SubCellStyle {

					public static class ComponentLabel extends GSLabelDisplayer {
					}
				}

				@Select(CHECK_BOX_DISPLAYER.class)
				public static class BooleanValueSubCell extends GSDiv implements FlexStyle.SubCellStyle {

					@Parent(BooleanValueSubCell.class)
					public static class BooleanDisplayer extends GSCheckBoxDisplayer {
					}
				}

				@Select(LABEL_DISPLAYER.class)
				public static class ValueSubCell extends GSDiv implements FlexStyle.SubCellStyle {

					public static class ValueDisplayer extends GSLabelDisplayer {
					}
				}

			}
		}

		// Button to delete the instance.
		public static class RemoveButtonDiv extends GSDiv implements FlexStyle.ButtonStyle {
			public static class RemoveButton extends HtmlButton implements FullSizeStyle {

				@Override
				public void init() {
					setText("Remove");
					bindAction(Context::remove);
				}
			}
		}
	}
}
