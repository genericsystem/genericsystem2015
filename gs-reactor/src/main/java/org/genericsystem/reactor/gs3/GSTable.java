package org.genericsystem.reactor.gs3;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Parent;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.gs.GSCheckBoxWithValue.GSCheckBoxDisplayer;
import org.genericsystem.reactor.gs.GSDiv;
import org.genericsystem.reactor.gstag.HtmlButton;
import org.genericsystem.reactor.gstag.HtmlH2;
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;
import org.genericsystem.reactor.model.ObservableListExtractor.ATTRIBUTES_OF_INSTANCES;
import org.genericsystem.reactor.model.ObservableListExtractor.ATTRIBUTES_OF_TYPE;
import org.genericsystem.reactor.model.ObservableListExtractor.HOLDERS;
import org.genericsystem.reactor.model.ObservableListExtractor.OTHER_COMPONENTS_1;
import org.genericsystem.reactor.model.ObservableListExtractor.OTHER_COMPONENTS_2;
import org.genericsystem.reactor.model.ObservableListExtractor.ObservableValueSelector.CHECK_BOX_DISPLAYER;
import org.genericsystem.reactor.model.ObservableListExtractor.ObservableValueSelector.LABEL_DISPLAYER;
import org.genericsystem.reactor.model.ObservableListExtractor.ObservableValueSelector.RELATION_SELECTOR;
import org.genericsystem.reactor.model.ObservableListExtractor.ObservableValueSelector.STRICT_ATTRIBUTE_SELECTOR;
import org.genericsystem.reactor.model.ObservableListExtractor.SUBINSTANCES;
import org.genericsystem.reactor.model.StringExtractor;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

public class GSTable extends GSDiv implements FlexStyle, SelectionDefaults {

	public static class HorizontalTable extends GSTable implements RowFlexStyle {
	}

	@Override
	public void postfix() {
		bindSelection(find(Row.class));
	}

	public static class TableTitle extends GSDiv implements TitleStyle {

		public static class TableTitleContent extends HtmlH2 {
			@Override
			public void init() {
				setStringExtractor(StringExtractor.MANAGEMENT);
				bindText();
			}
		}
	}

	public static class TitleRow extends GSDiv implements RowStyle {
		public static class TypeName extends GSDiv implements TitleLineCellStyle {
			public static class TypeNameDisplayer extends GSLabelDisplayer {
			}
		}

		@ForEach(ATTRIBUTES_OF_TYPE.class)
		public static class TypeAttribute extends GSDiv implements RowFlexStyle {

			@Select(STRICT_ATTRIBUTE_SELECTOR.class)
			public static class AttributeName extends GSDiv implements TitleLineCellStyle {

				public static class AttributeNameDisplayer extends GSLabelDisplayer {

				}
			}

			@Select(RELATION_SELECTOR.class)
			public static class RelationName extends GSDiv implements RowFlexStyle {

				@ForEach(OTHER_COMPONENTS_1.class)
				public static class ComponentName extends GSDiv implements TitleLineCellStyle {

					public static class ComponentNameDisplayer extends GSLabelDisplayer {
					}
				}
			}
		}

		// EmptyCell corresponding to the “Remove” or “Add” button of other lines.
		public static class EmptyCell extends GSDiv implements ButtonStyle {
		}
	}

	@ForEach(SUBINSTANCES.class)
	public static class Row extends GSDiv implements RowStyle {

		public static class RowName extends GSDiv implements CellStyle {

			public static class RowNameDisplayer extends HtmlHyperLink implements RowNameStyle {

				@Override
				public void init() {
					bindText();
					bindAction(model -> getSelectionProperty(model).setValue(model));
				}
			}
		}

		@ForEach(ATTRIBUTES_OF_INSTANCES.class)
		public static class Cell extends GSDiv implements CellStyle {

			@ForEach(HOLDERS.class)
			public static class SubCell extends GSDiv implements RowFlexStyle {

				@ForEach(OTHER_COMPONENTS_2.class)
				public static class ComponentSubCell extends GSDiv implements SubCellStyle {

					public static class ComponentLabel extends GSLabelDisplayer {
					}
				}

				@Select(CHECK_BOX_DISPLAYER.class)
				public static class BooleanValueSubCell extends GSDiv implements SubCellStyle {

					@Parent(BooleanValueSubCell.class)
					public static class BooleanDisplayer extends GSCheckBoxDisplayer {
					}
				}

				@Select(LABEL_DISPLAYER.class)
				public static class ValueSubCell extends GSDiv implements SubCellStyle {

					public static class ValueDisplayer extends GSLabelDisplayer {
					}
				}

			}
		}

		// Button to delete the instance.
		public static class RemoveButtonDiv extends GSDiv implements ButtonStyle {
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
