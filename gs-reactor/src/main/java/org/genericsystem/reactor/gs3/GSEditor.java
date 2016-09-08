package org.genericsystem.reactor.gs3;

import java.util.List;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.BindingsTools;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.annotations.Parent;
import org.genericsystem.reactor.gs.GSCheckBoxWithValue;
import org.genericsystem.reactor.gs.GSCheckBoxWithValue.GSCheckBoxEditor;
import org.genericsystem.reactor.gs.GSDiv;
import org.genericsystem.reactor.gs.GSInputTextWithConversion.GSInputTextEditorWithConversion;
import org.genericsystem.reactor.gs.GSSelect.InstanceCompositeSelect;
import org.genericsystem.reactor.gs3.FlexStyle.RowFlexStyle;
import org.genericsystem.reactor.gs3.Table.ComponentName;
import org.genericsystem.reactor.gs3.Table.RelationName;
import org.genericsystem.reactor.gs3.Table.Title;
import org.genericsystem.reactor.gs3.Table.TitleContent;
import org.genericsystem.reactor.gs3.Table.TypeAttribute;
import org.genericsystem.reactor.gs3.Table.TypeName;
import org.genericsystem.reactor.gstag.HtmlLabel;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;
import org.genericsystem.reactor.modelproperties.ComponentsDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import javafx.beans.binding.Bindings;
import javafx.beans.binding.ListBinding;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public class GSEditor extends GSDiv implements RowFlexStyle {

	public static class HorizontalGSEditor extends GSEditor implements ColumnFlexStyle, SelectionDefaults {

		@Override
		public void style() {
			ColumnFlexStyle.super.style();
		}
	}

	// Title.
	@Parent(Title.class)
	public static class EditorTitleContent extends TitleContent {

		@Override
		public void init() {
			setStringExtractor(StringExtractor.TYPE_INSTANCE_EXTRACTOR);
			bindText();
		}
	}

	// Content.
	@Parent(GSEditor.class)
	public static class EditorContent extends GSDiv implements EditorContentStyle {
	}

	// Line/column with the names of the attributes and components of relations.
	@Parent(EditorContent.class)
	public static class LinkTitles extends GSDiv implements LinkTitlesStyle {
	}

	@Parent(LinkTitles.class)
	public static class InstanceType extends TypeName {

		@Override
		public void init() {
			select(gs -> gs[1]);
		}
	}

	@Parent(LinkTitles.class)
	public static class InstanceTypeAttribute extends TypeAttribute {

		@Override
		public void init() {
			forEach(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
		}
	}

	@Parent(RelationName.class)
	public static class InstanceComponentName extends ComponentName {

		@Override
		public void init() {
			forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
		}
	}

	// Edition itself.
	@Parent(EditorContent.class)
	public static class InstanceEdition extends GSDiv implements ReversedFlexStyle {
	}

	// Edition of the name of the instance.
	@Parent(InstanceEdition.class)
	public static class InstanceNameEditorDiv extends GSDiv implements SubCellEditorStyle {
	}

	@Parent(InstanceNameEditorDiv.class)
	public static class InstanceNameEditor extends GSInputTextEditorWithConversion implements FullSizeStyle {
	}

	// Edition of the holders/links.
	@Parent(InstanceEdition.class)
	public static class InstanceAttributeEditor extends GSDiv implements AttributeEditorStyle {

		@Override
		public void init() {
			forEach(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
		}
	}

	// Multiple checkboxes : for binary relations without the singular constraint.
	@Parent(InstanceAttributeEditor.class)
	public static class MultiCheckbox extends GSDiv implements MultiCheckboxStyle {

		@Override
		public void init() {
			select(gs -> gs[0].getComponents().size() == 2 && !gs[0].isSingularConstraintEnabled(gs[0].getComponents().indexOf(gs[2])) ? gs[0] : null);
		}
	}

	@Parent(MultiCheckbox.class)
	public static class CheckboxLabel extends HtmlLabel implements CheckboxLabelStyle {

		@Override
		public void init() {
			bindText();
			forEach(gs -> ObservableListExtractor.SUBINSTANCES.apply(ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])).stream().toArray(Generic[]::new)));
		}
	}

	@Parent(CheckboxLabel.class)
	public static class Checkbox extends GSCheckBoxWithValue implements CheckboxStyle {

		@Override
		public void init() {
			initValueProperty(context -> context.getGenerics()[2].getLink(context.getGenerics()[1], context.getGeneric()) != null ? true : false);
			storeProperty("exists", context -> {
				ObservableValue<Boolean> exists = Bindings.createBooleanBinding(() -> context.getGenerics()[2].getObservableLink(context.getGenerics()[1], context.getGeneric()).getValue() != null ? true : false,
						context.getGenerics()[2].getObservableLink(context.getGenerics()[1], context.getGeneric()));
				exists.addListener((o, v, nva) -> {
					if (!context.isDestroyed())
						getConvertedValueProperty(context).setValue(nva);
				});
				return exists;
			});
			addConvertedValueChangeListener((context, nva) -> {
				if (Boolean.TRUE.equals(nva))
					context.getGenerics()[2].setHolder(context.getGenerics()[1], null, context.getGeneric());
				if (Boolean.FALSE.equals(nva)) {
					Generic link = context.getGenerics()[2].getLink(context.getGenerics()[1], context.getGeneric());
					if (link != null)
						link.remove();
				}
			});
		}
	}

	// Edition of other attributes.
	@Parent(InstanceAttributeEditor.class)
	public static class AttributeEditionColumn extends GenericColumn implements AttributeEditionColumnStyle {

		@Override
		public void init() {
			select(gs -> gs[0].getComponents().size() != 2 || gs[0].isSingularConstraintEnabled(gs[0].getComponents().indexOf(gs[2])) ? gs[0] : null);
		}
	}

	// Used when there is only one holder and it’s required so can’t be removed: allow only edition.
	@Parent(AttributeEditionColumn.class)
	public static class SubcellEditor extends GSDiv implements SubcellEditorContainerStyle {

		@Override
		public void init() {
			forEach2(model -> BindingsTools.transmitSuccessiveInvalidations(new ListBinding<Generic>() {
				ObservableList<Generic> holders = ObservableListExtractor.HOLDERS.apply(model.getGenerics());
				{
					bind(holders);
				}

				@Override
				protected ObservableList<Generic> computeValue() {
					return model.getGeneric().isRequiredConstraintEnabled(ApiStatics.BASE_POSITION) && holders.size() == 1 ? FXCollections.observableArrayList(holders) : FXCollections.emptyObservableList();
				}
			}));
		}
	}

	// Edition of non-boolean holders.
	@Parent(SubcellEditor.class)
	public static class HolderEditor extends GSDiv implements SubCellEditorStyle {

		@Override
		public void init() {
			select(gs -> gs[0].getComponents().size() < 2 && !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
		}
	}

	@Parent(HolderEditor.class)
	public static class HolderEditorInput extends GSInputTextEditorWithConversion implements FullSizeStyle {
	}

	// Edition of boolean holders.
	@Parent(SubcellEditor.class)
	public static class BooleanHolderEditor extends GSDiv implements SubCellEditorStyle {

		@Override
		public void init() {
			select(gs -> gs[0].getComponents().size() < 2 && Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null);
		}
	}

	@Parent(BooleanHolderEditor.class)
	public static class CheckboxContainerDiv extends GSDiv implements CenteredFlex {

	}

	@Parent(CheckboxContainerDiv.class)
	public static class BooleanHolderEditorInput extends GSCheckBoxEditor {
	}

	// Edition of links.
	@Parent(SubcellEditor.class)
	public static class LinkEditor extends GSDiv implements FlexStyle, ComponentsDefaults {

		@Override
		public void init() {
			select(gs -> gs[0].getComponents().size() >= 2 ? gs[0] : null);
			createComponentsListProperty();
		}
	}

	@Parent(LinkEditor.class)
	public static class ComponentEditor extends GSDiv implements ComponentEditorStyle {

		@Override
		public void init() {
			forEach((ObservableListExtractor) gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
		}
	}

	// TODO: Finish decomposition of InstanceCompositeSelect.
	@Parent(ComponentEditor.class)
	public static class DirectRelationComponentEditor extends InstanceCompositeSelect implements FullSizeStyle {

		@Override
		public void init() {
			select(gs -> gs[1].isReferentialIntegrityEnabled(gs[1].getComponents().indexOf(gs[0])) ? gs[0] : null);
			addPostfixBinding(model -> {
				Property<List<Property<Context>>> selectedComponents = getComponentsProperty(model);
				if (selectedComponents != null)
					selectedComponents.getValue().add(getSelectionProperty(model));
			});
		}
	}

	@Parent(ComponentEditor.class)
	public static class ReversedRelationDisplayer extends GSLabelDisplayer {

		@Override
		public void init() {
			select(gs -> !gs[1].isReferentialIntegrityEnabled(gs[1].getComponents().indexOf(gs[0])) && !gs[0].getLinks(gs[2]).isEmpty() ? gs[0] : null);
		}
	}

	// Used when the holders/links can be removed: there is no required constraint or there are at least two holders.
	// TODO: Finish.
	@Parent(AttributeEditionColumn.class)
	public static class SubcellEditorWithRemoval extends GSDiv implements SubcellEditorContainerStyle {

		@Override
		public void init() {
			forEach2(model -> BindingsTools.transmitSuccessiveInvalidations(new ListBinding<Generic>() {
				ObservableList<Generic> holders = ObservableListExtractor.HOLDERS.apply(model.getGenerics());
				{
					bind(holders);
				}

				@Override
				protected ObservableList<Generic> computeValue() {
					return (!model.getGeneric().isRequiredConstraintEnabled(ApiStatics.BASE_POSITION) && holders.size() == 1) || holders.size() > 1 ? FXCollections.observableArrayList(holders) : FXCollections.emptyObservableList();
				}
			}));
		}
	}

	// To add a new holder/link if it’s possible.
	// TODO: Finish.
	@Parent(AttributeEditionColumn.class)
	public static class SubcellAdder extends GSDiv implements SubcellEditorContainerStyle {

		@Override
		public void init() {
			select__(model -> {
				ObservableList<Generic> holders = ObservableListExtractor.HOLDERS.apply(model.getGenerics());
				return Bindings.createObjectBinding(() -> holders.isEmpty() || (model.getGeneric().getComponents().size() < 2 && !model.getGeneric().isPropertyConstraintEnabled())
						|| (model.getGeneric().getComponents().size() >= 2 && !model.getGeneric().isSingularConstraintEnabled(ApiStatics.BASE_POSITION)) ? model : null, ObservableListExtractor.HOLDERS.apply(model.getGenerics()));
			});
		}
	}
}
