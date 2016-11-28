package org.genericsystem.reactor.gscomponents;

import java.io.Serializable;
import java.util.Arrays;
import java.util.HashMap;

import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.context.ObservableValueSelector;
import org.genericsystem.reactor.context.TagSwitcher;
import org.genericsystem.reactor.context.ContextAction.CREATE_INSTANCE;
import org.genericsystem.reactor.context.ObservableListExtractor.SUBINSTANCES_OF_RELATION_COMPONENT;
import org.genericsystem.reactor.context.ObservableValueSelector.MULTICHECKBOX_SELECTOR_RELATION;
import org.genericsystem.reactor.context.ObservableValueSelector.NON_MULTICHECKBOX_SELECTOR_RELATION;
import org.genericsystem.reactor.context.ObservableValueSelector.PASSWORD_ATTRIBUTE_SELECTOR;
import org.genericsystem.reactor.context.TextBinding.ERROR_COMPONENTS;
import org.genericsystem.reactor.contextproperties.ComponentsDefaults;
import org.genericsystem.reactor.contextproperties.GSBuilderDefaults;
import org.genericsystem.reactor.contextproperties.PasswordDefaults;
import org.genericsystem.reactor.gscomponents.Composite.Content;
import org.genericsystem.reactor.gscomponents.Composite.Header;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlImg;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlSpan;
import org.genericsystem.reactor.gscomponents.InputTextWithConversion.PasswordInput;
import org.genericsystem.reactor.gscomponents.InstanceBuilder.AddLink;
import org.genericsystem.reactor.gscomponents.InstanceBuilder.GSHolderBuilderDiv;
import org.genericsystem.reactor.gscomponents.InstanceBuilder.MultiCheckboxBuilder;
import org.genericsystem.reactor.gscomponents.InstanceBuilder.PasswordBuilder;
import org.genericsystem.reactor.gscomponents.InstanceEditor.Checkbox;
import org.genericsystem.reactor.gscomponents.InstanceEditor.CheckboxLabel;
import org.genericsystem.reactor.gscomponents.InstanceEditor.DatalistEditor;
import org.genericsystem.reactor.gscomponents.InstanceEditor.HolderAdder;
import org.genericsystem.reactor.gscomponents.InstanceEditor.MultiCheckbox;
import org.genericsystem.reactor.gscomponents.InstanceEditor.PasswordAdder;
import org.genericsystem.reactor.gscomponents.InstancesTable.ButtonDiv;
import org.genericsystem.reactor.gscomponents2.InstanceBuilder2.BuilderCell.BooleanHolderBuilder.CheckboxContainerBuildDiv.BooleanHolderBuilderInput;
import org.genericsystem.reactor.gscomponents2.InstanceBuilder2.BuilderCell.HolderBuilder.HolderBuilderInput;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

@Switch(TagSwitcher.ADMIN_MODE_ONLY.class)
@Children({ Header.class, Content.class, ButtonDiv.class })
@Children(path = Header.class, value = GSHolderBuilderDiv.class)
@Children(path = Content.class, value = { PasswordBuilder.class, GSHolderBuilderDiv.class, MultiCheckboxBuilder.class })
@Children(path = ButtonDiv.class, value = AddLink.class)
@ForEach(path = Content.class, value = ObservableListExtractor.ATTRIBUTES_OF_TYPE.class)
@Select(path = { Content.class, PasswordBuilder.class }, value = PASSWORD_ATTRIBUTE_SELECTOR.class)
@Select(path = { Content.class, GSHolderBuilderDiv.class }, value = NON_MULTICHECKBOX_SELECTOR_RELATION.class)
@Select(path = { Content.class, MultiCheckbox.class }, value = MULTICHECKBOX_SELECTOR_RELATION.class)
public class InstanceBuilder extends Composite implements GSBuilderDefaults, PasswordDefaults {

	@Override
	public void init() {
		createValueComponentsMapProperty();
		createInvalidListProperty();
		createMultipleRelationProperty();
		createSaltProperty();
	}

	@Style(name = "overflow", value = "auto")
	public static class PasswordBuilder extends PasswordAdder implements GSBuilderDefaults {
		@Override
		public void init() {
			createConvertedValueProperty();
			storeInvalidProperty(context -> {
				Property<Serializable> firstHashProperty = find(PasswordInput.class, 0).getConvertedValueProperty(context);
				Property<Serializable> secondHashProperty = find(PasswordInput.class, 1).getConvertedValueProperty(context);
				return Bindings.createBooleanBinding(() -> firstHashProperty.getValue() == null || secondHashProperty.getValue() == null || !Arrays.equals((byte[]) firstHashProperty.getValue(), (byte[]) secondHashProperty.getValue()), firstHashProperty,
						secondHashProperty);
			});
			addPrefixBinding(context -> {
				getSaltProperty(context).addListener((o, v, nv) -> ((PasswordDefaults) getParent().getParent()).getSaltProperty(context.getParent().getParent()).setValue(nv));
				getGenericValueComponents(context).getValue().get(context.getGeneric()).setGenericValue(getConvertedValueProperty(context));
				getInvalidListProperty(context).getValue().add(getInvalidObservable(context));
			});
			find(PasswordInput.class, 1).addConvertedValueChangeListener((context, nva) -> {
				if (Arrays.equals((byte[]) nva, (byte[]) find(PasswordInput.class, 0).getConvertedValueProperty(context).getValue())) {
					find(HtmlSpan.class).addStyle(context, "display", "none");
					getConvertedValueProperty(context).setValue(nva);
				} else
					find(HtmlSpan.class).addStyle(context, "display", "inline");
			});
		}
	}

	@Children(path = CheckboxLabel.class, value = CheckboxBuilder.class)
	@ForEach(path = CheckboxLabel.class, value = SUBINSTANCES_OF_RELATION_COMPONENT.class)
	public static class MultiCheckboxBuilder extends MultiCheckbox implements GSBuilderDefaults {
		@Override
		public void init() {
			addPrefixBinding(context -> getMultipleRelationProperty(context).getValue().put(context.getGeneric(), new HashMap<>()));
		}
	}

	public static class CheckboxBuilder extends Checkbox implements GSBuilderDefaults {
		@Override
		public void init() {
			addConvertedValueChangeListener((context, nva) -> {
				if (Boolean.TRUE.equals(nva))
					getMultipleRelationProperty(context).getValue().get(context.getGenerics()[1]).put(context.getGeneric(), getConvertedValueProperty(context));
				if (Boolean.FALSE.equals(nva))
					getMultipleRelationProperty(context).getValue().get(context.getGenerics()[1]).remove(context.getGeneric());
			});
		}
	}

	@Style(name = "flex", value = "1")
	@Children({ Content.class, Header.class, })
	@Children(path = Header.class, value = { HolderBuilderInput.class, BooleanHolderBuilderInput.class })
	@Children(path = Content.class, value = DatalistEditor.class)
	@Select(path = Header.class, value = ObservableValueSelector.STRICT_ATTRIBUTE_SELECTOR_OR_CHECK_BOX_DISPLAYER_ATTRIBUTE.class)
	@Select(path = { Header.class, HolderBuilderInput.class }, value = ObservableValueSelector.LABEL_DISPLAYER_ATTRIBUTE.class)
	@Select(path = { Header.class, BooleanHolderBuilderInput.class }, value = ObservableValueSelector.CHECK_BOX_DISPLAYER_ATTRIBUTE.class)
	public static class HolderBuilder extends HolderAdder implements GSBuilderDefaults, ComponentsDefaults {

		@Override
		public void init() {
			createComponentsListProperty();
			addPostfixBinding(context -> {
				if (getGenericValueComponents(context) != null)
					getGenericValueComponents(context).getValue().get(context.getGeneric()).setComponents(getComponentsProperty(context).getValue());
			});
		}
	}

	@Children({ HolderBuilder.class, HtmlSpan.class })
	@BindText(path = HtmlSpan.class, value = ERROR_COMPONENTS.class)
	@Style(path = HtmlSpan.class, name = "color", value = "DarkRed")
	@Style(path = HtmlSpan.class, name = "display", value = "none")
	@Style(name = "flex", value = "1")
	public static class GSHolderBuilderDiv extends FlexDiv {

	}

	@Style(path = HtmlImg.class, name = "max-width", value = "24px")
	@Children(HtmlImg.class)
	@Attribute(path = HtmlImg.class, name = "src", value = "add2.png")
	@BindAction(CREATE_INSTANCE.class)
	public static class AddLink extends HtmlHyperLink implements GSBuilderDefaults {

		@Override
		public void init() {
			bindAttribute(ReactorStatics.DISABLED, ReactorStatics.DISABLED,
					model -> Bindings.createStringBinding(
							() -> Boolean.TRUE.equals(getInvalidListProperty(model).getValue().stream().map(input -> input.getValue()).filter(bool -> bool != null).reduce(false, (a, b) -> a || b)) ? ReactorStatics.DISABLED : "",
							getInvalidListProperty(model).getValue().stream().toArray(ObservableValue[]::new)));
		}
	}
}
