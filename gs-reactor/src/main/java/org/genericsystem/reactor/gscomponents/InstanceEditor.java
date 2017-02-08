package org.genericsystem.reactor.gscomponents;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Map;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.EncryptionUtils;
import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.SelectContext;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection;
import org.genericsystem.reactor.context.ContextAction.ADD_HOLDER;
import org.genericsystem.reactor.context.ContextAction.MODAL_DISPLAY_FLEX;
import org.genericsystem.reactor.context.ContextAction.REMOVE;
import org.genericsystem.reactor.context.ObservableContextSelector.HOLDER_ADDITION_ENABLED_SELECTOR;
import org.genericsystem.reactor.context.ObservableContextSelector.REMOVABLE_HOLDER_SELECTOR;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.context.ObservableListExtractor.NO_FOR_EACH;
import org.genericsystem.reactor.context.ObservableListExtractor.SUBINSTANCES_OF_LINK_COMPONENT;
import org.genericsystem.reactor.context.ObservableValueSelector;
import org.genericsystem.reactor.context.ObservableValueSelector.MULTICHECKBOX_INSTANCE_SELECTOR;
import org.genericsystem.reactor.context.ObservableValueSelector.NON_MULTICHECKBOX_INSTANCE_SELECTOR;
import org.genericsystem.reactor.context.ObservableValueSelector.PASSWORD_ATTRIBUTE_SELECTOR;
import org.genericsystem.reactor.context.ObservableValueSelector.STRICT_ATTRIBUTE_SELECTOR;
import org.genericsystem.reactor.context.ObservableValueSelector.TYPE_SELECTOR;
import org.genericsystem.reactor.contextproperties.ComponentsDefaults;
import org.genericsystem.reactor.contextproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.contextproperties.PasswordDefaults;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.contextproperties.StepperDefaults;
import org.genericsystem.reactor.contextproperties.UserRoleDefaults;
import org.genericsystem.reactor.gscomponents.CheckBoxWithValue.CheckBoxEditor;
import org.genericsystem.reactor.gscomponents.Composite.Content;
import org.genericsystem.reactor.gscomponents.Composite.Header;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlInputText;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel.GSLabelDisplayer;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlSpan;
import org.genericsystem.reactor.gscomponents.InputTextWithConversion.InputTextEditorWithConversion;
import org.genericsystem.reactor.gscomponents.InputTextWithConversion.PasswordInput;
import org.genericsystem.reactor.gscomponents.InputWithDatalist.InputTextEditorWithDatalist;
import org.genericsystem.reactor.gscomponents.InstanceEditor.AttributeContent;
import org.genericsystem.reactor.gscomponents.InstanceEditor.AttributeEdition;
import org.genericsystem.reactor.gscomponents.InstanceEditor.HoldersEditor;
import org.genericsystem.reactor.gscomponents.InstanceEditor.InstanceName;
import org.genericsystem.reactor.gscomponents.InstanceEditor.MultiCheckbox;
import org.genericsystem.reactor.gscomponents.InstanceEditor.PasswordHoldersEditor;
import org.genericsystem.reactor.gscomponents.InstanceEditor.ValueComponentsEditor;
import org.genericsystem.reactor.gscomponents.InstancesTable.Holders;
import org.genericsystem.reactor.gscomponents.InstancesTable.ValueComponents;
import org.genericsystem.reactor.gscomponents.Modal.ModalWithDisplay;
import org.genericsystem.security.model.User.Salt;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;

@Style(name = "flex", value = "1 1 0%")
@Style(name = "overflow", value = "hidden")
@Style(name = "background-color", value = "lightgrey")
@Style(name = "padding-left", value = "1px")
@Style(name = "padding-top", value = "1px")
@ReverseFlexDirection(path = Composite.class)
@Style(path = { Composite.class, ValueComponents.class }, pos = { -1, 0 }, name = "flex", value = "0.3")
@Style(path = { Composite.class, ValueComponents.class }, name = "color", value = "white")
@GenericValueBackgroundColor(path = { Composite.class, ValueComponents.class, FlexDiv.class }, pos = { -1, 0, -1 }, value = "#ea0084")
@Children({ InstanceName.class, AttributeEdition.class })
@Children(path = InstanceName.class, value = { ValueComponents.class, ValueComponentsEditor.class })
@Children(path = AttributeEdition.class, value = { ValueComponents.class, AttributeContent.class })
@Children(path = { Composite.class, ValueComponents.class, Header.class }, pos = { -1, 0, -1 }, value = GSLabelDisplayer.class)
@Children(path = { AttributeEdition.class, Content.class }, value = { PasswordHoldersEditor.class, HoldersEditor.class, MultiCheckbox.class })
@Children(path = { InstanceName.class, ValueComponentsEditor.class }, value = { Content.class, Header.class })
@ForEach(path = AttributeEdition.class, value = ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
@ForEach(path = { AttributeEdition.class, AttributeContent.class }, value = ObservableListExtractor.NO_FOR_EACH.class)
@ForEach(path = { AttributeEdition.class, ValueComponents.class, Content.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
@Select(path = { InstanceName.class, ValueComponents.class }, pos = { 0, 0 }, value = TYPE_SELECTOR.class)
@Select(path = { Composite.class, ValueComponents.class, Header.class }, pos = { -1, 0, -1 }, value = ObservableValueSelector.GENERIC_VALUE_DISPLAYER.class)
public class InstanceEditor extends FlexDiv implements SelectionDefaults, StepperDefaults {

	public static class InstanceName extends Composite {
	}

	public static class AttributeEdition extends Composite {
	}

	@Children({ PasswordHoldersEditor.class, HoldersEditor.class, MultiCheckbox.class })
	@ForEach(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
	@Select(path = PasswordHoldersEditor.class, value = PASSWORD_ATTRIBUTE_SELECTOR.class)
	@Select(path = HoldersEditor.class, value = NON_MULTICHECKBOX_INSTANCE_SELECTOR.class)
	@Select(path = MultiCheckbox.class, value = MULTICHECKBOX_INSTANCE_SELECTOR.class)
	public static class AttributeContent extends Content {
	}

	@Children(CheckboxLabel.class)
	@ForEach(path = CheckboxLabel.class, value = SUBINSTANCES_OF_LINK_COMPONENT.class)
	@Style(name = "flex-wrap", value = "wrap")
	@Style(name = "overflow", value = "auto")
	@Style(name = "margin-right", value = "1px")
	@Style(name = "margin-bottom", value = "1px")
	@Style(name = "flex", value = "1 1 0%")
	public static class MultiCheckbox extends FlexDiv {
	}

	@Style(name = "flex", value = "1 0 auto")
	@Style(name = "justify-content", value = "center")
	@Style(name = "align-items", value = "center")
	@Style(name = "text-align", value = "center")
	@GenericValueBackgroundColor("#e5ed00")
	@Children(Checkbox.class)
	@BindText
	public static class CheckboxLabel extends HtmlLabel {
	}

	@Style(name = "float", value = "left")
	@Style(name = "vertical-align", value = "middle")
	@Style(name = "margin", value = "4px")
	public static class Checkbox extends CheckBoxWithValue {

		@Override
		public void init() {
			initValueProperty(context -> context.getGenerics()[2].getLink(context.getGenerics()[1], context.getGeneric()) != null ? true : false);
			storeProperty("exists", context -> {
				ObservableValue<Generic> observableLink = context.getGenerics()[2].getObservableLink(context.getGenerics()[1], context.getGeneric());
				ObservableValue<Boolean> exists = Bindings.createBooleanBinding(() -> observableLink.getValue() != null ? true : false, observableLink);
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

	@Children({ ModalWithDisplay.class, HtmlHyperLink.class })
	@Children(path = { ModalWithDisplay.class, FlexDiv.class }, value = { PasswordEditorContent.class, HtmlHyperLink.class })
	@SetText(path = HtmlHyperLink.class, value = "Change password")
	@BindAction(path = HtmlHyperLink.class, value = MODAL_DISPLAY_FLEX.class)
	public static class PasswordEditor extends FlexDiv implements PasswordDefaults {
		@Override
		public void init() {
			addPrefixBinding(context -> {
				Property<byte[]> saltProperty = getSaltProperty(context);
				if (saltProperty.getValue() == null)
					saltProperty.setValue((byte[]) context.find(Salt.class).getInstance(context.getGeneric()).getValue());
			});
		}
	}

	@Children({ HtmlLabel.class, HtmlInputText.class, HtmlLabel.class, HtmlInputText.class, HtmlLabel.class, HtmlInputText.class, HtmlSpan.class, HtmlSpan.class, ValidateButton.class })
	@SetText(path = HtmlLabel.class, pos = 0, value = "Enter old password:")
	@SetText(path = HtmlLabel.class, pos = 1, value = "Enter new password:")
	@SetText(path = HtmlLabel.class, pos = 2, value = "Confirm password:")
	@SetText(path = HtmlSpan.class, pos = 0, value = "These passwords don’t match. Try again.")
	@SetText(path = HtmlSpan.class, pos = 1, value = "Old password incorrect.")
	@Style(path = HtmlSpan.class, name = "color", value = "darkred")
	@Style(path = HtmlSpan.class, name = "display", value = "none")
	@Attribute(path = HtmlInputText.class, pos = 0, name = "type", value = "password")
	@Attribute(path = HtmlInputText.class, pos = 1, name = "type", value = "password")
	@Attribute(path = HtmlInputText.class, pos = 2, name = "type", value = "password")
	public static class PasswordEditorContent extends FlexDiv {
	}

	@SetText("OK")
	public static class ValidateButton extends HtmlButton implements PasswordDefaults, UserRoleDefaults {
		@Override
		public void init() {
			bindAction(context -> {
				HtmlInputText oldPassword = getParent().find(HtmlInputText.class);
				HtmlInputText passwordInput = getParent().find(HtmlInputText.class, 1);
				HtmlInputText confirmPassword = getParent().find(HtmlInputText.class, 2);
				HtmlSpan invalidPassword = getParent().find(HtmlSpan.class, 1);
				HtmlSpan invalidConfirmPassword = getParent().find(HtmlSpan.class);
				if (Arrays.equals((byte[]) context.getGeneric().getValue(), EncryptionUtils.getEncryptedPassword(oldPassword.getDomNodeAttributes(context).get("value"), getSaltProperty(context).getValue()))) {
					String psw1 = passwordInput.getDomNodeAttributes(context).get("value");
					String psw2 = confirmPassword.getDomNodeAttributes(context).get("value");
					if (psw1 != null && psw1.equals(psw2)) {
						invalidConfirmPassword.addStyle(context, "display", "none");
						invalidPassword.addStyle(context, "display", "none");
						context.getGeneric().updateValue(EncryptionUtils.getEncryptedPassword(psw1, getSaltProperty(context).getValue()));
					} else {
						invalidConfirmPassword.addStyle(context, "display", "none");
					}
				} else {
					invalidPassword.addStyle(context, "display", "inline");
				}
			});
		}
	}

	@Children({ HtmlLabel.class, PasswordInput.class, HtmlLabel.class, PasswordInput.class, HtmlSpan.class })
	@SetText(path = HtmlLabel.class, pos = 0, value = "Enter new password:")
	@SetText(path = HtmlLabel.class, pos = 1, value = "Confirm password:")
	@SetText(path = HtmlSpan.class, value = "These passwords don’t match. Try again.")
	@Style(path = HtmlSpan.class, name = "color", value = "darkred")
	@Style(path = HtmlSpan.class, name = "display", value = "none")
	public static class PasswordAdder extends FlexDiv implements PasswordDefaults, ConvertedValueDefaults {
		@Override
		public void init() {
			createConvertedValueProperty();
			addConvertedValueChangeListener((context, nva) -> {
				if (nva != null) {
					Generic passwordHash = context.getGenerics()[1].addHolder(context.getGeneric(), nva);
					passwordHash.setHolder(context.find(Salt.class), getSaltProperty(context).getValue());
				}
			});
			find(PasswordInput.class).addConvertedValueChangeListener((context, nva) -> {
				if (nva != null && Arrays.equals((byte[]) nva, (byte[]) find(PasswordInput.class, 1).getConvertedValueProperty(context).getValue())) {
					find(HtmlSpan.class).addStyle(context, "display", "none");
					getConvertedValueProperty(context).setValue(nva);
				} else
					find(HtmlSpan.class).addStyle(context, "display", "inline");
			});
			find(PasswordInput.class, 1).addConvertedValueChangeListener((context, nva) -> {
				if (nva != null && Arrays.equals((byte[]) nva, (byte[]) find(PasswordInput.class, 0).getConvertedValueProperty(context).getValue())) {
					find(HtmlSpan.class).addStyle(context, "display", "none");
					getConvertedValueProperty(context).setValue(nva);
				} else
					find(HtmlSpan.class).addStyle(context, "display", "inline");
			});
		}
	}

	@Style(path = ValueComponents.class, name = "flex", value = "1 0 auto")
	@Children(value = { PasswordEditor.class, PasswordAdder.class })
	@ForEach(path = PasswordEditor.class, value = ObservableListExtractor.HOLDERS.class)
	@SelectContext(path = PasswordAdder.class, value = HOLDER_ADDITION_ENABLED_SELECTOR.class)
	@FlexDirectionStyle(FlexDirection.COLUMN)
	@Style(name = "flex-wrap", value = "wrap")
	@Style(name = "overflow", value = "auto")
	public static class PasswordHoldersEditor extends FlexDiv implements PasswordDefaults {
		@Override
		public void init() {
			createSaltProperty();
		}
	}

	@Style(path = { Header.class, InputTextEditorWithConversion.class }, name = "flex", value = "1")
	@Style(path = { Header.class, InputTextEditorWithConversion.class }, name = "width", value = "100%")
	@Children({ Content.class, Header.class, ActionLink.class })
	@Children(path = Header.class, value = { InputTextEditorWithConversion.class, CheckBoxEditor.class })
	@Children(path = Content.class, value = DirectRelationComponentEditor.class)
	@SelectContext(path = ActionLink.class, value = REMOVABLE_HOLDER_SELECTOR.class)
	@Select(path = { Header.class, InputTextEditorWithConversion.class }, value = ObservableValueSelector.INSTANCE_LABEL_DISPLAYER.class)
	@Select(path = { Header.class, CheckBoxEditor.class }, value = ObservableValueSelector.INSTANCE_CHECK_BOX_DISPLAYER.class)
	@SetText(path = ActionLink.class, value = "×")
	@BindAction(path = ActionLink.class, value = REMOVE.class)
	public static class ValueComponentsEditor extends ValueComponents implements ComponentsDefaults {
	}

	@Style(path = ValueComponents.class, name = "flex", value = "1 0 auto")
	@Children(value = { ValueComponentsEditor.class, HolderAdder.class })
	@Children(path = { HolderAdder.class, Header.class }, value = { HolderAdderInput.class, BooleanHolderAdderInput.class })
	@ForEach(path = HolderAdder.class, value = NO_FOR_EACH.class)
	@SelectContext(path = HolderAdder.class, value = HOLDER_ADDITION_ENABLED_SELECTOR.class)
	@FlexDirectionStyle(FlexDirection.COLUMN)
	@Style(name = "flex-wrap", value = "wrap")
	@Style(name = "overflow", value = "auto")
	public static class HoldersEditor extends Holders {
	}

	@Style(name = "flex", value = "1")
	@Style(name = "width", value = "100%")
	public static class DirectRelationComponentEditor extends InputTextEditorWithDatalist implements ComponentsDefaults {
		@Override
		public void init() {
			super.init();
			addPostfixBinding(model -> {
				Property<Map<Generic, Property<Serializable>>> selectedComponents = getComponentsProperty(model);
				if (selectedComponents != null)
					selectedComponents.getValue().put(model.getGeneric(), find(InputTextWithConversion.class).getConvertedValueProperty(model));
			});
		}
	}

	@Style(name = "flex", value = "1 0 auto")
	@Style(path = { Header.class, InputTextWithConversion.class }, name = "flex", value = "1")
	@Style(path = { Header.class, InputTextWithConversion.class }, name = "width", value = "100%")
	@Children({ Content.class, Header.class, ActionLink.class })
	@Children(path = Header.class, value = { HolderAdderInput.class, BooleanHolderAdderInput.class })
	@Children(path = Content.class, value = DatalistEditor.class)
	@Select(path = ActionLink.class, value = STRICT_ATTRIBUTE_SELECTOR.class) // TODO: Remove
	@Select(path = Header.class, value = ObservableValueSelector.GENERIC_VALUE_DISPLAYER.class)
	@Select(path = { Header.class, HolderAdderInput.class }, value = ObservableValueSelector.LABEL_DISPLAYER.class)
	@Select(path = { Header.class, BooleanHolderAdderInput.class }, value = ObservableValueSelector.CHECK_BOX_DISPLAYER.class)
	@SetText(path = ActionLink.class, value = "+")
	@BindAction(path = ActionLink.class, value = ADD_HOLDER.class)
	public static class HolderAdder extends ValueComponents implements ComponentsDefaults, ConvertedValueDefaults {
		@Override
		public void init() {
			createComponentsListProperty();
			createConvertedValueProperty();
			addConvertedValueChangeListener((context, nva) -> {
				if (nva != null)
					context.getGenerics()[1].addHolder(context.getGeneric(), nva);
			});
			addPostfixBinding(model -> {
				Property<Map<Generic, Property<Serializable>>> selectedComponents = getComponentsProperty(model);
				ChangeListener<Serializable> listener = (o, v, nva) -> {
					Generic[] selectedGenerics = selectedComponents.getValue().entrySet().stream().filter(entry -> entry.getValue() != null && entry.getValue().getValue() != null).map(entry -> entry.getKey().setInstance(entry.getValue().getValue()))
							.filter(gen -> gen != null).toArray(Generic[]::new);
					if (selectedGenerics.length + 1 == model.getGeneric().getComponents().size()) {
						selectedComponents.getValue().values().stream().forEach(sel -> sel.setValue(null));
						try {
							model.getGenerics()[1].setHolder(model.getGeneric(), null, selectedGenerics);
						} catch (RollbackException e) {
							e.printStackTrace();
						}
					}
				};
				selectedComponents.getValue().values().forEach(component -> component.addListener(listener));
			});
		}
	}

	public static class HolderAdderInput extends InputTextWithConversion {
		@Override
		public void init() {
			addConvertedValueChangeListener((context, nva) -> ((ConvertedValueDefaults) getParent().getParent()).getConvertedValueProperty(context.getParent().getParent()).setValue(nva));
		}
	}

	public static class BooleanHolderAdderInput extends CheckBoxWithValue {
		@Override
		public void init() {
			addConvertedValueChangeListener((context, nva) -> ((ConvertedValueDefaults) getParent().getParent()).getConvertedValueProperty(context.getParent().getParent()).setValue(nva));
		}
	}

	@Style(name = "flex", value = "1")
	public static class DatalistEditor extends InputWithDatalist implements ComponentsDefaults {
		@Override
		public void init() {
			super.init();
			addPostfixBinding(model -> {
				Property<Map<Generic, Property<Serializable>>> selectedComponents = getComponentsProperty(model);
				if (selectedComponents != null)
					selectedComponents.getValue().put(model.getGeneric(), find(InputTextWithConversion.class).getConvertedValueProperty(model));
			});
		}
	}

	@Style(name = "justify-content", value = "center")
	@Style(name = "height", value = "100%")
	@Style(name = "text-decoration", value = "none")
	public static class ActionLink extends HtmlHyperLink {
	}

	@FlexDirectionStyle(FlexDirection.ROW)
	public static class HorizontalInstanceEditor extends InstanceEditor {

	}
}
