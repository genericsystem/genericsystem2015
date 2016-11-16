package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.modelproperties.ComponentsDefaults;
import org.genericsystem.reactor.modelproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.modelproperties.PasswordDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;
import org.genericsystem.reactor.modelproperties.UserRoleDefaults;

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
import org.genericsystem.reactor.annotations.SelectModel;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection;
import org.genericsystem.reactor.gscomponents.CheckBoxWithValue.CheckBoxEditor;
import org.genericsystem.reactor.gscomponents.Composite.Content;
import org.genericsystem.reactor.gscomponents.Composite.Header;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlInputText;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlSpan;
import org.genericsystem.reactor.gscomponents.InputTextWithConversion.InputTextEditorWithConversion;
import org.genericsystem.reactor.gscomponents.InputTextWithConversion.PasswordInput;
import org.genericsystem.reactor.gscomponents.InputWithDatalist.InputTextEditorWithDatalist;
import org.genericsystem.reactor.gscomponents.InstanceEditor.AttributeContent;
import org.genericsystem.reactor.gscomponents.InstanceEditor.HoldersEditor;
import org.genericsystem.reactor.gscomponents.InstanceEditor.MultiCheckbox;
import org.genericsystem.reactor.gscomponents.InstanceEditor.PasswordHoldersEditor;
import org.genericsystem.reactor.gscomponents.InstanceEditor.ValueComponentsEditor;
import org.genericsystem.reactor.gscomponents.InstancesTable.ContentRow;
import org.genericsystem.reactor.gscomponents.InstancesTable.HeaderRow;
import org.genericsystem.reactor.gscomponents.InstancesTable.Holders;
import org.genericsystem.reactor.gscomponents.InstancesTable.ValueComponents;
import org.genericsystem.reactor.gscomponents.Modal.ModalWithDisplay;
import org.genericsystem.reactor.gscomponents2.CellDiv.ActionLink;
import org.genericsystem.reactor.model.ContextAction.ADD_HOLDER;
import org.genericsystem.reactor.model.ContextAction.MODAL_DISPLAY_FLEX;
import org.genericsystem.reactor.model.ContextAction.REMOVE;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.ObservableListExtractor.NO_FOR_EACH;
import org.genericsystem.reactor.model.ObservableListExtractor.SUBINSTANCES_OF_LINK_COMPONENT;
import org.genericsystem.reactor.model.ObservableModelSelector.HOLDER_ADDITION_ENABLED_SELECTOR;
import org.genericsystem.reactor.model.ObservableModelSelector.REMOVABLE_HOLDER_SELECTOR;
import org.genericsystem.reactor.model.ObservableValueSelector;
import org.genericsystem.reactor.model.ObservableValueSelector.MULTICHECKBOX_SELECTOR;
import org.genericsystem.reactor.model.ObservableValueSelector.NON_MULTICHECKBOX_SELECTOR;
import org.genericsystem.reactor.model.ObservableValueSelector.PASSWORD_ATTRIBUTE_SELECTOR;
import org.genericsystem.reactor.model.ObservableValueSelector.STRICT_ATTRIBUTE_SELECTOR;
import org.genericsystem.reactor.model.ObservableValueSelector.TYPE_SELECTOR;
import org.genericsystem.security.model.User.Salt;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;

@ReverseFlexDirection(path = Composite.class)
@FlexDirectionStyle(FlexDirection.ROW)
@Style(path = HeaderRow.class, name = "flex", value = "0.3")
@Style(path = ContentRow.class, name = "flex", value = "1")
@Style(path = { HeaderRow.class, ValueComponents.class }, name = "flex", value = "1 1 auto")
@Style(path = { ContentRow.class, ValueComponentsEditor.class }, name = "flex", value = "1 1 0%")
@Style(path = { ContentRow.class, AttributeContent.class }, name = "flex", value = "1 1 0%")
@Children({ HeaderRow.class, ContentRow.class })
@Children(path = HeaderRow.class, value = { ValueComponents.class, ValueComponents.class })
@Children(path = ContentRow.class, value = { ValueComponentsEditor.class, AttributeContent.class })
@Children(path = { ContentRow.class, ValueComponentsEditor.class }, value = { Header.class, Content.class })
@Children(path = { ContentRow.class, Content.class }, value = { PasswordHoldersEditor.class, HoldersEditor.class, MultiCheckbox.class })
@ForEach(path = { HeaderRow.class, ValueComponents.class }, pos = { 0, 1 }, value = ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
@ForEach(path = { Composite.class, ValueComponents.class, Content.class }, value = ObservableListExtractor.OTHER_COMPONENTS_2.class)
@Select(path = { HeaderRow.class, ValueComponents.class }, pos = { 0, 0 }, value = TYPE_SELECTOR.class)
public class InstanceEditor extends FlexDiv implements SelectionDefaults {

	@FlexDirectionStyle(FlexDirection.COLUMN)
	public static class HorizontalInstanceEditor extends InstanceEditor {
	}

	@Children({ PasswordHoldersEditor.class, HoldersEditor.class, MultiCheckbox.class })
	@ForEach(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES.class)
	@Select(path = PasswordHoldersEditor.class, value = PASSWORD_ATTRIBUTE_SELECTOR.class)
	@Select(path = HoldersEditor.class, value = NON_MULTICHECKBOX_SELECTOR.class)
	@Select(path = MultiCheckbox.class, value = MULTICHECKBOX_SELECTOR.class)
	public static class AttributeContent extends Content {
	}

	@Children(CheckboxLabel.class)
	@ForEach(path = CheckboxLabel.class, value = SUBINSTANCES_OF_LINK_COMPONENT.class)
	@Style(name = "flex-wrap", value = "wrap")
	@Style(name = "overflow", value = "auto")
	@Style(name = "margin-right", value = "1px")
	@Style(name = "margin-bottom", value = "1px")
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
					getConvertedValueProperty(context).setValue(nva);
					find(HtmlSpan.class).addStyle(context, "display", "none");
				} else
					find(HtmlSpan.class).addStyle(context, "display", "inline");
			});
			find(PasswordInput.class, 1).addConvertedValueChangeListener((context, nva) -> {
				if (nva != null && Arrays.equals((byte[]) nva, (byte[]) find(PasswordInput.class, 0).getConvertedValueProperty(context).getValue())) {
					getConvertedValueProperty(context).setValue(nva);
					find(HtmlSpan.class).addStyle(context, "display", "none");
				} else
					find(HtmlSpan.class).addStyle(context, "display", "inline");
			});
		}
	}

	@Style(path = ValueComponents.class, name = "flex", value = "1 0 auto")
	@Children(value = { PasswordEditor.class, PasswordAdder.class })
	@ForEach(path = PasswordEditor.class, value = ObservableListExtractor.HOLDERS.class)
	@SelectModel(path = PasswordAdder.class, value = HOLDER_ADDITION_ENABLED_SELECTOR.class)
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
	@Children({ Header.class, Content.class, ActionLink.class })
	@Children(path = Header.class, value = { InputTextEditorWithConversion.class, CheckBoxEditor.class })
	@Children(path = Content.class, value = DirectRelationComponentEditor.class)
	@SelectModel(path = ActionLink.class, value = REMOVABLE_HOLDER_SELECTOR.class)
	@Select(path = { Header.class, InputTextEditorWithConversion.class }, value = ObservableValueSelector.LABEL_DISPLAYER.class)
	@Select(path = { Header.class, CheckBoxEditor.class }, value = ObservableValueSelector.CHECK_BOX_DISPLAYER.class)
	@SetText(path = ActionLink.class, value = "×")
	@BindAction(path = ActionLink.class, value = REMOVE.class)
	public static class ValueComponentsEditor extends ValueComponents implements ComponentsDefaults {
	}

	@Style(path = ValueComponents.class, name = "flex", value = "1 0 auto")
	@Children(value = { ValueComponentsEditor.class, HolderAdder.class })
	@Children(path = { HolderAdder.class, Header.class }, value = { HolderAdderInput.class, BooleanHolderAdderInput.class })
	@ForEach(path = HolderAdder.class, value = NO_FOR_EACH.class)
	@SelectModel(path = HolderAdder.class, value = HOLDER_ADDITION_ENABLED_SELECTOR.class)
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
	@Children({ Header.class, Content.class, ActionLink.class })
	@Children(path = Header.class, value = { HolderAdderInput.class, BooleanHolderAdderInput.class })
	@Children(path = Content.class, value = DatalistEditor.class)
	@Select(path = ActionLink.class, value = STRICT_ATTRIBUTE_SELECTOR.class)
	@Select(path = Header.class, value = ObservableValueSelector.STRICT_ATTRIBUTE_SELECTOR_OR_CHECK_BOX_DISPLAYER_ATTRIBUTE.class)
	@Select(path = { Header.class, HolderAdderInput.class }, value = ObservableValueSelector.LABEL_DISPLAYER_ATTRIBUTE.class)
	@Select(path = { Header.class, BooleanHolderAdderInput.class }, value = ObservableValueSelector.CHECK_BOX_DISPLAYER_ATTRIBUTE.class)
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
}