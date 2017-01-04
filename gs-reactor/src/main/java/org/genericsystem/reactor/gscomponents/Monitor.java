package org.genericsystem.reactor.gscomponents;

import java.util.Arrays;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.EncryptionUtils;
import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.context.ContextAction.CANCEL;
import org.genericsystem.reactor.context.ContextAction.CREATE_USER;
import org.genericsystem.reactor.context.ContextAction.DISPLAY_NONE_CANCEL;
import org.genericsystem.reactor.context.ContextAction.FLASH;
import org.genericsystem.reactor.context.ContextAction.FLUSH;
import org.genericsystem.reactor.context.ContextAction.GC;
import org.genericsystem.reactor.context.ContextAction.MODAL_DISPLAY_FLEX;
import org.genericsystem.reactor.context.ContextAction.MOUNT;
import org.genericsystem.reactor.context.ContextAction.SHIFTTS;
import org.genericsystem.reactor.context.ContextAction.UNMOUNT;
import org.genericsystem.reactor.context.TagSwitcher;
import org.genericsystem.reactor.context.TextBinding;
import org.genericsystem.reactor.contextproperties.PasswordDefaults;
import org.genericsystem.reactor.contextproperties.UserRoleDefaults;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlInputText;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlSpan;
import org.genericsystem.reactor.gscomponents.InstancesTable.ButtonDiv;
import org.genericsystem.reactor.gscomponents.Modal.ModalWithDisplay;
import org.genericsystem.reactor.gscomponents.Monitor.MonitorLogin.LoggedUserDiv;
import org.genericsystem.reactor.gscomponents.Monitor.MonitorLogin.LoggedUserDiv.ModeSwitchButtons;
import org.genericsystem.reactor.gscomponents.Monitor.MonitorLogin.LoginDiv;
import org.genericsystem.reactor.gscomponents.Monitor.MonitorLogin.LoginDiv.ValidateButton;
import org.genericsystem.security.model.User;
import org.genericsystem.security.model.User.Password;
import org.genericsystem.security.model.User.Salt;

import javafx.beans.binding.Bindings;

@Children({ HtmlButton.class, HtmlLabel.class, HtmlButton.class })
@SetText(path = HtmlButton.class, pos = 0, value = "Save")
@BindAction(path = HtmlButton.class, pos = 0, value = FLUSH.class)
@SetText(path = HtmlButton.class, pos = 1, value = "Cancel")
@BindAction(path = HtmlButton.class, pos = 1, value = { CANCEL.class, UNMOUNT.class })
@BindText(path = HtmlLabel.class, pos = 0, value = TextBinding.LAST_UPDATE.class)
@FlexDirectionStyle(FlexDirection.ROW)
@Style(name = "justify-content", value = "space-around")
@Style(name = "padding", value = "10px")
public class Monitor extends FlexDiv {

	@Children({ HtmlButton.class, HtmlButton.class, HtmlButton.class, HtmlLabel.class, HtmlButton.class, HtmlButton.class, HtmlLabel.class, HtmlButton.class })
	@SetText(path = HtmlButton.class, pos = 2, value = "Mount")
	@BindAction(path = HtmlButton.class, pos = 2, value = MOUNT.class)
	@SetText(path = HtmlButton.class, pos = 3, value = "Unmount")
	@BindAction(path = HtmlButton.class, pos = 3, value = UNMOUNT.class)
	@SetText(path = HtmlButton.class, pos = 4, value = "ShiftTs")
	@BindAction(path = HtmlButton.class, pos = 4, value = SHIFTTS.class)
	@SetText(path = HtmlButton.class, pos = 5, value = "Collect")
	@BindAction(path = HtmlButton.class, pos = 5, value = GC.class)
	@BindText(path = HtmlLabel.class, pos = 1, value = TextBinding.CACHE_LEVEL.class)
	@SetText(path = HtmlButton.class, pos = 6, value = "Flash")
	@BindAction(path = HtmlButton.class, pos = 6, value = FLASH.class)
	public static class MonitorExtended extends Monitor {
	}

	@Children({ HtmlButton.class, HtmlButton.class, HtmlLabel.class, LoginDiv.class, LoggedUserDiv.class })
	public static class MonitorLogin extends Monitor implements UserRoleDefaults {

		@Switch(TagSwitcher.LOGGED_USER.class)
		@Children({ ModeSwitchButtons.class, HtmlLabel.class, HtmlButton.class })
		@FlexDirectionStyle(FlexDirection.ROW)
		@SetText(path = HtmlButton.class, value = "Disconnect")
		@BindAction(path = HtmlButton.class, value = ContextAction.DISCONNECT.class)
		public static class LoggedUserDiv extends FlexDiv implements UserRoleDefaults {
			@Override
			public void init() {
				find(HtmlLabel.class).bindText(context -> Bindings.createStringBinding(() -> getLoggedUserProperty(context).getValue() != null ? "Current user: " + (String) getLoggedUserProperty(context).getValue().getValue() : "No user logged.",
						getLoggedUserProperty(context)));
			}

			@Children({ HtmlButton.class, HtmlButton.class })
			@SetText(path = HtmlButton.class, pos = 0, value = "Admin mode")
			@SetText(path = HtmlButton.class, pos = 1, value = "User mode")
			@Switch(TagSwitcher.LOGGED_USER_ADMIN.class)
			@Switch(path = HtmlButton.class, pos = 0, value = TagSwitcher.NORMAL_MODE_ONLY.class)
			@Switch(path = HtmlButton.class, pos = 1, value = TagSwitcher.ADMIN_MODE_ONLY.class)
			@BindAction(path = HtmlButton.class, pos = 0, value = ContextAction.SET_ADMIN_MODE.class)
			@BindAction(path = HtmlButton.class, pos = 1, value = ContextAction.SET_NORMAL_MODE.class)
			@Switch(path = HtmlButton.class, pos = 3, value = TagSwitcher.ADMIN_MODE_ONLY.class)
			public static class ModeSwitchButtons extends FlexDiv {
			}
		}

		@Switch(TagSwitcher.NO_LOGGED_USER.class)
		@Children({ HtmlLabel.class, HtmlInputText.class, HtmlLabel.class, HtmlInputText.class, ValidateButton.class, HtmlSpan.class, HtmlSpan.class, ModalWithDisplay.class, HtmlHyperLink.class })
		@Children(path = { ModalWithDisplay.class, FlexDiv.class }, value = { UserCreation.class })
		@FlexDirectionStyle(FlexDirection.ROW)
		@SetText(path = HtmlLabel.class, pos = 0, value = "Login: ")
		@SetText(path = HtmlLabel.class, pos = 1, value = "Password: ")
		@SetText(path = HtmlSpan.class, pos = 0, value = "Invalid username.")
		@SetText(path = HtmlSpan.class, pos = 1, value = "Invalid password.")
		@Style(path = HtmlSpan.class, name = "display", value = "none")
		@SetText(path = HtmlHyperLink.class, value = "Sign up")
		@Attribute(path = HtmlInputText.class, pos = 1, name = "type", value = "password")
		@BindAction(path = HtmlHyperLink.class, value = MODAL_DISPLAY_FLEX.class)
		public static class LoginDiv extends FlexDiv implements UserRoleDefaults {

			@SetText("OK")
			public static class ValidateButton extends HtmlButton implements PasswordDefaults, UserRoleDefaults {
				@Override
				public void init() {
					createSaltProperty();
					bindAction(context -> {
						HtmlInputText loginInput = getParent().find(HtmlInputText.class);
						HtmlInputText passwordInput = getParent().find(HtmlInputText.class, 1);
						HtmlSpan invalidLogin = getParent().find(HtmlSpan.class);
						HtmlSpan invalidPassword = getParent().find(HtmlSpan.class, 1);
						Generic user = context.find(User.class).getInstance(loginInput.getDomNodeAttributes(context).get("value"));
						if (user != null) {
							Generic hashGeneric = context.find(Password.class).getInstance(user);
							byte[] salt = (byte[]) context.find(Salt.class).getInstance(hashGeneric).getValue();
							if (Arrays.equals((byte[]) hashGeneric.getValue(), EncryptionUtils.getEncryptedPassword(passwordInput.getDomNodeAttributes(context).get("value"), salt))) {
								loginInput.getDomNodeAttributes(context).put("value", "");
								passwordInput.getDomNodeAttributes(context).put("value", "");
								invalidLogin.addStyle(context, "display", "none");
								invalidPassword.addStyle(context, "display", "none");
								getLoggedUserProperty(context).setValue(user);
							} else {
								invalidLogin.addStyle(context, "display", "none");
								invalidPassword.addStyle(context, "display", "inline");
							}
						} else
							invalidLogin.addStyle(context, "display", "inline");
					});
				}
			}
		}

		@Children({ HtmlLabel.class, HtmlInputText.class, HtmlSpan.class, HtmlLabel.class, HtmlInputText.class, HtmlLabel.class, HtmlInputText.class, HtmlSpan.class, ButtonDiv.class })
		@Children(path = ButtonDiv.class, value = { HtmlButton.class, HtmlButton.class })
		@Style(path = ButtonDiv.class, name = "flex", value = "1")
		@SetText(path = HtmlLabel.class, pos = 0, value = "Username :")
		@SetText(path = HtmlLabel.class, pos = 1, value = "Password :")
		@SetText(path = HtmlLabel.class, pos = 2, value = "Confirm password:")
		@SetText(path = { ButtonDiv.class, HtmlButton.class }, pos = { 0, 0 }, value = "OK")
		@SetText(path = { ButtonDiv.class, HtmlButton.class }, pos = { 0, 1 }, value = "Cancel")
		@SetText(path = HtmlSpan.class, pos = 0, value = "Username already exists.")
		@SetText(path = HtmlSpan.class, pos = 1, value = "These passwords don’t match. Try again.")
		@Style(path = HtmlSpan.class, name = "display", value = "none")
		@Attribute(path = HtmlInputText.class, pos = 1, name = "type", value = "password")
		@Attribute(path = HtmlInputText.class, pos = 2, name = "type", value = "password")
		@BindAction(path = { ButtonDiv.class, HtmlButton.class }, pos = { 0, 0 }, value = CREATE_USER.class)
		@BindAction(path = { ButtonDiv.class, HtmlButton.class }, pos = { 0, 1 }, value = DISPLAY_NONE_CANCEL.class)
		public static class UserCreation extends FlexDiv {

		}

	}
}
