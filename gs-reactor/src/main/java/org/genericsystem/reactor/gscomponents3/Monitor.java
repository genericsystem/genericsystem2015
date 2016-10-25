package org.genericsystem.reactor.gscomponents3;

import org.genericsystem.reactor.modelproperties.PasswordDefaults;
import org.genericsystem.reactor.modelproperties.UserRoleDefaults;

import org.genericsystem.reactor.htmltag.HtmlButton;
import org.genericsystem.reactor.htmltag.HtmlInputText;
import org.genericsystem.reactor.htmltag.HtmlLabel;
import org.genericsystem.reactor.htmltag.HtmlSpan;

import java.util.Arrays;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.EncryptionUtils;
import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SelectModel;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.gscomponents3.Monitor.MonitorLogin.LoggedUserDiv;
import org.genericsystem.reactor.gscomponents3.Monitor.MonitorLogin.LoggedUserDiv.DisconnectButton;
import org.genericsystem.reactor.gscomponents3.Monitor.MonitorLogin.LoginDiv;
import org.genericsystem.reactor.gscomponents3.Monitor.MonitorLogin.LoginDiv.ValidateButton;
import org.genericsystem.reactor.model.ContextAction.CANCEL;
import org.genericsystem.reactor.model.ContextAction.FLUSH;
import org.genericsystem.reactor.model.ContextAction.GC;
import org.genericsystem.reactor.model.ContextAction.MOUNT;
import org.genericsystem.reactor.model.ContextAction.SHIFTTS;
import org.genericsystem.reactor.model.ContextAction.UNMOUNT;
import org.genericsystem.reactor.model.ObservableModelSelector;
import org.genericsystem.reactor.model.TextBinding;
import org.genericsystem.security.model.User;
import org.genericsystem.security.model.User.Password;
import org.genericsystem.security.model.User.Salt;

import javafx.beans.binding.Bindings;

@Children({ HtmlButton.class, HtmlLabel.class, HtmlButton.class })
@SetText(path = HtmlButton.class, pos = 0, value = "Save")
@BindAction(path = HtmlButton.class, pos = 0, value = FLUSH.class)
@SetText(path = HtmlButton.class, pos = 1, value = "Cancel")
@BindAction(path = HtmlButton.class, pos = 1, value = CANCEL.class)
@BindText(path = HtmlLabel.class, pos = 0, value = TextBinding.LAST_UPDATE.class)
@FlexDirectionStyle(FlexDirection.ROW)
@Style(name = "justify-content", value = "space-around")
@Style(name = "padding", value = "10px")
public class Monitor extends GSDiv {

	@Children({ HtmlButton.class, HtmlButton.class, HtmlButton.class, HtmlLabel.class, HtmlButton.class, HtmlButton.class, HtmlLabel.class/* , HtmlButton.class */ })
	@SetText(path = HtmlButton.class, pos = 2, value = "Mount")
	@BindAction(path = HtmlButton.class, pos = 2, value = MOUNT.class)
	@SetText(path = HtmlButton.class, pos = 3, value = "Unmount")
	@BindAction(path = HtmlButton.class, pos = 3, value = UNMOUNT.class)
	@SetText(path = HtmlButton.class, pos = 4, value = "ShiftTs")
	@BindAction(path = HtmlButton.class, pos = 4, value = SHIFTTS.class)
	@SetText(path = HtmlButton.class, pos = 5, value = "Collect")
	@BindAction(path = HtmlButton.class, pos = 5, value = GC.class)
	@BindText(path = HtmlLabel.class, pos = 1, value = TextBinding.CACHE_LEVEL.class)
	public static class MonitorExtended extends Monitor {
	}

	@Children({ HtmlButton.class, HtmlButton.class, HtmlLabel.class, LoginDiv.class, LoggedUserDiv.class })
	public static class MonitorLogin extends Monitor implements UserRoleDefaults {

		@SelectModel(ObservableModelSelector.LOGGED_USER.class)
		@Children({ HtmlLabel.class, DisconnectButton.class })
		@FlexDirectionStyle(FlexDirection.ROW)
		public static class LoggedUserDiv extends GSDiv implements UserRoleDefaults {
			@Override
			public void init() {
				find(HtmlLabel.class).bindText(context -> Bindings.createStringBinding(() -> getLoggedUserProperty(context).getValue() != null ? "Current user: " + (String) getLoggedUserProperty(context).getValue().getValue() : "No user logged.",
						getLoggedUserProperty(context)));
			}

			@SetText("Disconnect")
			public static class DisconnectButton extends HtmlButton implements UserRoleDefaults {
				@Override
				public void init() {
					bindAction(context -> getLoggedUserProperty(context).setValue(null));
				}
			}
		}

		@SelectModel(ObservableModelSelector.NO_LOGGED_USER.class)
		@Children({ HtmlLabel.class, HtmlInputText.class, HtmlLabel.class, HtmlInputText.class, ValidateButton.class, HtmlSpan.class, HtmlSpan.class })
		@FlexDirectionStyle(FlexDirection.ROW)
		@SetText(path = HtmlLabel.class, pos = 0, value = "Login: ")
		@SetText(path = HtmlLabel.class, pos = 1, value = "Password: ")
		@SetText(path = HtmlSpan.class, pos = 0, value = "Invalid username.")
		@SetText(path = HtmlSpan.class, pos = 1, value = "Invalid password.")
		@Style(path = HtmlSpan.class, name = "display", value = "none")
		@Attribute(path = HtmlInputText.class, pos = 1, name = "type", value = "password")
		public static class LoginDiv extends GSDiv implements UserRoleDefaults {

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
	}
}
