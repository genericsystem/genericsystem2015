package org.genericsystem.ir.app.gui.pages;

import java.util.Arrays;
import java.util.Locale;
import java.util.ResourceBundle;

import org.genericsystem.common.Generic;
import org.genericsystem.ir.app.gui.pages.HomePageUserLogin.LoggedUserDiv;
import org.genericsystem.ir.app.gui.pages.HomePageUserLogin.LoginDiv;
import org.genericsystem.ir.app.gui.utils.ContextActionCustom.CREATE_USER_CUSTOM;
import org.genericsystem.reactor.EncryptionUtils;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.context.ContextAction.MODAL_DISPLAY_FLEX;
import org.genericsystem.reactor.context.ContextAction.SET_ADMIN_MODE;
import org.genericsystem.reactor.context.ContextAction.SET_NORMAL_MODE;
import org.genericsystem.reactor.context.TagSwitcher.ADMIN_MODE_ONLY;
import org.genericsystem.reactor.context.TagSwitcher.LOGGED_USER;
import org.genericsystem.reactor.context.TagSwitcher.NORMAL_MODE_ONLY;
import org.genericsystem.reactor.context.TagSwitcher.NO_LOGGED_USER;
import org.genericsystem.reactor.context.TextBinding;
import org.genericsystem.reactor.contextproperties.PasswordDefaults;
import org.genericsystem.reactor.contextproperties.UserRoleDefaults;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlInputText;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlSpan;
import org.genericsystem.reactor.gscomponents.InstancesTable.ButtonDiv;
import org.genericsystem.reactor.gscomponents.Modal.ModalWithDisplay;
import org.genericsystem.reactor.gscomponents.Monitor.MonitorLogin.UserCreation;
import org.genericsystem.security.model.User;
import org.genericsystem.security.model.User.Password;
import org.genericsystem.security.model.User.Salt;

@Children({ LoginDiv.class, LoggedUserDiv.class })
@Switch(path = LoginDiv.class, value = NO_LOGGED_USER.class)
@Switch(path = LoggedUserDiv.class, value = LOGGED_USER.class)
public class HomePageUserLogin extends FlexDiv {

	@FlexDirectionStyle(FlexDirection.COLUMN)
	@Children({ InputTextUsername.class, InputTextPassword.class, ButtonsDiv.class })
	public static class LoginDiv extends FlexDiv {

	}

	@FlexDirectionStyle(FlexDirection.ROW)
	@Children({ HtmlLabel.class, FlexDiv.class })
	@Children(path = FlexDiv.class, value = { HtmlInputText.class, HtmlSpan.class })
	@SetText(path = HtmlLabel.class, value = "Login")
	@Style(path = HtmlLabel.class, name = "color", value = "white")
	@Style(path = HtmlLabel.class, name = "font-weight", value = "bold")
	@FlexDirectionStyle(path = FlexDiv.class, value = FlexDirection.COLUMN)
	@SetText(path = { FlexDiv.class, HtmlSpan.class }, value = "Invalid username")
	@Style(path = { FlexDiv.class, HtmlSpan.class }, name = "display", value = "none")
	@StyleClass(path = { FlexDiv.class, HtmlSpan.class }, value = "error-message-inv")
	@Style(path = { FlexDiv.class, HtmlInputText.class }, name = "margin", value = "0.5em")
	@StyleClass(path = HtmlLabel.class, value = "login")
	@StyleClass(path = { FlexDiv.class, HtmlInputText.class }, value = { "glowing-border", "login" })
	public static class InputTextUsername extends FlexDiv {

	}

	@FlexDirectionStyle(FlexDirection.ROW)
	@Children({ HtmlLabel.class, FlexDiv.class })
	@Children(path = FlexDiv.class, value = { HtmlInputText.class, HtmlSpan.class })
	@SetText(path = HtmlLabel.class, value = "Password")
	@Style(path = HtmlLabel.class, name = "color", value = "white")
	@Style(path = HtmlLabel.class, name = "font-weight", value = "bold")
	@FlexDirectionStyle(path = FlexDiv.class, value = FlexDirection.COLUMN)
	@SetText(path = { FlexDiv.class, HtmlSpan.class }, value = "Invalid password")
	@Style(path = { FlexDiv.class, HtmlSpan.class }, name = "display", value = "none")
	@StyleClass(path = { FlexDiv.class, HtmlSpan.class }, value = "error-message-inv")
	@Style(path = { FlexDiv.class, HtmlInputText.class }, name = "margin", value = "0.5em")
	@Attribute(path = { FlexDiv.class, HtmlInputText.class }, name = "type", value = "password")
	@StyleClass(path = HtmlLabel.class, value = "login")
	@StyleClass(path = { FlexDiv.class, HtmlInputText.class }, value = { "glowing-border", "login" })
	public static class InputTextPassword extends FlexDiv {

	}

	@FlexDirectionStyle(FlexDirection.ROW)
	@Children({ ModalWithDisplay.class, SignInButton.class, SignUpButton.class })
	@Children(path = { ModalWithDisplay.class, FlexDiv.class }, value = CustomUserCreation.class)
	public static class ButtonsDiv extends FlexDiv {

	}

	@SetText("Sign in")
	@Style(name = "flex", value = "1")
	public static class SignInButton extends HtmlButton implements PasswordDefaults, UserRoleDefaults {
		Locale currentLocale = Locale.getDefault();
		ResourceBundle loginMessages = ResourceBundle.getBundle("UserLogin", currentLocale);

		@Override
		public void init() {
			createSaltProperty();
			bindAction(context -> {
				Tag ancestor = getParent().getParent();

				HtmlInputText loginInput = ancestor.find(InputTextUsername.class).find(FlexDiv.class).find(HtmlInputText.class);
				HtmlInputText passwordInput = ancestor.find(InputTextPassword.class).find(FlexDiv.class).find(HtmlInputText.class);
				HtmlSpan invalidLogin = ancestor.find(InputTextUsername.class).find(FlexDiv.class).find(HtmlSpan.class);
				HtmlSpan invalidPassword = ancestor.find(InputTextPassword.class).find(FlexDiv.class).find(HtmlSpan.class);

				User userClass = (User) context.find(User.class);
				Password passwordClass = (Password) context.find(Password.class);
				Salt saltClass = (Salt) context.find(Salt.class);

				String loginInputValue = loginInput.getDomNodeAttributes(context).get("value");
				String passwordInputValue = passwordInput.getDomNodeAttributes(context).get("value");

				if (loginInputValue != null && !loginInputValue.isEmpty()) {
					invalidLogin.addStyle(context, "display", "none");

					if (passwordInputValue != null && !passwordInputValue.isEmpty()) {
						invalidPassword.addStyle(context, "display", "none");

						Generic user = userClass.getInstance(loginInputValue);
						System.out.println("user: " + user == null ? "null" : loginInputValue.isEmpty() ? "empty field" : user);

						if (user != null) {
							Generic hashGeneric = passwordClass.getInstance(user);

							if (hashGeneric != null) {
								byte[] salt = (byte[]) saltClass.getInstance(hashGeneric).getValue();
								if (Arrays.equals((byte[]) hashGeneric.getValue(), EncryptionUtils.getEncryptedPassword(passwordInputValue, salt))) {
									loginInput.getDomNodeAttributes(context).put("value", "");
									passwordInput.getDomNodeAttributes(context).put("value", "");
									invalidLogin.addStyle(context, "display", "none");
									invalidPassword.addStyle(context, "display", "none");
									getLoggedUserProperty(context).setValue(user);
								} else {
									invalidLogin.addStyle(context, "display", "none");
									invalidPassword.setText(context, loginMessages.getString("password.invalid"));
									invalidPassword.addStyle(context, "display", "inline");
								}
							} else {
								System.out.println(">>> hashGeneric == null");
								throw new IllegalStateException("Unable to get a valid password from the database");
							}
						} else { // Unknown username
							invalidLogin.setText(context, loginMessages.getString("user.unknown"));
							invalidLogin.addStyle(context, "display", "inline");
							invalidPassword.addStyle(context, "display", "none");
						}
					} else { // Empty password
						invalidPassword.setText(context, loginMessages.getString("password.empty"));
						invalidPassword.addStyle(context, "display", "inline");
						invalidLogin.addStyle(context, "display", "none");
					}
				} else { // Empty username
					invalidLogin.setText(context, loginMessages.getString("user.empty"));
					invalidLogin.addStyle(context, "display", "inline");
					invalidPassword.addStyle(context, "display", "none");
				}
			});
		}
	}

	@SetText("Sign up")
	@Style(name = "flex", value = "1")
	// @Switch(ADMIN_MODE_ONLY.class) // TODO: change the condition
	@BindAction(MODAL_DISPLAY_FLEX.class)
	public static class SignUpButton extends HtmlButton {

	}

	@SetText(path = HtmlLabel.class, pos = 0, value = "Username:")
	@SetText(path = HtmlLabel.class, pos = 1, value = "Password:")
	@SetText(path = HtmlLabel.class, pos = 2, value = "Confirm password:")
	@SetText(path = HtmlSpan.class, pos = 0, value = "This username already exists!")
	@SetText(path = HtmlSpan.class, pos = 1, value = "These passwords don’t match. Please try again.")
	@StyleClass(path = HtmlSpan.class, value = "error-message")
	@StyleClass(path = HtmlInputText.class, value = "glowing-border")
	@BindAction(path = { ButtonDiv.class, HtmlButton.class }, pos = { 0, 0 }, value = CREATE_USER_CUSTOM.class)
	public static class CustomUserCreation extends UserCreation {

	}

	@Children({ FlexDiv.class, FlexDiv.class })
	@Children(path = FlexDiv.class, pos = 0, value = HtmlLabel.class)
	@Children(path = FlexDiv.class, pos = 1, value = { HtmlButton.class, HtmlButton.class, HtmlButton.class })
	@FlexDirectionStyle(FlexDirection.COLUMN)
	@BindText(path = { FlexDiv.class, HtmlLabel.class }, pos = { 0, 0 }, value = TextBinding.LOGGED_USER.class)
	@SetText(path = { FlexDiv.class, HtmlButton.class }, pos = { 1, 0 }, value = "Logout")
	@SetText(path = { FlexDiv.class, HtmlButton.class }, pos = { 1, 1 }, value = "Switch to admin mode")
	@SetText(path = { FlexDiv.class, HtmlButton.class }, pos = { 1, 2 }, value = "Switch to normal mode")
	@BindAction(path = { FlexDiv.class, HtmlButton.class }, pos = { 1, 0 }, value = ContextAction.DISCONNECT.class)
	@BindAction(path = { FlexDiv.class, HtmlButton.class }, pos = { 1, 1 }, value = SET_ADMIN_MODE.class)
	@BindAction(path = { FlexDiv.class, HtmlButton.class }, pos = { 1, 2 }, value = SET_NORMAL_MODE.class)
	@Switch(path = { FlexDiv.class, HtmlButton.class }, pos = { 1, 1 }, value = { LOGGED_USER.class, NORMAL_MODE_ONLY.class })
	@Switch(path = { FlexDiv.class, HtmlButton.class }, pos = { 1, 2 }, value = ADMIN_MODE_ONLY.class)
	@Style(path = FlexDiv.class, name = "flex", value = "0")
	@Style(path = FlexDiv.class, name = "align-self", value = "center")
	public static class LoggedUserDiv extends FlexDiv {

	}

}
