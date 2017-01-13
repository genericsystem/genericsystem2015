package org.genericsystem.quiz.app.pages.components;

import java.util.Arrays;

import org.genericsystem.common.Generic;
import org.genericsystem.quiz.app.pages.components.QuizLogin.QuizLoginDiv;
import org.genericsystem.quiz.app.pages.components.QuizLogin.QuizLoginDiv.InputTextLogin;
import org.genericsystem.quiz.app.pages.components.QuizLogin.QuizLoginDiv.InputTextPassword;
import org.genericsystem.quiz.app.pages.components.QuizLogin.QuizLoginDiv.QuizChoiceOptions;
import org.genericsystem.quiz.app.pages.components.QuizLogin.QuizLoginDiv.QuizChoiceOptions.QUIZ_MODAL_DISPLAY_FLEX;
import org.genericsystem.quiz.app.pages.components.QuizLogin.QuizLoginDiv.QuizChoiceOptions.QuizUserCreation;
import org.genericsystem.quiz.app.pages.components.QuizLogin.QuizLoginDiv.QuizChoiceOptions.QuizValidateButton;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.EncryptionUtils;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.context.ContextAction.CREATE_USER;
import org.genericsystem.reactor.context.ContextAction.DISPLAY_NONE_CANCEL;
import org.genericsystem.reactor.context.TagSwitcher;
import org.genericsystem.reactor.contextproperties.PasswordDefaults;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.FlexDiv.FlexRow;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlInputText;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlSpan;
import org.genericsystem.reactor.gscomponents.InstancesTable.ButtonDiv;
import org.genericsystem.reactor.gscomponents.Modal.ModalWithDisplay;
import org.genericsystem.reactor.gscomponents.Monitor.MonitorLogin.LoggedUserDiv;
import org.genericsystem.security.model.User;
import org.genericsystem.security.model.User.Password;
import org.genericsystem.security.model.User.Salt;

@Children({ QuizLoginDiv.class, LoggedUserDiv.class })
public class QuizLogin extends HtmlDiv {

	@Switch(TagSwitcher.NO_LOGGED_USER.class)
	//
	@Children({ InputTextLogin.class, InputTextPassword.class, QuizChoiceOptions.class })
	//
	@StyleClass("loginQ")
	@StyleClass(path = { HtmlDiv.class, HtmlInputText.class }, value = { "inputTextQ", "vertical-align" })
	@Style(path = HtmlDiv.class, name = "margin-top", value = "10px")
	@Style(path = QuizChoiceOptions.class, name = "margin-bottom", value = "10px")
	public static class QuizLoginDiv extends QuizLogin {

		@Children({ HtmlLabel.class, HtmlInputText.class, HtmlSpan.class })
		//
		@SetText(path = HtmlLabel.class, value = "Login: ")
		@SetText(path = HtmlSpan.class, value = "Invalid username.")
		//
		@Style(path = HtmlSpan.class, name = "display", value = "none")
		@StyleClass(path = HtmlLabel.class, value = { "margin-lr-10", "vertical-align" })
		@StyleClass(path = HtmlSpan.class, value = { "margin-lr-10", "vertical-align" })
		public static class InputTextLogin extends HtmlDiv {

		}

		@SetText(path = HtmlLabel.class, value = "Password: ")
		@SetText(path = HtmlSpan.class, value = "Invalid password.")
		//
		@Attribute(path = HtmlInputText.class, name = "type", value = "password")
		public static class InputTextPassword extends InputTextLogin {

		}

		@Children({ QuizValidateButton.class, ModalWithDisplay.class, HtmlHyperLink.class })
		@Children(path = { ModalWithDisplay.class, FlexDiv.class }, value = QuizUserCreation.class)
		//
		@SetText(path = HtmlHyperLink.class, value = "Sign up")
		//
		@StyleClass("vertical-align")
		@Style(name = "justify-content", value = "space-between")
		@Style(name = "padding-right", value = "1em")
		@StyleClass(path = HtmlHyperLink.class, value = { "vertical-align", "choiceResponsive" })
		@Style(path = HtmlHyperLink.class, name = "text-align", value = "center")
		@StyleClass(path = QuizValidateButton.class, value = { "choiceResponsive", "monitorButton", "white" })
		@Style(path = QuizValidateButton.class, name = "text-align", value = "center")
		@Style(path = QuizValidateButton.class, name = "background-color", value = "green")
		//
		@BindAction(path = HtmlHyperLink.class, value = QUIZ_MODAL_DISPLAY_FLEX.class)
		public static class QuizChoiceOptions extends FlexRow {

			@SetText("<strong>OK</strong>")
			//
			@Style(name = "text-align", value = "center")
			public static class QuizValidateButton extends HtmlButton implements PasswordDefaults {
				@Override
				public void init() {
					createSaltProperty();
					bindAction(context -> {
						HtmlInputText loginInput = getParent().getParent().find(InputTextLogin.class).find(HtmlInputText.class);
						HtmlInputText passwordInput = getParent().getParent().find(InputTextPassword.class).find(HtmlInputText.class);
						HtmlSpan invalidLogin = getParent().getParent().find(InputTextLogin.class).find(HtmlSpan.class);
						HtmlSpan invalidPassword = getParent().getParent().find(InputTextPassword.class).find(HtmlSpan.class);
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

			public static class QUIZ_MODAL_DISPLAY_FLEX implements ContextAction {
				@Override
				public void accept(Context context, Tag tag) {
					tag.getParent().find(ModalWithDisplay.class).getDisplayProperty(context).setValue("flex");
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
			public static class QuizUserCreation extends FlexDiv {

			}

		}
	}
}
