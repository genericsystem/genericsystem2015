package org.genericsystem.reactor.context;

import java.io.Serializable;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;

import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.EncryptionUtils;
import org.genericsystem.reactor.RootHtmlDomNode;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.contextproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.contextproperties.GSBuilderDefaults;
import org.genericsystem.reactor.contextproperties.GSBuilderDefaults.GenericValueComponents;
import org.genericsystem.reactor.contextproperties.PasswordDefaults;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.contextproperties.StepperDefaults;
import org.genericsystem.reactor.gscomponents.Composite.Header;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlInputText;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlSpan;
import org.genericsystem.reactor.gscomponents.Modal.ModalWithDisplay;
import org.genericsystem.reactor.gscomponents.instancebuilder.InstanceBuilder.GSHolderBuilderDiv;
import org.genericsystem.security.model.User;
import org.genericsystem.security.model.User.Password;
import org.genericsystem.security.model.User.Salt;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javafx.beans.property.Property;

public interface ContextAction extends BiConsumer<Context, Tag> {

	public static final Logger log = LoggerFactory.getLogger(ContextAction.class);

	public static class ADD_HOLDER implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			Property<Serializable> convertedValue = ((ConvertedValueDefaults) tag.getParent()).getConvertedValueProperty(context.getParent());
			if (convertedValue.getValue() != null) {
				Serializable newValue = convertedValue.getValue();
				convertedValue.setValue(null);
				context.getGenerics()[1].addHolder(context.getGeneric(), newValue);
			} else if (Boolean.class.equals(context.getGeneric().getInstanceValueClassConstraint()))
				context.getGenerics()[1].addHolder(context.getGeneric(), false);
		}
	}

	public static class REMOVE implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			context.remove();
		}
	}

	public static class FLUSH implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			context.flush();
		}
	}

	public static class FLASH implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			Context rootContext = context.getRootContext();
			Tag rootTag = rootContext.getTagDataMap().keySet().iterator().next();
			RootHtmlDomNode rootNode = (RootHtmlDomNode) rootContext.getHtmlDomNode(rootTag);
			String body = rootNode.toHTMLString();
			rootNode.toHtmlFile(rootNode.header() + body + rootNode.footer(), "html", "/home/middleware/git/genericsystem2015/gs-reactor/src/main/resources/");
		}
	}

	public static class CANCEL implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			context.cancel();
		}
	}

	public static class MOUNT implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			context.mount();
		}
	}

	public static class UNMOUNT implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			context.unmount();
		}
	}

	public static class SHIFTTS implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			context.shiftTs();
		}
	}

	public static class GC implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			System.gc();
		}
	}

	public static class DISPLAY_NONE implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			tag.getDisplayProperty(context).setValue("none");
		}
	}

	public static class DISPLAY_NONE_CANCEL implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			tag.getParent().getParent().find(HtmlInputText.class).getDomNodeAttributes(context).put("value", "");
			tag.getParent().getParent().find(HtmlInputText.class, 1).getDomNodeAttributes(context).put("value", "");
			tag.getParent().getParent().find(HtmlInputText.class, 2).getDomNodeAttributes(context).put("value", "");
			tag.getDisplayProperty(context).setValue("none");
		}
	}

	public static class MODAL_DISPLAY_FLEX implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			tag.getParent().find(ModalWithDisplay.class).getDisplayProperty(context).setValue("flex");
		}
	}

	public static class SET_SELECTION implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			if (SelectionDefaults.class.isAssignableFrom(tag.getClass()))
				((SelectionDefaults) tag).getSelectionProperty(context).setValue(context);
			else
				log.warn("The SET_SELECTION action can apply only to a tag class implementing SelectionDefaults.");
		}
	}

	public static class RESET_SELECTION implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			if (SelectionDefaults.class.isAssignableFrom(tag.getClass()))
				((SelectionDefaults) tag).getSelectionProperty(context).setValue(null);
			else
				log.warn("The RESET_SELECTION action can apply only to a tag class implementing SelectionDefaults.");
		}
	}

	public static class CREATE_USER implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			context.mount();
			HtmlInputText name = tag.getParent().getParent().find(HtmlInputText.class);
			HtmlInputText passwordInput = tag.getParent().getParent().find(HtmlInputText.class, 1);
			HtmlInputText confirmPassword = tag.getParent().getParent().find(HtmlInputText.class, 2);
			HtmlSpan invalidUsername = tag.getParent().getParent().find(HtmlSpan.class);
			HtmlSpan invalidConfirmPassword = tag.getParent().getParent().find(HtmlSpan.class, 1);

			String psw1 = passwordInput.getDomNodeAttributes(context).get("value");
			String psw2 = confirmPassword.getDomNodeAttributes(context).get("value");
			Generic user;
			try {
				user = context.find(User.class).addInstance(name.getDomNodeAttributes(context).get("value"));
			} catch (RollbackException e) {
				invalidUsername.addStyle(context, "display", "inline");
				return;
			}
			if (psw1 != null && psw1.equals(psw2)) {
				invalidConfirmPassword.addStyle(context, "display", "none");
				invalidUsername.addStyle(context, "display", "none");
				byte[] salt = EncryptionUtils.generateSalt();
				byte[] hash = EncryptionUtils.getEncryptedPassword(psw1, salt);
				Generic hashGeneric = user.setHolder(context.find(Password.class), hash);
				hashGeneric.setHolder(context.find(Salt.class), salt);
				tag.getDisplayProperty(context).setValue("none");
				name.getDomNodeAttributes(context).put("value", "");
				passwordInput.getDomNodeAttributes(context).put("value", "");
				confirmPassword.getDomNodeAttributes(context).put("value", "");
				context.flush();
				context.unmount();
				context.flush();
			} else {
				invalidConfirmPassword.addStyle(context, "display", "inline");
				context.unmount();
			}
		}
	}

	public static class CREATE_INSTANCE implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			if (GSBuilderDefaults.class.isAssignableFrom(tag.getClass())) {
				GSBuilderDefaults buildTag = tag;
				Generic type = context.getGeneric();
				Map<Generic, GenericValueComponents> gvc = buildTag.getGenericValueComponentsMap(context);
				Generic[] components = gvc.get(type).getComponents().entrySet().stream().filter(obs -> obs.getValue() != null && obs.getValue().getValue() != null).map(entry -> getOrSetInstance(entry.getKey(), entry.getValue().getValue()))
						.filter(gen -> gen != null).toArray(Generic[]::new);
				if ((gvc.get(type).getGenericValue().getValue() != null || type.getInstanceValueGenerator() != null || components.length != 0) && components.length == type.getComponents().size()) {
					Generic newInstance = type.setInstance(gvc.get(type).getGenericValue().getValue(), components);
					for (Entry<Generic, GenericValueComponents> entry : gvc.entrySet().stream().filter(e -> !e.getKey().equals(type)).collect(Collectors.toSet())) {
						Generic[] selectedGenerics = entry.getValue().getComponents().entrySet().stream().filter(entry_ -> entry_.getValue() != null && entry_.getValue().getValue() != null)
								.map(entry_ -> getOrSetInstance(entry_.getKey(), entry_.getValue().getValue())).filter(gen -> gen != null).toArray(Generic[]::new);
						if ((entry.getValue().getGenericValue().getValue() != null || selectedGenerics.length != 0) && selectedGenerics.length + 1 == entry.getKey().getComponents().size()) {
							Generic newHolder = newInstance.setHolder(entry.getKey(), entry.getValue().getGenericValue().getValue(), selectedGenerics);
							if (PasswordDefaults.class.isAssignableFrom(tag.getParent().getParent().getClass()) && context.find(Password.class) != null && newHolder.isInstanceOf(context.find(Password.class)))
								newHolder.setHolder(context.find(Salt.class), ((PasswordDefaults) tag.getParent().getParent()).getSaltProperty(context).getValue());
						}
						entry.getValue().getComponents().values().stream().forEach(sel -> sel.setValue(null));
						entry.getValue().getGenericValue().setValue(null);
					}

					gvc.get(type).getComponents().values().stream().forEach(sel -> sel.setValue(null));
					gvc.get(type).getGenericValue().setValue(null);

					Map<Generic, Map<Generic, Property<Serializable>>> relationMap = buildTag.getMultipleRelationProperty(context).getValue();
					for (Entry<Generic, Map<Generic, Property<Serializable>>> entry : relationMap.entrySet())
						for (Generic target : entry.getValue().keySet())
							newInstance.setHolder(entry.getKey(), null, target);
					for (Property<Serializable> convertedProperty : relationMap.values().stream().flatMap(hm -> hm.values().stream()).collect(Collectors.toList()))
						convertedProperty.setValue(null);
					tag.getParent().getParent().find(Header.class).find(GSHolderBuilderDiv.class).find(HtmlSpan.class).addStyle(context, "display", "none");
				} else {
					tag.getParent().getParent().find(Header.class).find(GSHolderBuilderDiv.class).find(HtmlSpan.class).addStyle(context, "display", "inline");
				}
			} else
				log.warn("The CREATE_INSTANCE action can apply only to a tag class implementing GSBuilderDefaults.");
		}

		// Necessary because setInstance returns only instances without a super, whereas getInstance can return
		// instances with a super too.
		private Generic getOrSetInstance(Generic type, Serializable value) {
			Generic instance = type.getInstance(value);
			return instance != null ? instance : type.addInstance(value);
		}

	}

	public static class PREVIOUS implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			if (StepperDefaults.class.isAssignableFrom(tag.getClass()))
				((StepperDefaults) tag).prev(context);
			else
				log.warn("The PREVIOUS action is applicable only to a tag implementing StepperDefaults.");
		}
	}

	public static class NEXT implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			if (StepperDefaults.class.isAssignableFrom(tag.getClass()))
				((StepperDefaults) tag).next(context);
			else
				log.warn("The NEXT action is applicable only to a tag implementing SwitchDefaults.");
		}
	}

	public static class SET_ADMIN_MODE implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			tag.getAdminModeProperty(context).setValue(true);
		}
	}

	public static class SET_NORMAL_MODE implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			tag.getAdminModeProperty(context).setValue(false);
		}
	}

	public static class DISCONNECT implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			tag.getLoggedUserProperty(context).setValue(null);
			tag.getAdminModeProperty(context).setValue(false);
			if (SelectionDefaults.class.isAssignableFrom(tag.getClass()))
				((SelectionDefaults) tag).getSelectionProperty(context).setValue(null);
		}
	}
}
