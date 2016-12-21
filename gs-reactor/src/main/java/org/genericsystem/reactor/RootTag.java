package org.genericsystem.reactor;

import org.genericsystem.reactor.HtmlDomNode.RootHtmlDomNode;
import org.genericsystem.reactor.HtmlDomNode.Sender;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.context.StringExtractor;
import org.genericsystem.reactor.context.TextBinding;
import org.genericsystem.reactor.context.TextBinding.GENERIC_STRING;
import org.genericsystem.reactor.contextproperties.ActionDefaults;
import org.genericsystem.reactor.contextproperties.FlexDirectionDefaults;
import org.genericsystem.reactor.contextproperties.GenericStringDefaults;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.TagImpl;

public interface RootTag extends Tag {

	default RootHtmlDomNode init(Context rootModelContext, String rootId, Sender send) {
		return new RootHtmlDomNode(rootModelContext, this, rootId, send);
	}

	@Override
	default RootTag getRootTag() {
		return this;
	}

	AnnotationsManager getAnnotationsManager();

	default void processAnnotations(Tag tag) {
		getAnnotationsManager().processAnnotations(tag);
	}

	TagNode buildTagNode(Tag child);

	default void processChildren(Tag tag, Class<? extends TagImpl>[] classes) {
		for (Class<? extends TagImpl> clazz : classes) {
			TagImpl result = createChild(tag, clazz);
			result.setTagNode(buildTagNode(result));
		}
	}

	default <T extends TagImpl> TagImpl createChild(Tag tag, Class<T> clazz) {
		T result = null;
		try {
			result = clazz.newInstance();
		} catch (IllegalAccessException | InstantiationException e) {
			throw new IllegalStateException(e);
		}
		result.setParent(tag);
		return result;
	}

	default void processStyle(Tag tag, String name, String value) {
		tag.addPrefixBinding(context -> tag.getDomNodeStyles(context).put(name, value));
	}

	default void processGenericValueBackgroundColor(Tag tag, String value) {
		tag.addPrefixBinding(
				context -> tag.addStyle(context, "background-color", "Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(context.getGeneric().getMeta())) ? ((GenericStringDefaults) tag).getGenericStringProperty(context).getValue() : value));
	}

	default void processFlexDirectionStyle(Tag tag, FlexDirection flexDirection) {
		if (FlexDirectionDefaults.class.isAssignableFrom(tag.getClass()))
			((FlexDirectionDefaults) tag).setDirection(flexDirection);
		else
			log.warn("Warning: FlexDirection is applicable only to classes implementing FlexDirectionDefaults.");
	}

	default void processReverseFlexDirection(Tag tag) {
		if (FlexDirectionDefaults.class.isAssignableFrom(tag.getClass()))
			((FlexDirectionDefaults) tag).reverseDirection();
		else
			log.warn("Warning: ReverseFlexDirection is applicable only to classes implementing FlexDirectionDefaults.");
	}

	default void processKeepFlexDirection(Tag tag) {
		if (FlexDirectionDefaults.class.isAssignableFrom(tag.getClass()))
			((FlexDirectionDefaults) tag).keepDirection();
		else
			log.warn("Warning: KeepFlexDirection is applicable only to classes implementing FlexDirectionDefaults.");
	}

	default void processStyleClass(Tag tag, String[] classes) {
		for (String sc : classes)
			tag.addStyleClass(sc);
	}

	default void processSetText(Tag tag, Class<?>[] path, String[] texts) {
		if (texts.length == 1)
			tag.setText(texts[0]);
		else
			tag.setText(texts[AnnotationsManager.position(tag, path[path.length - 1])]);
	}

	default void processSetText(Tag tag, Context context, Class<?>[] path, String[] texts) {
		if (texts.length == 1)
			tag.setText(context, texts[0]);
		else
			tag.setText(context, texts[AnnotationsManager.position(tag, path[path.length - 1])]);
	}

	default void processBindText(Tag tag, Class<? extends TextBinding> value) {
		if (GENERIC_STRING.class.equals(value))
			tag.bindText();
		else
			tag.bindText(context -> {
				try {
					return value.newInstance().apply(context, tag);
				} catch (InstantiationException | IllegalAccessException e) {
					throw new IllegalStateException(e);
				}
			});
	}

	default void processBindText(Tag tag, Context context, Class<? extends TextBinding> value) {
		if (GENERIC_STRING.class.equals(value))
			tag.bindText();
		else
			tag.bindText(context_ -> {
				try {
					return value.newInstance().apply(context_, tag);
				} catch (InstantiationException | IllegalAccessException e) {
					throw new IllegalStateException(e);
				}
			});
	}

	default void processBindAction(Tag tag, Class<? extends ContextAction> value) {
		if (ActionDefaults.class.isAssignableFrom(tag.getClass()))
			((ActionDefaults) tag).bindAction(context -> {
				try {
					value.newInstance().accept(context, tag);
				} catch (InstantiationException | IllegalAccessException e) {
					throw new IllegalStateException(e);
				}
			});
		else
			log.warn("BindAction is applicable only to tags implementing ActionDefaults.");
	}

	default void processBindAction(Tag tag, Context context, Class<? extends ContextAction> value) {
		if (ActionDefaults.class.isAssignableFrom(tag.getClass()))
			((ActionDefaults) tag).bindAction(context, context_ -> {
				try {
					value.newInstance().accept(context_, tag);
				} catch (InstantiationException | IllegalAccessException e) {
					throw new IllegalStateException(e);
				}
			});
		else
			log.warn("BindAction is applicable only to tags implementing ActionDefaults.");
	}

	default void initDomNode(HtmlDomNode htmlDomNode) {
	}
}