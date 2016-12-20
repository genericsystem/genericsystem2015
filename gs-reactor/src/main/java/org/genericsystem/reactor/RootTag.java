package org.genericsystem.reactor;

import org.genericsystem.reactor.HtmlDomNode.RootHtmlDomNode;
import org.genericsystem.reactor.HtmlDomNode.Sender;
import org.genericsystem.reactor.context.StringExtractor;
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

	default void initDomNode(HtmlDomNode htmlDomNode) {
	}
}