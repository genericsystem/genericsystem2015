package org.genericsystem.reactor;

import org.genericsystem.reactor.HtmlDomNode.RootHtmlDomNode;
import org.genericsystem.reactor.HtmlDomNode.Sender;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.context.ObservableContextSelector;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.context.ObservableListExtractor.NO_FOR_EACH;
import org.genericsystem.reactor.context.ObservableValueSelector;
import org.genericsystem.reactor.context.StringExtractor;
import org.genericsystem.reactor.context.TagSwitcher;
import org.genericsystem.reactor.context.TextBinding;
import org.genericsystem.reactor.context.TextBinding.GENERIC_STRING;
import org.genericsystem.reactor.contextproperties.ActionDefaults;
import org.genericsystem.reactor.contextproperties.FlexDirectionDefaults;
import org.genericsystem.reactor.contextproperties.GenericStringDefaults;
import org.genericsystem.reactor.contextproperties.TextPropertyDefaults;
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
		tag.addPrefixBinding(context -> processStyle(tag, context, name, value));
	}

	default void processStyle(Tag tag, Context context, String name, String value) {
		tag.addStyle(context, name, value);
	}

	default void removeStyle(Tag tag, Context context, String name) {
		tag.getDomNodeStyles(context).remove(name);
	}

	default void processAttribute(Tag tag, String name, String value) {
		tag.addPrefixBinding(context -> processAttribute(tag, context, name, value));
	}

	default void processAttribute(Tag tag, Context context, String name, String value) {
		tag.addAttribute(context, name, value);
	}

	default void removeAttribute(Tag tag, Context context, String name) {
		tag.getDomNodeAttributes(context).remove(name);
	}

	default void processGenericValueBackgroundColor(Tag tag, String value) {
		tag.addPrefixBinding(context -> processGenericValueBackgroundColor(tag, context, value));
	}

	default void processGenericValueBackgroundColor(Tag tag, Context context, String value) {
		tag.addStyle(context, "background-color", "Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(context.getGeneric().getMeta())) ? ((GenericStringDefaults) tag).getGenericStringProperty(context).getValue() : value);
	}

	default void removeGenericValueBackgroundColor(Tag tag, Context context) {
		tag.getDomNodeStyles(context).remove("background-color");
	}

	default void processFlexDirectionStyle(Tag tag, FlexDirection flexDirection) {
		tag.addPrefixBinding(context -> processFlexDirectionStyle(tag, context, flexDirection));
	}

	default void processFlexDirectionStyle(Tag tag, Context context, FlexDirection flexDirection) {
		if (FlexDirectionDefaults.class.isAssignableFrom(tag.getClass()))
			((FlexDirectionDefaults) tag).setDirection(context, flexDirection);
		else
			log.warn("Warning: FlexDirection is applicable only to classes implementing FlexDirectionDefaults.");
	}

	default void removeFlexDirectionStyle(Tag tag, Context context) {
		if (FlexDirectionDefaults.class.isAssignableFrom(tag.getClass()))
			((FlexDirectionDefaults) tag).setDirection(context, null);
		else
			log.warn("Warning: FlexDirection is applicable only to classes implementing FlexDirectionDefaults.");
	}

	default void processReverseFlexDirection(Tag tag) {
		tag.addPrefixBinding(context -> processReverseFlexDirection(tag, context));
	}

	default void processReverseFlexDirection(Tag tag, Context context) {
		if (FlexDirectionDefaults.class.isAssignableFrom(tag.getClass()))
			((FlexDirectionDefaults) tag).reverseDirection(context);
		else
			log.warn("Warning: ReverseFlexDirection is applicable only to classes implementing FlexDirectionDefaults.");
	}

	default void removeDirectionTracking(Tag tag, Context context) {
		((FlexDirectionDefaults) tag).stopTrackingDirection(context);
	}

	default void processKeepFlexDirection(Tag tag) {
		tag.addPrefixBinding(context -> processKeepFlexDirection(tag, context));
	}

	default void processKeepFlexDirection(Tag tag, Context context) {
		if (FlexDirectionDefaults.class.isAssignableFrom(tag.getClass()))
			((FlexDirectionDefaults) tag).keepDirection(context);
		else
			log.warn("Warning: KeepFlexDirection is applicable only to classes implementing FlexDirectionDefaults.");
	}

	default void processStyleClass(Tag tag, String[] classes) {
		tag.addPrefixBinding(context -> processStyleClass(tag, context, classes));
	}

	default void processStyleClass(Tag tag, Context context, String[] classes) {
		for (String sc : classes)
			tag.addStyleClass(context, sc);
	}

	default void removeStyleClass(Tag tag, Context context, String[] classes) {
		for (String styleClass : classes)
			tag.removeStyleClass(context, styleClass);
	}

	default void processSetText(Tag tag, Class<?>[] path, String[] texts) {
		tag.addPrefixBinding(context -> processSetText(tag, context, path, texts));
	}

	default void processSetText(Tag tag, Context context, Class<?>[] path, String[] texts) {
		if (texts.length == 1)
			tag.setText(context, texts[0]);
		else
			tag.setText(context, texts[AnnotationsManager.position(tag, path[path.length - 1])]);
	}

	default void removeSetText(Tag tag, Context context) {
		tag.setText(context, "");
	}

	default void processBindText(Tag tag, Class<? extends TextBinding> value) {
		tag.addPrefixBinding(context -> processBindText(tag, context, value));
	}

	default void processBindText(Tag tag, Context context, Class<? extends TextBinding> value) {
		if (GENERIC_STRING.class.equals(value))
			tag.bindText(context);
		else
			tag.bindText(context, context_ -> {
				try {
					return value.newInstance().apply(context_, tag);
				} catch (InstantiationException | IllegalAccessException e) {
					throw new IllegalStateException(e);
				}
			});
	}

	default void removeBindText(Tag tag, Context context) {
		tag.getDomNodeTextProperty(context).unbind();
		context.getPropertiesMaps(tag).remove(TextPropertyDefaults.TEXT_BINDING);
		tag.getDomNodeTextProperty(context).setValue(null);
	}

	default void processBindAction(Tag tag, Class<? extends ContextAction>[] value) {
		tag.addPrefixBinding(context -> processBindAction(tag, context, value));
	}

	default void processBindAction(Tag tag, Context context, Class<? extends ContextAction>[] value) {
		if (ActionDefaults.class.isAssignableFrom(tag.getClass()))
			((ActionDefaults) tag).bindAction(context, context_ -> {
				for (Class<? extends ContextAction> classValue : value)
					try {
						classValue.newInstance().accept(context_, tag);
					} catch (InstantiationException | IllegalAccessException e) {
						throw new IllegalStateException(e);
					}
			});
		else
			log.warn("BindAction is applicable only to tags implementing ActionDefaults.");
	}

	default void removeBindAction(Tag tag, Context context) {
		context.getPropertiesMaps(tag).remove(ActionDefaults.ACTION);
	}

	default void processSetStringExtractor(Tag tag, Class<? extends StringExtractor> value) {
		tag.addPrefixBinding(context -> processSetStringExtractor(tag, context, value));
	}

	default void processSetStringExtractor(Tag tag, Context context, Class<? extends StringExtractor> value) {
		try {
			tag.setStringExtractor(context, value.newInstance());
		} catch (InstantiationException | IllegalAccessException e) {
			throw new IllegalStateException(e);
		}
	}

	default void removeStringExtractor(Tag tag, Context context) {
		tag.getStringExtractorProperty(context).setValue(null);
	}

	default void processSelect(Tag tag, Class<?> value) {
		if (ObservableValueSelector.class.isAssignableFrom(value))
			try {
				tag.select((ObservableValueSelector) value.newInstance());
			} catch (InstantiationException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
		else
			throw new IllegalStateException("Select value must implement ObservableValueSelector. Given class: " + value.getName());
	}

	default void processSelectContext(Tag tag, Class<?> value) {
		if (ObservableContextSelector.class.isAssignableFrom(value))
			tag.select__(context -> {
				try {
					return ((ObservableContextSelector) value.newInstance()).apply(context, tag);
				} catch (InstantiationException | IllegalAccessException e) {
					throw new IllegalStateException(e);
				}
			});
		else
			throw new IllegalStateException("SelectContext value must implement ObservableContextSelector. Given class: " + value.getName());
	}

	default void processForEach(Tag tag, Class<?> value) {
		if (ObservableListExtractor.class.isAssignableFrom(value))
			try {
				if (!NO_FOR_EACH.class.equals(value))
					tag.forEach((ObservableListExtractor) value.newInstance());
			} catch (InstantiationException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
		else
			throw new IllegalStateException("ForEach value must implement ObservableListExtractor. Given class: " + value.getName());
	}

	default void processDirectSelect(Tag tag, Class<?>[] path, Class<?>[] selects) {
		if (selects.length == 1)
			tag.select(selects[0]);
		else
			tag.select(selects[AnnotationsManager.position(tag, path[path.length - 1])]);
	}

	default void removeMetaBinding(Tag tag) {
		((TagImpl) tag).setMetaBinding(null);
	}

	default void processSwitch(Tag tag, Class<? extends TagSwitcher>[] value) {
		try {
			for (Class<? extends TagSwitcher> switcher : value)
				tag.addSwitcher(switcher.newInstance());
		} catch (IllegalAccessException | InstantiationException e) {
			throw new IllegalStateException(e);
		}
	}

	default void removeSwitches(Tag tag, Class<? extends TagSwitcher>[] value) {
		for (Class<? extends TagSwitcher> switcherClass : value)
			tag.getObservableSwitchers().removeAll(tag.getObservableSwitchers().filtered(switcher -> switcherClass.equals(switcher.getClass())));
	}

	default void initDomNode(HtmlDomNode domNode) {
	}
}