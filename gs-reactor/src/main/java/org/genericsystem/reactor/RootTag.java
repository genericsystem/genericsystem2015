package org.genericsystem.reactor;

import java.lang.reflect.InvocationTargetException;
import java.util.function.Function;

import org.genericsystem.defaults.tools.BindingsTools;
import org.genericsystem.reactor.HtmlDomNode.HtmlDomNodeAction;
import org.genericsystem.reactor.HtmlDomNode.HtmlDomNodeCheckbox;
import org.genericsystem.reactor.HtmlDomNode.HtmlDomNodeInputText;
import org.genericsystem.reactor.HtmlDomNode.HtmlDomNodeSelect;
import org.genericsystem.reactor.HtmlDomNode.Sender;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.context.ObservableContextSelector;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.context.ObservableListExtractor.NO_FOR_EACH;
import org.genericsystem.reactor.context.ObservableListExtractorFromContext;
import org.genericsystem.reactor.context.ObservableValueSelector;
import org.genericsystem.reactor.context.StringExtractor;
import org.genericsystem.reactor.context.TagSwitcher;
import org.genericsystem.reactor.context.TextBinding;
import org.genericsystem.reactor.context.TextBinding.GENERIC_STRING;
import org.genericsystem.reactor.contextproperties.ActionDefaults;
import org.genericsystem.reactor.contextproperties.FlexDirectionDefaults;
import org.genericsystem.reactor.contextproperties.GenericStringDefaults;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.gscomponents.Controller;
import org.genericsystem.reactor.gscomponents.Controller.MainSwitcher;
import org.genericsystem.reactor.gscomponents.Controller.StepsStep;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.TagImpl;

import javafx.beans.binding.Bindings;
import javafx.beans.binding.ListBinding;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

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
		for (Class<? extends TagImpl> clazz : classes)
			tag.createChild(clazz);
	}

	default void processStyle(Tag tag, String name, String value) {
		tag.addPrefixBinding(context -> processStyle(tag, context, name, value));
	}

	default void processStyle(Tag tag, Context context, String name, String value) {
		tag.addStyle(context, name, value);
	}

	default void processAttribute(Tag tag, String name, String value) {
		tag.addPrefixBinding(context -> processAttribute(tag, context, name, value));
	}

	default void processAttribute(Tag tag, Context context, String name, String value) {
		tag.addAttribute(context, name, value);
	}

	default void processGenericValueBackgroundColor(Tag tag, String value) {
		tag.addPrefixBinding(context -> processGenericValueBackgroundColor(tag, context, value));
	}

	default void processGenericValueBackgroundColor(Tag tag, Context context, String value) {
		tag.addStyle(context, "background-color", "Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(context.getGeneric().getMeta())) ? ((GenericStringDefaults) tag).getGenericStringProperty(context).getValue() : value);
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

	default void processReverseFlexDirection(Tag tag) {
		tag.addPrefixBinding(context -> processReverseFlexDirection(tag, context));
	}

	default void processReverseFlexDirection(Tag tag, Context context) {
		if (FlexDirectionDefaults.class.isAssignableFrom(tag.getClass()))
			((FlexDirectionDefaults) tag).reverseDirection(context);
		else
			log.warn("Warning: ReverseFlexDirection is applicable only to classes implementing FlexDirectionDefaults.");
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

	default void processSetText(Tag tag, Class<?>[] path, String[] texts) {
		tag.addPrefixBinding(context -> processSetText(tag, context, path, texts));
	}

	default void processSetText(Tag tag, Context context, Class<?>[] path, String[] texts) {
		if (texts.length == 1)
			tag.setText(context, texts[0]);
		else
			tag.setText(context, texts[AnnotationsManager.position(tag, path[path.length - 1])]);
	}

	default void processBindText(Tag tag, Class<? extends TextBinding> value) {
		tag.addPrefixBinding(context -> processBindText(tag, context, value));
	}

	default void processBindText(Tag tag, Context context, Class<? extends TextBinding> value) {
		if (GENERIC_STRING.class.equals(value))
			tag.bindText(context);
		else
			try {
				tag.bindText(context, value.newInstance().apply(context, tag));
			} catch (InstantiationException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
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

	default void processForEachContext(Tag tag, Class<?> value) {
		if (ObservableListExtractorFromContext.class.isAssignableFrom(value))
			tag.forEach2(context -> {
				try {
					return ((ObservableListExtractorFromContext) value.getDeclaredConstructor().newInstance()).apply(context, tag);
				} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
					throw new IllegalStateException(e);
				}
			});
		else
			throw new IllegalStateException("ForEach value must implement ObservableListExtractorFromContext. Given class: " + value.getName());
	}

	default void processDirectSelect(Tag tag, Class<?>[] path, Class<?>[] selects) {
		if (selects.length == 1)
			tag.select(selects[0]);
		else
			tag.select(selects[AnnotationsManager.position(tag, path[path.length - 1])]);
	}

	default void processSwitch(Tag tag, Class<? extends TagSwitcher>[] value) {
		try {
			for (Class<? extends TagSwitcher> switcher : value)
				tag.addSwitcher(switcher.newInstance());
		} catch (IllegalAccessException | InstantiationException e) {
			throw new IllegalStateException(e);
		}
	}

	default void processBindSelection(Tag tag, Context context, Class<? extends TagImpl> value, int valuePos) {
		if (SelectionDefaults.class.isAssignableFrom(tag.getClass()))
			((SelectionDefaults) tag).bindSelection(tag.find(value, valuePos), context);
		else
			log.warn("BindSelection is applicable only to a class implementing SelectionDefaults.");
	}

	default void processBindSelection(Tag tag, Class<? extends TagImpl> value, int valuePos) {
		tag.addPostfixBinding(context -> processBindSelection(tag, context, value, valuePos));
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	default void processStep(Tag tag, Class<? extends TagImpl> next, String prevText, String nextText) {
		if (tag.getMetaBinding() == null) {
			tag.addPrefixBinding(context -> {
				Controller controller = Controller.get(tag, context);
				controller.addStep(tag, new SimpleIntegerProperty(1), next, prevText, nextText);
			});
		} else {
			Function<Context, ObservableList<Object>> contextOl = tag.getMetaBinding().getBetweenChildren();
			tag.getMetaBinding().setBetweenChildren(context -> {
				ObservableList ol = contextOl.apply(context);
				Controller controller = Controller.get(tag, context);
				StepsStep subSteps = controller.getStep(tag);
				if (subSteps == null)
					subSteps = controller.addStep(tag, Bindings.size(ol), next, prevText, nextText);
				Property<Boolean> activeProperty = controller.getActiveProperty();
				SimpleIntegerProperty indexProperty = subSteps.getIndexProperty();
				return BindingsTools.transmitSuccessiveInvalidations(new ListBinding() {
					{
						bind(indexProperty, activeProperty);
					}

					@Override
					protected ObservableList computeValue() {
						if (!activeProperty.getValue())
							return ol;
						return (indexProperty.get() >= 0) && (indexProperty.get() < ol.size()) ? FXCollections.singletonObservableList(ol.get(indexProperty.get())) : FXCollections.emptyObservableList();
					}
				});
			});
		}
		tag.getRootTag().processSwitch(tag, new Class[] { MainSwitcher.class });
	}

	default void processTagName(Tag tag, String tagName, String type) {
		tag.setTag(tagName);
		if ("input".equals(tagName))
			tag.addAttribute("type", type);

		switch (tagName.toLowerCase()) {
			case "input" :
				switch (type.toLowerCase()) {
					case "checkbox" :
					case "radio" :
						tag.setDomNodeClass(HtmlDomNodeCheckbox.class);
						break;
					default :
						tag.setDomNodeClass(HtmlDomNodeInputText.class);
				}
				break;
			case "datalist" :
			case "select" :
				tag.setDomNodeClass(HtmlDomNodeSelect.class);
				break;
			case "button" :
			case "a" :
				tag.setDomNodeClass(HtmlDomNodeAction.class);
				break;
		}
	}

	default void initDomNode(HtmlDomNode domNode) {
	}
}