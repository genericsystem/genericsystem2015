package org.genericsystem.reactor.gscomponents;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

import org.genericsystem.api.core.AxedPropertyClass;
import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.NoInheritance;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.api.core.annotations.constraints.SingularConstraint;
import org.genericsystem.api.core.annotations.value.AxedPropertyClassValue;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.defaults.tools.ObservableListWrapperExtended;
import org.genericsystem.defaults.tools.TransformationObservableList;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.TagNode;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagType.AnnotationParameter;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagType.AnnotationParameterValue;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagType.StyleName;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagType.StyleValue;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagType.TagAnnotation;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.tagadmin.TagAdmin;

import com.google.common.base.Objects;

import javafx.beans.binding.MapBinding;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;

public class ExtendedRootTag extends RootTagImpl {

	private final Root engine;

	public ExtendedRootTag(Root engine) {
		this.engine = engine;
		super.initRoot();
		processDelegateComposites(this);
	}

	@Override
	protected void initRoot() {
	}

	public Root getEngine() {
		return engine;
	}

	@Override
	public TagNode buildTagNode(Tag child) {
		return new GenericTagNode(child, (GTag) engine.find(GTagType.class).setInstance(new AxedPropertyClass(getClass(), 0))).init();
	}

	@Override
	public void processChildren(Tag tag, Class<? extends TagImpl>[] classes) {
		GTag delegate = ((GenericTagNode) ((TagImpl) tag).getTagNode()).getDelegateGeneric();
		for (int i = 0; i < classes.length; i++)
			delegate.getMeta().setInstance(delegate, new AxedPropertyClass(classes[i], i));
	}

	public class GenericTagNode implements TagNode {
		private TransformationObservableList<GTag, Tag> children;
		private final GTag delegate;

		public GenericTagNode(Tag tag, GTag delegate) {
			this.delegate = delegate;
			this.children = new TransformationObservableList<GTag, Tag>((ObservableList) delegate.getObservableInheritings(), (i, gtag) -> {
				AbstractTag result = createChild(tag, (Class<? extends TagImpl>) ((AxedPropertyClass) gtag.getValue()).getClazz());
				GenericTagNode tagNode = new GenericTagNode(result, gtag);
				result.setTagNode(tagNode);
				tagNode.init();
				processAnnotations(result);
				processDelegateComposites(result);
				result.init();
				return result;
			});
		}

		@Override
		public ObservableList<Tag> getObservableChildren() {
			return children;
		}

		@Override
		public ObservableMap<String, String> getObservableStyles() {
			return new MapBinding<String, String>() {
				private final ObservableList<Generic> stylesGenerics;
				private final Map<Generic, ObservableValue<Generic>[]> stylesValuesGenerics = new HashMap<>(); // Prevents garbage collection

				{
					this.stylesGenerics = new ObservableListWrapperExtended<Generic>(delegate.getObservableHolders(delegate.getRoot().find(StyleName.class)), style -> {
						ObservableValue<Generic>[] styleValue = new ObservableValue[] { style.getObservableHolder(delegate.getRoot().find(StyleValue.class)) };
						stylesValuesGenerics.put(style, styleValue);
						return styleValue;
					});
					stylesGenerics.addListener((ListChangeListener<? super Generic>) c -> {
						while (c.next()) {
							System.out.println("------- Change to stylesGenerics " + c + ", removed : " + c.getRemoved() + " added : " + c.getAddedSubList() + " on tag " + delegate);
						}
					});
					bind(stylesGenerics);
				}

				@Override
				protected ObservableMap<String, String> computeValue() {
					ObservableMap<String, String> result = FXCollections.observableHashMap();
					result.putAll(stylesGenerics.stream().filter(g -> stylesValuesGenerics.get(g)[0].getValue() != null).collect(Collectors.toMap(g -> (String) g.getValue(), g -> (String) stylesValuesGenerics.get(g)[0].getValue().getValue())));
					if (delegate.getValue().equals(new AxedPropertyClass(TagAdmin.class, 0)))
						System.out.println("computeValue, tag " + delegate + ", styles : " + result);
					return result;
				}
			};
		}

		public GTag getDelegateGeneric() {
			return delegate;
		}

		public GenericTagNode init() {
			children.init();
			return this;
		}
	}

	void processDelegateComposites(Tag tag) {
		GTag delegate = ((GenericTagNode) ((TagImpl) tag).getTagNode()).getDelegateGeneric();

		Generic gvbColor = delegate.getComposite(GenericValueBackgroundColor.class);
		if (gvbColor != null && gvbColor.isInstanceOf(delegate.getRoot().find(TagAnnotation.class)))
			ExtendedRootTag.super.processGenericValueBackgroundColor(tag, (String) gvbColor.getComposite("value").getHolder(delegate.getRoot().find(AnnotationParameterValue.class)).getValue());
	}

	static long styleTime;
	static long stylesNumber;
	static int flush;

	@Override
	public void processStyle(Tag tag, String name, String value) {
		GTag delegate = ((GenericTagNode) ((TagImpl) tag).getTagNode()).getDelegateGeneric();
		// long startTime = System.currentTimeMillis();

		Generic styleNameAttribute = delegate.getRoot().find(StyleName.class);
		Generic style = delegate.getComposites().filter(g -> styleNameAttribute.equals(g.getMeta())).filter(g -> Objects.equal(name, g.getValue())).first();
		if (style == null)
			style = delegate.addHolder(styleNameAttribute, name);

		Generic styleValueAttribute = delegate.getRoot().find(StyleValue.class);
		Generic styleValue = style.getComposites().filter(g -> styleValueAttribute.equals(g.getMeta())).first();

		if (styleValue != null) {
			if (!Objects.equal(value, styleValue.getValue()))
				styleValue = style.setHolder(styleValueAttribute, value);
			else
				assert styleValue == style.getHolder(styleValueAttribute);
		} else
			styleValue = style.addHolder(styleValueAttribute, value);
		// if (flush++ > 1) {
		styleValue.getCurrentCache().flush();
		flush = 0;

		// long endTime = System.currentTimeMillis();
		// styleTime += (endTime - startTime);
		// System.out.println("Style added, time spent: " + (endTime - startTime) + ", total time: " + styleTime + ", number of styles: " + (++stylesNumber) + ", time per style: " + (styleTime / stylesNumber));
	}

	@Override
	public void processGenericValueBackgroundColor(Tag tag, String value) {
		GTag delegate = ((GenericTagNode) ((TagImpl) tag).getTagNode()).getDelegateGeneric();
		Generic annotation = delegate.setHolder(delegate.getRoot().find(TagAnnotation.class), GenericValueBackgroundColor.class);
		Generic name = annotation.setHolder(delegate.getRoot().find(AnnotationParameter.class), "value");
		name.setHolder(delegate.getRoot().find(AnnotationParameterValue.class), value);
	}

	@SystemGeneric
	@InstanceClass(GTag.class)
	@Dependencies({ TagAnnotation.class, StyleName.class, StyleValue.class })
	@InstanceValueClassConstraint(AxedPropertyClass.class)
	public static interface GTagType extends Generic {

		@SystemGeneric
		@Components(GTagType.class)
		@InstanceValueClassConstraint(String.class)
		@NoInheritance
		public static interface StyleName extends Generic {

		}

		@SystemGeneric
		@Components(StyleName.class)
		@InstanceValueClassConstraint(String.class)
		@SingularConstraint
		@NoInheritance
		public static interface StyleValue extends Generic {

		}

		@SystemGeneric
		@Components(GTagType.class)
		@InstanceValueClassConstraint(Class.class)
		@Dependencies({ AnnotationParameter.class, AnnotationParameterValue.class })
		@NoInheritance
		public static interface TagAnnotation extends Generic {

		}

		@SystemGeneric
		@Components(TagAnnotation.class)
		@InstanceValueClassConstraint(String.class)
		public static interface AnnotationParameter extends Generic {

		}

		@SystemGeneric
		@Components(AnnotationParameter.class)
		@PropertyConstraint
		public static interface AnnotationParameterValue extends Generic {

		}
	}

	@SystemGeneric
	@Meta(GTagType.class)
	@AxedPropertyClassValue(propertyClass = HtmlDiv.class, pos = 0)
	public static interface GTag extends Generic {

	}
}
