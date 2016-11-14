package org.genericsystem.reactor.gscomponents;

import org.genericsystem.api.core.AxedPropertyClass;
import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.NoInheritance;
import org.genericsystem.api.core.annotations.constraints.SingularConstraint;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.defaults.tools.TransformationObservableList;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.TagNode;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagType.StyleName;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagType.StyleValue;

import com.google.common.base.Objects;

import javafx.collections.ObservableList;

public class ExtendedRootTag extends RootTagImpl {

	private final Root engine;

	public ExtendedRootTag(Root engine) {
		this.engine = engine;
		super.initRoot();
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
				result.init();
				return result;
			});
		}

		@Override
		public ObservableList<Tag> getObservableChildren() {
			return children;
		}

		public GTag getDelegateGeneric() {
			return delegate;
		}

		public GenericTagNode init() {
			children.init();
			return this;
		}
	}

	static long styleTime;
	static long stylesNumber;
	static int flush;

	@Override
	public void processStyle(Tag tag, String name, String value) {
		GTag delegate = ((GenericTagNode) ((TagImpl) tag).getTagNode()).getDelegateGeneric();
		long startTime = System.currentTimeMillis();

		Generic styleNameAttribute = delegate.getRoot().find(StyleName.class);
		Generic style = delegate.getComposites().filter(g -> styleNameAttribute.equals(g.getMeta())).filter(g -> Objects.equal(name, g.getValue())).first();
		if (style == null)
			style = delegate.addHolder(styleNameAttribute, name);

		Generic styleValueAttribute = delegate.getRoot().find(StyleValue.class);
		Generic styleValue = style.getComposites().filter(g -> styleValueAttribute.equals(g.getMeta())).first();

		// assert styleValue.getHolder(styleValueAttribute);
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
		// }
		// style.setHolder(delegate.getRoot().find(StyleValue.class), value);
		long endTime = System.currentTimeMillis();
		styleTime += (endTime - startTime);
		System.out.println("Style, temps passé : " + (endTime - startTime) + ", temps total : " + styleTime + ", nombre de styles : " + (++stylesNumber) + ", temps par style : " + (styleTime / stylesNumber));
		super.processStyle(tag, name, value);
	}

	@SystemGeneric
	@InstanceClass(GTag.class)
	// @InstanceValueClassConstraint(AxedPropertyClass.class)
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
	}

	@SystemGeneric
	@Meta(GTagType.class)
	public static interface GTag extends Generic {

	}
}
