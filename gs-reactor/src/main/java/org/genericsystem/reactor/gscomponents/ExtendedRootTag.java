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
		return new GenericTagNode(child);
	}

	static long time = 0;

	public static class GenericTagNode implements TagNode {
		private ObservableList<Tag> children;
		private GTag delegate;

		public GenericTagNode(Tag tag) {
			delegate = (GTag) ((ExtendedRootTag) tag.getRootTag()).getEngine().find(GTagType.class).setInstance(new AxedPropertyClass(tag.getClass(), 0));
			init(tag);
		}

		public GenericTagNode(Tag parent, Tag tag, GTag delegate) {
			this.delegate = delegate;
			init(tag);
		}

		private void init(Tag tag) {
			this.children = new TransformationObservableList<GTag, Tag>((ObservableList) delegate.getObservableInheritings(), (i, gtag) -> {
				TagImpl result = null;
				try {
					result = (TagImpl) ((AxedPropertyClass) gtag.getValue()).getClazz().newInstance();
				} catch (IllegalAccessException | InstantiationException e) {
					throw new IllegalStateException(e);
				}
				System.out.println("Tag created: " + result + ", child of " + tag);
				result.setParent(tag);
				result.setTagNode(new GenericTagNode(tag, result, gtag));
				result.getRootTag().getAnnotationsManager().processAnnotations(result);
				result.init();
				return result;
			}, tag_ -> {
				// nothing to do here
			});
			System.out.println("=============== End of TagNode construction " + tag);
		}

		@Override
		public ObservableList<Tag> getObservableChildren() {
			return children;
		}

		public GTag getDelegateGeneric() {
			return delegate;
		}
	}

	@SystemGeneric
	@InstanceClass(GTag.class)
	//	@InstanceValueClassConstraint(AxedPropertyClass.class)
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
