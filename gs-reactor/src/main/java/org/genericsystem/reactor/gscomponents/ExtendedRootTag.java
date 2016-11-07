package org.genericsystem.reactor.gscomponents;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.TransformationObservableList;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.TagNode;

import javafx.collections.ObservableList;

public class ExtendedRootTag extends RootTagImpl {

	@Override
	public TagNode buildTagNode(Tag parent) {
		return new TagNode() {

			private ObservableList<Tag> children;
			private GTag delegate;
			{
				this.children = new TransformationObservableList<GTag, Tag>((ObservableList) delegate.getObservableInheritings(), (i, gtag) -> {
					TagImpl result = null;
					try {
						result = getClazz().newInstance();
					} catch (IllegalAccessException | InstantiationException e) {
						throw new IllegalStateException(e);
					}
					result.setParent(ExtendedRootTag.this);
					result.getRootTag().getAnnotationsManager().processAnnotations(result);
					result.init();
					return result;
				}, tag -> {
					// nothing to do here
				}) {
					@Override
					public boolean add(Tag child) {
						delegate.getMeta().setInstance(delegate, getTag());// Aie! , marchera pas, comment distinguer les signatures des generics ???
						return true;
					};
				};
			}

			Class<? extends TagImpl> getClazz() {
				return null;
			}

			@SuppressWarnings("unchecked")
			@Override
			public <COMPONENT extends Tag> COMPONENT getParent() {
				return (COMPONENT) parent;
			}

			@Override
			public ObservableList<Tag> getObservableChildren() {
				return children;
			}

		};

	};

	@SystemGeneric
	@InstanceClass(GTag.class)
	public static interface GTagType extends Generic {

		@SystemGeneric
		@Components(GTagType.class)
		public static interface GTagAnnotationType extends Generic {

		}

		@SystemGeneric
		@Components(GTagAnnotationType.class)
		public static interface AnnotationElement extends Generic {

		}

		@SystemGeneric
		@Components(AnnotationElement.class)
		public static interface ElementName extends Generic {

		}

		@SystemGeneric
		@Components(AnnotationElement.class)
		public static interface ElementValue extends Generic {

		}
	}

	@SystemGeneric
	@Meta(GTagType.class)
	public static interface GTag extends Generic {

	}
}
