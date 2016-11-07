package org.genericsystem.reactor.gscomponents;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.TransformationObservableList;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.TagNode;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagType.GTagName;

import javafx.collections.ObservableList;

public class ExtendedRootTag extends RootTagImpl {

	public class GenericTagNode implements TagNode {
		private ObservableList<Tag> children;
		private GTag delegate;
		private Tag parent;

		public GenericTagNode(Tag parent) {
			this.parent = parent;
			this.children = new TransformationObservableList<GTag, Tag>((ObservableList) delegate.getObservableInheritings(), (i, gtag) -> {
				TagImpl result = null;
				try {
					result = ((Class<? extends TagImpl>) gtag.getValue()).newInstance();
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
					Generic genericChild = delegate.getMeta().setInstance(delegate, child.getClass());// Aie! , marchera pas, comment distinguer les signatures des generics ???
					genericChild.setHolder(delegate.getRoot().find(GTagName.class), child.getTag());
					return true;
				};
			};
		}

		Class<? extends TagImpl> getClazz() {
			return (Class<? extends TagImpl>) delegate.getValue();
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

		public void setDelegateGeneric(GTag delegate) {
			this.delegate = delegate;
		}

		public GTag getDelegateGeneric() {
			return delegate;
		}
	}

	@Override
	public TagNode buildTagNode(Tag parent) {
		return new GenericTagNode(parent);
	}

	@SystemGeneric
	@InstanceClass(GTag.class)
	@InstanceValueClassConstraint(Class.class)
	public static interface GTagType extends Generic {

		@SystemGeneric
		@Components(GTagType.class)
		@InstanceValueClassConstraint(String.class)
		public static interface GTagName extends Generic {

		}

		@SystemGeneric
		@Components(GTagType.class)
		@InstanceValueClassConstraint(String.class)
		public static interface GTagStyleType extends Generic {

		}

		@SystemGeneric
		@Components(GTagStyleType.class)
		@InstanceValueClassConstraint(String.class)
		public static interface StyleValue extends Generic {

		}
	}

	@SystemGeneric
	@Meta(GTagType.class)
	public static interface GTag extends Generic {

	}
}
