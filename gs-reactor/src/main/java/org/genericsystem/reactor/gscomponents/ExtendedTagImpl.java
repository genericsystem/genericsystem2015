// package org.genericsystem.reactor.gscomponents;
//
// import java.util.List;
// import java.util.function.Consumer;
//
// import org.genericsystem.api.core.annotations.Components;
// import org.genericsystem.api.core.annotations.InstanceClass;
// import org.genericsystem.api.core.annotations.Meta;
// import org.genericsystem.api.core.annotations.SystemGeneric;
// import org.genericsystem.common.Generic;
// import org.genericsystem.common.Root;
// import org.genericsystem.defaults.tools.TransformationObservableList;
// import org.genericsystem.reactor.Context;
// import org.genericsystem.reactor.MetaBinding;
// import org.genericsystem.reactor.Tag;
// import org.genericsystem.reactor.gscomponents.ExtendedTagImpl.GTagType.GTagAttribute;
// import org.genericsystem.reactor.model.TagSwitcher;
//
// import javafx.collections.ObservableList;
//
// public class ExtendedTagImpl implements Tag {
//
// private GTag delegate;// TODO initialize
// private Tag parent;
//
// private ObservableList<? extends Tag> children;
//
// @SuppressWarnings("unchecked")
// @Override
// public <COMPONENT extends Tag> COMPONENT getParent() {
// return (COMPONENT) parent;
// }
//
// public void setParent(Tag parent) {
// this.parent = parent;
// this.children = new TransformationObservableList<GTag, ExtendedTagImpl>((ObservableList) delegate.getObservableInheritings(), (i, gtag) -> {
// ExtendedTagImpl result = new ExtendedTagImpl();
// result.setParent(ExtendedTagImpl.this);
// result.getRootTag().getAnnotationsManager().processAnnotations(result);
// result.init();
// return result;
// }, tag -> {
// // nothing to do here
// });
// getDelegateGeneric().getMeta().setInstance(getDelegateGeneric(), getTag());// Aie! , marchera pas, comment distinguer les signatures des generics ???
// }
//
// @Override
// public ObservableList<Tag> getObservableChildren() {
// return (ObservableList<Tag>) children;
// }
//
// @SystemGeneric
// @InstanceClass(GTag.class)
// public static interface GTagType extends Generic {
// @SystemGeneric
// @Components(GTagType.class)
// public static interface GTagAttribute extends Generic {
//
// }
//
// @SystemGeneric
// @Components(GTagType.class)
// public static interface GTagAnnotationType extends Generic {
//
// }
//
// @SystemGeneric
// @Components(GTagAnnotationType.class)
// public static interface AnnotationElement extends Generic {
//
// }
//
// @SystemGeneric
// @Components(AnnotationElement.class)
// public static interface ElementName extends Generic {
//
// }
//
// @SystemGeneric
// @Components(AnnotationElement.class)
// public static interface ElementValue extends Generic {
//
// }
// }
//
// @SystemGeneric
// @Meta(GTagType.class)
// public static interface GTag extends Generic {
//
// }
//
// public Root getRootGeneric() {
// return getDelegateGeneric().getRoot();
// }
//
// protected GTag getDelegateGeneric() {
// return delegate;
// }
//
// public GTagType getGenericTagType() {
// return getRootGeneric().find(GTagType.class);
// }
//
// @Override
// public String getTag() {
// return (String) getDelegateGeneric().getValue(getRootGeneric().find(GTagAttribute.class));
// }
//
// public void setTag(String tag) {
// getDelegateGeneric().setHolder(getRootGeneric().find(GTagAttribute.class), tag);
// }
//
// @Override
// public List<Consumer<Context>> getPreFixedBindings() {
// // TODO
// return null;
// }
//
// @Override
// public List<Consumer<Context>> getPostFixedBindings() {
// // TODO
// return null;
// }
//
// @Override
// public <BETWEEN> MetaBinding<BETWEEN> getMetaBinding() {
// // TODO
// return null;
// }
//
// @Override
// public <BETWEEN> void setMetaBinding(MetaBinding<BETWEEN> metaBinding) {
// // TODO
// // if (this.metaBinding != null)
// // throw new IllegalStateException("MetaBinding already defined");
// // this.metaBinding = metaBinding;
// }
//
// @Override
// public void addSwitcher(TagSwitcher switcher) {
// // TODO
// // this.switcher = switcher;
// }
//
// @Override
// public List<TagSwitcher> getSwitchers() {
// // TODO
// return null;
// }
// }
