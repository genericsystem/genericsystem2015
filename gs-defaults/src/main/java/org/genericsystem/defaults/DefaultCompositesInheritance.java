package org.genericsystem.defaults;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.function.Predicate;
import java.util.stream.Stream;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.WeakInvalidationListener;
import javafx.beans.binding.ListBinding;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.IVertex;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.defaults.DefaultConfig.NonHeritableProperty;
import org.genericsystem.defaults.async.AsyncInheritanceComputer;

public interface DefaultCompositesInheritance<T extends DefaultVertex<T>> extends IVertex<T> {

	// Remove
	CompletableFuture<Snapshot<T>> getAsyncComposites();

	CompletableFuture<T> getAsyncKey(Class<? extends SystemProperty> propertyClass, int pos);

	ObservableList<T> getObservableComposites();

	//

	@SuppressWarnings("unchecked")
	@Override
	default T getAttribute(Serializable value, T... targets) {
		return getNonAmbiguousResult(getAttributes(value, targets).stream());
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<T> getAsyncAttribute(Serializable value, T... targets) {
		return getAsyncNonAmbiguousResult(getAsyncAttributes(value, targets).thenApply(s -> s.stream()));
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getAttribute(T... targets) {
		return getNonAmbiguousResult(getAttributes(targets).stream());
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<T> getAsyncAttribute(T... targets) {
		return getAsyncNonAmbiguousResult(getAsyncAttributes(targets).thenApply(s -> s.stream()));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getAttributes(Serializable value, T... targets) {
		return getAttributes(targets).filter(DefaultDependencies.valueFilter(value));
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<Snapshot<T>> getAsyncAttributes(Serializable value, T... targets) {
		return getAsyncAttributes(targets).thenApply(attributes -> attributes.filter(DefaultDependencies.valueFilter(value)));
	}

	@Override
	default Snapshot<T> getAttributes() {
		return getAttributes(getRoot().getMetaAttribute());
	}

	default CompletableFuture<Snapshot<T>> getAsyncAttributes() {
		return getAsyncAttributes(getRoot().getMetaAttribute());
	}

	default ObservableList<T> getObservableAttributes() {
		return getObservableAttributes(getRoot().getMetaAttribute());
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getAttributes(T... targets) {
		return getAttributes(getRoot().getMetaAttribute()).filter(componentsFilter(addThisToTargets(targets)));
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<Snapshot<T>> getAsyncAttributes(T... targets) {
		return getAsyncAttributes().thenApply(attributes -> attributes.filter(componentsFilter(addThisToTargets(targets))));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getAttributes(int pos) {
		return () -> getAttributes().stream().filter(attribute -> attribute.getComponent(pos) != null && ((T) this).isSpecializationOf(attribute.getComponent(pos)));
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<Snapshot<T>> getAsyncAttributes(int pos) {
		return getAsyncAttributes().thenApply(attributes -> attributes.filter(attribute -> attribute.getComponent(pos) != null && ((T) this).isSpecializationOf(attribute.getComponent(pos))));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getAttributes(T attribute) {
		T nonHeritableProperty = getKey(NonHeritableProperty.class, ApiStatics.NO_POSITION);
		if (nonHeritableProperty == null || attribute.inheritsFrom(nonHeritableProperty) || attribute.isInheritanceEnabled())
			return () -> new InheritanceComputer<>((T) DefaultCompositesInheritance.this, attribute, ApiStatics.STRUCTURAL).inheritanceStream();
		return () -> this.getComposites().stream().filter(holder -> holder.isSpecializationOf(attribute) && holder.getLevel() == ApiStatics.STRUCTURAL);
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<Snapshot<T>> getAsyncAttributes(T attribute) {
		CompletableFuture<T> nonHeritablePropertyPromise = getAsyncKey(NonHeritableProperty.class, ApiStatics.NO_POSITION);
		return nonHeritablePropertyPromise.thenCompose(nonHeritableProperty -> {
			if (nonHeritableProperty == null || attribute.inheritsFrom(nonHeritableProperty) || attribute.isInheritanceEnabled())
				return new AsyncInheritanceComputer<>((T) DefaultCompositesInheritance.this, attribute, ApiStatics.STRUCTURAL).inheritanceStreamAsync();
			return getAsyncComposites().thenApply(composites -> composites.stream().filter(holder -> holder.isSpecializationOf(attribute) && holder.getLevel() == ApiStatics.STRUCTURAL)).thenApply(s -> () -> s);
		});
	}

	default ObservableList<T> getObservableAttributes(T attribute) {
		return new ListBinding<T>() {
			@SuppressWarnings("unused")
			private final InvalidationListener listener;
			private final Observable invalidator = getObservableComposites();// TODO temporary
			private List<T> promisedList = new ArrayList<T>();

			{
				invalidator.addListener(new WeakInvalidationListener(listener = (o) -> {
					getAsyncAttributes(attribute).thenAccept(snapshot -> {
						List<T> newPromisedList = snapshot.toList();
						if (!promisedList.equals(newPromisedList)) {
							promisedList = newPromisedList;
							invalidate();
						}
					});
				}));
			}

			@Override
			protected ObservableList<T> computeValue() {
				return FXCollections.unmodifiableObservableList(FXCollections.observableList(promisedList));
			}
		};
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getHolder(T attribute, Serializable value, T... targets) {
		return getNonAmbiguousResult(getHolders(attribute, value, targets).stream());
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<T> getAsyncHolder(T attribute, Serializable value, T... targets) {
		return getAsyncNonAmbiguousResult(getAsyncHolders(attribute, value, targets).thenApply(s -> s.stream()));
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getHolder(T attribute, T... targets) {
		return getNonAmbiguousResult(getHolders(attribute, targets).stream());
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<T> getAsyncHolder(T attribute, T... targets) {
		return getAsyncNonAmbiguousResult(getAsyncHolders(attribute, targets).thenApply(s -> s.stream()));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getHolders(T attribute, Serializable value, T... targets) {
		return getHolders(attribute).filter(DefaultDependencies.valueFilter(value)).filter(componentsFilter(addThisToTargets(targets)));
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<Snapshot<T>> getAsyncHolders(T attribute, Serializable value, T... targets) {
		return getAsyncHolders(attribute).thenApply(holders -> holders.filter(DefaultDependencies.valueFilter(value)).filter(componentsFilter(addThisToTargets(targets))));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getHolders(T attribute, T... targets) {
		return getHolders(attribute).filter(componentsFilter(addThisToTargets(targets)));
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<Snapshot<T>> getAsyncHolders(T attribute, T... targets) {
		return getAsyncHolders(attribute).thenApply(holders -> holders.filter(componentsFilter(addThisToTargets(targets))));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getHolders(T attribute, int pos) {
		return () -> getHolders(attribute).stream().filter(holder -> holder.getComponent(pos) != null && ((T) this).isSpecializationOf(holder.getComponent(pos)));
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<Snapshot<T>> getAsyncHolders(T attribute, int pos) {
		return getAsyncHolders(attribute).thenApply(holders -> () -> holders.stream().filter(holder -> holder.getComponent(pos) != null && ((T) this).isSpecializationOf(holder.getComponent(pos))));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getHolders(T attribute) {
		T nonHeritableProperty = getKey(NonHeritableProperty.class, ApiStatics.NO_POSITION);
		if (nonHeritableProperty == null || attribute.inheritsFrom(nonHeritableProperty) || attribute.isInheritanceEnabled())
			return () -> new InheritanceComputer<>((T) DefaultCompositesInheritance.this, attribute, ApiStatics.CONCRETE).inheritanceStream();
		return () -> this.getComposites().stream().filter(holder -> holder.isSpecializationOf(attribute) && holder.getLevel() == ApiStatics.CONCRETE);
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<Snapshot<T>> getAsyncHolders(T attribute) {
		return getAsyncKey(NonHeritableProperty.class, ApiStatics.NO_POSITION).thenCompose(nonHeritableProperty -> {
			if (nonHeritableProperty == null || attribute.inheritsFrom(nonHeritableProperty) || attribute.isInheritanceEnabled())
				return new AsyncInheritanceComputer<>((T) DefaultCompositesInheritance.this, attribute, ApiStatics.CONCRETE).inheritanceStreamAsync();
			return this.getAsyncComposites().thenApply(composites -> () -> composites.stream().filter(holder -> holder.isSpecializationOf(attribute) && holder.getLevel() == ApiStatics.CONCRETE));
		});
	}

	default ObservableList<T> getObservableHolders(T attribute) {
		return new ListBinding<T>() {
			@SuppressWarnings("unused")
			private final InvalidationListener listener;
			private final Observable invalidator = getObservableComposites();// TODO temporary
			private List<T> promisedList = new ArrayList<T>();

			{
				invalidator.addListener(new WeakInvalidationListener(listener = (o) -> {
					getAsyncHolders(attribute).thenAccept(snapshot -> {
						List<T> newPromisedList = snapshot.toList();
						if (!promisedList.equals(newPromisedList)) {
							promisedList = newPromisedList;
							invalidate();
						}
					});
				}));
			}

			@Override
			protected ObservableList<T> computeValue() {
				return FXCollections.unmodifiableObservableList(FXCollections.observableList(promisedList));
			}
		};
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getRelation(Serializable value, T... targets) {
		return getNonAmbiguousResult(getRelations(value, targets).stream());
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<T> getAsyncRelation(Serializable value, T... targets) {
		return getAsyncNonAmbiguousResult(getAsyncRelations(value, targets).thenApply(s -> s.stream()));
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getRelation(T... targets) {
		return getNonAmbiguousResult(getRelations(targets).stream());
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<T> getAsyncRelation(T... targets) {
		return getAsyncNonAmbiguousResult(getAsyncRelations(targets).thenApply(s -> s.stream()));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getRelations(Serializable value, T... targets) {
		return getRelations(targets).filter(DefaultDependencies.valueFilter(value));
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<Snapshot<T>> getAsyncRelations(Serializable value, T... targets) {
		return getAsyncRelations(targets).thenApply(relations -> relations.filter(DefaultDependencies.valueFilter(value)));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getRelations(T... targets) {
		return getRelations(getRoot().getMetaRelation()).filter(componentsFilter(addThisToTargets(targets)));
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<Snapshot<T>> getAsyncRelations(T... targets) {
		return getAsyncRelations(getRoot().getMetaRelation()).thenApply(relations -> relations.filter(componentsFilter(addThisToTargets(targets))));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getRelations(int pos) {
		return () -> getRelations().stream().filter(relation -> relation.getComponent(pos) != null && ((T) this).isSpecializationOf(relation.getComponent(pos)));
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<Snapshot<T>> getAsyncRelations(int pos) {
		return getAsyncRelations().thenApply(relations -> () -> relations.stream().filter(relation -> relation.getComponent(pos) != null && ((T) this).isSpecializationOf(relation.getComponent(pos))));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getRelations(T relation) {
		return ((T) this).getAttributes(relation);
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<Snapshot<T>> getAsyncRelations(T relation) {
		return ((T) this).getAsyncAttributes(relation);
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getLink(T relation, Serializable value, T... targets) {
		return getNonAmbiguousResult(getLinks(relation, value, targets).stream());
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<T> getAsyncLink(T relation, Serializable value, T... targets) {
		return getAsyncNonAmbiguousResult(getAsyncLinks(relation, value, targets).thenApply(s -> s.stream()));
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getLink(T relation, T... targets) {
		return getNonAmbiguousResult(getLinks(relation, targets).stream());
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<T> getAsyncLink(T relation, T... targets) {
		return getAsyncNonAmbiguousResult(getAsyncLinks(relation, targets).thenApply(s -> s.stream()));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getLinks(T relation, Serializable value, T... targets) {
		return getLinks(relation).filter(DefaultDependencies.valueFilter(value)).filter(componentsFilter(addThisToTargets(targets)));
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<Snapshot<T>> getAsyncLinks(T relation, Serializable value, T... targets) {
		return getAsyncLinks(relation).thenApply(links -> links.filter(DefaultDependencies.valueFilter(value)).filter(componentsFilter(addThisToTargets(targets))));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getLinks(T relation, T... targets) {
		return getLinks(relation).filter(componentsFilter(addThisToTargets(targets)));
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<Snapshot<T>> getAsyncLinks(T relation, T... targets) {
		return getAsyncLinks(relation).thenApply(links -> links.filter(componentsFilter(addThisToTargets(targets))));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getLinks(T relation, int pos) {
		return () -> getLinks(relation).stream().filter(link -> link.getComponent(pos) != null && ((T) this).isSpecializationOf(link.getComponent(pos)));
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<Snapshot<T>> getAsyncLinks(T relation, int pos) {
		return getAsyncLinks(relation).thenApply(links -> () -> links.stream().filter(link -> link.getComponent(pos) != null && ((T) this).isSpecializationOf(link.getComponent(pos))));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getLinks(T relation) {
		return ((T) this).getHolders(relation);
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<Snapshot<T>> getAsyncLinks(T relation) {
		return ((T) this).getAsyncHolders(relation);
	}

	@SuppressWarnings("unchecked")
	@Override
	default Serializable getValue(T attribute, Serializable value, T... targets) {
		T holder = getHolder(attribute, value, targets);
		return holder != null ? holder.getValue() : null;
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<Serializable> getAsyncValue(T attribute, Serializable value, T... targets) {
		return getAsyncHolder(attribute, value, targets).thenApply(holder -> holder != null ? holder.getValue() : null);
	}

	@SuppressWarnings("unchecked")
	@Override
	default Serializable getValue(T attribute, T... targets) {
		T holder = getHolder(attribute, targets);
		return holder != null ? holder.getValue() : null;
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<Serializable> getAsyncValue(T attribute, T... targets) {
		return getAsyncHolder(attribute, targets).thenApply(holder -> holder != null ? holder.getValue() : null);
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<Serializable> getValues(T attribute, Serializable value, T... targets) {
		return () -> getLinks(attribute, value, targets).stream().map(x -> x.getValue());
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<Snapshot<Serializable>> getAsyncValues(T attribute, Serializable value, T... targets) {
		return getAsyncLinks(attribute, value, targets).thenApply(links -> () -> links.stream().map(x -> x.getValue()));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<Serializable> getValues(T attribute, T... targets) {
		return () -> getLinks(attribute, targets).stream().map(x -> x.getValue());
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<Snapshot<Serializable>> getAsyncValues(T attribute, T... targets) {
		return getAsyncLinks(attribute, targets).thenApply(links -> () -> links.stream().map(x -> x.getValue()));
	}

	@Override
	default Snapshot<Serializable> getValues(T attribute, int pos) {
		return () -> getHolders(attribute, pos).stream().map(x -> x.getValue());
	}

	default CompletableFuture<Snapshot<Serializable>> getAsyncValues(T attribute, int pos) {
		return getAsyncHolders(attribute, pos).thenApply(holders -> () -> holders.stream().map(x -> x.getValue()));
	}

	@SuppressWarnings("unchecked")
	static <T extends DefaultVertex<T>> Predicate<T> componentsFilter(T... componentsReached) {
		return attribute -> {
			List<T> attributeComps = new ArrayList<>(attribute.getComponents());
			for (T componentReach : componentsReached) {
				T matchedComponent = attributeComps.stream().filter(attributeComp -> componentsReached[0].isSpecializationOf(attributeComp) ? true : componentReach.equals(attributeComp)).findFirst().orElse(null);
				if (matchedComponent != null)
					attributeComps.remove(matchedComponent);
				else
					return false;
			}
			return true;
		};
	}

	T getNonAmbiguousResult(Stream<T> stream);

	CompletableFuture<T> getAsyncNonAmbiguousResult(CompletableFuture<Stream<T>> streamPromise);

	@SuppressWarnings("unchecked")
	default T getLinkTargetComponent(T relation, T... targets) {
		T link = getLink(relation, targets);
		return link != null ? link.getTargetComponent() : null;
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<T> getAsyncLinkTargetComponent(T relation, T... targets) {
		return getAsyncLink(relation, targets).thenApply(link -> link != null ? link.getTargetComponent() : null);
	}

	@SuppressWarnings("unchecked")
	default T getLinkTargetComponent(T relation, Serializable value, T... targets) {
		T link = getLink(relation, value, targets);
		return link != null ? link.getTargetComponent() : null;
	}

	@SuppressWarnings("unchecked")
	default CompletableFuture<T> getAsyncLinkTargetComponent(T relation, Serializable value, T... targets) {
		return getAsyncLink(relation, value, targets).thenApply(link -> link != null ? link.getTargetComponent() : null);
	}
}
