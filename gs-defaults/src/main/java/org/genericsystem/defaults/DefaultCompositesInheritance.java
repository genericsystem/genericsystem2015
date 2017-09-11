package org.genericsystem.defaults;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.FiltersBuilder;
import org.genericsystem.api.core.IGeneric;
import org.genericsystem.api.core.IndexFilter;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.defaults.DefaultConfig.NonHeritableProperty;
import org.genericsystem.defaults.tools.InheritanceComputer;

import io.reactivex.Observable;
import javafx.beans.binding.Bindings;
import javafx.beans.value.ObservableValue;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 */
public interface DefaultCompositesInheritance<T extends DefaultGeneric<T>> extends IGeneric<T> {

	@SuppressWarnings("unchecked")
	@Override
	default T getAttribute(Serializable value, T... targets) {
		return getNonAmbiguousResult(getAttributes(value, targets).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableAttribute(Serializable value, T... targets) {
		return Bindings.valueAt(getAttributes(value, targets).toObservableList(), 0);
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getAttribute(T... targets) {
		return getNonAmbiguousResult(getAttributes(targets).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableAttribute(T... targets) {
		return Bindings.valueAt(getAttributes(targets).toObservableList(), 0);
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getAttributes(Serializable value, T... targets) {
		return getAttributes(targets).filter(new IndexFilter(FiltersBuilder.HAS_VALUE, value));
	}

	@Override
	default Snapshot<T> getAttributes() {
		return getAttributes(getRoot().getMetaAttribute());
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getAttributes(T... targets) {
		return getAttributes(getRoot().getMetaAttribute()).filter(new IndexFilter(FiltersBuilder.COMPOSITE_HAS_COMPONENTS, (Object[]) addThisToTargets(targets)));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getAttributes(int pos) {
		return getAttributes().filter(new IndexFilter(FiltersBuilder.HAS_COMPONENT_AT_POS, this, pos));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getAttributes(T attribute) {
		T nonHeritableProperty = getKey(NonHeritableProperty.class, ApiStatics.NO_POSITION);
		if (nonHeritableProperty == null || attribute.inheritsFrom(nonHeritableProperty) || attribute.isInheritanceEnabled())
			return new Snapshot<T>() {

			InheritanceComputer<T> inheritanceComputer = new InheritanceComputer<>((T) DefaultCompositesInheritance.this, attribute, ApiStatics.STRUCTURAL);

			@Override
			public Stream<T> unfilteredStream() {
				return inheritanceComputer.inheritanceStream();
			}

			@Override
			public Observable<T> getAddsObservable() {
				return inheritanceComputer.getAddsObservable();
			}

			@Override
			public Observable<T> getRemovesObservable() {
				return inheritanceComputer.getRemovesObservable();
			}
		};
		return getComposites().filter(new IndexFilter(FiltersBuilder.IS_SPECIALIZATION_OF, attribute)).filter(new IndexFilter(FiltersBuilder.HAS_LEVEL, ApiStatics.STRUCTURAL));
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getHolder(T attribute, Serializable value, T... targets) {
		return getNonAmbiguousResult(getHolders(attribute, value, targets).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableHolder(T attribute, Serializable value, T... targets) {
		return Bindings.valueAt(getHolders(attribute, value, targets).toObservableList(), 0);
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getHolder(T attribute, T... targets) {
		return getNonAmbiguousResult(getHolders(attribute, targets).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableHolder(T attribute, T... targets) {
		return Bindings.valueAt(getHolders(attribute, targets).toObservableList(), 0);
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getHolders(T attribute, Serializable value, T... targets) {
		return getHolders(attribute, targets).filter(new IndexFilter(FiltersBuilder.HAS_VALUE, value));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getHolders(T attribute, T... targets) {
		return getHolders(attribute).filter(new IndexFilter(FiltersBuilder.COMPOSITE_HAS_COMPONENTS, (Object[]) addThisToTargets(targets)));
	}

	@Override
	default Snapshot<T> getHolders(T attribute, int pos) {
		return getHolders(attribute).filter(new IndexFilter(FiltersBuilder.HAS_COMPONENT_AT_POS, this, pos));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getHolders(T attribute) {
		T nonHeritableProperty = getKey(NonHeritableProperty.class, ApiStatics.NO_POSITION);
		if (nonHeritableProperty == null || attribute.inheritsFrom(nonHeritableProperty) || attribute.isInheritanceEnabled())
			return new Snapshot<T>() {

			InheritanceComputer<T> inheritanceComputer = new InheritanceComputer<>((T) DefaultCompositesInheritance.this, attribute, ApiStatics.CONCRETE);

			@Override
			public Stream<T> unfilteredStream() {
				return inheritanceComputer.inheritanceStream();
			}

			@Override
			public Observable<T> getAddsObservable() {
				return inheritanceComputer.getAddsObservable();
			}

			@Override
			public Observable<T> getRemovesObservable() {
				return inheritanceComputer.getRemovesObservable();
			}
		};
		return DefaultCompositesInheritance.this.getComposites().filter(new IndexFilter(FiltersBuilder.IS_SPECIALIZATION_OF, attribute)).filter(new IndexFilter(FiltersBuilder.HAS_LEVEL, ApiStatics.CONCRETE));
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getRelation(Serializable value, T... targets) {
		return getNonAmbiguousResult(getRelations(value, targets).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableRelation(Serializable value, T... targets) {
		return Bindings.valueAt(getRelations(value, targets).toObservableList(), 0);
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getRelation(T... targets) {
		return getNonAmbiguousResult(getRelations(targets).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableRelation(T... targets) {
		return Bindings.valueAt(getRelations(targets).toObservableList(), 0);
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getRelations(Serializable value, T... targets) {
		return getRelations(targets).filter(new IndexFilter(FiltersBuilder.HAS_VALUE, value));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getRelations(T... targets) {
		return getRelations(getRoot().getMetaRelation()).filter(new IndexFilter(FiltersBuilder.COMPOSITE_HAS_COMPONENTS, (Object[]) addThisToTargets(targets)));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getRelations(int pos) {
		return getRelations().filter(new IndexFilter(FiltersBuilder.HAS_COMPONENT_AT_POS, this, pos));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getRelations(T relation) {
		return ((T) this).getAttributes(relation);
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getLink(T relation, Serializable value, T... targets) {
		return getNonAmbiguousResult(getLinks(relation, value, targets).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableLink(T relation, Serializable value, T... targets) {
		return Bindings.valueAt(getLinks(relation, value, targets).toObservableList(), 0);
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getLink(T relation, T... targets) {
		return getNonAmbiguousResult(getLinks(relation, targets).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableLink(T relation, T... targets) {
		return Bindings.valueAt(getLinks(relation, targets).toObservableList(), 0);
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getLinks(T relation, Serializable value, T... targets) {
		return getLinks(relation, targets).filter(new IndexFilter(FiltersBuilder.HAS_VALUE, value));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getLinks(T relation, T... targets) {
		return getLinks(relation).filter(new IndexFilter(FiltersBuilder.COMPOSITE_HAS_COMPONENTS, (Object[]) addThisToTargets(targets)));
	}

	@Override
	default Snapshot<T> getLinks(T relation, int pos) {
		return getLinks(relation).filter(new IndexFilter(FiltersBuilder.HAS_COMPONENT_AT_POS, this, pos));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getLinks(T relation) {
		return ((T) this).getHolders(relation);
	}

	@SuppressWarnings("unchecked")
	@Override
	default Serializable getValue(T attribute, Serializable value, T... targets) {
		T holder = getHolder(attribute, value, targets);
		return holder != null ? holder.getValue() : null;
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<Serializable> getObservableValue(T attribute, Serializable value, T... targets) {
		return Bindings.createObjectBinding(() -> {
			ObservableValue<T> holder = getObservableHolder(attribute, value, targets);
			return holder.getValue() != null ? holder.getValue().getValue() : null;
		}, getObservableHolder(attribute, value, targets));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Serializable getValue(T attribute, T... targets) {
		T holder = getHolder(attribute, targets);
		return holder != null ? holder.getValue() : null;
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<Serializable> getObservableValue(T attribute, T... targets) {
		return Bindings.createObjectBinding(() -> {
			ObservableValue<T> holder = getObservableHolder(attribute, targets);
			return holder.getValue() != null ? holder.getValue().getValue() : null;
		}, getObservableHolder(attribute, targets));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<Serializable> getValues(T attribute, Serializable value, T... targets) {
		return new Snapshot<Serializable>() {

			@Override
			public Stream<Serializable> unfilteredStream() {
				return getLinks(attribute, value, targets).stream().map(x -> x.getValue());
			}

			@Override
			public Observable<Serializable> getAddsObservable() {
				return getLinks(attribute, value, targets).getAddsObservable().map(h -> h.getValue()).replay().refCount();
			}

			@Override
			public Observable<Serializable> getRemovesObservable() {
				return getLinks(attribute, value, targets).getRemovesObservable().map(h -> h.getValue()).replay().refCount();
			}
		};
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<Serializable> getValues(T attribute, T... targets) {
		return new Snapshot<Serializable>() {

			@Override
			public Stream<Serializable> unfilteredStream() {
				return getLinks(attribute, targets).stream().map(x -> x.getValue());
			}

			@Override
			public Observable<Serializable> getAddsObservable() {
				return getLinks(attribute, targets).getAddsObservable().map(h -> h.getValue()).replay().refCount();
			}

			@Override
			public Observable<Serializable> getRemovesObservable() {
				return getLinks(attribute, targets).getRemovesObservable().map(h -> h.getValue()).replay().refCount();
			}
		};
	}

	@Override
	default Snapshot<Serializable> getValues(T attribute, int pos) {
		return new Snapshot<Serializable>() {

			@Override
			public Stream<Serializable> unfilteredStream() {
				return getHolders(attribute, pos).stream().map(x -> x.getValue());
			}

			@Override
			public Observable<Serializable> getAddsObservable() {
				return getHolders(attribute, pos).getAddsObservable().map(h -> h.getValue()).replay().refCount();
			}

			@Override
			public Observable<Serializable> getRemovesObservable() {
				return getHolders(attribute, pos).getRemovesObservable().map(h -> h.getValue()).replay().refCount();
			}
		};
	}

	@SuppressWarnings("unchecked")
	static <T extends DefaultGeneric<T>> Predicate<T> componentsFilter(T... componentsReached) {
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

	@SuppressWarnings("unchecked")
	default T getLinkTargetComponent(T relation, T... targets) {
		T link = getLink(relation, targets);
		return link != null ? link.getTargetComponent() : null;
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableLinkTargetComponent(T relation, T... targets) {
		return Bindings.createObjectBinding(() -> {
			ObservableValue<T> link = getObservableLink(relation, targets);
			return link.getValue() != null ? link.getValue().getTargetComponent() : null;
		}, getObservableLink(relation, targets));
	}

	@SuppressWarnings("unchecked")
	default T getLinkTargetComponent(T relation, Serializable value, T... targets) {
		T link = getLink(relation, value, targets);
		return link != null ? link.getTargetComponent() : null;
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservablecLinkTargetComponent(T relation, Serializable value, T... targets) {
		return Bindings.createObjectBinding(() -> {
			ObservableValue<T> link = getObservableLink(relation, value, targets);
			return link.getValue() != null ? link.getValue().getTargetComponent() : null;
		}, getObservableLink(relation, value, targets));
	}
}
