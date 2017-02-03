package org.genericsystem.defaults;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.IGeneric;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.defaults.DefaultConfig.NonHeritableProperty;
import org.genericsystem.defaults.tools.BindingsTools;
import org.genericsystem.defaults.tools.InheritanceComputer;
import org.genericsystem.defaults.tools.ObservableInheritanceComputer;

import javafx.beans.binding.Bindings;
import javafx.beans.binding.ListBinding;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 */
public interface DefaultCompositesInheritance<T extends DefaultGeneric<T>> extends IGeneric<T> {

	// Remove

	ObservableList<T> getObservableComposites();

	//

	@SuppressWarnings("unchecked")
	@Override
	default T getAttribute(Serializable value, T... targets) {
		return getNonAmbiguousResult(getAttributes(value, targets).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableAttribute(Serializable value, T... targets) {
		return BindingsTools
				.transmitSuccessiveInvalidations(Bindings.valueAt(getObservableAttributes(value, targets), 0));
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getAttribute(T... targets) {
		return getNonAmbiguousResult(getAttributes(targets).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableAttribute(T... targets) {
		return BindingsTools.transmitSuccessiveInvalidations(Bindings.valueAt(getObservableAttributes(targets), 0));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getAttributes(Serializable value, T... targets) {
		return getAttributes(targets).filter(DefaultDependencies.valueFilter(value));
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableAttributes(Serializable value, T... targets) {
		return getObservableAttributes(targets).filtered(DefaultDependencies.valueFilter(value));
	}

	@Override
	default Snapshot<T> getAttributes() {
		return getAttributes(getRoot().getMetaAttribute());
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
	default ObservableList<T> getObservableAttributes(T... targets) {
		return getObservableAttributes().filtered(componentsFilter(addThisToTargets(targets)));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getAttributes(int pos) {
		return () -> getAttributes().stream().filter(attribute -> attribute.getComponent(pos) != null
				&& ((T) this).isSpecializationOf(attribute.getComponent(pos)));
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableAttributes(int pos) {
		return getObservableAttributes().filtered(attribute -> attribute.getComponent(pos) != null
				&& ((T) this).isSpecializationOf(attribute.getComponent(pos)));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getAttributes(T attribute) {
		T nonHeritableProperty = getKey(NonHeritableProperty.class, ApiStatics.NO_POSITION);
		if (nonHeritableProperty == null || attribute.inheritsFrom(nonHeritableProperty)
				|| attribute.isInheritanceEnabled())
			return () -> new InheritanceComputer<>((T) DefaultCompositesInheritance.this, attribute,
					ApiStatics.STRUCTURAL).inheritanceStream();
		return () -> this.getComposites().stream()
				.filter(holder -> holder.isSpecializationOf(attribute) && holder.getLevel() == ApiStatics.STRUCTURAL);
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableAttributes(T attribute) {
		T nonHeritableProperty = getKey(NonHeritableProperty.class, ApiStatics.NO_POSITION);
		if (nonHeritableProperty == null || attribute.inheritsFrom(nonHeritableProperty)
				|| attribute.isInheritanceEnabled())
			return BindingsTools.createMinimalUnitaryChangesBinding(new ObservableInheritanceComputer<>(
					(T) DefaultCompositesInheritance.this, attribute, ApiStatics.STRUCTURAL));
		return getObservableComposites()
				.filtered(holder -> holder.isSpecializationOf(attribute) && holder.getLevel() == ApiStatics.STRUCTURAL);
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getHolder(T attribute, Serializable value, T... targets) {
		return getNonAmbiguousResult(getHolders(attribute, value, targets).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableHolder(T attribute, Serializable value, T... targets) {
		return BindingsTools
				.transmitSuccessiveInvalidations(Bindings.valueAt(getObservableHolders(attribute, value, targets), 0));
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getHolder(T attribute, T... targets) {
		return getNonAmbiguousResult(getHolders(attribute, targets).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableHolder(T attribute, T... targets) {
		return BindingsTools
				.transmitSuccessiveInvalidations(Bindings.valueAt(getObservableHolders(attribute, targets), 0));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getHolders(T attribute, Serializable value, T... targets) {
		return getHolders(attribute).filter(DefaultDependencies.valueFilter(value))
				.filter(componentsFilter(addThisToTargets(targets)));
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableHolders(T attribute, Serializable value, T... targets) {
		return getObservableHolders(attribute).filtered(DefaultDependencies.valueFilter(value))
				.filtered(componentsFilter(addThisToTargets(targets)));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getHolders(T attribute, T... targets) {
		return getHolders(attribute).filter(componentsFilter(addThisToTargets(targets)));
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableHolders(T attribute, T... targets) {
		return getObservableHolders(attribute).filtered(componentsFilter(addThisToTargets(targets)));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getHolders(T attribute, int pos) {
		return () -> getHolders(attribute).stream().filter(
				holder -> holder.getComponent(pos) != null && ((T) this).isSpecializationOf(holder.getComponent(pos)));
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableHolders(T attribute, int pos) {
		return getObservableHolders(attribute).filtered(
				holder -> holder.getComponent(pos) != null && ((T) this).isSpecializationOf(holder.getComponent(pos)));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getHolders(T attribute) {
		T nonHeritableProperty = getKey(NonHeritableProperty.class, ApiStatics.NO_POSITION);
		if (nonHeritableProperty == null || attribute.inheritsFrom(nonHeritableProperty)
				|| attribute.isInheritanceEnabled())
			return () -> new InheritanceComputer<>((T) DefaultCompositesInheritance.this, attribute,
					ApiStatics.CONCRETE).inheritanceStream();
		return () -> this.getComposites().stream()
				.filter(holder -> holder.isSpecializationOf(attribute) && holder.getLevel() == ApiStatics.CONCRETE);
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableHolders(T attribute) {
		T nonHeritableProperty = getKey(NonHeritableProperty.class, ApiStatics.NO_POSITION);
		if (nonHeritableProperty == null || attribute.inheritsFrom(nonHeritableProperty)
				|| attribute.isInheritanceEnabled())
			return BindingsTools.createMinimalUnitaryChangesBinding(new ObservableInheritanceComputer<>(
					(T) DefaultCompositesInheritance.this, attribute, ApiStatics.CONCRETE));
		return getObservableComposites()
				.filtered(holder -> holder.isSpecializationOf(attribute) && holder.getLevel() == ApiStatics.CONCRETE);
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getRelation(Serializable value, T... targets) {
		return getNonAmbiguousResult(getRelations(value, targets).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableRelation(Serializable value, T... targets) {
		return BindingsTools
				.transmitSuccessiveInvalidations(Bindings.valueAt(getObservableRelations(value, targets), 0));
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getRelation(T... targets) {
		return getNonAmbiguousResult(getRelations(targets).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableRelation(T... targets) {
		return BindingsTools.transmitSuccessiveInvalidations(Bindings.valueAt(getObservableRelations(targets), 0));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getRelations(Serializable value, T... targets) {
		return getRelations(targets).filter(DefaultDependencies.valueFilter(value));
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableRelations(Serializable value, T... targets) {
		return getObservableRelations(targets).filtered(DefaultDependencies.valueFilter(value));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getRelations(T... targets) {
		return getRelations(getRoot().getMetaRelation()).filter(componentsFilter(addThisToTargets(targets)));
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableRelations(T... targets) {
		return getObservableRelations(getRoot().getMetaRelation())
				.filtered(componentsFilter(addThisToTargets(targets)));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getRelations(int pos) {
		return () -> getRelations().stream().filter(relation -> relation.getComponent(pos) != null
				&& ((T) this).isSpecializationOf(relation.getComponent(pos)));
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableRelations(int pos) {
		return getObservableRelations().filtered(relation -> relation.getComponent(pos) != null
				&& ((T) this).isSpecializationOf(relation.getComponent(pos)));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getRelations(T relation) {
		return ((T) this).getAttributes(relation);
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableRelations(T relation) {
		return ((T) this).getObservableAttributes(relation);
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getLink(T relation, Serializable value, T... targets) {
		return getNonAmbiguousResult(getLinks(relation, value, targets).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableLink(T relation, Serializable value, T... targets) {
		return BindingsTools
				.transmitSuccessiveInvalidations(Bindings.valueAt(getObservableLinks(relation, value, targets), 0));
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getLink(T relation, T... targets) {
		return getNonAmbiguousResult(getLinks(relation, targets).stream());
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservableLink(T relation, T... targets) {
		return BindingsTools
				.transmitSuccessiveInvalidations(Bindings.valueAt(getObservableLinks(relation, targets), 0));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getLinks(T relation, Serializable value, T... targets) {
		return getLinks(relation).filter(DefaultDependencies.valueFilter(value))
				.filter(componentsFilter(addThisToTargets(targets)));
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableLinks(T relation, Serializable value, T... targets) {
		return getObservableLinks(relation).filtered(DefaultDependencies.valueFilter(value))
				.filtered(componentsFilter(addThisToTargets(targets)));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getLinks(T relation, T... targets) {
		return getLinks(relation).filter(componentsFilter(addThisToTargets(targets)));
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableLinks(T relation, T... targets) {
		return getObservableLinks(relation).filtered(componentsFilter(addThisToTargets(targets)));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getLinks(T relation, int pos) {
		return () -> getLinks(relation).stream().filter(
				link -> link.getComponent(pos) != null && ((T) this).isSpecializationOf(link.getComponent(pos)));
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableLinks(T relation, int pos) {
		return getObservableLinks(relation).filtered(
				link -> link.getComponent(pos) != null && ((T) this).isSpecializationOf(link.getComponent(pos)));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<T> getLinks(T relation) {
		return ((T) this).getHolders(relation);
	}

	@SuppressWarnings("unchecked")
	default ObservableList<T> getObservableLinks(T relation) {
		return ((T) this).getObservableHolders(relation);
	}

	@SuppressWarnings("unchecked")
	@Override
	default Serializable getValue(T attribute, Serializable value, T... targets) {
		T holder = getHolder(attribute, value, targets);
		return holder != null ? holder.getValue() : null;
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<Serializable> getObservableValue(T attribute, Serializable value, T... targets) {
		return BindingsTools.transmitSuccessiveInvalidations(Bindings.createObjectBinding(() -> {
			ObservableValue<T> holder = getObservableHolder(attribute, value, targets);
			return holder.getValue() != null ? holder.getValue().getValue() : null;
		}, getObservableHolder(attribute, value, targets)));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Serializable getValue(T attribute, T... targets) {
		T holder = getHolder(attribute, targets);
		return holder != null ? holder.getValue() : null;
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<Serializable> getObservableValue(T attribute, T... targets) {
		return BindingsTools.transmitSuccessiveInvalidations(Bindings.createObjectBinding(() -> {
			ObservableValue<T> holder = getObservableHolder(attribute, targets);
			return holder.getValue() != null ? holder.getValue().getValue() : null;
		}, getObservableHolder(attribute, targets)));
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<Serializable> getValues(T attribute, Serializable value, T... targets) {
		return () -> getLinks(attribute, value, targets).stream().map(x -> x.getValue());
	}

	@SuppressWarnings("unchecked")
	default ObservableList<Serializable> getObservableValues(T attribute, Serializable value, T... targets) {
		return BindingsTools.transmitSuccessiveInvalidations(new ListBinding<Serializable>() {
			private final ObservableList<T> links = getObservableLinks(attribute, value, targets);
			{
				bind(links);
			}

			@Override
			protected ObservableList<Serializable> computeValue() {
				return FXCollections.unmodifiableObservableList(FXCollections
						.observableList(links.stream().map(x -> x.getValue()).collect(Collectors.toList())));
			}
		});
	}

	@SuppressWarnings("unchecked")
	@Override
	default Snapshot<Serializable> getValues(T attribute, T... targets) {
		return () -> getLinks(attribute, targets).stream().map(x -> x.getValue());
	}

	@SuppressWarnings("unchecked")
	default ObservableList<Serializable> getObservableValues(T attribute, T... targets) {
		return BindingsTools.transmitSuccessiveInvalidations(new ListBinding<Serializable>() {
			private final ObservableList<T> links = getObservableLinks(attribute, targets);
			{
				bind(links);
			}

			@Override
			protected ObservableList<Serializable> computeValue() {
				return FXCollections.unmodifiableObservableList(FXCollections
						.observableList(links.stream().map(x -> x.getValue()).collect(Collectors.toList())));
			}
		});
	}

	@Override
	default Snapshot<Serializable> getValues(T attribute, int pos) {
		return () -> getHolders(attribute, pos).stream().map(x -> x.getValue());
	}

	default ObservableList<Serializable> getObservableValues(T attribute, int pos) {
		return BindingsTools.transmitSuccessiveInvalidations(new ListBinding<Serializable>() {
			private final ObservableList<T> holders = getObservableHolders(attribute, pos);
			{
				bind(holders);
			}

			@Override
			protected ObservableList<Serializable> computeValue() {
				return FXCollections.unmodifiableObservableList(FXCollections
						.observableList(holders.stream().map(x -> x.getValue()).collect(Collectors.toList())));

			}
		});
	}

	@SuppressWarnings("unchecked")
	static <T extends DefaultGeneric<T>> Predicate<T> componentsFilter(T... componentsReached) {
		return attribute -> {
			List<T> attributeComps = new ArrayList<>(attribute.getComponents());
			for (T componentReach : componentsReached) {
				T matchedComponent = attributeComps.stream()
						.filter(attributeComp -> componentsReached[0].isSpecializationOf(attributeComp) ? true
								: componentReach.equals(attributeComp))
						.findFirst().orElse(null);
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
		return BindingsTools.transmitSuccessiveInvalidations(Bindings.createObjectBinding(() -> {
			ObservableValue<T> link = getObservableLink(relation, targets);
			return link.getValue() != null ? link.getValue().getTargetComponent() : null;
		}, getObservableLink(relation, targets)));
	}

	@SuppressWarnings("unchecked")
	default T getLinkTargetComponent(T relation, Serializable value, T... targets) {
		T link = getLink(relation, value, targets);
		return link != null ? link.getTargetComponent() : null;
	}

	@SuppressWarnings("unchecked")
	default ObservableValue<T> getObservablecLinkTargetComponent(T relation, Serializable value, T... targets) {
		return BindingsTools.transmitSuccessiveInvalidations(Bindings.createObjectBinding(() -> {
			ObservableValue<T> link = getObservableLink(relation, value, targets);
			return link.getValue() != null ? link.getValue().getTargetComponent() : null;
		}, getObservableLink(relation, value, targets)));
	}
}
