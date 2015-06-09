package org.genericsystem.api.core;

import java.io.Serializable;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.json.JsonObject;

import org.genericsystem.api.core.annotations.constraints.InstanceValueGenerator.ValueGenerator;
import org.genericsystem.api.core.exceptions.RollbackException;

/**
 * Represents a node of the graph.
 * 
 * @author Nicolas Feybesse
 *
 * @param <T>
 *            the implementation of IVertex used for all nodes.
 */
public interface IVertex<T extends IVertex<T>> extends ISignature<T> {

	/**
	 * Indicates whether this vertex is the root of the graph.
	 *
	 * @return <code>true</code> if this signature is the root of the graph, <code>false</code> otherwise.
	 */
	boolean isRoot();

	/**
	 * Returns the signature of the root of the graph.
	 *
	 * @return the signature of the root of the graph.
	 */
	IRoot<T> getRoot();

	/**
	 * Indicates whether this signature is alive.
	 *
	 * It means, as appropriate, this exact instance should be directly reachable from the graph and not killed at a given moment.
	 *
	 * @return <code>true</code> if this signature is alive, <code>false</code> otherwise.
	 */
	boolean isAlive();

	/**
	 * Indicates whether this signature is system. System signatures are created at startup and can not be remove.
	 * 
	 * @return <code>true</code> if this signature is system, <code>false</code> otherwise.
	 */
	boolean isSystem();

	/**
	 * Technical method for creating a real array of <code>T</code> implementation for passing safe varags parameter and avoid heap pollution.
	 *
	 * @param array
	 *            array of object.
	 * @return an array of T.
	 */
	T[] coerceToTArray(Object... array);

	/**
	 * Utility method for creating a real array of <code>T</code> implementation with <code>this</code> in first position and <code>targets</code> after.
	 *
	 * @param targets
	 *            an array of targets stored in an objects array
	 *
	 * @return a real array of <code>T</code> implementation augmented of this Vertex in first position.
	 */
	@SuppressWarnings("unchecked")
	T[] addThisToTargets(T... targets);

	/**
	 * Utility method for create a real array of <code>T</code> implementation with <code>this</code> in first position, <code>firstTarget</code> in second position and <code>otherTargets</code> after.
	 * 
	 * @param firstTarget
	 *            the first target.
	 * @param otherTargets
	 *            an array of targets stored in an objects array.
	 *
	 * @return a real array of <code>T</code> implementation augmented of this Vertex in first position, <code>firstTarget</code> in second and then <code>otherTargets</code>.
	 */
	@SuppressWarnings("unchecked")
	T[] addThisToTargets(T firstTarget, T... otherTargets);

	/**
	 * Returns the meta level of this vertex.
	 *
	 * @return the meta level : 0 for {@link ApiStatics#META META} level, 1 for {@link ApiStatics#STRUCTURAL STRUCTURAL} and 2 for {@link ApiStatics#CONCRETE CONCRETE}.
	 */
	int getLevel();

	/**
	 * Indicates if the meta level of this vertex is {@link ApiStatics#META META}.
	 *
	 * @return <code>true</code> if the meta level of this vertex is 0, <code>false</code> otherwise.
	 */
	boolean isMeta();

	/**
	 * Indicates if the meta level of this vertex is {@link ApiStatics#STRUCTURAL STRUCTURAL}.
	 *
	 * @return <code>true</code> if the meta level of this vertex is 1, <code>false</code> otherwise.
	 */
	boolean isStructural();

	/**
	 * Indicates if the meta level of this vertex is {@link ApiStatics#CONCRETE CONCRETE}.
	 *
	 * @return <code>true</code> if the meta level of this vertex is 2, <code>false</code> otherwise.
	 */
	boolean isConcrete();

	/**
	 * Indicates whether this vertex "inherits from" another.
	 *
	 * @param superVertex
	 *            the vertex reference to be tested for inheritance.
	 * @return <code>true</code> if this vertex inherits from <code>superVertex</code>, <code>false</code> otherwise.
	 */
	boolean inheritsFrom(T superVertex);

	/**
	 * Indicates whether this vertex "is instance of" another.
	 *
	 * @param metaVertex
	 *            the vertex reference to be tested for instantiation.
	 * @return <code>true</code> if this vertex is instance of <code>metaVertex</code>, <code>false</code> otherwise.
	 */
	boolean isInstanceOf(T metaVertex);

	/**
	 * Indicates whether this vertex "inherits from" or "is instance of" or "is instance of instance of" another.
	 *
	 * @param vertex
	 *            the vertex reference to be tested for specialization.
	 * @return <code>true</code> if this vertex is a specialization of <code>vertex</code>, <code>false</code> otherwise.
	 */
	boolean isSpecializationOf(T vertex);

	/**
	 * Returns if exists the attribute of this (meta) vertex. The returned attribute satisfies the specified <code>value</code> and <code>targets</code>.
	 *
	 * @param value
	 *            the value of returned attribute.
	 * @param targets
	 *            the targets of returned attribute.
	 * @return an attribute if exists, <code>null</code> otherwise.
	 */
	@SuppressWarnings("unchecked")
	T getAttribute(Serializable value, T... targets);

	/**
	 * Returns if exists the attribute of this (meta) vertex. The returned attribute satisfies the specified <code>targets</code>.
	 *
	 * @param targets
	 *            the targets of returned attribute.
	 * @return an attribute if exists, <code>null</code> otherwise.
	 */
	@SuppressWarnings("unchecked")
	T getAttribute(T... targets);

	/**
	 * Returns if exists the holder of this (meta) vertex. The returned holder satisfies the specified <code>attribute</code>, <code>value</code> and <code>targets</code>.
	 * 
	 * @param attribute
	 *            the attribute from which retrieve the holder.
	 * @param value
	 *            the value of returned holder.
	 * @param targets
	 *            the targets of returned holder.
	 * @return a holder if exists, <code>null</code> otherwise.
	 */
	@SuppressWarnings("unchecked")
	T getHolder(T attribute, Serializable value, T... targets);

	/**
	 * Returns if exists the relation of this (meta) vertex. The returned relation satisfies the specified <code>value</code> and <code>targets</code>.
	 *
	 * @param value
	 *            the value of returned relation.
	 * @param targets
	 *            the targets of returned relation.
	 * @return a relation if exists, <code>null</code> otherwise.
	 */
	@SuppressWarnings("unchecked")
	T getRelation(Serializable value, T... targets);

	/**
	 * Returns if exists the relation of this (meta) vertex. The returned relation satisfies the specified <code>targets</code>.
	 *
	 * @param targets
	 *            the targets of returned relation.
	 * @return a relation if exists, <code>null</code> otherwise.
	 */
	@SuppressWarnings("unchecked")
	T getRelation(T... targets);

	/**
	 * Returns if exists the link of this (meta) vertex. The returned link satisfies the specified <code>relation</code>, <code>value</code> and <code>targets</code>.
	 * 
	 * @param relation
	 *            the relation from which retrieve the link.
	 * @param value
	 *            the value of returned link.
	 * @param targets
	 *            the targets of returned link.
	 * @return a link if exists, <code>null</code> otherwise.
	 */
	@SuppressWarnings("unchecked")
	T getLink(T relation, Serializable value, T... targets);

	/**
	 * Returns if exists the holder of this (meta) vertex. The returned holder satisfies the specified <code>attribute</code> and <code>targets</code>.
	 * 
	 * @param attribute
	 *            the attribute from which retrieve the holder.
	 * @param targets
	 *            the targets of returned holder.
	 * @return a holder if exists, <code>null</code> otherwise.
	 */
	@SuppressWarnings("unchecked")
	T getHolder(T attribute, T... targets);

	/**
	 * Returns if exists the link of this (meta) vertex. The returned link satisfies the specified <code>relation</code> and <code>targets</code>.
	 * 
	 * @param relation
	 *            the relation from which retrieve the link.
	 * @param targets
	 *            the targets of returned link.
	 * @return a link if exists, <code>null</code> otherwise.
	 */
	@SuppressWarnings("unchecked")
	T getLink(T relation, T... targets);

	/**
	 * Indicates whether this vertex has a component that is a specialization of <code>vertex</code>.
	 *
	 * @param vertex
	 *            the vertex reference to be tested for attribution.
	 * @return <code>true</code> if this vertex has a component that is a specialization of <code>vertex</code>.
	 */
	boolean isCompositeOf(T vertex);

	/**
	 * Returns the attributes of this vertex (directly if this vertex is a type, the attributes of its type if this vertex is an instance).<br>
	 * The returned attributes satisfies the specified <code>targets</code>.
	 *
	 * @return the attributes of this vertex regardless of the position of this vertex in the components of these attributes.
	 */
	Snapshot<T> getAttributes();

	/**
	 * Returns the attributes of this vertex (directly if this vertex is a type, the attributes of its type if this vertex is an instance).<br>
	 * The returned attributes satisfies the specified <code>targets</code>.
	 *
	 * @param targets
	 *            the targets of returned attribute.
	 *
	 * @return the attributes of this vertex regardless of the position of this vertex in the components of these attributes.
	 */
	@SuppressWarnings("unchecked")
	Snapshot<T> getAttributes(T... targets);

	/**
	 * Returns the attributes of this vertex (directly if this vertex is a type, the attributes of its type if this vertex is an instance) for which this vertex is in the specified position in their components.
	 *
	 * @param pos
	 *            the expected position of this vertex in the components of these attributes.
	 *
	 * @return the attributes of this vertex.
	 */
	Snapshot<T> getAttributes(int pos);

	/**
	 * Returns the attributes of this vertex that inherit from the specified <code>attribute</code>.
	 *
	 * @param attribute
	 *            the attribute from which the returned attributes inherit
	 *
	 * @return the attributes of this vertex regardless of the position of this vertex in their components.
	 */
	Snapshot<T> getAttributes(T attribute);

	/**
	 * Returns the attributes of this vertex (directly if this vertex is a type, the attributes of its type if this vertex is an instance).<br>
	 * The returned attributes satisfies the specified <code>value</code> and <code>targets</code>.
	 *
	 * @param value
	 *            the value of returned attribute.
	 * @param targets
	 *            the targets of returned attribute.
	 *
	 * @return the attributes of this vertex regardless of the position of this vertex in the components of these attributes.
	 */
	@SuppressWarnings("unchecked")
	Snapshot<T> getAttributes(Serializable value, T... targets);

	/**
	 * Returns the holders of this vertex that are instances of the specified <code>attribute</code>.
	 *
	 * @param attribute
	 *            the attribute of which the returned holders are instances.
	 *
	 * @return the holders of this vertex regardless of the position of this vertex in their components.
	 */
	Snapshot<T> getHolders(T attribute);

	/**
	 * Returns the holders of this vertex that are instances of the specified attribute and for which this vertex is in the specified position in the components of these holders.
	 *
	 * @param attribute
	 *            the attribute of which the returned holders are instances.
	 * @param pos
	 *            the expected position of this vertex in the components of these holders.
	 *
	 * @return the holders of this vertex for the specified <code>attribute</code> and <code>position</code>.
	 */
	Snapshot<T> getHolders(T attribute, int pos);

	/**
	 * Returns the holders of this vertex that are instances of the specified attribute and for which this vertex is in the specified position in the components of these holders. <br>
	 * The returned holders satisfies the specified <code>value</code> and <code>targets</code>.
	 * 
	 * @param attribute
	 *            the attribute of which the returned holders are instances.
	 * @param value
	 *            the value of returned holders.
	 * @param targets
	 *            the targets of returned holders.
	 *
	 * @return the holders of this vertex for the specified <code>attribute</code> and <code>position</code>.
	 */
	@SuppressWarnings("unchecked")
	Snapshot<T> getHolders(T attribute, Serializable value, T... targets);

	/**
	 * Returns the holders of this vertex that are instances of the specified attribute and for which this vertex is in the specified position in the components of these holders. <br>
	 * The returned holders satisfies the specified <code>targets</code>.
	 * 
	 * @param attribute
	 *            the attribute of which the returned holders are instances.
	 * @param targets
	 *            the targets of returned holders.
	 *
	 * @return the holders of this vertex for the specified <code>attribute</code> and <code>position</code>.
	 */
	@SuppressWarnings("unchecked")
	Snapshot<T> getHolders(T attribute, T... targets);

	/**
	 * Returns the relations of this vertex that inherit from the specified <code>relation</code>. <br>
	 * The returned relations satisfies the specified <code>targets</code>.
	 * 
	 * @param targets
	 *            the targets of returned relations.
	 *
	 * @return the relations of this vertex regardless of the position of this vertex in their components.
	 */
	@SuppressWarnings("unchecked")
	Snapshot<T> getRelations(T... targets);

	/**
	 * Returns the relations of this vertex (directly if this vertex is a type, the relations of its type if this vertex is an instance) for which this vertex is in the specified position in their components.
	 *
	 * @param pos
	 *            the expected position of this vertex in the components of these relations.
	 *
	 * @return the relations of this vertex.
	 */
	Snapshot<T> getRelations(int pos);

	/**
	 * Returns the relations of this vertex that inherit from the specified <code>relation</code>.
	 *
	 * @param relation
	 *            the relation from which the returned relations inherit.
	 *
	 * @return the relations of this vertex regardless of the position of this vertex in their components.
	 */
	Snapshot<T> getRelations(T relation);

	/**
	 * Returns the relations of this vertex that inherit from the specified <code>relation</code>. <br>
	 * The returned relations satisfies the specified <code>value</code> and <code>targets</code>.
	 * 
	 * @param value
	 *            the value of returned relations.
	 * @param targets
	 *            the targets of returned relations.
	 *
	 * @return the relations of this vertex regardless of the position of this vertex in their components.
	 */
	@SuppressWarnings("unchecked")
	Snapshot<T> getRelations(Serializable value, T... targets);

	/**
	 * Returns the links of this vertex that are instances of the specified <code>relation</code>.
	 *
	 * @param relation
	 *            the relation of which the returned links are instances.
	 *
	 * @return the links of this vertex regardless of the position of this vertex in their components.
	 */
	Snapshot<T> getLinks(T relation);

	/**
	 * Returns the links of this vertex that are instances of the specified <code>relation</code> and for which this vertex is in the specified position in the components of these links.
	 *
	 * @param relation
	 *            the relation of which the returned links are instances.
	 * @param pos
	 *            the expected position of this vertex in the components of these links.
	 *
	 * @return the links of this vertex for the specified <code>relation</code> and <code>position</code>.
	 */
	Snapshot<T> getLinks(T relation, int pos);

	/**
	 * Returns the links of this vertex that are instances of the specified <code>relation</code> and for which this vertex is in the specified position in the components of these links.<br>
	 * The returned links satisfies the specified <code>value</code> and <code>targets</code>.
	 * 
	 * @param relation
	 *            the relation of which the returned links are instances.
	 * @param value
	 *            the value of returned links.
	 * @param targets
	 *            the targets of returned links.
	 *
	 * @return the links of this vertex for the specified <code>relation</code> and <code>position</code>.
	 */
	@SuppressWarnings("unchecked")
	Snapshot<T> getLinks(T relation, Serializable value, T... targets);

	/**
	 * Returns the links of this vertex that are instances of the specified <code>relation</code> and for which this vertex is in the specified position in the components of these links.<br>
	 * The returned links satisfies the specified <code>targets</code>.
	 * 
	 * @param relation
	 *            the relation of which the returned links are instances.
	 * @param targets
	 *            the targets of returned links.
	 *
	 * @return the links of this vertex for the specified <code>relation</code> and <code>position</code>.
	 */
	@SuppressWarnings("unchecked")
	Snapshot<T> getLinks(T relation, T... targets);

	/**
	 * Return value for each holder that is instance of the specified <code>attribute</code> and position and for which this vertex is in the specified position in its components. <br>
	 * The returned values satisfies the specified <code>targets</code>.
	 * 
	 * @param attribute
	 *            the attribute of which value holders are instances.
	 * @param targets
	 *            the targets of returned values.
	 *
	 *
	 * @return value for each holder that is instance of the specified <code>attribute</code> and position.
	 */
	@SuppressWarnings("unchecked")
	Serializable getValue(T attribute, T... targets);

	/**
	 * Returns values for each holder that is instance of the specified <code>attribute</code> and position and for which this vertex is in the specified position in its components. <br>
	 * The returned values satisfies the specified <code>targets</code>.
	 * 
	 * @param attribute
	 *            the attribute of which value holders are instances.
	 * @param targets
	 *            the targets of returned values.
	 *
	 *
	 * @return values for each holder that is instance of the specified <code>attribute</code> and position.
	 */
	@SuppressWarnings("unchecked")
	Snapshot<Serializable> getValues(T attribute, T... targets);

	/**
	 * Returns values for each holder that is instance of the specified <code>attribute</code> and position and for which this vertex is in the specified position in its components.
	 *
	 * @param attribute
	 *            the attribute of which value holders are instances.
	 *
	 * @param pos
	 *            the expected position of this vertex in the components of the holders.
	 *
	 * @return values for each holder that is instance of the specified <code>attribute</code> and position.
	 */
	Snapshot<Serializable> getValues(T attribute, int pos);

	/**
	 * Return value for each holder that is instance of the specified <code>attribute</code> and position and for which this vertex is in the specified position in its components. <br>
	 * The returned values satisfies the specified <code>value</code> and <code>targets</code>.
	 * 
	 * @param attribute
	 *            the attribute of which value holders are instances.
	 * @param value
	 *            the value of returned values.
	 * @param targets
	 *            the targets of returned values.
	 *
	 *
	 * @return value for each holder that is instance of the specified <code>attribute</code> and position.
	 */
	@SuppressWarnings("unchecked")
	Serializable getValue(T attribute, Serializable value, T... targets);

	/**
	 * Returns values for each holder that is instance of the specified <code>attribute</code> and position and for which this vertex is in the specified position in its components. <br>
	 * The returned values satisfies the specified <code>value</code> and <code>targets</code>.
	 * 
	 * @param attribute
	 *            the attribute of which value holders are instances.
	 * @param value
	 *            the value of returned values.
	 * @param targets
	 *            the targets of returned values.
	 *
	 *
	 * @return values for each holder that is instance of the specified <code>attribute</code> and position.
	 */
	@SuppressWarnings("unchecked")
	Snapshot<Serializable> getValues(T attribute, Serializable value, T... targets);

	/**
	 * Returns if exists an instance of this (meta) vertex.<br>
	 * The returned vertex satisfies the specified <code>value</code> and <code>components</code>.
	 *
	 * @param value
	 *            the value of returned vertex.
	 * @param components
	 *            the components of returned vertex.
	 * @return a vertex if exists, <code>null</code> otherwise.
	 */
	@SuppressWarnings("unchecked")
	T getInstance(Serializable value, T... components);

	/**
	 * Returns if exists an instance of this (meta) vertex.<br>
	 * The returned vertex satisfies the specified <code>components</code>.
	 *
	 * @param components
	 *            the components of returned vertex.
	 * @return a vertex if exists, <code>null</code> otherwise.
	 */
	@SuppressWarnings("unchecked")
	T getInstance(T... components);

	/**
	 * Returns if exists an instance of this (meta) vertex. The returned vertex satisfies the specified <code>super</code>, <code>value</code> and <code>components</code>.<br>
	 * Note that the returned vertex if any, inherits from the <code>override</code> specified but can have more or more precise in an undefined order.
	 *
	 * @param value
	 *            the value of returned vertex.
	 * @param override
	 *            the override of returned vertex.
	 * @param components
	 *            the components of returned vertex.
	 * @return a vertex if exists, <code>null</code> otherwise.
	 */
	@SuppressWarnings("unchecked")
	T getInstance(T override, Serializable value, T... components);

	/**
	 * Returns if exists an instance of this (meta) vertex. The returned vertex satisfies the specified <code>overrides</code>, <code>value</code> and <code>components</code>.<br>
	 * Note that the returned vertex if any, inherits from any vertex specified in the list of <code>overrides</code> but can have more or more precise supers in an undefined order.
	 *
	 * @param overrides
	 *            the list of overrides of returned vertex.
	 * @param value
	 *            the value of returned vertex.
	 * @param components
	 *            the components of returned vertex.
	 * @return a vertex if exists, <code>null</code> otherwise.
	 */
	@SuppressWarnings("unchecked")
	T getInstance(List<T> overrides, Serializable value, T... components);

	/**
	 * Returns vertices that have this vertex as meta.<br>
	 * The returned vertex satisfies the specified <code>value</code> and <code>components</code>.
	 * <p>
	 * To get all vertices that are instances of this vertex, consider <code>getAllInstances()</code>.
	 * </p>
	 * 
	 * @param value
	 *            the value of returned vertex.
	 * @param components
	 *            the targets of returned vertex.
	 *
	 * @return the vertices that have this vertex for meta.
	 */
	@SuppressWarnings("unchecked")
	Snapshot<T> getInstances(Serializable value, T... components);

	/**
	 * Returns vertices that have this vertex as meta.
	 * <p>
	 * To get all vertices that are instances of this vertex, consider <code>getAllInstances()</code>.
	 * </p>
	 *
	 * @return the vertices that have this vertex for meta.
	 */
	Snapshot<T> getInstances();

	/**
	 * Returns vertices that have this vertex as meta.<br>
	 * The returned vertex satisfies the specified <code>components</code>.
	 * <p>
	 * To get all vertices that are instances of this vertex, consider <code>getAllInstances()</code>.
	 * </p>
	 * 
	 * @param components
	 *            the targets of returned vertex.
	 *
	 * @return the vertices that have this vertex for meta.
	 */
	@SuppressWarnings("unchecked")
	Snapshot<T> getInstances(T... components);

	/**
	 * Returns vertices that have this vertex as meta.<br>
	 * The returned vertex satisfies the specified <code>override</code>, <code>value</code> and <code>components</code>.
	 * <p>
	 * To get all vertices that are instances of this vertex, consider <code>getAllInstances()</code>.
	 * </p>
	 * 
	 * @param override
	 *            the override of returned vertex.
	 * @param value
	 *            the value of returned vertex.
	 * @param components
	 *            the components of returned vertex.
	 *
	 * @return the vertices that have this vertex for meta.
	 */
	@SuppressWarnings("unchecked")
	Snapshot<T> getInstances(T override, Serializable value, T... components);

	/**
	 * Returns vertices that have this vertex as meta.<br>
	 * The returned vertex satisfies the specified <code>overrides</code>, <code>value</code> and <code>components</code>.
	 * <p>
	 * To get all vertices that are instances of this vertex, consider <code>getAllInstances()</code>.
	 * </p>
	 * 
	 * @param overrides
	 *            the overrides of returned vertex.
	 * @param value
	 *            the value of returned vertex.
	 * @param components
	 *            the components of returned vertex.
	 *
	 * @return the vertices that have this vertex for meta.
	 */
	@SuppressWarnings("unchecked")
	Snapshot<T> getInstances(List<T> overrides, Serializable value, T... components);

	/**
	 * Return vertex that are instances of this vertex.<br>
	 * The returned vertex satisfies the specified <code>value</code> and <code>components</code>.
	 *
	 * @param value
	 *            the value of returned vertex.
	 * @param components
	 *            the targets of returned vertex.
	 *
	 * @return the vertex that are instances of this vertex.
	 */
	@SuppressWarnings("unchecked")
	T getSubInstance(Serializable value, T... components);

	/**
	 * Returns vertices that are instances of this vertex.<br>
	 * The returned vertex satisfies the specified <code>value</code> and <code>components</code>.
	 *
	 * @param value
	 *            the value of returned vertex.
	 * @param components
	 *            the targets of returned vertex.
	 *
	 * @return the vertices that are instances of this vertex.
	 */
	@SuppressWarnings("unchecked")
	Snapshot<T> getSubInstances(Serializable value, T... components);

	/**
	 * Return vertex that are instances of this vertex.<br>
	 * The returned vertex satisfies the specified <code>components</code>.
	 *
	 * @param components
	 *            the targets of returned vertex.
	 *
	 * @return the vertex that are instances of this vertex.
	 */
	@SuppressWarnings("unchecked")
	T getSubInstance(T... components);

	/**
	 * Returns vertices that are instances of this vertex.<br>
	 * The returned vertex satisfies the specified <code>components</code>.
	 *
	 * @param components
	 *            the targets of returned vertex.
	 *
	 * @return the vertices that are instances of this vertex.
	 */
	@SuppressWarnings("unchecked")
	Snapshot<T> getSubInstances(T... components);

	/**
	 * Returns vertices that are instances of this vertex.
	 *
	 * @return the vertices that are instances of this vertex.
	 */
	Snapshot<T> getSubInstances();

	/**
	 * Return vertex that are instances of this vertex.<br>
	 * The returned vertex satisfies the specified <code>override</code>, <code>value</code> and <code>components</code>.
	 *
	 * @param override
	 *            the override of returned vertex.
	 * @param value
	 *            the value of returned vertex.
	 * @param components
	 *            the targets of returned vertex.
	 *
	 * @return the vertex that are instances of this vertex.
	 */
	@SuppressWarnings("unchecked")
	T getSubInstance(T override, Serializable value, T... components);

	/**
	 * Returns vertices that are instances of this vertex.<br>
	 * The returned vertex satisfies the specified <code>override</code>, <code>value</code> and <code>components</code>.
	 *
	 * @param override
	 *            the override of returned vertex.
	 * @param value
	 *            the value of returned vertex.
	 * @param components
	 *            the targets of returned vertex.
	 *
	 * @return the vertices that are instances of this vertex.
	 */
	@SuppressWarnings("unchecked")
	Snapshot<T> getSubInstances(T override, Serializable value, T... components);

	/**
	 * Return vertex that are instances of this vertex.<br>
	 * The returned vertex satisfies the specified <code>overrides</code>, <code>value</code> and <code>components</code>.
	 *
	 * @param overrides
	 *            the overrides of returned vertex.
	 * @param value
	 *            the value of returned vertex.
	 * @param components
	 *            the targets of returned vertex.
	 *
	 * @return the vertex that are instance of this vertex.
	 */
	@SuppressWarnings("unchecked")
	T getSubInstance(List<T> overrides, Serializable value, T... components);

	/**
	 * Returns vertices that are instances of this vertex.<br>
	 * The returned vertex satisfies the specified <code>overrides</code>, <code>value</code> and <code>components</code>.
	 *
	 * @param overrides
	 *            the overrides of returned vertex.
	 * @param value
	 *            the value of returned vertex.
	 * @param components
	 *            the targets of returned vertex.
	 *
	 * @return the vertices that are instances of this vertex.
	 */
	@SuppressWarnings("unchecked")
	Snapshot<T> getSubInstances(List<T> overrides, Serializable value, T... components);

	/**
	 * Return vertex that have this vertex as super.<br>
	 * The returned vertex satisfies the specified <code>value</code> and <code>components</code>.
	 * <p>
	 * To get all vertices that inherit from this vertex, consider <code>getAllInheritings()</code>.
	 * </p>
	 * 
	 * @param value
	 *            the value of returned vertex.
	 * @param components
	 *            the targets of returned vertex.
	 *
	 * @return the vertex that have this vertex for super.
	 */
	@SuppressWarnings("unchecked")
	T getInheriting(Serializable value, T... components);

	/**
	 * Return vertex that have this vertex as super.<br>
	 * The returned vertex satisfies the specified <code>value</code> and <code>components</code>.
	 * <p>
	 * To get all vertices that inherit from this vertex, consider <code>getAllInheritings()</code>.
	 * </p>
	 * 
	 * @param components
	 *            the targets of returned vertex.
	 *
	 * @return the vertex that have this vertex for super.
	 */
	@SuppressWarnings("unchecked")
	T getInheriting(T... components);

	/**
	 * Returns vertices that have this vertex as super.<br>
	 * The returned vertex satisfies the specified <code>value</code> and <code>components</code>.
	 * <p>
	 * To get all vertices that inherit from this vertex, consider <code>getAllInheritings()</code>.
	 * </p>
	 * 
	 * @param value
	 *            the value of returned vertex.
	 * @param components
	 *            the targets of returned vertex.
	 *
	 * @return the vertices that have this vertex for super.
	 */
	@SuppressWarnings("unchecked")
	Snapshot<T> getInheritings(Serializable value, T... components);

	/**
	 * Returns vertices that have this vertex as super.<br>
	 * The returned vertex satisfies the specified <code>components</code>.
	 * <p>
	 * To get all vertices that inherit from this vertex, consider <code>getAllInheritings()</code>.
	 * </p>
	 * 
	 * @param components
	 *            the targets of returned vertex.
	 *
	 * @return the vertices that have this vertex for super.
	 */
	@SuppressWarnings("unchecked")
	Snapshot<T> getInheritings(T... components);

	// TODO KK java.lang.ClassCastException: [Lorg.genericsystem.api.core.IVertex; cannot be cast to [Lorg.genericsystem.defaults.DefaultVertex
	/**
	 * Returns vertices that have this vertex as super.<br>
	 * <p>
	 * To get all vertices that inherit from this vertex, consider <code>getAllInheritings()</code>.
	 * </p>
	 * 
	 *
	 * @return the vertices that have this vertex for super.
	 */
	Snapshot<T> getInheritings();

	/**
	 * Returns vertex that inherit from this vertex.<br>
	 * The returned vertex satisfies the specified <code>value</code> and <code>components</code>.
	 * 
	 * @param value
	 *            the value of returned vertex.
	 * @param components
	 *            the targets of returned vertex.
	 *
	 * @return the vertex that inherits from this vertex.
	 */
	@SuppressWarnings("unchecked")
	T getSubInheriting(Serializable value, T... components);

	/**
	 * Returns vertices that inherit from this vertex.<br>
	 * The returned vertex satisfies the specified <code>value</code> and <code>components</code>.
	 * 
	 * @param value
	 *            the value of returned vertex.
	 * @param components
	 *            the targets of returned vertex.
	 *
	 * @return the vertices that inherits from this vertex.
	 */
	@SuppressWarnings("unchecked")
	Snapshot<T> getSubInheritings(Serializable value, T... components);

	/**
	 * Returns vertex that inherit from this vertex.<br>
	 * The returned vertex satisfies the specified <code>components</code>.
	 * 
	 * @param components
	 *            the targets of returned vertex.
	 *
	 * @return the vertex that inherits from this vertex.
	 */
	@SuppressWarnings("unchecked")
	T getSubInheriting(T... components);

	/**
	 * Returns vertices that inherit from this vertex.<br>
	 * The returned vertex satisfies the specified <code>components</code>.
	 * 
	 * @param components
	 *            the targets of returned vertex.
	 *
	 * @return the vertices that inherits from this vertex.
	 */
	@SuppressWarnings("unchecked")
	Snapshot<T> getSubInheritings(T... components);

	/**
	 * Returns vertices that inherit from this vertex.
	 *
	 * @return the vertices that inherits from this vertex.
	 */
	Snapshot<T> getSubInheritings();

	/**
	 * Returns composite vertices for which this vertex is a component.<br>
	 * The returned vertex satisfies the specified <code>value</code>.
	 * 
	 * @param value
	 *            the value of returned vertex.
	 *
	 * @return the composite vertices.
	 */
	T getComposite(Serializable value);

	/**
	 * Returns composite vertices for which this vertex is a component.<br>
	 * The returned vertex satisfies the specified <code>value</code>.
	 * 
	 * @param value
	 *            the value of returned vertex.
	 *
	 * @return the composite vertices.
	 */
	Snapshot<T> getComposites(Serializable value);

	/**
	 * Returns composite vertices for which this vertex is a component.
	 *
	 * @return the composite vertices.
	 */
	Snapshot<T> getComposites();

	/**
	 * Indicates whether this vertex is ancestor of the specified dependency.
	 * <p>
	 * The ancestors of a node are recursively :
	 * </p>
	 * <ul>
	 * <li>its meta,</li>
	 * <li>its supers,</li>
	 * <li>its components.</li>
	 * </ul>
	 *
	 * @param dependency
	 *            the dependency of which this vertex is an ancestor.
	 *
	 * @return <code>true</code> if this vertex is ancestor of the specified dependency, <code>false</code> otherwise.
	 */
	boolean isAncestorOf(T dependency);

	/**
	 * Returns a <code>String</code> representation of this vertex in the format :
	 * 
	 * <pre>
	 * (meta)[supers]value[components]
	 * </pre>
	 *
	 * @return the <code>String</code> representation of this vertex.
	 */
	String info();

	/**
	 * Returns a <code>String</code> detailed representation of this vertex.
	 *
	 * @return the <code>String</code> detailed representation of this vertex.
	 */
	String detailedInfo();

	/**
	 * Returns a <code>String</code> pretty representation of the composites of this vertex.
	 *
	 * @return the <code>String</code> representation of the composites of this vertex.
	 */
	String toPrettyString();

	/**
	 * Returns a <code>JSon</code> representation of the composites of this vertex.
	 *
	 * @return the <code>JSon</code> representation of the composites of this vertex.
	 */
	JsonObject toPrettyJSon();

	/**
	 * Represents a system property in Generic System.
	 */
	public static interface SystemProperty {

	}

	/**
	 * Returns the property value of this vertex for the specified system property and position.
	 *
	 * @param propertyClass
	 *            the <code>Class</code> of the property.
	 * @param pos
	 *            the position of this vertex in components to consider for axed properties, for example : {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION}, {@link ApiStatics#BASE_POSITION ApiStatics.BASE_POSITION}, {@link ApiStatics#TARGET_POSITION
	 *            ApiStatics.TARGET_POSITION}... Use {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION} for no axed properties.
	 * @return the property value.
	 */
	Serializable getSystemPropertyValue(Class<? extends SystemProperty> propertyClass, int pos);

	/**
	 * Set the property value of this vertex for the specified system property, position and <code>targets</code>.
	 *
	 * @param propertyClass
	 *            the <code>Class</code> of the system property.
	 * @param pos
	 *            the position of this vertex in components to consider for axed properties, for example : {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION}, {@link ApiStatics#BASE_POSITION ApiStatics.BASE_POSITION}, {@link ApiStatics#TARGET_POSITION
	 *            ApiStatics.TARGET_POSITION}... Use {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION} for no axed properties.
	 * @param value
	 *            the property value to set.
	 * @param targets
	 *            the targets for the system property.
	 * @return <code>this</code>.
	 */
	@SuppressWarnings("unchecked")
	T setSystemPropertyValue(Class<? extends SystemProperty> propertyClass, int pos, Serializable value, T... targets);

	/**
	 * Enable this vertex for the specified boolean system property, position and <code>targets</code>.
	 *
	 * @param propertyClass
	 *            the <code>Class</code> of the boolean system property.
	 * @param pos
	 *            the position of this vertex in components to consider for axed properties, for example : {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION}, {@link ApiStatics#BASE_POSITION ApiStatics.BASE_POSITION}, {@link ApiStatics#TARGET_POSITION
	 *            ApiStatics.TARGET_POSITION}... Use {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION} for no axed properties.
	 * @param targets
	 *            the targets for the system property.
	 * @return <code>this</code>.
	 */
	@SuppressWarnings("unchecked")
	T enableSystemProperty(Class<? extends SystemProperty> propertyClass, int pos, T... targets);

	/**
	 * Disable this vertex for the specified boolean system property, position and <code>targets</code>.
	 *
	 * @param propertyClass
	 *            the <code>Class</code> of the boolean system property.
	 * @param pos
	 *            the position of this vertex in components to consider for axed properties, for example : {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION}, {@link ApiStatics#BASE_POSITION ApiStatics.BASE_POSITION}, {@link ApiStatics#TARGET_POSITION
	 *            ApiStatics.TARGET_POSITION}... Use {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION} for no axed properties.
	 * @param targets
	 *            the targets for the system property.
	 * @return <code>this</code>.
	 */
	@SuppressWarnings("unchecked")
	T disableSystemProperty(Class<? extends SystemProperty> propertyClass, int pos, T... targets);

	/**
	 * Indicates whether this vertex is enabled for the specified boolean system property and position.
	 *
	 * @param propertyClass
	 *            the <code>Class</code> of the boolean system property.
	 * @param pos
	 *            the position of this vertex in components to consider for axed properties, for example : {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION}, {@link ApiStatics#BASE_POSITION ApiStatics.BASE_POSITION}, {@link ApiStatics#TARGET_POSITION
	 *            ApiStatics.TARGET_POSITION}... Use {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION} for no axed properties.
	 *
	 * @return <code>true</code> if this vertex is enabled for the specified boolean system property and position, <code>false</code> otherwise.
	 */
	boolean isSystemPropertyEnabled(Class<? extends SystemProperty> propertyClass, int pos);

	/**
	 * Enable the referential integrity constraint of this vertex for the specified position.
	 *
	 * @param pos
	 *            the position of this vertex in components to consider for axed properties, for example : {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION}, {@link ApiStatics#BASE_POSITION ApiStatics.BASE_POSITION}, {@link ApiStatics#TARGET_POSITION
	 *            ApiStatics.TARGET_POSITION}... Use {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION} for no axed properties.
	 *
	 * @return <code>this</code>.
	 */
	T enableReferentialIntegrity(int pos);

	/**
	 * Disable the referential integrity constraint of this vertex for the specified position.
	 *
	 * @param pos
	 *            the position of this vertex in components to consider for axed properties, for example : {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION}, {@link ApiStatics#BASE_POSITION ApiStatics.BASE_POSITION}, {@link ApiStatics#TARGET_POSITION
	 *            ApiStatics.TARGET_POSITION}... Use {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION} for no axed properties.
	 *
	 * @return <code>this</code>.
	 */
	T disableReferentialIntegrity(int pos);

	/**
	 * Indicates whether this vertex is referential integrity for the specified position.
	 *
	 * @param pos
	 *            the position of this vertex in components to consider for axed properties, for example : {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION}, {@link ApiStatics#BASE_POSITION ApiStatics.BASE_POSITION}, {@link ApiStatics#TARGET_POSITION
	 *            ApiStatics.TARGET_POSITION}... Use {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION} for no axed properties.
	 * @return <code>true</code> if this vertex is referential integrity, <code>false</code> otherwise.
	 */
	boolean isReferentialIntegrityEnabled(int pos);

	/**
	 * Enable the singular constraint of this vertex for the specified position.
	 *
	 * @param pos
	 *            the position of this vertex in components to consider for axed properties, for example : {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION}, {@link ApiStatics#BASE_POSITION ApiStatics.BASE_POSITION}, {@link ApiStatics#TARGET_POSITION
	 *            ApiStatics.TARGET_POSITION}... Use {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION} for no axed properties.
	 *
	 * @return <code>this</code>.
	 */
	T enableSingularConstraint(int pos);

	/**
	 * Disable the singular constraint of this vertex for the specified position.
	 *
	 * @param pos
	 *            the position of this vertex in components to consider for axed properties, for example : {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION}, {@link ApiStatics#BASE_POSITION ApiStatics.BASE_POSITION}, {@link ApiStatics#TARGET_POSITION
	 *            ApiStatics.TARGET_POSITION}... Use {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION} for no axed properties.
	 *
	 * @return <code>this</code>.
	 */
	T disableSingularConstraint(int pos);

	/**
	 * Indicates whether this vertex is singular constraint for the specified position.
	 *
	 * @param pos
	 *            the position of this vertex in components to consider for axed properties, for example : {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION}, {@link ApiStatics#BASE_POSITION ApiStatics.BASE_POSITION}, {@link ApiStatics#TARGET_POSITION
	 *            ApiStatics.TARGET_POSITION}... Use {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION} for no axed properties.
	 * @return <code>true</code> if this vertex is singular constraint, <code>false</code> otherwise.
	 */
	boolean isSingularConstraintEnabled(int pos);

	/**
	 * Enable the property constraint of this vertex.
	 *
	 * @return <code>this</code>.
	 */
	T enablePropertyConstraint();

	/**
	 * Disable the property constraint of this vertex.
	 *
	 * @return <code>this</code>.
	 */
	T disablePropertyConstraint();

	/**
	 * Indicates whether this vertex is property constraint.
	 *
	 * @return <code>true</code> if this vertex is property constraint, <code>false</code> otherwise.
	 */
	boolean isPropertyConstraintEnabled();

	/**
	 * Enable the unique value constraint of this vertex.
	 *
	 * @return <code>this</code>.
	 */
	T enableUniqueValueConstraint();

	/**
	 * Disable the unique value constraint of this vertex.
	 *
	 * @return <code>this</code>.
	 */
	T disableUniqueValueConstraint();

	/**
	 * Indicates whether this vertex is unique value constraint.
	 *
	 * @return <code>true</code> if this vertex is unique value constraint, <code>false</code> otherwise.
	 */
	boolean isUniqueValueEnabled();

	/**
	 * Get the <code>Class</code> value constraint of this vertex.
	 *
	 * @return the <code>Class</code> constraint.
	 */
	Class<? extends Serializable> getInstanceValueClassConstraint();

	/**
	 * Set the <code>Class</code> value constraint of this vertex.
	 *
	 * @param constraintClass
	 *            the <code>Class</code> value of the constraint.
	 * @return <code>this</code>.
	 */
	T setInstanceValueClassConstraint(Class<? extends Serializable> constraintClass);

	/**
	 * Get the <code>Class</code> value generator of this vertex.
	 *
	 * @return the <code>Class</code> constraint.
	 */
	Class<? extends ValueGenerator> getInstanceValueGenerator();

	/**
	 * Set the <code>Class</code> value generator of this vertex.
	 *
	 * @param instanceValueGeneratorClass
	 *            the <code>Class</code> value of the generator for instance values.
	 * @return <code>this</code>.
	 */
	T setInstanceValueGenerator(Class<? extends ValueGenerator> instanceValueGeneratorClass);

	/**
	 * Enable the required constraint of this vertex for the specified position.
	 *
	 * @param pos
	 *            the position of this vertex in components to consider for axed properties, for example : {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION}, {@link ApiStatics#BASE_POSITION ApiStatics.BASE_POSITION}, {@link ApiStatics#TARGET_POSITION
	 *            ApiStatics.TARGET_POSITION}... Use {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION} for no axed properties.
	 *
	 * @return <code>this</code>.
	 */
	T enableRequiredConstraint(int pos);

	/**
	 * Disable the required constraint of this vertex for the specified position.
	 *
	 * @param pos
	 *            the position of this vertex in components to consider for axed properties, for example : {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION}, {@link ApiStatics#BASE_POSITION ApiStatics.BASE_POSITION}, {@link ApiStatics#TARGET_POSITION
	 *            ApiStatics.TARGET_POSITION}... Use {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION} for no axed properties.
	 *
	 * @return <code>this</code>.
	 */
	T disableRequiredConstraint(int pos);

	/**
	 * Indicates whether this vertex is required constraint for the specified position.
	 *
	 * @param pos
	 *            the position of this vertex in components to consider for axed properties, for example : {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION}, {@link ApiStatics#BASE_POSITION ApiStatics.BASE_POSITION}, {@link ApiStatics#TARGET_POSITION
	 *            ApiStatics.TARGET_POSITION}... Use {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION} for no axed properties.
	 * @return <code>true</code> if this vertex is required constraint, <code>false</code> otherwise.
	 */
	boolean isRequiredConstraintEnabled(int pos);

	/**
	 * Enable the cascade remove property of this vertex for the specified position.
	 *
	 * @param pos
	 *            the position of this vertex in components to consider for axed properties, for example : {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION}, {@link ApiStatics#BASE_POSITION ApiStatics.BASE_POSITION}, {@link ApiStatics#TARGET_POSITION
	 *            ApiStatics.TARGET_POSITION}... Use {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION} for no axed properties.
	 *
	 * @return <code>this</code>.
	 */
	T enableCascadeRemove(int pos);

	/**
	 * Disable the cascade remove property of this vertex for the specified position.
	 *
	 * @param pos
	 *            the position of this vertex in components to consider for axed properties, for example : {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION}, {@link ApiStatics#BASE_POSITION ApiStatics.BASE_POSITION}, {@link ApiStatics#TARGET_POSITION
	 *            ApiStatics.TARGET_POSITION}... Use {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION} for no axed properties.
	 *
	 * @return <code>this</code>.
	 */
	T disableCascadeRemove(int pos);

	/**
	 * Indicates whether this vertex is cascade remove for the specified position.
	 *
	 * @param pos
	 *            the position of this vertex in components to consider for axed properties, for example : {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION}, {@link ApiStatics#BASE_POSITION ApiStatics.BASE_POSITION}, {@link ApiStatics#TARGET_POSITION
	 *            ApiStatics.TARGET_POSITION}... Use {@link ApiStatics#NO_POSITION ApiStatics.NO_POSITION} for no axed properties.
	 * @return <code>true</code> if this vertex is cascade remove, <code>false</code> otherwise.
	 */
	boolean isCascadeRemoveEnabled(int pos);

	/**
	 * Enable the inheritance on this vertex.
	 *
	 * @return <code>this</code>.
	 */
	T enableInheritance();

	/**
	 * Disable inheritance on this vertex.
	 *
	 * @return <code>this</code>.
	 */
	T disableInheritance();

	/**
	 * Indicates whether this vertex is heritable.
	 *
	 * @return <code>true</code> if this vertex is heritable, <code>false</code> otherwise.
	 */
	boolean isInheritanceEnabled();

	/**
	 * Removes this vertex.
	 *
	 * @throws RollbackException
	 *             if this vertex is not alive or the operation violates an integrity constraint.
	 *
	 */
	void remove();

	/**
	 * Returns a new instance of this type that satisfies the specified <code>value</code> and <code>components</code>.
	 *
	 * @param value
	 *            the expected value.
	 * @param components
	 *            the expected component references.
	 * @return the new instance.
	 *
	 * @throws RollbackException
	 *             if the instance already exists.
	 */
	@SuppressWarnings("unchecked")
	T addInstance(Serializable value, T... components);

	/**
	 * Returns a new instance of this type that satisfies the specified <code>override</code>, <code>value</code> and <code>components</code>.
	 *
	 * @param override
	 *            a vertex reference from which the returned instance shall inherit.
	 * @param value
	 *            the expected value.
	 * @param components
	 *            the expected component references.
	 * @return the new instance.
	 */
	@SuppressWarnings("unchecked")
	T addInstance(T override, Serializable value, T... components);

	/**
	 * Returns a new instance of this type that satisfies the specified <code>overrides</code>, <code>value</code> and <code>components</code>.
	 *
	 * @param overrides
	 *            vertex references from which the returned instance shall inherit.
	 * @param value
	 *            the expected value.
	 * @param components
	 *            the expected component references.
	 * @return the new instance.
	 */
	@SuppressWarnings("unchecked")
	T addInstance(List<T> overrides, Serializable value, T... components);

	/**
	 * Returns an existing or a new instance of this type that satisfies the specified <code>value</code> and <code>components</code>.
	 *
	 * @param value
	 *            the expected value.
	 * @param components
	 *            the expected component references.
	 * @return a new instance or the existing instance that satisfies the specified <code>value</code> and <code>components</code>.
	 */
	@SuppressWarnings("unchecked")
	T setInstance(Serializable value, T... components);

	/**
	 * Returns an existing or a new instance of this type that satisfies the specified <code>override</code>, <code>value</code> and <code>components</code>.
	 *
	 * @param override
	 *            a vertex reference from which the returned instance shall inherit.
	 * @param value
	 *            the expected value.
	 * @param components
	 *            the expected component references.
	 * @return a new instance or the existing instance that satisfies the specified <code>override</code>, <code>value</code> and <code>components</code>.
	 */
	@SuppressWarnings("unchecked")
	T setInstance(T override, Serializable value, T... components);

	/**
	 * Returns an existing or a new instance of this type that satisfies the specified <code>overrides</code>, <code>value</code> and <code>components</code>.
	 *
	 * @param overrides
	 *            vertex references from which the returned instance shall inherit.
	 * @param value
	 *            the expected value.
	 * @param components
	 *            the expected component references.
	 * @return a new instance or the existing instance that satisfies the specified <code>overrides</code>, <code>value</code> and <code>components</code>.
	 */
	@SuppressWarnings("unchecked")
	T setInstance(List<T> overrides, Serializable value, T... components);

	/**
	 * Traverse the Tree.
	 *
	 * @param visitor
	 *            The class <code>Visitor</code>.
	 */
	void traverse(Visitor<T> visitor);

	/**
	 * Used for the path of a tree.
	 * <p>
	 * To indicate specific treatments before and after traversing a node, simply redefine respectively methods <code>before</code> and <code>after</code>.
	 * </p>
	 * 
	 * @param <T>
	 *            the implementation of IVertex used for all nodes.
	 */
	public abstract static class Visitor<T extends IVertex<T>> {
		private final Set<T> alreadyVisited = new HashSet<>();

		/**
		 * Effectively traverse the tree from the specified <code>node</code>.
		 * 
		 * @param node
		 *            the node from which start traversing the tree.
		 */
		public void traverse(T node) {
			if (alreadyVisited.add(node)) {
				before(node);
				for (T child : node.getInheritings())
					traverse(child);
				after(node);
			}
		}

		/**
		 * Treatment performed before each node is traversed.
		 * <p>
		 * This implementation does nothing.
		 * </p>
		 * 
		 * @param node
		 *            the node to treat before traversed it.
		 */
		public void before(T node) {

		}

		/**
		 * Treatment performed after each node is traversed.
		 * <p>
		 * This implementation does nothing.
		 * </p>
		 * 
		 * @param node
		 *            the node to treat after traversed it.
		 */
		public void after(T node) {

		}
	}

	/**
	 * Returns a new attribute on this type that satisfies the specified <code>value</code> and <code>targets</code>.
	 *
	 * @param value
	 *            the expected value.
	 * @param targets
	 *            the expected targets references.
	 * @return a new attribute.
	 */
	@SuppressWarnings("unchecked")
	T addAttribute(Serializable value, T... targets);

	/**
	 * Returns a new attribute on this type that satisfies the specified <code>override</code>, <code>value</code> and <code>targets</code>.
	 *
	 * @param override
	 *            a vertex reference from which the returned attribute shall inherit.
	 * @param value
	 *            the expected value.
	 * @param targets
	 *            the expected targets references.
	 * @return a new attribute.
	 */
	@SuppressWarnings("unchecked")
	T addAttribute(T override, Serializable value, T... targets);

	/**
	 * Returns a new attribute on this type that satisfies the specified <code>overrides</code>, <code>value</code> and <code>targets</code>.
	 *
	 * @param overrides
	 *            vertex references from which the returned attribute shall inherit.
	 * @param value
	 *            the expected value.
	 * @param targets
	 *            the expected targets references.
	 * @return a new attribute.
	 */
	@SuppressWarnings("unchecked")
	T addAttribute(List<T> overrides, Serializable value, T... targets);

	/**
	 * Returns a new or the existing attribute on this type that satisfies the specified <code>value</code> and <code>targets</code>.
	 *
	 * @param value
	 *            the expected value.
	 * @param targets
	 *            the expected targets references.
	 * @return a new or the existing attribute.
	 */
	@SuppressWarnings("unchecked")
	T setAttribute(Serializable value, T... targets);

	/**
	 * Returns a new or the existing attribute on this type that satisfies the specified <code>override</code>, <code>value</code> and <code>targets</code>.
	 *
	 * @param override
	 *            a vertex reference from which the returned attribute inherits.
	 * @param value
	 *            the expected value.
	 * @param targets
	 *            the expected targets references.
	 * @return a new or the existing attribute.
	 */
	@SuppressWarnings("unchecked")
	T setAttribute(T override, Serializable value, T... targets);

	/**
	 * Returns a new or the existing attribute on this type that satisfies the specified <code>overrides</code>, <code>value</code> and <code>targets</code>.
	 *
	 * @param overrides
	 *            vertex references from which the returned attribute inherits.
	 * @param value
	 *            the expected value.
	 * @param targets
	 *            the expected targets references.
	 * @return a new or the existing attribute.
	 */
	@SuppressWarnings("unchecked")
	T setAttribute(List<T> overrides, Serializable value, T... targets);

	/**
	 * Returns a new holder on this instance that satisfies the specified <code>value</code> and <code>targets</code>.
	 *
	 * @param attribute
	 *            the attribute of which the result holder is instance.
	 * @param value
	 *            the expected value.
	 * @param targets
	 *            the expected targets references.
	 * @return a new holder.
	 */
	@SuppressWarnings("unchecked")
	T addHolder(T attribute, Serializable value, T... targets);

	/**
	 * Returns a new holder on this instance that satisfies the specified <code>override</code>, <code>value</code> and <code>targets</code>.
	 *
	 * @param attribute
	 *            the attribute of which the result holder is instance.
	 * @param override
	 *            a vertex reference from which the returned holder shall inherit.
	 * @param value
	 *            the expected value.
	 * @param targets
	 *            the expected targets references.
	 * @return a new holder.
	 */
	@SuppressWarnings("unchecked")
	T addHolder(T attribute, T override, Serializable value, T... targets);

	/**
	 * Returns a new holder on this instance that satisfies the specified <code>overrides</code>, <code>value</code> and <code>targets</code>.
	 *
	 * @param attribute
	 *            the attribute of which the result holder is instance.
	 * @param overrides
	 *            vertex references from which the returned holder shall inherit.
	 * @param value
	 *            the expected value.
	 * @param targets
	 *            the expected targets references.
	 * @return a new holder.
	 */
	@SuppressWarnings("unchecked")
	T addHolder(T attribute, List<T> overrides, Serializable value, T... targets);

	/**
	 * Returns a new or the existing holder on this type that satisfies the specified <code>overrides</code>, <code>value</code> and <code>targets</code>.
	 *
	 * @param attribute
	 *            the attribute of which the result holder is instance.
	 * @param value
	 *            the expected value.
	 * @param targets
	 *            the expected targets references.
	 * @return a new or the existing holder.
	 */
	@SuppressWarnings("unchecked")
	T setHolder(T attribute, Serializable value, T... targets);

	/**
	 * Returns a new or the existing holder on this type that satisfies the specified <code>overrides</code>, <code>value</code> and <code>targets</code>.
	 *
	 * @param attribute
	 *            the attribute of which the result holder is instance.
	 * @param override
	 *            vertex reference from which the returned attribute inherits.
	 * @param value
	 *            the expected value.
	 * @param targets
	 *            the expected targets references.
	 * @return a new or the existing holder.
	 */
	@SuppressWarnings("unchecked")
	T setHolder(T attribute, T override, Serializable value, T... targets);

	/**
	 * Returns a new or the existing holder on this type that satisfies the specified <code>overrides</code>, <code>value</code> and <code>targets</code>.
	 *
	 * @param attribute
	 *            the attribute of which the result holder is instance.
	 * @param overrides
	 *            vertex references from which the returned attribute shall inherit.
	 * @param value
	 *            the expected value.
	 * @param targets
	 *            the expected targets references.
	 * @return a new or the existing holder.
	 */
	@SuppressWarnings("unchecked")
	T setHolder(T attribute, List<T> overrides, Serializable value, T... targets);

	/**
	 * Returns a new relation on this type that satisfies the specified <code>value</code>, <code>firstTarget</code> and <code>otherTargets</code>.
	 *
	 * @param value
	 *            the expected value.
	 * @param firstTarget
	 *            the expected first target of the relation.
	 * @param otherTargets
	 *            the expected other targets references.
	 * @return a new relation.
	 */
	@SuppressWarnings("unchecked")
	T addRelation(Serializable value, T firstTarget, T... otherTargets);

	/**
	 * Returns a new relation on this type that satisfies the specified <code>override</code>, <code>value</code>, <code>firstTarget</code> and <code>otherTargets</code>.
	 *
	 * @param override
	 *            a vertex reference from which the returned relation shall inherit.
	 * @param value
	 *            the expected value.
	 * @param firstTarget
	 *            the expected first target of the relation.
	 * @param otherTargets
	 *            the expected other targets references.
	 * @return a new relation.
	 */
	@SuppressWarnings("unchecked")
	T addRelation(T override, Serializable value, T firstTarget, T... otherTargets);

	/**
	 * Returns a new relation on this type that satisfies the specified <code>overrides</code>, <code>value</code>, <code>firstTarget</code> and <code>otherTargets</code>.
	 *
	 * @param overrides
	 *            vertex references from which the returned relation shall inherit.
	 * @param value
	 *            the expected value.
	 * @param firstTarget
	 *            the expected first target of the relation.
	 * @param otherTargets
	 *            the expected other targets references.
	 * @return a new relation.
	 */
	@SuppressWarnings("unchecked")
	T addRelation(List<T> overrides, Serializable value, T firstTarget, T... otherTargets);

	/**
	 * Returns a new or the existing relation on this type that satisfies the specified <code>value</code>, <code>firstTarget</code> and <code>otherTargets</code>.
	 *
	 * @param value
	 *            the expected value.
	 * @param firstTarget
	 *            the expected first target of the relation.
	 * @param otherTargets
	 *            the expected other targets references.
	 * @return a new or the existing relation.
	 */
	@SuppressWarnings("unchecked")
	T setRelation(Serializable value, T firstTarget, T... otherTargets);

	/**
	 * Returns a new or the existing relation on this type that satisfies the specified <code>override</code>, <code>value</code>, <code>firstTarget</code> and <code>otherTargets</code>.
	 *
	 * @param override
	 *            a vertex reference from which the returned relation inherits.
	 * @param value
	 *            the expected value.
	 * @param firstTarget
	 *            the expected first target of the relation.
	 * @param otherTargets
	 *            the expected other targets references.
	 * @return a new or the existing relation.
	 */
	@SuppressWarnings("unchecked")
	T setRelation(T override, Serializable value, T firstTarget, T... otherTargets);

	/**
	 * Returns a new or the existing relation on this type that satisfies the specified <code>overrides</code>, <code>value</code>, <code>firstTarget</code> and <code>otherTargets</code>.
	 *
	 * @param overrides
	 *            vertex references from which the returned relation inherits.
	 * @param value
	 *            the expected value.
	 * @param firstTarget
	 *            the expected first target of the relation.
	 * @param otherTargets
	 *            the expected other targets references.
	 * @return a new or the existing relation.
	 */
	@SuppressWarnings("unchecked")
	T setRelation(List<T> overrides, Serializable value, T firstTarget, T... otherTargets);

	/**
	 * Returns a new link on this instance that satisfies the specified <code>value</code>, <code>firstTarget</code> and <code>otherTargets</code>.
	 *
	 * @param relation
	 *            the relation of which the result link is instance.
	 * @param value
	 *            the expected value (can be null).
	 * @param firstTarget
	 *            the expected first target.
	 * @param otherTargets
	 *            the expected other targets references.
	 * @return a new link.
	 */
	@SuppressWarnings("unchecked")
	T addLink(T relation, Serializable value, T firstTarget, T... otherTargets);

	/**
	 * Returns a new link on this instance that satisfies the specified <code>override</code>, <code>value</code>, <code>firstTarget</code> and <code>otherTargets</code>.
	 *
	 * @param relation
	 *            the relation of which the result link is instance.
	 * @param override
	 *            a vertex reference from which the returned link shall inherit.
	 * @param value
	 *            the expected value (can be null).
	 * @param firstTarget
	 *            the expected first target.
	 * @param otherTargets
	 *            the expected other targets references.
	 * @return a new link.
	 */
	@SuppressWarnings("unchecked")
	T addLink(T relation, T override, Serializable value, T firstTarget, T... otherTargets);

	/**
	 * Returns a new link on this instance that satisfies the specified <code>overrides</code>, <code>value</code>, <code>firstTarget</code> and <code>otherTargets</code>.
	 *
	 * @param relation
	 *            the relation of which the result link is instance.
	 * @param overrides
	 *            vertex references from which the returned link shall inherit.
	 * @param value
	 *            the expected value (can be null).
	 * @param firstTarget
	 *            the expected first target.
	 * @param otherTargets
	 *            the expected other targets references.
	 * @return a new link.
	 */
	@SuppressWarnings("unchecked")
	T addLink(T relation, List<T> overrides, Serializable value, T firstTarget, T... otherTargets);

	/**
	 * Returns a new or the existing link on this type that satisfies the specified <code>overrides</code>, <code>value</code>, <code>firstTarget</code> and <code>otherTargets</code>.
	 *
	 * @param relation
	 *            the relation of which the result link is instance.
	 * @param value
	 *            the expected value (can be null).
	 * @param firstTarget
	 *            the expected first target.
	 * @param otherTargets
	 *            the expected other targets references.
	 * @return a new or the existing link.
	 */
	@SuppressWarnings("unchecked")
	T setLink(T relation, Serializable value, T firstTarget, T... otherTargets);

	/**
	 * Returns a new or the existing link on this type that satisfies the specified <code>override</code>, <code>value</code>, <code>firstTarget</code> and <code>otherTargets</code>.
	 *
	 * @param relation
	 *            the relation of which the result link is instance.
	 * @param override
	 *            vertex reference from which the returned relation inherits.
	 * @param value
	 *            the expected value (can be null).
	 * @param firstTarget
	 *            the expected first target.
	 * @param otherTargets
	 *            the expected other targets references.
	 * @return a new or the existing link.
	 */
	@SuppressWarnings("unchecked")
	T setLink(T relation, T override, Serializable value, T firstTarget, T... otherTargets);

	/**
	 * Returns a new or the existing link on this type that satisfies the specified <code>overrides</code>, <code>value</code>, <code>firstTarget</code> and <code>otherTargets</code>.
	 *
	 * @param relation
	 *            the relation of which the result link is instance.
	 * @param overrides
	 *            vertex references from which the returned relation shall inherit.
	 * @param value
	 *            the expected value (can be null).
	 * @param firstTarget
	 *            the expected first target.
	 * @param otherTargets
	 *            the expected other targets references.
	 * @return a new or the existing link.
	 */
	@SuppressWarnings("unchecked")
	T setLink(T relation, List<T> overrides, Serializable value, T firstTarget, T... otherTargets);

	/**
	 * Update this Generic with the specified <code>newValue</code>.
	 * 
	 * @param newValue
	 *            the new value of this Generic.
	 * @return the updated Generic.
	 */
	T updateValue(Serializable newValue);

	/**
	 * Update the supers of this Generic with the specified <code>overrides</code>.
	 * 
	 * @param overrides
	 *            the new overrides of this Generic.
	 * @return the updated Generic.
	 */
	@SuppressWarnings("unchecked")
	T updateSupers(T... overrides);

	/**
	 * Update a component of this Generic with the specified <code>newTargetComponent</code>.
	 * 
	 * @param newTargetComponent
	 *            the new target component of this Generic.
	 * @param pos
	 *            the position of the new target component in components
	 * @return the updated Generic.
	 */
	T updateComponent(T newTargetComponent, int pos);

	/**
	 * Update the components of this Generic with the specified <code>newComponents</code>.
	 * 
	 * @param newComponents
	 *            the new components of this Generic.
	 * @return the updated Generic.
	 */
	@SuppressWarnings("unchecked")
	T updateComponents(T... newComponents);

	/**
	 * Update this Generic with the specified <code>overrides</code>, <code>newValue</code> and <code>newComponents</code>.
	 * 
	 * @param overrides
	 *            the new overrides of this Generic.
	 * @param newValue
	 *            the new value of this Generic.
	 * @param newComponents
	 *            the new components of this Generic.
	 * @return the updated Generic.
	 */
	@SuppressWarnings("unchecked")
	T update(List<T> overrides, Serializable newValue, T... newComponents);

	/**
	 * Update this Generic with the specified <code>override</code>, <code>newValue</code> and <code>newComponents</code>.
	 * 
	 * @param override
	 *            the new override of this Generic.
	 * @param newValue
	 *            the new value of this Generic.
	 * @param newComponents
	 *            the new components of this Generic.
	 * @return the updated Generic.
	 */
	@SuppressWarnings("unchecked")
	T update(T override, Serializable newValue, T... newComponents);

	/**
	 * Update this Generic with the specified <code>newValue</code> and <code>newComponents</code>.
	 * 
	 * @param newValue
	 *            the new value of this Generic.
	 * @param newComponents
	 *            the new components of this Generic.
	 * @return the updated Generic.
	 */
	@SuppressWarnings("unchecked")
	T update(Serializable newValue, T... newComponents);

	/**
	 * Returns the component at the {@link ApiStatics#BASE_POSITION BASE} position, that is to say the component at the position 0.
	 * 
	 * @return the component at the {@link ApiStatics#BASE_POSITION BASE} position.
	 */
	T getBaseComponent();

	/**
	 * Returns the component at the {@link ApiStatics#TARGET_POSITION TARGET} position, that is to say the component at the position 1.
	 * 
	 * @return the component at the {@link ApiStatics#TARGET_POSITION TARGET} position.
	 */
	T getTargetComponent();

	/**
	 * Returns the component at the {@link ApiStatics#TERNARY_POSITION TERNARY} position, that is to say the component at the position 2.
	 * 
	 * @return the component at the {@link ApiStatics#TERNARY_POSITION TERNARY} position.
	 */
	T getTernaryComponent();

	/**
	 * Returns the component at the specified position.
	 * 
	 * @param pos
	 *            the position of the component to be returned.
	 * @return the component at the specified position.
	 */
	T getComponent(int pos);

	/**
	 * Returns the key for the property at the position
	 * 
	 * @param propertyClass
	 *            the <code>Class</code> of the property.
	 * @param pos
	 *            the position of the property
	 * @return the key for the property at the position
	 */
	T getKey(Class<? extends SystemProperty> propertyClass, int pos);

	/**
	 * Returns the current cache.
	 * 
	 * @return the current cache.
	 */
	IContext<T> getCurrentCache();

}
