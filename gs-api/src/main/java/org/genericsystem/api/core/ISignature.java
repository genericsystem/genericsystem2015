package org.genericsystem.api.core;

import java.io.Serializable;
import java.util.List;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;

/**
 * Represents the minimum informations for identifying a IVertex.
 * 
 * @author Nicolas Feybesse
 *
 * @param <T>
 *            the implementation of this interface.
 */
public interface ISignature<T extends ISignature<T>> {

	/**
	 * Returns the meta of this signature.
	 *
	 * @return the <code>Signature</code> for which this signature is an instance.<br>
	 *         This method returns <code>this</code> for the root signature.
	 */
	T getMeta();

	/**
	 * Returns the supers of this signature.
	 *
	 * @return the list of supers signatures.<br>
	 *         The returned list can be empty.
	 */
	List<T> getSupers();

	/**
	 * Returns the value of this signature.
	 *
	 * @return the serializable value of this signature.<br>
	 *         The returned value can be <code>null</code>.
	 */
	Serializable getValue();

	/**
	 * Returns the components of this signature.
	 *
	 * @return the components of this signature.<br>
	 *         The returned list can be empty.
	 */
	List<T> getComponents();

	/**
	 * Returns the <code>JSonId</code> of this signature.
	 *
	 * @return the <code>JSonObject</code> representing this signature.
	 */
	default JsonObject toJSonId() {
		JsonObjectBuilder builder = Json.createObjectBuilder();
		builder.add("Id", System.identityHashCode(this));
		builder.add("Value", toString());
		builder.add("Meta", System.identityHashCode(getMeta()));
		JsonArrayBuilder supersBuilder = Json.createArrayBuilder();
		for (T superVertex : getSupers())
			supersBuilder.add(System.identityHashCode(superVertex));
		builder.add("Supers", supersBuilder);
		JsonArrayBuilder arrayBuilder = Json.createArrayBuilder();
		for (T composite : getComponents())
			arrayBuilder.add(System.identityHashCode(composite));
		builder.add("Components", arrayBuilder);
		return builder.build();
	}
}
