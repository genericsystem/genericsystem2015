package org.genericsystem.api.core;

import java.io.Serializable;
import java.util.List;

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

	long getTs();

	long getBirthTs();

}
