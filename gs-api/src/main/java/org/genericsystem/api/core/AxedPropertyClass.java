package org.genericsystem.api.core;

import java.io.Serializable;

import org.genericsystem.api.core.IVertex.SystemProperty;

/**
 * Represents the <code>Class</code> for an axed property.
 * 
 * @author Nicolas Feybesse
 * @see org.genericsystem.api.core.IVertex.SystemProperty
 */
public class AxedPropertyClass implements Serializable {
	private static final long serialVersionUID = -2631066712866842794L;

	private final Class<? extends SystemProperty> clazz;
	private final int axe;

	/**
	 * Constructs an <code>AxedPropertyClass</code> with the specified <code>clazz</code> and <code>axe</code>.
	 *
	 * @param clazz
	 *            the <code>Class</code> of the <code>SystemProperty</code>.
	 * @param axe
	 *            the position of the <code>SystemProperty</code>.
	 */
	public AxedPropertyClass(Class<? extends SystemProperty> clazz, int axe) {
		this.clazz = clazz;
		this.axe = axe;
	}

	/**
	 * Returns the <code>Class</code> of the <code>SystemProperty</code>.
	 * 
	 * @return the <code>Class</code> of the <code>SystemProperty</code>.
	 */
	public Class<? extends SystemProperty> getClazz() {
		return clazz;
	}

	/**
	 * Returns the axe of the <code>SystemProperty</code>.
	 * 
	 * @return the axe of the <code>SystemProperty</code>.
	 */
	public int getAxe() {
		return axe;
	}

	/**
	 * Compares this <code>AxedPropertyClass</code> to the specified object. The result is <code>true</code> if and only if the argument is not <code>null</code> and is an <code>AxedPropertyClass</code> object that contains the same class and axe as this
	 * object.
	 * 
	 * @param obj
	 *            The object to compare this <code>AxedPropertyClass</code> against.
	 *
	 * @return <code>true</code> if the given object represents a <code>AxedPropertyClass</code> equivalent to this <code>AxedPropertyClass</code>, <code>false</code> otherwise.
	 *
	 */
	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof AxedPropertyClass))
			return false;
		AxedPropertyClass compare = (AxedPropertyClass) obj;
		return clazz.equals(compare.getClazz()) && axe == compare.axe;
	}

	/**
	 * Returns a hash code for this <code>AxedPropertyClass</code>. The hash code for a <code>AxedPropertyClass</code> object is computed as the hash code of the <code>Class</code> of the <code>SystemProperty</code>.
	 *
	 * @return a hash code value for this object.
	 */
	@Override
	public int hashCode() {
		return clazz.hashCode();
	}

	/**
	 * Returns a <code>String</code> object representing this <code>AxedPropertyClass</code>'s value. The string consists of the left brace character '<code>{</code>', the word <code>class</code>, a space character, a colon character '<code>:</code>', a
	 * space character, the name of the class of the system property, the comma character '<code>,</code>', a space character, the word <code>axe</code>, a space character, a colon character '<code>:</code>', the axe for the system property and a right
	 * brace character '<code>}</code>'. In other words, this method returns a string equal to the value of:
	 * 
	 * <pre>
	 * &quot;{class : &quot; + clazz.getSimpleName() + &quot;, axe : &quot; + axe + &quot;}&quot;
	 * </pre>
	 *
	 * @return a string representation of the value of this object describing its content.
	 */
	@Override
	public String toString() {
		return clazz.getSimpleName() + "#" + axe;
	}
}
