package org.genericsystem.api.core.exceptions;

/**
 * Thrown when an operation of the engine triggers a collision.<br>
 * For example, suppose we have a <code>vehicle</code> with an attribute <code>power</code>. Finally we try to add an attribute called <code>power</code> and which overrides <code>power</code>. A CollisionException will be thrown.
 * 
 * @author Nicolas Feybesse
 */
public class CollisionException extends Exception {

	private static final long serialVersionUID = -2963478445831656238L;

	/**
	 * Constructs a <code>CollisionException</code> with the specified detail message.
	 *
	 * @param message
	 *            the detail message.
	 */
	public CollisionException(String message) {
		super(message);
	}
}
