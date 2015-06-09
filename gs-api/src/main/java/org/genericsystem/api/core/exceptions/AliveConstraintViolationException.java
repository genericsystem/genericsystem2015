package org.genericsystem.api.core.exceptions;

/**
 * Thrown when an operation of the engine violates the constraint of a Generic to be alive.<br>
 * For example, adding an attribute to a type that has been removed will throw an AliveConstraintViolationException.
 * 
 * @author Nicolas Feybesse
 */
public class AliveConstraintViolationException extends ConstraintViolationException {
	private static final long serialVersionUID = -4066409595001566155L;

	/**
	 * Constructs an <code>AliveConstraintViolationException</code> with the specified detail message.
	 *
	 * @param message
	 *            the detail message.
	 */
	public AliveConstraintViolationException(String message) {
		super(message);
	}
}
