package org.genericsystem.api.core.exceptions;

/**
 * Thrown when an operation of the engine violates the constraint of a Generic <em>NOT</em> to be alive.
 * 
 * @author Nicolas Feybesse
 */
public class NotAliveConstraintViolationException extends ConstraintViolationException {
	private static final long serialVersionUID = 6397180829362547659L;

	/**
	 * Constructs a <code>NotAliveConstraintViolationException</code> with the specified detail message.
	 *
	 * @param message
	 *            the detail message.
	 */
	public NotAliveConstraintViolationException(String message) {
		super(message);
	}
}
