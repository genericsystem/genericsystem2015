package org.genericsystem.api.core.exceptions;

/**
 * The base exception for all constraint violation exceptions.
 * 
 * @author Nicolas Feybesse
 */
public abstract class ConstraintViolationException extends Exception {
	private static final long serialVersionUID = 4647517844227534027L;

	/**
	 * Constructs a <code>ConstraintViolationException</code> with no detail message.
	 */
	public ConstraintViolationException() {
	}

	/**
	 * Constructs a <code>ConstraintViolationException</code> with the specified detail message.
	 *
	 * @param message
	 *            the detail message.
	 */
	public ConstraintViolationException(String message) {
		super(message);
	}
}
