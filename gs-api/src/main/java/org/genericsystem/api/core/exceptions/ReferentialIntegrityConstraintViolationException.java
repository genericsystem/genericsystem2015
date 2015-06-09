package org.genericsystem.api.core.exceptions;

/**
 * Thrown when a user operation violates the constraint of referential integrity.
 * 
 * @author Nicolas Feybesse
 */
public class ReferentialIntegrityConstraintViolationException extends ConstraintViolationException {
	private static final long serialVersionUID = -4066409595001566155L;

	/**
	 * Constructs a <code>ReferentialIntegrityConstraintViolationException</code> with the specified detail message.
	 *
	 * @param message
	 *            the detail message.
	 */
	public ReferentialIntegrityConstraintViolationException(String message) {
		super(message);
	}
}
