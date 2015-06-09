package org.genericsystem.api.core.exceptions;

/**
 * Thrown when assignments are done on two different engines.
 * 
 * @author Nicolas Feybesse
 */
public class CrossEnginesAssignementsException extends ConstraintViolationException {
	private static final long serialVersionUID = 1L;

	/**
	 * Constructs a <code>CrossEnginesAssignementsException</code> with the specified detail message.
	 *
	 * @param message
	 *            the detail message.
	 */
	public CrossEnginesAssignementsException(String message) {
		super(message);
	}
}
