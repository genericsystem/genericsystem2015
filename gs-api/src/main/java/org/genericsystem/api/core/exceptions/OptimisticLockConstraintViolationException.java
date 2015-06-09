package org.genericsystem.api.core.exceptions;

//TODO ???
/**
 * Thrown when an attempt is done to remove an alive Generic.
 * 
 * @author Nicolas Feybesse
 */
public class OptimisticLockConstraintViolationException extends ConstraintViolationException {
	private static final long serialVersionUID = 6347098699041855226L;

	/**
	 * Constructs a <code>OptimisticLockConstraintViolationException</code> with the specified detail message.
	 *
	 * @param message
	 *            the detail message.
	 */
	public OptimisticLockConstraintViolationException(String message) {
		super(message);
	}
}
