package org.genericsystem.defaults.exceptions;

import org.genericsystem.api.core.exceptions.ConstraintViolationException;

/**
 * Thrown when a user operation violates the <code>UniqueValueConstraint</code>. The user tries to add a second instance on a type.
 * 
 * @author Nicolas Feybesse
 * @see org.genericsystem.kernel.systemproperty.constraints.UniqueValueConstraint
 */
public class UniqueValueConstraintViolationException extends ConstraintViolationException {
	private static final long serialVersionUID = -5523312075306575631L;

	/**
	 * Constructs a <code>UniqueValueConstraintViolationException</code> with the specified detail message.
	 *
	 * @param message
	 *            the detail message.
	 */
	public UniqueValueConstraintViolationException(String message) {
		super(message);
	}
}
