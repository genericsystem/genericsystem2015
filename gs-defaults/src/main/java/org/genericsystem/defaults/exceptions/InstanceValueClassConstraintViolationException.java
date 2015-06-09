package org.genericsystem.defaults.exceptions;

import org.genericsystem.api.core.exceptions.ConstraintViolationException;

/**
 * Thrown when a user operation violates the <code>InstanceValueClassConstraint</code>.
 * <p>
 * The <code>Class</code> for the value of an instance is incorrect.<br>
 * For example, adding a value of type <code>String</code> when the expected type is <code>Integer</code> will throw a InstanceValueClassConstraintViolationException.
 * </p>
 * 
 * @author Nicolas Feybesse
 * @see org.genericsystem.kernel.systemproperty.constraints.InstanceValueClassConstraint
 */
public class InstanceValueClassConstraintViolationException extends ConstraintViolationException {
	private static final long serialVersionUID = -2489391060880812117L;

	/**
	 * Constructs a <code>InstanceValueClassConstraintViolationException</code> with the specified detail message.
	 *
	 * @param message
	 *            the detail message.
	 */
	public InstanceValueClassConstraintViolationException(String message) {
		super(message);
	}
}
