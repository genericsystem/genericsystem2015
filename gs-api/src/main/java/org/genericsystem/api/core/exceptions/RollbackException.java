package org.genericsystem.api.core.exceptions;

/**
 * The base exception when an operation on Generic System causes a rollback.
 * 
 * @author Nicolas Feybesse
 */
public class RollbackException extends RuntimeException {
	private static final long serialVersionUID = 4600650372617391568L;

	/**
	 * Constructs a <code>RollbackException</code> with the specified cause.
	 * 
	 * @param cause
	 *            the cause of he exception.
	 */
	public RollbackException(Throwable cause) {
		super(cause);
	}
}
