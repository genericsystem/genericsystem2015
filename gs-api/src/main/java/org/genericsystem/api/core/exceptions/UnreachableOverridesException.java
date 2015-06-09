package org.genericsystem.api.core.exceptions;

/**
 * Thrown when at least an <code>override</code> is <em>NOT</em> inherited by at least one <code>super</code>.
 * 
 * @author Nicolas Feybesse
 */
public class UnreachableOverridesException extends Exception {
	private static final long serialVersionUID = 6403761571054684162L;

	/**
	 * Constructs a <code>UnreachableOverridesException</code> with the specified detail message.
	 *
	 * @param message
	 *            the detail message.
	 */
	public UnreachableOverridesException(String message) {
		super(message);
	}
}
