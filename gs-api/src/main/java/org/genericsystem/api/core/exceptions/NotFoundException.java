package org.genericsystem.api.core.exceptions;

/**
 * Thrown when a component is not found on the specified position for an axed system property.
 * 
 * @author Nicolas Feybesse
 */
public class NotFoundException extends Exception {
	private static final long serialVersionUID = -7472730943638836698L;

	/**
	 * Constructs a <code>NotFoundException</code> with the specified detail message.
	 *
	 * @param message
	 *            the detail message.
	 */
	public NotFoundException(String message) {
		super(message);
	}
}
