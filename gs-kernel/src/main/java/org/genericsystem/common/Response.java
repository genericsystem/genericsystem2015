package org.genericsystem.common;

public class Response<T> {
	private final T result;
	private final Exception exception;
	private final boolean switchedToT;

	public Response(T result) {
		this.switchedToT = true;
		this.exception = null;
		this.result = result;
	}

	public Response(Exception exception) {
		this.switchedToT = false;
		this.exception = exception;
		this.result = null;
	}

	public T getResult() {
		return result;
	}

	public Exception getException() {
		return exception;
	}

	public boolean isSwitchedToT() {
		return switchedToT;
	}
	// public Buffer append ( Buffer buffer) {
	//
	// }
	// public static Response getResponse (Buffer buffer) {
	//
	// }

}
