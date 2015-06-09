package org.genericsystem.issuetracker.config;

import javax.faces.FacesException;
import javax.faces.application.FacesMessage;
import javax.faces.context.ExceptionHandler;
import javax.faces.context.ExceptionHandlerFactory;
import javax.faces.context.ExceptionHandlerWrapper;
import javax.faces.context.FacesContext;
import javax.faces.event.ExceptionQueuedEvent;
import javax.faces.event.ExceptionQueuedEventContext;

public class MyExceptionHandlerFactory extends ExceptionHandlerFactory {

	private ExceptionHandlerFactory exceptionHandlerFactory;

	public MyExceptionHandlerFactory(final javax.faces.context.ExceptionHandlerFactory parent) {
		this.exceptionHandlerFactory = parent;
	}

	@Override
	public ExceptionHandler getExceptionHandler() {
		return new MyExceptionHandler(exceptionHandlerFactory.getExceptionHandler());
	}

	public static class MyExceptionHandler extends ExceptionHandlerWrapper {

		private ExceptionHandler exceptionHandler;

		public MyExceptionHandler(ExceptionHandler wrapped) {
			this.exceptionHandler = wrapped;
		}

		@Override
		public javax.faces.context.ExceptionHandler getWrapped() {
			return this.exceptionHandler;
		}

		@Override
		public void handle() throws FacesException {
			for (ExceptionQueuedEvent exceptionQueuedEvent : getUnhandledExceptionQueuedEvents()) {
				Throwable throwable = ((ExceptionQueuedEventContext) exceptionQueuedEvent.getSource()).getException();
				if (throwable != null && throwable.getCause() != null)
					FacesContext.getCurrentInstance().addMessage("msg", new FacesMessage(throwable.getCause().toString()));
			}
			getWrapped().handle();
		}
	}

}
