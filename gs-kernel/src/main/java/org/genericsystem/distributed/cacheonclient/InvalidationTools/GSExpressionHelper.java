package org.genericsystem.distributed.cacheonclient.InvalidationTools;

import java.util.Arrays;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;

public abstract class GSExpressionHelper extends GSExpressionHelperBase {

	// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Static methods
	public static GSExpressionHelper addListener(GSExpressionHelper helper, Observable observable, InvalidationListener listener) {
		if ((observable == null) || (listener == null)) {
			throw new NullPointerException();
		}

		return (helper == null) ? new SingleInvalidation(observable, listener) : helper.addListener(listener);
	}

	public static GSExpressionHelper removeListener(GSExpressionHelper helper, InvalidationListener listener) {
		if (listener == null) {
			throw new NullPointerException();
		}
		return (helper == null) ? null : helper.removeListener(listener);
	}

	public static void fireValueChangedEvent(GSExpressionHelper helper) {
		if (helper != null) {
			helper.fireValueChangedEvent();
		}
	}

	// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Common implementations

	protected final Observable observable;

	private GSExpressionHelper(Observable observable) {
		this.observable = observable;
	}

	protected abstract GSExpressionHelper addListener(InvalidationListener listener);

	protected abstract GSExpressionHelper removeListener(InvalidationListener listener);

	protected abstract void fireValueChangedEvent();

	// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Implementations

	private static class SingleInvalidation extends GSExpressionHelper {

		private final InvalidationListener listener;

		private SingleInvalidation(Observable expression, InvalidationListener listener) {
			super(expression);
			this.listener = listener;
		}

		@Override
		protected GSExpressionHelper addListener(InvalidationListener listener) {
			return new Generic(observable, this.listener, listener);
		}

		@Override
		protected GSExpressionHelper removeListener(InvalidationListener listener) {
			return (listener.equals(this.listener)) ? null : this;
		}

		@Override
		protected void fireValueChangedEvent() {
			try {
				listener.invalidated(observable);
			} catch (Exception e) {
				Thread.currentThread().getUncaughtExceptionHandler().uncaughtException(Thread.currentThread(), e);
			}
		}
	}

	private static class Generic extends GSExpressionHelper {

		private InvalidationListener[] invalidationListeners;
		private int invalidationSize;
		private boolean locked;

		private Generic(Observable observable, InvalidationListener listener0, InvalidationListener listener1) {
			super(observable);
			this.invalidationListeners = new InvalidationListener[] { listener0, listener1 };
			this.invalidationSize = 2;
		}

		@Override
		protected Generic addListener(InvalidationListener listener) {
			if (invalidationListeners == null) {
				invalidationListeners = new InvalidationListener[] { listener };
				invalidationSize = 1;
			} else {
				final int oldCapacity = invalidationListeners.length;
				if (locked) {
					final int newCapacity = (invalidationSize < oldCapacity) ? oldCapacity : (oldCapacity * 3) / 2 + 1;
					invalidationListeners = Arrays.copyOf(invalidationListeners, newCapacity);
				} else if (invalidationSize == oldCapacity) {
					invalidationSize = trim(invalidationSize, invalidationListeners);
					if (invalidationSize == oldCapacity) {
						final int newCapacity = (oldCapacity * 3) / 2 + 1;
						invalidationListeners = Arrays.copyOf(invalidationListeners, newCapacity);
					}
				}
				invalidationListeners[invalidationSize++] = listener;
			}
			return this;
		}

		@Override
		protected GSExpressionHelper removeListener(InvalidationListener listener) {
			if (invalidationListeners != null) {
				for (int index = 0; index < invalidationSize; index++) {
					if (listener.equals(invalidationListeners[index])) {
						if (invalidationSize == 1) {
							invalidationListeners = null;
							invalidationSize = 0;
						} else if ((invalidationSize == 2)/* && (changeSize == 0) */) {
							return new SingleInvalidation(observable, invalidationListeners[1 - index]);
						} else {
							final int numMoved = invalidationSize - index - 1;
							final InvalidationListener[] oldListeners = invalidationListeners;
							if (locked) {
								invalidationListeners = new InvalidationListener[invalidationListeners.length];
								System.arraycopy(oldListeners, 0, invalidationListeners, 0, index);
							}
							if (numMoved > 0) {
								System.arraycopy(oldListeners, index + 1, invalidationListeners, index, numMoved);
							}
							invalidationSize--;
							if (!locked) {
								invalidationListeners[invalidationSize] = null; // Let gc do its work
							}
						}
						break;
					}
				}
			}
			return this;
		}

		@Override
		protected void fireValueChangedEvent() {
			final InvalidationListener[] curInvalidationList = invalidationListeners;
			final int curInvalidationSize = invalidationSize;

			try {
				locked = true;
				for (int i = 0; i < curInvalidationSize; i++) {
					try {
						curInvalidationList[i].invalidated(observable);
					} catch (Exception e) {
						Thread.currentThread().getUncaughtExceptionHandler().uncaughtException(Thread.currentThread(), e);
					}
				}
			} finally {
				locked = false;
			}
		}
	}
}
