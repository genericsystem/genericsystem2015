package org.genericsystem.distributed.cacheonclient.InvalidationTools;

import javafx.beans.WeakListener;

public class GSExpressionHelperBase {

	protected static int trim(int size, Object[] listeners) {
		for (int index = 0; index < size; index++) {
			final Object listener = listeners[index];
			if (listener instanceof WeakListener) {
				if (((WeakListener) listener).wasGarbageCollected()) {
					final int numMoved = size - index - 1;
					if (numMoved > 0)
						System.arraycopy(listeners, index + 1, listeners, index, numMoved);
					listeners[--size] = null; // Let gc do its work
					index--;
				}
			}
		}
		return size;
	}

}
