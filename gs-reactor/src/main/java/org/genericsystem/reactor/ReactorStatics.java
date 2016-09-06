package org.genericsystem.reactor;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Base64;

public class ReactorStatics {

	public static final String BACKGROUND = "background";
	public static final String CHECKED = "checked";
	public static final String DISABLED = "disabled";

	public static <T> T fromString(String s) {
		byte[] data = Base64.getDecoder().decode(s);
		ObjectInputStream ois;
		try {
			ois = new ObjectInputStream(
					new ByteArrayInputStream(data));
			Object o = ois.readObject();
			ois.close();
			return (T) o;
		} catch (IOException | ClassNotFoundException e) {
			throw new IllegalStateException(e);
		}
	}

	public static String toString(Serializable o) {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		ObjectOutputStream oos;
		try {
			oos = new ObjectOutputStream(baos);
			oos.writeObject(o);
			oos.close();
		} catch (IOException e) {
			return null;
		}
		return Base64.getEncoder().encodeToString(baos.toByteArray());
	}
}
