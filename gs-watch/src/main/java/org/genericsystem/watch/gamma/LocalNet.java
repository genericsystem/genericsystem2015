package org.genericsystem.watch.gamma;

import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.InterfaceAddress;
import java.net.NetworkInterface;
import java.util.Enumeration;

public class LocalNet {

	public static String getIpAddress() {
		try {
			for (final Enumeration<NetworkInterface> interfaces = NetworkInterface.getNetworkInterfaces(); interfaces.hasMoreElements();) {
				final NetworkInterface cur = interfaces.nextElement();

				if (cur.isLoopback()) {
					continue;
				}
				for (final InterfaceAddress addr : cur.getInterfaceAddresses()) {
					final InetAddress inet_addr = addr.getAddress();
					if (!(inet_addr instanceof Inet4Address)) {
						continue;
					}
					return inet_addr.getHostAddress();
				}
			}
			throw new IllegalStateException("Unable to find a valid local ip address");
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

}
