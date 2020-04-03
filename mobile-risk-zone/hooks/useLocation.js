import { useState, useEffect } from "react";
import { Platform } from "react-native";
import Constants from "expo-constants";
import { getCurrentPositionAsync, watchPositionAsync } from "expo-location";
import { askAsync, LOCATION } from "expo-permissions";

const positionOptions = {
  accuracy: 3,
  enableHighAccuracy: true,
  timeInterval: 1000, // milliseconds
  timeout: 10000,
};

const start2Watch = async (watcher, callback) => {
  watcher = await watchPositionAsync(
    { ...positionOptions, distanceInterval: 1 },
    callback
  );

  return watcher;
};

const getLocationAsync = async (onSuccess, onError) => {
  const { status } = await askAsync(LOCATION);
  if (status !== "granted") {
    return onError("Permission to access location was denied");
  }
  const location = await getCurrentPositionAsync(positionOptions);
  return onSuccess(location);
};

const isSketch = Platform.OS === "android" && !Constants.isDevice;

export function useLocation({ watch = false }) {
  const [location, setLocation] = useState(null);
  const [error, setError] = useState(() => {
    if (isSketch) {
      return "Oops, this will not work on Sketch in an Android emulator. Try it on your device!";
    }
    return null;
  });

  useEffect(() => {
    if (!error) {
      getLocationAsync(setLocation, setError);
    } else {
      setLocation(null);
    }
  }, [error]);

  useEffect(() => {
    let watcher = null;

    if (!error && watch) {
      start2Watch(watcher, setLocation);
    }

    return () => {
      if (watcher?.remove) {
        watcher.remove();
      }
    };
  }, [error, watch]);

  return [location, error];
}
