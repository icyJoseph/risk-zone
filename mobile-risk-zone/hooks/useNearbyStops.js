import { useState, useEffect } from "react";
import axios from "axios";
import { baseAPI } from "../api";

export function useNearbyStops({ latitude, longitude }) {
  const [stops, setStops] = useState([]);

  useEffect(() => {
    if (latitude && longitude) {
      const source = axios.CancelToken.source();
      baseAPI
        .post(
          "/trafficStopsNearby",
          {
            latitude,
            longitude,
          },
          {
            headers: { "Content-Type": "application/json" },
            cancelToken: source.token,
          }
        )
        .then(({ data: { nearbyStopLocations = {} } }) => {
          setStops(nearbyStopLocations);
        })
        .catch(() => setStops([]));

      return () => {
        source.cancel("Cancelled by user");
      };
    }
  }, [latitude, longitude]);

  return stops;
}
