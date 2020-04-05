import { useState, useEffect } from "react";
import axios from "axios";
import { baseAPI } from "../api";

export function useGeoData() {
  const [geoDataPoints, setGeoDataPoints] = useState([]);

  useEffect(() => {
    let source;
    async function loadData() {
      source = axios.CancelToken.source();
      const { data } = await baseAPI
        .post(
          "/geodata",
          {
            city: "gothenburg",
          },
          {
            CancelToken: source.token,
          }
        )
        .catch(() => {
          return { data: [] };
        });

      setGeoDataPoints(data);
    }
    loadData();

    return () => {
      if (source && source.cancel) {
        source.cancel("Request cancelled");
      }
    };
  }, []);

  return geoDataPoints;
}
