import { useState, useEffect } from "react";
import axios from "axios";
import { baseAPI } from "../api";

export function useStreamLocation({ location, error }) {
  const [live, setLive] = useState([]);
  useEffect(() => {
    if (!error) {
      // send location to forward-server and from there to our backend
      // returns the live state
      const source = axios.CancelToken.source();
      baseAPI
        .post(
          "report",
          {
            location,
          },
          {
            headers: { "Content-Type": "application/json" },
            cancelToken: source.token,
          }
        )
        .then(({ data }) => setLive(data))
        .catch(() => console.log("Failed to report"));

      return () => {
        source.cancel("Cancelled by user");
      };
    }
  }, [location, error]);

  return live;
}
