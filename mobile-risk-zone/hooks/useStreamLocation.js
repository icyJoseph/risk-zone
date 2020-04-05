import { useEffect } from "react";
import { baseAPI } from "../api";

export function useStreamLocation({ location, error }) {
  useEffect(() => {
    if (!error) {
      // send location to forward-server and from there to our backend
      baseAPI
        .post(
          "report",
          {
            location,
          },
          {
            headers: { "Content-Type": "application/json" },
          }
        )
        .catch(() => console.log("Failed to report"));
    }
  }, [location, error]);

  return null;
}
