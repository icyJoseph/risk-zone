import React, { useState } from "react";
import { Image, Text } from "react-native";
import { Marker } from "react-native-maps";

export function StopMarker({ latitude, longitude }) {
  const [trackViewChanges, setTrackViewChanges] = useState(true);
  return (
    <Marker
      coordinate={{ latitude, longitude }}
      tracksViewChanges={trackViewChanges}
    >
      <Image
        onLoad={() => setTrackViewChanges(false)}
        fadeDuration={0}
        source={{ uri: "https://freesvg.org/img/Trolleybus.png" }}
        style={{ height: 20, width: 20 }}
      />
    </Marker>
  );
}
