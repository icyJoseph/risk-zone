import React from "react";
import { Marker } from "react-native-maps";

import { Image, StyleSheet } from "react-native";

const styles = StyleSheet.create({
  logo: {
    width: 40,
    height: 40,
  },
});

export function StopMarker({ latitude, longitude }) {
  return (
    <Marker coordinate={{ latitude, longitude }}>
      <Image
        style={styles.logo}
        source={require("../assets/images/busicon.png")}
      />
    </Marker>
  );
}
