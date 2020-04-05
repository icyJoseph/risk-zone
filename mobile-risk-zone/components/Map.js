import React from "react";
import MapView, { PROVIDER_GOOGLE } from "react-native-maps";
import { StyleSheet, View, Dimensions } from "react-native";
import { StopMarker } from "./StopMarker";
import { useLocation } from "../hooks/useLocation";
import { useGeoData } from "../hooks/useGeoData";
import { useNearbyStops } from "../hooks/useNearbyStops";
import { useStreamLocation } from "../hooks/useStreamLocation";

export function Map() {
  const [location, error] = useLocation({ watch: true });
  const geoDataPoints = useGeoData();
  const stops = useNearbyStops(
    location?.coords ?? { latitude: 57.7118511, longitude: 11.9699815 }
  );

  useStreamLocation({ location, error });

  const hasPoints = !!geoDataPoints.length;

  return (
    <View style={styles.container}>
      <MapView
        style={styles.mapStyle}
        provider={PROVIDER_GOOGLE}
        initialRegion={{
          latitude: location?.coords?.latitude ?? 57.7118511,
          longitude: location?.coords?.longitude ?? 11.9699815,
          latitudeDelta: 1,
          longitudeDelta: 1,
        }}
        showsUserLocation={true}
        minZoomLevel={10}
        maxZoomLevel={20}
      >
        {stops.map(({ id, lat, lon }) => (
          <StopMarker
            key={id}
            latitude={parseFloat(lat)}
            longitude={parseFloat(lon)}
          />
        ))}
        {hasPoints && (
          <MapView.Heatmap
            points={geoDataPoints}
            opacity={0.65}
            radius={20}
            maxIntensity={100}
            gradientSmoothing={10}
            heatmapMode={"POINTS_DENSITY"}
          />
        )}
      </MapView>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: "#fff",
    alignItems: "center",
    justifyContent: "center",
  },
  mapStyle: {
    width: Dimensions.get("window").width,
    height: Dimensions.get("window").height,
  },
});
