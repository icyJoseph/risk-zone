import React, { useState, useMemo } from "react";
import MapView, { PROVIDER_GOOGLE } from "react-native-maps";
import { StyleSheet, View, Dimensions, Button } from "react-native";
import { StopMarker } from "./StopMarker";
import { useLocation } from "../hooks/useLocation";
import { useGeoData } from "../hooks/useGeoData";
import { useNearbyStops } from "../hooks/useNearbyStops";
import { useStreamLocation } from "../hooks/useStreamLocation";

const not = (x) => !x;

export function Map() {
  const [showTraffic, setShowTraffic] = useState(true);
  const [showLive, setShowLive] = useState(true);
  const [showGeoData, setShowGeoData] = useState(true);

  const [location, error] = useLocation({ watch: true });
  const geoDataPoints = useGeoData();
  const stops = useNearbyStops(
    location?.coords ?? { latitude: 57.7118511, longitude: 11.9699815 }
  );

  const live = useStreamLocation({ location, error });

  const toggleTraffic = () => setShowTraffic(not);
  const toggleGeoData = () => setShowGeoData(not);
  const toggleLive = () => setShowLive(not);

  const _stops = useMemo(() => (showTraffic ? stops : []), [
    showTraffic,
    stops,
  ]);

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
        minZoomLevel={12}
        maxZoomLevel={18}
      >
        {_stops.map(({ id, lat, lon }) => (
          <StopMarker
            key={id}
            latitude={parseFloat(lat)}
            longitude={parseFloat(lon)}
          />
        ))}
        {!!geoDataPoints.length && showGeoData && (
          <MapView.Heatmap
            points={geoDataPoints}
            opacity={0.65}
            radius={20}
            maxIntensity={100}
            gradientSmoothing={10}
            heatmapMode={"POINTS_DENSITY"}
          />
        )}
        {!!live.length && showLive && (
          <MapView.Heatmap
            points={live}
            opacity={0.85}
            radius={50}
            maxIntensity={100}
            gradientSmoothing={100}
            heatmapMode={"POINTS_DENSITY"}
          />
        )}
      </MapView>
      <View
        style={{
          position: "absolute",
          display: "flex",
          flex: 1,
          justifyContent: "space-around",
          bottom: 10,
          flexDirection: "row",
          width: "100%",
        }}
      >
        <Button
          title={`Traffic: ${showTraffic ? "ON" : "OFF"}`}
          onPress={toggleTraffic}
        />
        <Button
          title={`Risk: ${showGeoData ? "ON" : "OFF"}`}
          onPress={toggleGeoData}
        />
        <Button
          title={`Live: ${showLive ? "ON" : "OFF"}`}
          onPress={toggleLive}
        />
      </View>
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
