import { createAsyncThunk, createSlice } from "@reduxjs/toolkit";
import {
  deleteAct,
  getActs,
  getSettings,
  updatePassword,
  updateSettings,
} from "../api";
import { Act, initMemo, Keyed, Loadable, Settings } from "../types";

const initialState = {
  settings: initMemo<Settings>({
    prices: { consultation: 45000, emergency: 60000 },
    schedule: {},
  }),
  updateState: "NotRequest" as Loadable<"Ok">,
  acts: initMemo<Keyed<Act>[]>([]),
  search: undefined as string | undefined,
  deleteState: "NotRequested" as Loadable<"Ok">,
  changePasswordState: "NotRequested" as Loadable<"Ok">,
};

export type LocalState = typeof initialState;
type ThunkAPI = { state: { settings: LocalState } };

export const getSettingsAction = createAsyncThunk(
  "settings/getSettings",
  getSettings,
);

export const updateSettingsAction = createAsyncThunk(
  "settings/updateSettings",
  updateSettings,
);

export const searchActsAction = createAsyncThunk(
  "settings/searchActs",
  (search?: string | undefined) => getActs({ search }),
);

export const deleteActAction = createAsyncThunk<void, number, ThunkAPI>(
  "settings/deleteAct",
  async (s, thunkAPI) => {
    await deleteAct(s);
    thunkAPI.dispatch(searchActsAction(thunkAPI.getState().settings.search));
  },
);

export const updatePasswordAction = createAsyncThunk(
  "settings/updatePassword",
  updatePassword,
);

const settingsSlice = createSlice({
  name: "settings",
  initialState,
  reducers: {},
  extraReducers: (b) =>
    b
      .addCase(getSettingsAction.pending, (s) => {
        s.settings.value = "Loading";
      })
      .addCase(getSettingsAction.fulfilled, (s, a) => {
        s.settings.value = a.payload;
        s.settings.memo = a.payload;
      })
      .addCase(getSettingsAction.rejected, (s) => {
        s.settings.value = "NetworkError";
      })
      .addCase(updateSettingsAction.pending, (s) => {
        s.updateState = "Loading";
      })
      .addCase(updateSettingsAction.fulfilled, (s) => {
        s.updateState = "Ok";
      })
      .addCase(updateSettingsAction.rejected, (s) => {
        s.updateState = "NetworkError";
      })
      .addCase(searchActsAction.pending, (s, a) => {
        s.search = a.meta.arg;
        s.acts.value = "Loading";
      })
      .addCase(searchActsAction.fulfilled, (s, a) => {
        s.acts.value = a.payload;
        s.acts.memo = a.payload;
      })
      .addCase(searchActsAction.rejected, (s) => {
        s.acts.value = "NetworkError";
      })
      .addCase(deleteActAction.pending, (s) => {
        s.deleteState = "Loading";
      })
      .addCase(deleteActAction.fulfilled, (s) => {
        s.deleteState = "Ok";
      })
      .addCase(deleteActAction.rejected, (s) => {
        s.deleteState = "NetworkError";
      })
      .addCase(updatePasswordAction.pending, (s) => {
        s.changePasswordState = "Loading";
      })
      .addCase(updatePasswordAction.fulfilled, (s) => {
        s.changePasswordState = "Ok";
      })
      .addCase(updatePasswordAction.rejected, (s) => {
        s.changePasswordState = "NetworkError";
      })
      .addCase("logout/pending", () => initialState),
});

export default settingsSlice.reducer;
